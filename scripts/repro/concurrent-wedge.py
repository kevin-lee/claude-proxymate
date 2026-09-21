#!/usr/bin/env python3
"""Concurrent-load reproduction for the Scala Native trap-yieldpoint wedge.

Usage: python3 scripts/repro/concurrent-wedge.py <binary> <port> [rounds=40]

Background. claude-proxymate-server is a Scala Native binary. When it is linked
in release mode with the multithreaded Commix GC, the runtime uses "trap-based
yieldpoints": every function entry reads a per-thread page that the collector
protects when it wants all threads to stop. On macOS that fault is sometimes not
delivered to the runtime's handler, so a thread spins at the read forever, the
collector waits for it forever, and the whole process stops answering. The
condition only occurs when a collection starts while several requests are being
handled at once, which is why sequential load never reproduces it. See
build.sbt (checkNativeLinkEnv) and the README section "Build the native proxy
binary" for the fix, which links with SCALANATIVE_GC_TRAP_BASED_YIELDPOINTS=0.

What this script does. It starts the binary the same way the Electron app does,
then fires rounds of 8 simultaneous HTTP requests through it. The requests are
deliberately unauthenticated, so the upstream API answers 401 in a fraction of
a second and no real API usage occurs. A healthy proxy returns 401 for every
request in every round. A wedged proxy stops answering, so a request times out
or the connection is refused, and the script prints the collector's own
diagnostics before exiting with status 1.

Expected outcomes:
  binary linked with trap-based yieldpoints (the default):  wedge in rounds 0-2
  binary linked with SCALANATIVE_GC_TRAP_BASED_YIELDPOINTS=0: "no wedge in N rounds"

Python 3 standard library only, macOS only for the per-thread diagnostics.
"""
import concurrent.futures
import http.client
import json
import subprocess
import sys
import threading
import time


def main() -> int:
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    binary, port = sys.argv[1], int(sys.argv[2])
    rounds = int(sys.argv[3]) if len(sys.argv) > 3 else 40

    # Spawn the binary exactly as the Electron main process does (IpcHandlers):
    # all three stdio streams are pipes. --exit-on-stdin-close makes the binary
    # exit when its stdin closes, so it cannot outlive this script if the
    # script dies. stdout carries the proxy's JSON-lines event protocol and
    # stderr carries the Scala Native GC diagnostics we want to capture.
    p = subprocess.Popen(
        [binary, "--port", str(port), "--exit-on-stdin-close"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
    )

    # Both pipes must be drained continuously. A pipe nobody reads fills up
    # after about 64 KiB and then blocks the child's next write, which would be
    # a second, unrelated way to hang the proxy and would spoil the result.
    # Daemon threads so they never keep the script alive after the child dies.
    events, errs = [], []
    threading.Thread(target=lambda: [events.append(l) for l in p.stdout], daemon=True).start()
    threading.Thread(target=lambda: [errs.append(l) for l in p.stderr], daemon=True).start()

    # Give the binary time to bind its port before the first connection.
    time.sleep(1.5)

    # A realistic Claude Code request body. The 150 KB "system" field matters:
    # it makes every request allocate enough on the Scala heap that a
    # collection is triggered within the first couple of rounds, which is the
    # precondition for the wedge. A tiny body could take hundreds of rounds.
    body = json.dumps({
        "model": "claude-haiku-4-5-20251001",
        "max_tokens": 1,
        "system": "x" * 150_000,
        "messages": [{"role": "user", "content": "hi"}],
    })

    # The proxy forwards to api.anthropic.com. An invalid key makes upstream
    # reject the request immediately with 401, so the load is real end to end
    # (the proxy reads, filters, forwards and captures every request) without
    # consuming any API quota. The proxy treats 401 like any other response.
    headers = {
        "content-type": "application/json",
        "anthropic-version": "2023-06-01",
        "x-api-key": "invalid",
    }

    # One request on a fresh connection. A new connection per request, rather
    # than keep-alive, mirrors how a burst of parallel Claude Code calls looks
    # to the proxy. The 25 s timeout is far above the normal 0.25 s round trip,
    # so a timeout can only mean the proxy stopped answering.
    def one() -> int:
        conn = http.client.HTTPConnection("127.0.0.1", port, timeout=25)
        conn.request("POST", "/v1/messages", body=body, headers=headers)
        resp = conn.getresponse()
        resp.read()
        conn.close()
        return resp.status

    try:
        for rnd in range(rounds):
            # 8 requests submitted at once. The wedge needs at least two
            # requests in flight when a collection starts, and 8 makes that
            # overlap near certain on every round.
            with concurrent.futures.ThreadPoolExecutor(8) as ex:
                results = []
                for fut in [ex.submit(one) for _ in range(8)]:
                    try:
                        results.append(str(fut.result()))
                    except Exception as e:
                        # A timeout or connection error is recorded by its
                        # class name so it shows up beside the status codes.
                        results.append(type(e).__name__)

            # The collector prints "Waiting for N thread(s) to reach safepoint"
            # to stderr every 10 s while it is stuck. Counting those lines shows
            # whether the runtime itself has noticed the problem.
            safepoint = sum("safepoint" in l for l in errs)
            print(f"round {rnd}: {sorted(set(results))}; safepoint warnings={safepoint}", flush=True)

            if any(r != "401" for r in results):
                # Wedged. The first safepoint warning appears 10 s after the
                # collector started waiting, so wait a little longer than that
                # before reading the diagnostics.
                time.sleep(12)
                # The warning names the stuck threads by stack address. These
                # lines are what to quote in a bug report, and the addresses
                # can be matched against `vmmap <pid>` to find the threads.
                for l in [l for l in errs if "stackBottom" in l][-2:]:
                    print("GC:", l.strip(), flush=True)
                # Per-thread CPU state (macOS ps). Stuck threads show as R with
                # high system time: they are re-faulting in a loop, not blocked.
                print(subprocess.run(["ps", "-M", "-p", str(p.pid)], capture_output=True, text=True).stdout)
                return 1

        print(f"no wedge in {rounds} rounds", flush=True)
        return 0
    finally:
        # Always tear the proxy down, including on Ctrl-C, so the port is free
        # for the next run.
        p.kill()


if __name__ == "__main__":
    sys.exit(main())
