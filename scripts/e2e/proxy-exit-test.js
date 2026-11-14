'use strict';
/*
 * The Electron end-to-end test of the proxy exit handling (issue #95). macOS only.
 *
 * The app must show the cause when the proxy exits and the user did not ask it to stop:
 *   1. start fails: a proxy binary that dyld stops before main ("Library not loaded", SIGABRT), the same failure as
 *      issue #95,
 *   2. crash after start: a proxy that reports that it started and then exits with code 3,
 *   3. normal start and stop: the real proxy. The app must show nothing.
 *
 * What it needs: `sbt devUi` (or `sbt prodUi`), then `npm install` in electron-app, then `npm ci` in scripts/e2e.
 * Run it with `npm test` in scripts/e2e.
 *
 * Why it is safe on a developer's Mac:
 *   - each app instance gets its own new userData folder in a temp folder (--user-data-dir), and the test stops at once
 *     when app.getPath("userData") is not that folder,
 *   - each instance starts in Route Claude manual mode (route-mode.json written before the launch). There is no record
 *     file in that userData folder, so the launch and quit cleanup changes no settings file,
 *   - the normal case uses port 18892, not 8888.
 */
const { _electron } = require('playwright-core');
const childProcess = require('child_process');
const fs = require('fs');
const net = require('net');
const os = require('os');
const path = require('path');

const repo = path.resolve(__dirname, '..', '..');
const appDir = path.join(repo, 'electron-app');
const electronBin = path.join(appDir, 'node_modules', 'electron', 'dist', 'Electron.app', 'Contents', 'MacOS', 'Electron');
const realProxy = path.join(appDir, 'claude-proxymate');
const NormalPort = 18892;
const TimeoutMs = 30000;

function requireFile(file, hint) {
  if (!fs.existsSync(file)) {
    throw new Error(`file not found: ${file} (${hint})`);
  }
}

/* A binary linked to a dylib that the test then deletes, so dyld stops it with "Library not loaded" and SIGABRT. */
function buildBrokenProxy(tmp) {
  const libDir = path.join(tmp, 'lib');
  fs.mkdirSync(libDir);
  const goneC = path.join(tmp, 'gone.c');
  const brokenC = path.join(tmp, 'broken.c');
  const lib = path.join(libDir, 'libgone.dylib');
  const bin = path.join(tmp, 'broken-proxy');
  fs.writeFileSync(goneC, 'int gone(void) { return 0; }\n');
  fs.writeFileSync(brokenC, 'int gone(void);\nint main(void) { return gone(); }\n');
  childProcess.execFileSync('clang', ['-dynamiclib', goneC, '-o', lib]);
  childProcess.execFileSync('clang', [brokenC, lib, '-o', bin]);
  fs.rmSync(lib);
  return bin;
}

/* A fake proxy that reports that it started, writes to stderr, then exits with code 3. */
function writeCrashProxy(tmp) {
  const bin = path.join(tmp, 'crash-proxy');
  fs.writeFileSync(
    bin,
    [
      '#!/bin/sh',
      `echo '{"type":"proxy_started","port":18891}'`,
      'echo "fake proxy: something went wrong" >&2',
      'sleep 1',
      'exit 3',
      '',
    ].join('\n'),
    { mode: 0o755 },
  );
  return bin;
}

/* Resolves with "connected" or the error code (for example "ECONNREFUSED"). */
function tcpConnect(port) {
  return new Promise(resolve => {
    const socket = net.connect({ host: '127.0.0.1', port });
    socket.setTimeout(5000);
    socket.once('connect', () => {
      socket.destroy();
      resolve('connected');
    });
    socket.once('timeout', () => {
      socket.destroy();
      resolve('ETIMEDOUT');
    });
    socket.once('error', e => resolve(e.code));
  });
}

async function launchApp(tmp, label, proxyBinary) {
  const userData = path.join(tmp, `userData-${label}`);
  fs.mkdirSync(userData);
  /* Manual mode: Route Claude writes nothing to the real settings files. */
  fs.writeFileSync(path.join(userData, 'route-mode.json'), JSON.stringify({ mode: 'manual' }));

  const env = { ...process.env, PROXY_BINARY_PATH: proxyBinary };
  delete env.ELECTRON_RUN_AS_NODE;
  delete env.ELECTRON_NO_ATTACH_CONSOLE;

  const app = await _electron.launch({
    executablePath: electronBin,
    args: [`--user-data-dir=${userData}`, appDir],
    env,
  });
  try {
    const actual = await app.evaluate(({ app }) => app.getPath('userData'));
    if (actual !== userData) {
      throw new Error(`userData is not isolated: ${actual}`);
    }
    const page = await app.firstWindow();
    await page.waitForSelector('#proxyStartBtn', { timeout: TimeoutMs });
    /* Record alerts instead of showing them: a dialog would block the page. */
    await page.evaluate(() => {
      window.__alerts = [];
      window.alert = msg => window.__alerts.push(String(msg));
    });
    return { app, page };
  } catch (e) {
    await app.close();
    throw e;
  }
}

async function clickStartAndGetAlerts(page) {
  await page.evaluate(() => document.getElementById('proxyStartBtn').click());
  await page.waitForFunction(() => window.__alerts.length > 0, null, { timeout: TimeoutMs });
  return page.evaluate(() => window.__alerts);
}

async function startFails(tmp, bins) {
  const { app, page } = await launchApp(tmp, 'start-fails', bins.broken);
  try {
    const alerts = await clickStartAndGetAlerts(page);
    const alert = alerts[0];
    if (alerts.length !== 1) {
      throw new Error(`expected 1 alert, got ${JSON.stringify(alerts)}`);
    }
    if (!alert.startsWith('Failed to start proxy: signal SIGABRT')) {
      throw new Error(`unexpected alert: ${JSON.stringify(alert)}`);
    }
    if (!alert.includes('Library not loaded')) {
      throw new Error(`the alert has no dyld message: ${JSON.stringify(alert)}`);
    }
  } finally {
    await app.close();
  }
}

async function crashAfterStart(tmp, bins) {
  const { app, page } = await launchApp(tmp, 'crash-after-start', bins.crash);
  try {
    const alerts = await clickStartAndGetAlerts(page);
    const expected = 'The proxy stopped unexpectedly: exit code 3\nfake proxy: something went wrong';
    if (alerts.length !== 1 || alerts[0] !== expected) {
      throw new Error(`expected [${JSON.stringify(expected)}], got ${JSON.stringify(alerts)}`);
    }
  } finally {
    await app.close();
  }
}

async function normalStartAndStop(tmp) {
  const { app, page } = await launchApp(tmp, 'normal', realProxy);
  try {
    await page.evaluate(port => {
      const input = document.getElementById('proxyPort');
      input.value = String(port);
      input.dispatchEvent(new Event('input', { bubbles: true }));
      document.getElementById('proxyStartBtn').click();
    }, NormalPort);
    await page.waitForFunction(() => /Stop/.test(document.getElementById('proxyStartBtn').textContent), null, {
      timeout: TimeoutMs,
    });
    const whileRunning = await tcpConnect(NormalPort);
    if (whileRunning !== 'connected') {
      throw new Error(`the proxy does not accept connections on ${NormalPort}: ${whileRunning}`);
    }
    await page.evaluate(() => document.getElementById('proxyStartBtn').click());
    await page.waitForFunction(() => /Start/.test(document.getElementById('proxyStartBtn').textContent), null, {
      timeout: TimeoutMs,
    });
    await new Promise(resolve => setTimeout(resolve, 1500));
    const alerts = await page.evaluate(() => window.__alerts);
    if (alerts.length !== 0) {
      throw new Error(`expected no alert, got ${JSON.stringify(alerts)}`);
    }
    const afterStop = await tcpConnect(NormalPort);
    if (afterStop !== 'ECONNREFUSED') {
      throw new Error(`expected ECONNREFUSED on ${NormalPort} after the stop, got ${afterStop}`);
    }
  } finally {
    await app.close();
  }
}

async function main() {
  if (process.platform !== 'darwin') {
    throw new Error('this test runs only on macOS');
  }
  requireFile(electronBin, 'run npm install in electron-app');
  requireFile(realProxy, 'run sbt devUi or sbt prodUi');

  /* realpath: on macOS os.tmpdir() is under /var, a symbolic link to /private/var, and Electron reports the resolved
   * path, so the userData check needs the resolved form too. */
  const tmp = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), 'claude-proxymate-e2e-')));
  try {
    const bins = { broken: buildBrokenProxy(tmp), crash: writeCrashProxy(tmp) };
    const cases = [
      ['start fails', () => startFails(tmp, bins)],
      ['crash after start', () => crashAfterStart(tmp, bins)],
      ['normal start and stop', () => normalStartAndStop(tmp)],
    ];
    let failed = false;
    for (const [name, run] of cases) {
      try {
        await run();
        console.log(`PASS: ${name}`);
      } catch (e) {
        console.log(`FAIL: ${name}: ${e.message}`);
        failed = true;
      }
    }
    return failed;
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
}

main().then(
  failed => {
    process.exitCode = failed ? 1 : 0;
  },
  e => {
    console.error(`error: ${e.message}`);
    process.exitCode = 1;
  },
);
