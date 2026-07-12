# Claude Proxymate

A real-time man-in-the-middle (MITM) proxy that intercepts and visualizes API traffic between Claude Code and the Anthropic API, surfacing the hidden prompt-augmentation machinery (CLAUDE.md injection, slash commands, skills, sub-agents, MCP tools) in a desktop UI — with privacy-first masking so captures are safe to share.

Inspired by an early version of [claude-inspector](https://github.com/kangraemin/claude-inspector), it's built as a hybrid **Scala Native + Scala.js** application: a native HTTP proxy binary forwards traffic to Anthropic and streams capture events to an Electron UI written entirely in Scala.

## Features

- **Live capture** — every request/response pair between Claude Code and `api.anthropic.com`, reconstructed from the SSE stream into a full message.
- **Mechanism detection** — finds and highlights prompt-augmentation mechanisms in each request body: injected CLAUDE.md / rules / memory sections, output styles, slash commands, skills, sub-agents, and MCP tools.
- **Request Anatomy** — a dashboard that quantifies a request: per-segment byte/token estimates, structural facts (message/turn/tool/image/thinking counts, cache-eligible system blocks), a mechanism inventory, and flagged anomalies.
- **Token cost** — per-capture cost breakdown with model-based pricing.
- **Privacy masking** (display-only; raw captures are never mutated):
  - sensitive **field-name** values (`api_key`, `*_token`, `password`, account/device/session identifiers, …),
  - known-shape **secrets** in free text (`sk-ant-…` and friends) via regex detection,
  - verbose **correlation IDs** (`msg_…`, `toolu_…`) compacted to a short tag,
  - sensitive **URL query-string** values.
  - **Presenter mode** — one decisive global mask-all / reveal-all control: the **Mask secrets** switch in the status bar (or ⌘⇧M). **Copy is WYSIWYG**: the clipboard follows the on-screen mask state.
- **Route Claude** — a Manual / VS Code / Global control that manages `ANTHROPIC_BASE_URL` for you: in VS Code-family `settings.json` or globally in `~/.claude/settings.json`, applied only while the proxy runs and cleaned up on stop/quit/crash.
- **Search** across request / response / analysis / messages with match navigation.
- **Theme** — system / light / dark, with OS `prefers-color-scheme` tracking.
- **i18n** — English and Korean, externalized to `.properties` files and loaded at runtime.

## Install

### Homebrew (recommended)

The easiest way to install on macOS is via the [`kevin-lee/tap`](https://github.com/kevin-lee/homebrew-tap)
Homebrew tap. Because the cask installs a notarized build, Gatekeeper lets it
launch without any of the manual steps below.

```bash
brew install --cask kevin-lee/tap/claude-proxymate
```

Or tap first, then install:

```bash
brew tap kevin-lee/tap
brew install --cask claude-proxymate
```

Upgrade to the latest release:

```bash
brew upgrade --cask claude-proxymate
```

Uninstall (add `--zap` to also remove preferences):

```bash
brew uninstall --cask claude-proxymate
```

### Manual download (DMG)

Prebuilt macOS DMGs are published on the [Releases](https://github.com/kevin-lee/claude-proxymate/releases) page. Download
the one matching your Mac:

- **Apple Silicon** (M1/M2/M3/…): `Claude-Proxy-<version>-arm64.dmg`
- **Intel**: `Claude-Proxy-<version>-x64.dmg`

Open the DMG and drag **Claude Proxymate** to Applications.

### First launch (Gatekeeper)

If the app opens normally with no warning, it is a signed + notarized build
and none of the steps below are needed.

**Unsigned builds** (early releases, or builds made without the project's
Developer ID credentials) are refused by macOS Gatekeeper on the first try
("Apple could not verify … is free of malware", or "… is damaged"). If you are
opening one of those:

- **macOS 15 Sequoia and later** (right-click → Open no longer bypasses
  Gatekeeper): double-click the app once and dismiss the dialog, then open
  **System Settings → Privacy & Security**, scroll down to the message about
  Claude Proxymate, and click **Open Anyway**. Alternatively, clear the
  download quarantine flag once in Terminal:
  ```bash
  xattr -dr com.apple.quarantine "/Applications/Claude Proxymate.app"
  ```
- **macOS 14 and earlier**: in **Applications**, right-click (or Control-click)
  **Claude Proxymate**, choose **Open**, then confirm **Open** in the dialog.
  macOS remembers this choice, so subsequent launches work by double-click.


## Architecture

```
┌─────────────────────────────────────────────────┐
│  Electron App (Scala.js)                        │
│  ┌───────────────┐    ┌───────────────────────┐ │
│  │ main.js       │◄──►│ renderer.js           │ │
│  │ (main proc)   │IPC │ (Scala.js renderer)   │ │
│  └───────┬───────┘    └───────────────────────┘ │
│          │ spawns child process                 │
│          ▼                                      │
│  ┌───────────────────┐                          │
│  │ claude-proxymate  │ ◄── Scala Native binary  │
│  │ (localhost:8888)  │                          │
│  │ stdout → JSON     │──► parsed to UI via IPC  │
│  └─────────┬─────────┘                          │
│            │ HTTPS                              │
│            ▼                                    │
│   api.anthropic.com                             │
└─────────────────────────────────────────────────┘
```

The project is split into five modules:

| Module                      | Platform          | Purpose                                                                                                                                                                                                                                                                                                                                                                      |
|-----------------------------|-------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `claude-proxymate-core`     | JVM / JS / Native | Shared cross-compiled core: data models, JSON codecs, SSE parser, CLAUDE.md parser, mechanism detector, Request Anatomy, masking primitives (sensitive keys, token/correlation-id patterns, query-param mask), IPC channel & HTML element ID constants, JSON-line protocol. The **JVM** side additionally generates `index.html` (ScalaTags) and the i18n JSON at build time |
| `claude-proxymate-server`   | Scala Native      | HTTP proxy binary. Intercepts requests, forwards to Anthropic, tees the response, emits events as JSON lines on stdout. Default TLS backend is libcurl via FFI (`CurlMain`); an http4s Ember / s2n backend (`Main`) is available as a fallback                                                                                                                               |
| `claude-proxymate-electron` | Scala.js          | Electron main process. Spawns the native proxy, reads its stdout, forwards events to the renderer UI via IPC                                                                                                                                                                                                                                                                 |
| `claude-proxymate-preload`  | Scala.js          | Electron preload bridge. Exposes IPC channels to the renderer via `contextBridge`                                                                                                                                                                                                                                                                                            |
| `claude-proxymate-renderer` | Scala.js          | Electron renderer UI. All frontend logic: i18n, theme, JSON tree viewer, message parsing, analysis, masking, search, and more. Built on a testable **View/logic split** (pure ScalaTags `*View` modules render to strings; sibling modules wire them to the DOM)                                                                                                             |

Dependency graph:

```
              core (JVM, JS, Native)
            /     |      |        \
  server(Native) electron(JS) preload(JS) renderer(JS)
```

### Module Interactions

At a glance, the modules interact as a left-to-right runtime pipeline, with `core` as a shared library that every other module depends on at compile time:

```
  ┌──────────────────┐ localhost:8888 ┌─────────────────────────────────────────────────────┐
  │ Claude Code CLI  │──────────────► │                Electron Application                 │
  └──────────────────┘                │                                                     │
                                      │  ┌───────────┐ IPC  ┌──────────┐ IPC  ┌───────────┐ │
                                      │  │ renderer  │◄────►│ preload  │◄────►│ electron  │ │
                                      │  │ (Scala.js)│      │(Scala.js)│      │ (Scala.js)│ │
                                      │  └───────────┘      └──────────┘      └─────┬─────┘ │
                                      │                                        spawn│stdout │
                                      │                                             ▼       │
                                      │                                      ┌───────────┐  │
                                      │                                      │  server   │  │
                                      │                                      │  (Scala   │  │
                                      │                                      │  Native)  │  │
                                      │                                      └─────┬─────┘  │
                                      └────────────────────────────────────────────┼────────┘
                                                                              HTTPS│
                                                                                   ▼
                                                                          ┌──────────────────┐
                                                                          │ api.anthropic.com│
                                                                          └──────────────────┘

  All four runtime modules above depend on:
    ┌─────────────────────────────────────────────────────────────────────┐
    │ core (JVM, JS, Native) — models, codecs, IPC channels, parsers, ... │
    └─────────────────────────────────────────────────────────────────────┘
```

Zooming in, there are exactly two IPC flow directions. Commands (start/stop/status) originate in the renderer and invoke handlers in the Electron main process. Events (captured requests/responses) originate in the native proxy's stdout and are pushed into the renderer. Arrow labels show the wire channel names from [`IpcChannels`](modules/claude-proxymate-core/shared/src/main/scala/claudeproxymate/core/IpcChannels.scala):

```
Command flow — user clicks "Start Proxy" in the UI:

  renderer                preload                electron (main)           server (native)
  ─────────               ─────────              ─────────────────         ─────────────────
  ElectronApi       ───►  window.electronAPI  ──►  ipcMain.handle      ──►  ChildProcessModule
  .proxyStart(port)       .proxyStart(port)        ("proxy-start")          .spawn(binary,
                          │                        │                         ["--port", N])
                          │ ipcRenderer.invoke     │ IpcHandlers.startProxy
                          ▼                        ▼
                          ───── "proxy-start" ─────►     ───── OS process ─────►

  Same pattern for: "proxy-stop"   (ElectronApi.proxyStop   → IpcHandlers.stopProxy)
                    "proxy-status" (ElectronApi.proxyStatus → IpcHandlers.getStatus)
```

```
Event flow — native proxy captures a request/response:

  server (native)          electron (main)             preload               renderer
  ─────────────────        ────────────────────        ──────────────        ─────────────
  EventEmitter       ───►  child.stdout.on("data") ──► ipcRenderer.on   ───► ElectronApi
  .emit(event)             │                           ("proxy-request")     .onProxyRequest
  prints JSON line         │ IpcHandlers                    ▲                (cb)
  to stdout                │ .processProxyEvent             │
                           ▼                                │
                           webContents.send ─── "proxy-request" ──►
                           ("proxy-request", data)
                           webContents.send ─── "proxy-response" ──►
                           ("proxy-response", data)

  Channels are one-way from main → renderer. The renderer never uses ipcRenderer
  directly; it only sees window.electronAPI exposed by preload via contextBridge.
```

## Prerequisites

- **JDK 21+** (for sbt and Scala compilation)
- **sbt 1.12.5+** (build tool)
- **Node.js 20+** and **npm** (for Electron)
- **LLVM/Clang** (for Scala Native compilation; included with Xcode Command Line Tools on macOS)

## Build

### Compile all modules

```bash
sbt compile
```

### Run tests

Tests are written with [hedgehog](https://github.com/hedgehogqa/scala-hedgehog) (property-based) on the JVM and Scala.js, and [munit](https://scalameta.org/munit/) on Scala Native (the hedgehog test runner hangs on SN 0.5, so the Native platform runs deterministic munit ports of the shared core specs). 49 spec files in total.

```bash
# All tests (core on JVM + JS + Native, renderer on JS, server on Native)
sbt "coreJVM/test; coreJS/test; coreNative/test; renderer/test; proxyServer/test"

# Core only — SSE/CLAUDE.md/mechanism parsers, Request Anatomy,
#             masking primitives, JSON-line protocol, i18n loader, HTML gen
sbt coreJVM/test

# Renderer only — views, message rendering, masking copy, pricing,
#                 JSON tree, i18n templating, presenter mode
sbt renderer/test

# Server only (munit on Native) — proxy error mapping
sbt proxyServer/test
```

### Build the native proxy binary

The proxy has two TLS backends. The default (`CurlMain`) uses the system's libcurl via Scala Native FFI — no extra dependencies on macOS:

```bash
sbt proxyServer/nativeLink
```

To use the alternative http4s Ember / s2n backend instead, install s2n and switch the main class:

```bash
brew install s2n
```

Then in `build.sbt`, change `Compile / mainClass` in `proxyServer` to:
```scala
Compile / mainClass := Some("claudeproxymate.proxy.Main")
```

The binary is output to:
```
modules/claude-proxymate-server/target/scala-3.8.4/claude-proxymate-server-out
```

### Build the Scala.js modules

Each Scala.js module (`electron`, `preload`, `renderer`) builds with `fastLinkJS` (development, unoptimized) or `fullLinkJS` (production, optimized):

```bash
sbt electron/fastLinkJS   # or electron/fullLinkJS
sbt preload/fastLinkJS    # or preload/fullLinkJS
sbt renderer/fastLinkJS   # or renderer/fullLinkJS
```

Output paths (replace `-fastopt` for `fastLinkJS`):
```
modules/claude-proxymate-electron/target/scala-3.8.4/claude-proxymate-electron-opt/main.js
modules/claude-proxymate-preload/target/scala-3.8.4/claude-proxymate-preload-opt/main.js
modules/claude-proxymate-renderer/target/scala-3.8.4/claude-proxymate-renderer-opt/main.js
```

### Generated assets

`index.html` and the runtime i18n JSON are not checked in — they're generated from Scala/properties sources at build time (and wired into `devUi` / `prodUi` / `package.sh`):

```bash
sbt generateHtml   # ScalaTags → electron-app/public/index.html
sbt generateI18n   # i18n/*.properties → electron-app/public/i18n/*.json
```

### Package the Electron app (macOS DMG)

```bash
./scripts/package.sh
```

This runs all build steps (native binary, electron JS, renderer JS, preload JS, copy artifacts, generate index.html, generate i18n JSON, npm install, electron-builder) and produces a DMG in `electron-app/release/`. Signing uses the Developer ID certificate from the login keychain (electron-builder auto-discovery; ad-hoc if none is present), and notarization runs when the `APPLE_ID` / `APPLE_APP_SPECIFIC_PASSWORD` / `APPLE_TEAM_ID` environment variables are set — electron-builder handles it inline (`"notarize": true`). [`scripts/notarize.sh`](scripts/notarize.sh) remains as a standalone tool for re-notarizing an existing DMG.

### Releases (CI)

Pushing a `v*` tag (or manual dispatch) triggers
[`release.yml`](.github/workflows/release.yml), which runs the full test suite,
then builds the app with `sbt prodUi` + electron-builder on both an Apple
Silicon and an Intel macOS runner — each architecture must build on matching
hardware because the Scala Native proxy binary is compiled for the host CPU —
verifies each DMG (image integrity, single arch, matching binary
architectures, and — when signing is configured — Developer ID signature,
notarization, and stapled ticket), and attaches the DMGs to a GitHub Release.
With the `CSC_LINK` / `CSC_KEY_PASSWORD` / `APPLE_ID` /
`APPLE_APP_SPECIFIC_PASSWORD` / `APPLE_TEAM_ID` repository secrets configured,
the DMGs are Developer-ID signed and notarized; without them, electron-builder
falls back to an ad-hoc signature and skips notarization. The
`GA_MEASUREMENT_ID` / `GA_API_SECRET` repository secrets are passed to the
build step for the GA4 analytics integration.

## Usage

### Option 1: Standalone proxy (headless)

Run the native proxy binary directly, without the Electron UI:

```bash
# Default port 8888
./modules/claude-proxymate-server/target/scala-3.8.4/claude-proxymate-server-out

# Custom port
./modules/claude-proxymate-server/target/scala-3.8.4/claude-proxymate-server-out --port 8080
```

The proxy emits JSON line events to stdout:
- `{"type":"proxy_started","port":8888}`
- `{"type":"request_captured","request":{...}}`
- `{"type":"response_captured","response":{...}}`

Then configure Claude Code to route through the proxy:

```bash
export ANTHROPIC_BASE_URL=http://localhost:8888
claude
```

### Option 2: Electron desktop app

For development, assemble the `electron-app/` directory and run:

```bash
# 1. Clean, build all modules, and assemble electron-app/
sbt devUi

# 2. Set the binary path for development (see electron-app/.env.example)
export PROXY_BINARY_PATH=./claude-proxymate

# 3. Install deps and run
cd electron-app
npm install
npm start
```

Once the app is running:

1. Click **▶ Start Proxy** in the address bar (defaults to port 8888; the port is remembered across launches)
2. In a separate terminal, run the command shown in the address bar (the ⧉ button copies it) — or let **Route Claude** in the status bar set `ANTHROPIC_BASE_URL` for you:
   ```bash
   ANTHROPIC_BASE_URL=http://localhost:8888 claude
   ```
3. Use Claude Code normally. All API requests and responses appear in the inspector UI in real-time.

The UI visualizes:
- Request/response pairs with timing
- Parsed SSE streams reconstructed into full messages
- CLAUDE.md sections (global, local, rules, memory)
- Detected prompt augmentation mechanisms (output style, slash commands, skills, sub-agents, MCP tools)
- The Request Anatomy dashboard (segment sizes, structural facts, mechanism inventory, anomalies)
- Token cost breakdown with model-based pricing
- Privacy masking with WYSIWYG copy and the status-bar Mask secrets switch

### Route Claude (Manual / VS Code / Global)

The **Route Claude** segmented control in the status bar manages
`ANTHROPIC_BASE_URL` automatically, so you never edit a settings file by hand.
Exactly one mode is active at a time — selecting one removes the entry from
the other target:

- **Manual** — no settings file is touched; copy the command from the address
  bar and run Claude yourself.
- **VS Code** — while the proxy runs, `{"name": "ANTHROPIC_BASE_URL", "value": "http://localhost:<port>"}`
  is kept in `claudeCode.environmentVariables` of every detected editor
  (VS Code, VS Code Insiders, VSCodium, Cursor). Applies to newly started
  Claude Code sessions in those editors.
- **Global** — while the proxy runs, `"ANTHROPIC_BASE_URL": "http://localhost:<port>"`
  is kept in the top-level `env` object of `~/.claude/settings.json` (the file
  and `~/.claude/` are created if missing). Applies to all newly started
  Claude sessions.

The selection is remembered across app restarts, but the env var itself only
exists while the proxy actually runs: proxy stop, app quit, and a
crash-recovery sweep at the next launch all remove it from **every** target,
whatever the mode. Selecting VS Code or Global while the proxy is stopped just
arms the route; the value is written when the proxy starts, using the port it
actually bound.

Safety: settings files are edited with `jsonc-parser` (comments and formatting
survive), backed up under the app's `userData` directory before every write,
verified after writing, and restored from the backup if anything goes wrong.
An `ANTHROPIC_BASE_URL` you set yourself (e.g. a corporate gateway) is never
modified or removed. Removal only deletes the entry Claude Proxymate added:
the `claudeCode.environmentVariables` property / the `env` object and any
other entries in them are always left in place (an empty array or object may
remain when Claude Proxymate created the property itself, and comments placed
between entries of that one array may be reformatted by the edit).

Limitations: only the default VS Code profile is managed (not
`User/profiles/<id>`), snap/flatpak/`Code - OSS` install locations are not
detected, running two app instances can fight over the entry, and already-open
Claude sessions keep their environment until a new session starts.

## Project Structure

```
claude-proxymate/
├── .github/
│   └── workflows/
│       └── release.yml               # CI: test → build arm64/x64 DMGs → GitHub Release on v* tags
├── build.sbt                         # sbt build definition (props, libs, devUi/prodUi, generateHtml/generateI18n)
├── project/
│   ├── build.properties              # sbt 1.12.5
│   ├── plugins.sbt                   # Scala.js, Scala Native, cross-project, sbt-buildinfo, sbt-devoops
│   └── package.json.template         # template for the generated electron-app/package.json
├── i18n/
│   ├── en.properties                 # English translations (source)
│   └── ko.properties                 # Korean translations (source)
├── modules/
│   ├── claude-proxymate-core/            # Cross-compiled (JVM, JS, Native)
│   │   ├── shared/src/main/scala/claudeproxymate/core/
│   │   │   ├── models.scala             # ADTs: ProxyRequest, ProxyResponse, ProxyEvent, Mechanisms, …
│   │   │   ├── codecs.scala             # circe Encoder/Decoder for all models
│   │   │   ├── SseParser.scala          # SSE stream → Anthropic message reconstruction
│   │   │   ├── ClaudeMdParser.scala     # Extract CLAUDE.md / rules / memory sections
│   │   │   ├── MechanismDetector.scala  # Detect prompt augmentation mechanisms
│   │   │   ├── RequestAnatomy.scala     # Segment sizing, structural facts, anomalies
│   │   │   ├── SensitiveKeys.scala      # Field-name patterns for value masking
│   │   │   ├── TokenPatterns.scala      # Regex-shape secret detection (sk-ant-…)
│   │   │   ├── CorrelationIds.scala     # Compact msg_…/toolu_… identifiers
│   │   │   ├── QueryParamMask.scala     # URL query-string masking
│   │   │   ├── UrlScheme.scala          # Validate URLs before shell.openExternal
│   │   │   ├── ProxyError.scala         # Typed proxy errors (platform-neutral)
│   │   │   ├── JsonLineProtocol.scala   # Encode/decode ProxyEvent as JSON lines
│   │   │   ├── RouteMode.scala          # Route Claude mode ADT (manual / vscode / global)
│   │   │   ├── VsCodeEnv.scala          # Pure apply/remove decisions for VS Code settings
│   │   │   ├── ClaudeEnv.scala          # Pure apply/remove decisions for ~/.claude/settings.json
│   │   │   ├── IpcChannels.scala        # IPC channel name constants (single source of truth)
│   │   │   └── HtmlIds.scala            # DOM element ID constants shared with renderer
│   │   └── jvm/src/main/scala/claudeproxymate/core/
│   │       ├── IndexHtmlGenerator.scala     # ScalaTags-based index.html generator
│   │       ├── IndexHtmlGeneratorMain.scala # CLI entry point for build-time HTML generation
│   │       ├── I18nGenerator.scala          # .properties → browser-consumable JSON
│   │       └── I18nPropertiesLoader.scala   # Read .properties locale files
│   ├── claude-proxymate-server/          # Scala Native only
│   │   └── src/main/scala/claudeproxymate/proxy/
│   │       ├── CurlMain.scala            # Entry point using libcurl FFI (default)
│   │       ├── Main.scala                # Entry point using http4s Ember / s2n (fallback)
│   │       ├── ProxyServer.scala         # http4s routes: intercept, forward, tee response
│   │       ├── AnthropicForwarder.scala  # Forwarding logic to api.anthropic.com
│   │       ├── CurlHttpClient.scala      # http4s Client[IO] backed by libcurl's easy API
│   │       ├── LibCurl.scala             # libcurl FFI bindings
│   │       ├── ProxyErrorHttp4s.scala    # http4s status/entity mapping for ProxyError
│   │       └── EventEmitter.scala        # Emit ProxyEvent JSON lines to stdout
│   ├── claude-proxymate-electron/        # Scala.js only
│   │   └── src/main/scala/claudeproxymate/electron/
│   │       ├── ElectronMain.scala        # BrowserWindow, app lifecycle, IPC, hardening
│   │       ├── IpcHandlers.scala         # Spawn/kill proxy, parse stdout, forward events
│   │       ├── RouteSync.scala           # Route Claude coordinator: mode, exclusion, lifecycle, persistence
│   │       ├── SyncFileOps.scala         # Shared backup → write → verify → restore protocol + ownership record
│   │       ├── VsCodeSync.scala          # VS Code-family settings.json backend
│   │       ├── ClaudeSettingsSync.scala  # ~/.claude/settings.json (env object) backend
│   │       ├── Analytics.scala           # GA4 integration
│   │       ├── Config.scala              # Environment variable access
│   │       └── facades/                  # @js.native Electron & Node.js facades
│   ├── claude-proxymate-preload/         # Scala.js only
│   │   └── src/main/scala/claudeproxymate/preload/
│   │       ├── Preload.scala              # Entry point: contextBridge.exposeInMainWorld
│   │       └── facades/ElectronPreload.scala
│   └── claude-proxymate-renderer/        # Scala.js only (depends on scalatags for views)
│       └── src/main/scala/claudeproxymate/renderer/
│           ├── RendererMain.scala           # Entry point: init, keydown, platform, build-info
│           ├── facades/                      # window.electronAPI & browser API facades
│           ├── state/                        # AppState, PresenterMode (global mask toggle)
│           ├── i18n/                         # I18n: runtime locale loading via fetch
│           ├── theme/                        # Theme: system/light/dark + logo swap
│           ├── view/                         # ViewHelpers, I18nTemplate (ScalaTags seam)
│           ├── util/                         # HtmlUtil, Debounce, JsJsonBridge
│           ├── json/                         # JsonTreeView (+ JsonTreeViewer logic)
│           ├── proxy/                        # Proxy control, capture list, info popover
│           ├── route/                        # Route Claude segmented control
│           ├── messages/                     # Message parsing, rendering, masked copy, badges
│           ├── search/                       # Search match navigation + listeners
│           ├── analysis/                     # Analysis tab, Request Anatomy, mechanism chips
│           ├── detail/                       # Detail view, pricing, token popover
│           ├── copy/                         # CopyUtil, MaskedCopy (WYSIWYG clipboard)
│           ├── update/                       # GitHub release update check
│           └── onboarding/                   # First-run onboarding modal (image carousel)
│           # convention: each *View.scala renders ScalaTags to a string and is unit-tested;
│           #             its sibling module wires that output to the DOM
├── electron-app/                     # Electron runtime package
│   ├── package.json                  # generated from project/package.json.template
│   └── .env.example                  # PROXY_BINARY_PATH for development
├── public/
│   └── styles.css                    # All CSS styles (index.html is generated at build time)
├── assets/
│   ├── icon.png
│   ├── icon.icns
│   ├── getting-started/              # onboarding carousel screenshots
│   └── logo/                         # light/dark logo SVG variants
└── scripts/
    ├── package.sh                    # Full 9-step build + Electron packaging
    ├── notarize.sh                   # macOS notarization
    └── generate-icons.sh             # Generate app icons from source art
```

## Tech Stack

| Component    | Technology                                 | Version          |
|--------------|--------------------------------------------|------------------|
| Language     | Scala 3                                    | 3.8.4            |
| Build        | sbt                                        | 1.12.5           |
| Build info   | sbt-buildinfo                              | 0.13.1           |
| JSON         | circe                                      | 0.14.16          |
| HTTP server  | http4s Ember                               | 0.23.34          |
| HTTPS client | libcurl FFI (default) / http4s Ember + s2n | system / 0.23.34 |
| Streaming    | fs2                                        | 3.13.0           |
| Effects      | cats-effect                                | 3.7.0            |
| Native       | Scala Native                               | 0.5.12           |
| JS           | Scala.js                                   | 1.22.0           |
| HTML/views   | ScalaTags                                  | 0.13.1           |
| DOM          | scalajs-dom                                | 2.8.1            |
| Desktop      | Electron                                   | 41.x             |
| Tests        | hedgehog (JVM/JS) / munit (Native)         | 0.13.1 / 1.3.3   |

## License

[MIT](LICENSE)
