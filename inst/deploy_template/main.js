const { app, BrowserWindow, ipcMain, dialog, Menu } = require("electron");
const path = require("path");
const fs = require("fs");
const http = require("http");
const AdmZip = require("adm-zip");

// ---- Configuration ----------------------------------------------------------

// Load launcher config (branding, colors, icon, file extension)
const CONFIG_FILE = path.join(__dirname, "launcher.config.json");
let launcherConfig = {
  appName: "Shiny App Launcher",
  headerTitle: "Shiny App Launcher",
  headerSubtitle: "Load and run Shinylive apps without R",
  headerBackground: "#1a1a2e",
  accentColor: "#4361ee",
  icon: null,
  fileExtension: "resondex",
};
try {
  const raw = fs.readFileSync(CONFIG_FILE, "utf-8");
  launcherConfig = { ...launcherConfig, ...JSON.parse(raw) };
} catch {
  // Fall back to defaults
}

const APP_EXTENSION = launcherConfig.fileExtension; // Custom file extension (no dot)

// ---- Pending file open (received before window is ready) --------------------
let pendingFilePath = null;

// ---- Paths ------------------------------------------------------------------
const USER_DATA = app.getPath("userData");
const APPS_DIR = path.join(USER_DATA, "apps");
const LIBRARY_FILE = path.join(USER_DATA, "library.json");

// ---- Runtime paths ----------------------------------------------------------
// RUNTIME_DIR: bundled with the Electron app (read-only source of truth)
// SHARED_RUNTIME_DIR: single writable copy in userData, shared by all apps
const RUNTIME_DIR = path.join(__dirname, "runtime");
const SHARED_RUNTIME_DIR = path.join(USER_DATA, "runtime");

// ---- Fresh install detection -------------------------------------------------
// On first launch after a new install, clear any stale app data that may have
// been left by a previous build sharing the same userData path.
const INSTALL_MARKER = path.join(USER_DATA, ".install-id");
const currentInstallId = `${launcherConfig.appName}-${app.getVersion()}`;

function cleanStaleData() {
  try {
    if (fs.existsSync(INSTALL_MARKER)) {
      const existing = fs.readFileSync(INSTALL_MARKER, "utf-8").trim();
      if (existing === currentInstallId) return; // same install, keep data
    }
    // New install or different app — wipe apps, library, and shared runtime
    console.log("[init] New install detected, clearing stale app data...");
    if (fs.existsSync(APPS_DIR)) fs.rmSync(APPS_DIR, { recursive: true, force: true });
    if (fs.existsSync(LIBRARY_FILE)) fs.unlinkSync(LIBRARY_FILE);
    if (fs.existsSync(SHARED_RUNTIME_DIR)) fs.rmSync(SHARED_RUNTIME_DIR, { recursive: true, force: true });
    fs.mkdirSync(APPS_DIR, { recursive: true });
    fs.writeFileSync(INSTALL_MARKER, currentInstallId);
    console.log("[init] Clean slate ready");
  } catch (err) {
    console.error("[init] Error cleaning stale data:", err.message);
  }
}

cleanStaleData();
ensureSharedRuntime();

if (!fs.existsSync(APPS_DIR)) fs.mkdirSync(APPS_DIR, { recursive: true });

// ---- Library management -----------------------------------------------------
function loadLibrary() {
  if (!fs.existsSync(LIBRARY_FILE)) return [];
  try {
    return JSON.parse(fs.readFileSync(LIBRARY_FILE, "utf-8"));
  } catch {
    return [];
  }
}

function saveLibrary(library) {
  fs.writeFileSync(LIBRARY_FILE, JSON.stringify(library, null, 2));
}

// ---- MIME types -------------------------------------------------------------
const MIME_TYPES = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".mjs": "application/javascript",
  ".cjs": "application/javascript",
  ".json": "application/json",
  ".css": "text/css",
  ".wasm": "application/wasm",
  ".png": "image/png",
  ".ico": "image/x-icon",
  ".svg": "image/svg+xml",
  ".txt": "text/plain",
  ".woff2": "font/woff2",
  ".woff": "font/woff",
  ".ttf": "font/ttf",
  ".otf": "font/otf",
  ".gz": "application/gzip",
  ".so": "application/octet-stream",
  ".map": "application/json",
  ".rds": "application/octet-stream",
  ".tgz": "application/gzip",
  ".ts": "application/javascript",
  ".data": "application/octet-stream",
};

// Currently running server for a launched app
let activeServer = null;
let activeAppId = null;

// ---- HTTP server for a Shinylive app (layered fallback) ---------------------
// Serves files from the app directory first. If not found there, falls back
// to the shared runtime in userData/runtime/. This allows lightweight bundles
// to store only app-specific files while sharing the 59 MB runtime.
function startAppServer(appDir) {
  return new Promise((resolve, reject) => {
    const server = http.createServer((req, res) => {
      const urlPath = decodeURIComponent(req.url.split("?")[0]);
      const requestedPath = urlPath === "/" ? "index.html" : urlPath;
      const appFilePath = path.join(appDir, requestedPath);
      const runtimeFilePath = path.join(SHARED_RUNTIME_DIR, requestedPath);
      const ext = path.extname(requestedPath);
      const contentType = MIME_TYPES[ext] || "application/octet-stream";

      // Add CORS and service worker headers
      const headers = {
        "Content-Type": contentType,
        "Access-Control-Allow-Origin": "*",
      };

      // Service worker scope requires this header
      if (urlPath.endsWith("shinylive-sw.js") || urlPath.endsWith("webr-serviceworker.js")) {
        headers["Service-Worker-Allowed"] = "/";
      }

      // Try app-specific file first, fall back to shared runtime
      fs.readFile(appFilePath, (err, data) => {
        if (err) {
          // Not in app dir — try shared runtime
          fs.readFile(runtimeFilePath, (err2, data2) => {
            if (err2) {
              console.log(`[HTTP 404] ${urlPath} (not in app or runtime)`);
              res.writeHead(404);
              res.end("Not found");
              return;
            }
            res.writeHead(200, headers);
            res.end(data2);
          });
          return;
        }
        res.writeHead(200, headers);
        res.end(data);
      });
    });

    server.listen(0, "127.0.0.1", () => {
      resolve({ server, port: server.address().port });
    });

    server.on("error", reject);
  });
}

function stopActiveServer() {
  if (activeServer) {
    activeServer.close();
    activeServer = null;
    activeAppId = null;
  }
}

// ---- Copy directory recursively ---------------------------------------------
function copyDirSync(src, dest) {
  fs.mkdirSync(dest, { recursive: true });
  for (const entry of fs.readdirSync(src, { withFileTypes: true })) {
    const srcPath = path.join(src, entry.name);
    const destPath = path.join(dest, entry.name);
    if (entry.isDirectory()) {
      copyDirSync(srcPath, destPath);
    } else {
      fs.copyFileSync(srcPath, destPath);
    }
  }
}

// ---- Ensure shared runtime exists in userData -------------------------------
function ensureSharedRuntime() {
  if (fs.existsSync(SHARED_RUNTIME_DIR)) {
    const hasIndex = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "index.html"));
    const hasSW = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "shinylive-sw.js"));
    if (hasIndex && hasSW) {
      console.log("[init] Shared runtime already exists and is valid");
      return;
    }
    console.log("[init] Shared runtime invalid, recreating...");
    fs.rmSync(SHARED_RUNTIME_DIR, { recursive: true, force: true });
  }
  console.log("[init] Creating shared runtime from bundled template...");
  copyDirSync(RUNTIME_DIR, SHARED_RUNTIME_DIR);
  console.log("[init] Shared runtime ready");
}

// ---- Extract and assemble app -----------------------------------------------
function extractAndAssembleApp(zipPath, destDir) {
  fs.mkdirSync(destDir, { recursive: true });

  // Extract the bundle
  const zip = new AdmZip(zipPath);
  const tmpDir = destDir + "-tmp";
  fs.mkdirSync(tmpDir, { recursive: true });
  zip.extractAllTo(tmpDir, true);

  // Flatten if zip had a single top-level folder
  const entries = fs.readdirSync(tmpDir).filter((e) => !e.startsWith("."));
  if (entries.length === 1) {
    const singleDir = path.join(tmpDir, entries[0]);
    if (fs.statSync(singleDir).isDirectory()) {
      const innerEntries = fs.readdirSync(singleDir);
      for (const entry of innerEntries) {
        fs.renameSync(path.join(singleDir, entry), path.join(tmpDir, entry));
      }
      fs.rmdirSync(singleDir);
    }
  }

  // Detect if this is a lightweight bundle (has app.json but no index.html)
  // or a full bundle (has index.html)
  const isLightweight =
    fs.existsSync(path.join(tmpDir, "app.json")) &&
    !fs.existsSync(path.join(tmpDir, "index.html"));

  if (isLightweight) {
    // Lightweight bundle: only store app-specific files.
    // The shared runtime in userData/runtime/ is used at serve time
    // via layered fallback in startAppServer().
    console.log("[add-app] Lightweight bundle — using shared runtime (no 59 MB copy)");

    // Copy app.json (the app's R source code)
    fs.copyFileSync(path.join(tmpDir, "app.json"), path.join(destDir, "app.json"));

    // Copy extra R packages if present (app-specific packages only)
    const pkgDir = path.join(tmpDir, "packages");
    if (fs.existsSync(pkgDir)) {
      const destPkgDir = path.join(destDir, "shinylive", "webr", "packages");
      fs.mkdirSync(destPkgDir, { recursive: true });
      copyDirSync(pkgDir, destPkgDir);
      console.log("[add-app] Copied app-specific packages");
    }

    // Copy bundle metadata and icon (for per-app icons in the launcher)
    const metaFile = path.join(tmpDir, "bundle-meta.json");
    if (fs.existsSync(metaFile)) {
      fs.copyFileSync(metaFile, path.join(destDir, "bundle-meta.json"));
    }
    const iconFile = path.join(tmpDir, "icon.png");
    if (fs.existsSync(iconFile)) {
      fs.copyFileSync(iconFile, path.join(destDir, "icon.png"));
    }
  } else {
    // Full bundle — copy everything directly (includes its own runtime)
    console.log("[add-app] Full bundle — copying complete runtime");
    copyDirSync(tmpDir, destDir);
  }

  // Clean up temp dir
  fs.rmSync(tmpDir, { recursive: true, force: true });
}

// ---- Import a bundle by file path (shared logic) ---------------------------
function importAppFromPath(zipPath) {
  const ext = path.extname(zipPath);
  const defaultName = path.basename(zipPath, ext).replace(/[-_]/g, " ");
  const id = Date.now().toString(36) + Math.random().toString(36).slice(2, 6);
  const folderName = defaultName.toLowerCase().replace(/[^a-z0-9]+/g, "-") + "-" + id;
  const appDir = path.join(APPS_DIR, folderName);

  try {
    console.log(`[add-app] Extracting ${zipPath} to ${appDir}`);
    extractAndAssembleApp(zipPath, appDir);

    // Check for critical files in app dir OR shared runtime
    const hasAppIndex = fs.existsSync(path.join(appDir, "index.html"));
    const hasRuntimeIndex = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "index.html"));
    const hasAppJson = fs.existsSync(path.join(appDir, "app.json"));
    const hasAppSW = fs.existsSync(path.join(appDir, "shinylive-sw.js"));
    const hasRuntimeSW = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "shinylive-sw.js"));
    console.log(`[add-app] Assembly complete — index.html: app=${hasAppIndex} runtime=${hasRuntimeIndex}, app.json: ${hasAppJson}, shinylive-sw.js: app=${hasAppSW} runtime=${hasRuntimeSW}`);

    if (!hasAppIndex && !hasRuntimeIndex) {
      fs.rmSync(appDir, { recursive: true, force: true });
      return { error: "No index.html found in app or shared runtime. Make sure this is a valid Shinylive bundle." };
    }

    const library = loadLibrary();
    const entry = { id, name: defaultName, folderName, dateAdded: new Date().toISOString() };

    // Check for bundle-meta.json (per-app icon, name, and icon styling)
    const metaPath = path.join(appDir, "bundle-meta.json");
    if (fs.existsSync(metaPath)) {
      try {
        const meta = JSON.parse(fs.readFileSync(metaPath, "utf-8"));
        if (meta.appName) entry.name = meta.appName;
        if (meta.icon && fs.existsSync(path.join(appDir, meta.icon))) {
          entry.icon = meta.icon; // relative path within the app folder
        }
        if (meta.iconBackground) entry.iconBackground = meta.iconBackground;
        if (meta.iconBorder) entry.iconBorder = meta.iconBorder;
        console.log(`[add-app] Read bundle-meta.json — name: "${entry.name}", icon: ${entry.icon || "none"}, iconBg: ${entry.iconBackground || "default"}, iconBorder: ${entry.iconBorder || "default"}`);
      } catch {
        console.log("[add-app] Could not parse bundle-meta.json, using defaults");
      }
    }

    library.push(entry);
    saveLibrary(library);
    console.log(`[add-app] Added "${entry.name}" (${id}) — folder: ${folderName}`);
    return entry;
  } catch (err) {
    console.error(`[add-app] Error: ${err.message}`);
    if (fs.existsSync(appDir)) fs.rmSync(appDir, { recursive: true, force: true });
    return { error: err.message };
  }
}

// ---- IPC handlers -----------------------------------------------------------
ipcMain.handle("get-config", () => launcherConfig);

ipcMain.handle("get-apps", () => loadLibrary());

ipcMain.handle("get-app-icon", (event, id) => {
  const library = loadLibrary();
  const entry = library.find((a) => a.id === id);
  if (!entry || !entry.icon) return null;
  const iconPath = path.join(APPS_DIR, entry.folderName, entry.icon);
  if (!fs.existsSync(iconPath)) return null;
  // Read icon as base64 data URL so the renderer can display it directly
  const data = fs.readFileSync(iconPath);
  const ext = path.extname(entry.icon).slice(1).toLowerCase();
  const mime = ext === "png" ? "image/png" : ext === "jpg" || ext === "jpeg" ? "image/jpeg" : "image/png";
  return `data:${mime};base64,${data.toString("base64")}`;
});

ipcMain.handle("add-app", async (event) => {
  const win = BrowserWindow.fromWebContents(event.sender);

  const result = await dialog.showOpenDialog(win, {
    title: `Select a Shinylive App (.${APP_EXTENSION})`,
    filters: [
      { name: "Shinylive App", extensions: [APP_EXTENSION] },
      { name: "Zip Archives", extensions: ["zip"] },
    ],
    properties: ["openFile"],
  });

  if (result.canceled || result.filePaths.length === 0) return null;
  return importAppFromPath(result.filePaths[0]);
});

ipcMain.handle("add-app-by-path", (event, filePath) => {
  if (!fs.existsSync(filePath)) return { error: "File not found" };
  return importAppFromPath(filePath);
});

ipcMain.handle("rename-app", (event, id, newName) => {
  const library = loadLibrary();
  const entry = library.find((a) => a.id === id);
  if (!entry) return false;
  entry.name = newName;
  saveLibrary(library);
  return true;
});

ipcMain.handle("remove-app", (event, id) => {
  const library = loadLibrary();
  const entry = library.find((a) => a.id === id);
  if (!entry) return false;

  if (activeAppId === id) stopActiveServer();

  const appDir = path.join(APPS_DIR, entry.folderName);
  if (fs.existsSync(appDir)) fs.rmSync(appDir, { recursive: true, force: true });

  saveLibrary(library.filter((a) => a.id !== id));
  return true;
});

ipcMain.handle("launch-app", async (event, id) => {
  const library = loadLibrary();
  const entry = library.find((a) => a.id === id);
  if (!entry) return { error: "App not found" };

  const appDir = path.join(APPS_DIR, entry.folderName);
  if (!fs.existsSync(appDir)) return { error: "App files missing" };

  // Verify critical files exist (check both app dir and shared runtime)
  const hasAppIndex = fs.existsSync(path.join(appDir, "index.html"));
  const hasRuntimeIndex = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "index.html"));
  const hasAppSW = fs.existsSync(path.join(appDir, "shinylive-sw.js"));
  const hasRuntimeSW = fs.existsSync(path.join(SHARED_RUNTIME_DIR, "shinylive-sw.js"));
  console.log(`[launch] App dir: ${appDir}`);
  console.log(`[launch] index.html: app=${hasAppIndex} runtime=${hasRuntimeIndex}`);
  console.log(`[launch] shinylive-sw.js: app=${hasAppSW} runtime=${hasRuntimeSW}`);
  console.log(`[launch] app.json: ${fs.existsSync(path.join(appDir, "app.json"))}`);

  if (!hasAppIndex && !hasRuntimeIndex) {
    return { error: "index.html not found in app directory or shared runtime" };
  }

  // Stop any previously running app server
  stopActiveServer();

  try {
    const { server, port } = await startAppServer(appDir);
    activeServer = server;
    activeAppId = id;
    console.log(`[launch] Server started on http://127.0.0.1:${port}`);
    return { success: true, port, name: entry.name };
  } catch (err) {
    console.error(`[launch] Error: ${err.message}`);
    return { error: err.message };
  }
});

ipcMain.handle("stop-app", () => {
  stopActiveServer();
  return true;
});

// ---- Main window ------------------------------------------------------------
let mainWindow;

// Set the About panel to always show Resondex copyright
app.setAboutPanelOptions({
  applicationName: launcherConfig.appName,
  copyright: `Copyright \u00A9 ${new Date().getFullYear()} Resondex`,
});

function createMainWindow() {
  const menu = Menu.buildFromTemplate([
    {
      label: launcherConfig.appName,
      submenu: [
        { role: "about" },
        { type: "separator" },
        { role: "quit" },
      ],
    },
    {
      label: "Edit",
      submenu: [
        { role: "undo" }, { role: "redo" },
        { type: "separator" },
        { role: "cut" }, { role: "copy" }, { role: "paste" }, { role: "selectAll" },
      ],
    },
    {
      label: "View",
      submenu: [
        { role: "reload" }, { role: "toggleDevTools" },
        { type: "separator" },
        { role: "zoomIn" }, { role: "zoomOut" }, { role: "resetZoom" },
      ],
    },
  ]);
  Menu.setApplicationMenu(menu);

  const winOptions = {
    width: 1100,
    height: 750,
    title: launcherConfig.appName,
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      nodeIntegration: false,
      contextIsolation: true,
    },
  };

  // Set window icon if configured
  if (launcherConfig.icon) {
    const iconPath = path.resolve(__dirname, launcherConfig.icon);
    if (fs.existsSync(iconPath)) {
      winOptions.icon = iconPath;
    }
  }

  mainWindow = new BrowserWindow(winOptions);

  mainWindow.loadFile(path.join(__dirname, "renderer", "index.html"));
}

// ---- Handle file open from OS -----------------------------------------------
// macOS: open-file fires when user double-clicks a .resondex file
app.on("open-file", (event, filePath) => {
  event.preventDefault();
  console.log(`[open-file] Received: ${filePath}`);

  if (!mainWindow) {
    // App not ready yet — queue it
    pendingFilePath = filePath;
    return;
  }

  handleExternalFileOpen(filePath);
});

function handleExternalFileOpen(filePath) {
  if (!filePath || !fs.existsSync(filePath)) return;

  const ext = path.extname(filePath).toLowerCase();
  if (ext !== `.${APP_EXTENSION}` && ext !== ".zip") return;

  const result = importAppFromPath(filePath);
  if (mainWindow && mainWindow.webContents) {
    mainWindow.webContents.send("file-opened", result);
  }
}

// ---- Single-instance lock (Windows/Linux: second launch passes file to first) -
const gotTheLock = app.requestSingleInstanceLock();
if (!gotTheLock) {
  app.quit();
} else {
  app.on("second-instance", (event, argv) => {
    // Focus existing window
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }

    // On Windows/Linux, the file path is in argv
    const fileArg = argv.find((arg) => {
      const ext = path.extname(arg).toLowerCase();
      return ext === `.${APP_EXTENSION}` || ext === ".zip";
    });

    if (fileArg) handleExternalFileOpen(fileArg);
  });

  app.whenReady().then(() => {
    createMainWindow();

    // Windows/Linux: file path is passed as a command-line argument on first launch
    const fileArg = process.argv.find((arg) => {
      const ext = path.extname(arg).toLowerCase();
      return ext === `.${APP_EXTENSION}` || ext === ".zip";
    });

    // Process pending file (macOS open-file before ready) or argv file
    const fileToOpen = pendingFilePath || fileArg;
    if (fileToOpen) {
      mainWindow.webContents.once("did-finish-load", () => {
        handleExternalFileOpen(fileToOpen);
      });
      pendingFilePath = null;
    }
  });

  // macOS: re-open window when dock icon is clicked
  app.on("activate", () => {
    if (BrowserWindow.getAllWindows().length === 0) createMainWindow();
  });

  app.on("window-all-closed", () => {
    stopActiveServer();
    app.quit();
  });
}
