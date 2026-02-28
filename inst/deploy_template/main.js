const { app, BrowserWindow, ipcMain, dialog, Menu } = require("electron");
const path = require("path");
const fs = require("fs");
const net = require("net");
const { spawn } = require("child_process");
const AdmZip = require("adm-zip");

// ---- Configuration ----------------------------------------------------------

const CONFIG_FILE = path.join(__dirname, "launcher.config.json");
let launcherConfig = {
  appName: "App Launcher",
  headerTitle: "App Launcher",
  headerSubtitle: "Load and run Shiny apps",
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

const APP_EXTENSION = launcherConfig.fileExtension;

// ---- Pending file open (received before window is ready) --------------------
let pendingFilePath = null;

// ---- Paths ------------------------------------------------------------------
const USER_DATA = app.getPath("userData");
const APPS_DIR = path.join(USER_DATA, "apps");
const LIBRARY_FILE = path.join(USER_DATA, "library.json");

// ---- R process tracking -----------------------------------------------------
// Map of appId -> { process, port }
const activeRProcesses = new Map();

// ---- Fresh install detection -------------------------------------------------
const INSTALL_MARKER = path.join(USER_DATA, ".install-id");
const BUILD_TIMESTAMP = launcherConfig.buildTimestamp || "unknown";
const currentInstallId = `${launcherConfig.appName}-${app.getVersion()}-${BUILD_TIMESTAMP}`;

function cleanStaleData() {
  try {
    if (fs.existsSync(INSTALL_MARKER)) {
      const existing = fs.readFileSync(INSTALL_MARKER, "utf-8").trim();
      if (existing === currentInstallId) return;
    }
    console.log("[init] New install detected, clearing stale app data...");
    if (fs.existsSync(APPS_DIR)) fs.rmSync(APPS_DIR, { recursive: true, force: true });
    if (fs.existsSync(LIBRARY_FILE)) fs.unlinkSync(LIBRARY_FILE);
    fs.mkdirSync(APPS_DIR, { recursive: true });
    fs.writeFileSync(INSTALL_MARKER, currentInstallId);
    console.log("[init] Clean slate ready");
  } catch (err) {
    console.error("[init] Error cleaning stale data:", err.message);
  }
}

cleanStaleData();

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

// ---- R paths ----------------------------------------------------------------
// Determine where the bundled R installation lives. In a packaged Electron
// app, __dirname is inside the asar archive, but asarUnpack puts RPortable/
// in the unpacked resources directory.
function getRPaths() {
  const isPackaged = app.isPackaged;
  let resourcesPath;

  if (isPackaged) {
    // In packaged app: resources are at <app>.app/Contents/Resources/app.asar.unpacked/
    resourcesPath = path.join(path.dirname(app.getAppPath()), "app.asar.unpacked");
  } else {
    // In development: same dir as main.js
    resourcesPath = __dirname;
  }

  const rHome = path.join(resourcesPath, "RPortable");
  // On macOS, use bin/R (shell script) instead of bin/Rscript (compiled binary)
  // because Rscript has R_HOME hardcoded at compile time and can't be relocated.
  // bin/R is a shell script we patch to compute R_HOME from its own location.
  const rBin = process.platform === "win32"
    ? path.join(rHome, "bin", "Rscript.exe")
    : path.join(rHome, "bin", "R");
  const launcherLib = path.join(rHome, "library");

  return { rHome, rBin, launcherLib, resourcesPath };
}

// ---- Port utilities ---------------------------------------------------------
function findFreePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.on("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address();
      server.close(() => resolve(port));
    });
  });
}

function waitForPort(port, timeout = 30000) {
  const start = Date.now();
  return new Promise((resolve, reject) => {
    function tryConnect() {
      if (Date.now() - start > timeout) {
        return reject(new Error(`R process did not start within ${timeout / 1000}s`));
      }
      const sock = net.createConnection({ port, host: "127.0.0.1" }, () => {
        sock.end();
        resolve();
      });
      sock.on("error", () => {
        setTimeout(tryConnect, 200);
      });
    }
    tryConnect();
  });
}

// ---- R process management ---------------------------------------------------
function launchRProcess(appDir, port) {
  const { rHome, rBin, launcherLib } = getRPaths();

  if (!fs.existsSync(rBin)) {
    throw new Error(`R not found at ${rBin}. The launcher may be missing its bundled R installation.`);
  }

  // Build library paths: app-specific library (extras) takes priority,
  // launcher library (base + work deps) as fallback
  const appLib = path.join(appDir, "library");
  const libPaths = fs.existsSync(appLib)
    ? [appLib, launcherLib]
    : [launcherLib];

  const rCode = [
    `.libPaths(c(${libPaths.map(p => `"${p.replace(/\\/g, '/')}"` ).join(", ")}))`,
    `shiny::runApp("${appDir.replace(/\\/g, '/')}", port = ${port}, host = "127.0.0.1", launch.browser = FALSE)`
  ].join("; ");

  const env = { ...process.env, R_HOME: rHome };

  // macOS: set dynamic library path so R can find its shared libraries
  if (process.platform === "darwin") {
    const rLib = path.join(rHome, "lib");
    env.DYLD_LIBRARY_PATH = env.DYLD_LIBRARY_PATH
      ? `${rLib}:${env.DYLD_LIBRARY_PATH}`
      : rLib;
    // Prevent R from trying to use the user's site library
    env.R_LIBS_USER = " ";
  }

  // Windows: add R bin to PATH
  if (process.platform === "win32") {
    const rBinDir = path.join(rHome, "bin", "x64");
    env.PATH = `${rBinDir};${env.PATH || ""}`;
    env.R_LIBS_USER = " ";
  }

  // On macOS, use "R --no-echo -e" instead of "Rscript -e" because the
  // Rscript binary has R_HOME hardcoded at compile time.
  const spawnArgs = process.platform === "win32"
    ? ["-e", rCode]
    : ["--no-echo", "-e", rCode];

  console.log(`[R] Spawning: ${rBin} ${spawnArgs.join(" ")}`);
  console.log(`[R] R_HOME: ${rHome}`);
  console.log(`[R] App dir: ${appDir}`);
  console.log(`[R] Library paths: ${libPaths.join(", ")}`);

  const child = spawn(rBin, spawnArgs, {
    cwd: appDir,
    env,
    stdio: ["ignore", "pipe", "pipe"],
    windowsHide: true,
  });

  child.stdout.on("data", (data) => {
    console.log(`[R stdout] ${data.toString().trim()}`);
  });

  child.stderr.on("data", (data) => {
    console.log(`[R stderr] ${data.toString().trim()}`);
  });

  child.on("error", (err) => {
    console.error(`[R] Process error: ${err.message}`);
  });

  child.on("exit", (code, signal) => {
    console.log(`[R] Process exited — code: ${code}, signal: ${signal}`);
  });

  return child;
}

function stopRProcess(appId) {
  const entry = activeRProcesses.get(appId);
  if (!entry) return;

  const { process: proc } = entry;
  console.log(`[R] Stopping process for app ${appId} (pid: ${proc.pid})`);

  try {
    if (process.platform === "win32") {
      // Windows: use taskkill to kill the process tree
      spawn("taskkill", ["/pid", proc.pid.toString(), "/f", "/t"], { windowsHide: true });
    } else {
      proc.kill("SIGTERM");
      // Force kill after 3 seconds if still running
      setTimeout(() => {
        try {
          proc.kill("SIGKILL");
        } catch {
          // Already dead, ignore
        }
      }, 3000);
    }
  } catch {
    // Process may already be dead
  }

  activeRProcesses.delete(appId);
}

function stopAllRProcesses() {
  for (const appId of activeRProcesses.keys()) {
    stopRProcess(appId);
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

// ---- Extract and assemble app -----------------------------------------------
function extractAndAssembleApp(zipPath, destDir) {
  fs.mkdirSync(destDir, { recursive: true });

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

  // Copy all extracted files to the destination
  copyDirSync(tmpDir, destDir);

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

    // Validate: must contain app.R
    const hasAppR = fs.existsSync(path.join(appDir, "app.R"));
    console.log(`[add-app] Assembly complete — app.R: ${hasAppR}`);

    if (!hasAppR) {
      fs.rmSync(appDir, { recursive: true, force: true });
      return { error: "No app.R found in bundle. Make sure this is a valid app bundle." };
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
          entry.icon = meta.icon;
        }
        if (meta.iconBackground) entry.iconBackground = meta.iconBackground;
        if (meta.iconBorder) entry.iconBorder = meta.iconBorder;
        console.log(`[add-app] Read bundle-meta.json — name: "${entry.name}", icon: ${entry.icon || "none"}`);
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
  const data = fs.readFileSync(iconPath);
  const ext = path.extname(entry.icon).slice(1).toLowerCase();
  const mime = ext === "png" ? "image/png" : ext === "jpg" || ext === "jpeg" ? "image/jpeg" : "image/png";
  return `data:${mime};base64,${data.toString("base64")}`;
});

ipcMain.handle("add-app", async (event) => {
  const win = BrowserWindow.fromWebContents(event.sender);

  const result = await dialog.showOpenDialog(win, {
    title: `Select an App Bundle (.${APP_EXTENSION})`,
    filters: [
      { name: "App Bundle", extensions: [APP_EXTENSION] },
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

  // Stop R process if running for this app
  stopRProcess(id);

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

  // Verify app.R exists
  if (!fs.existsSync(path.join(appDir, "app.R"))) {
    return { error: "app.R not found in app directory" };
  }

  // Stop any previously running R process for this app
  stopRProcess(id);

  try {
    const port = await findFreePort();
    console.log(`[launch] Starting R for "${entry.name}" on port ${port}`);
    console.log(`[launch] App dir: ${appDir}`);

    const rProc = launchRProcess(appDir, port);

    activeRProcesses.set(id, { process: rProc, port });

    // Collect stderr for error reporting
    let stderrBuf = "";
    rProc.stderr.on("data", (data) => {
      stderrBuf += data.toString();
    });

    // Race: wait for port to open OR R process to exit (whichever comes first)
    await Promise.race([
      waitForPort(port),
      new Promise((_, reject) => {
        rProc.on("exit", (code, signal) => {
          const reason = stderrBuf.trim()
            ? `R exited (code ${code}): ${stderrBuf.trim().split("\n").slice(-10).join("\n")}`
            : `R exited with code ${code} (signal: ${signal})`;
          reject(new Error(reason));
        });
      }),
    ]);

    console.log(`[launch] R is ready on http://127.0.0.1:${port}`);

    return { success: true, port, name: entry.name, appId: id };
  } catch (err) {
    console.error(`[launch] Error: ${err.message}`);
    stopRProcess(id);
    return { error: err.message };
  }
});

ipcMain.handle("stop-app", (event, appId) => {
  if (appId) {
    stopRProcess(appId);
  } else {
    // Stop all R processes (backward compat)
    stopAllRProcesses();
  }
  return true;
});

// ---- Main window ------------------------------------------------------------
let mainWindow;

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
app.on("open-file", (event, filePath) => {
  event.preventDefault();
  console.log(`[open-file] Received: ${filePath}`);

  if (!mainWindow) {
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

// ---- Single-instance lock ---------------------------------------------------
const gotTheLock = app.requestSingleInstanceLock();
if (!gotTheLock) {
  app.quit();
} else {
  app.on("second-instance", (event, argv) => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }

    const fileArg = argv.find((arg) => {
      const ext = path.extname(arg).toLowerCase();
      return ext === `.${APP_EXTENSION}` || ext === ".zip";
    });

    if (fileArg) handleExternalFileOpen(fileArg);
  });

  app.whenReady().then(() => {
    createMainWindow();

    const fileArg = process.argv.find((arg) => {
      const ext = path.extname(arg).toLowerCase();
      return ext === `.${APP_EXTENSION}` || ext === ".zip";
    });

    const fileToOpen = pendingFilePath || fileArg;
    if (fileToOpen) {
      mainWindow.webContents.once("did-finish-load", () => {
        handleExternalFileOpen(fileToOpen);
      });
      pendingFilePath = null;
    }
  });

  app.on("activate", () => {
    if (BrowserWindow.getAllWindows().length === 0) createMainWindow();
  });

  app.on("window-all-closed", () => {
    stopAllRProcesses();
    app.quit();
  });
}
