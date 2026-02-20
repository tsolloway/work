const libraryView = document.getElementById("library-view");
const appView = document.getElementById("app-view");
const grid = document.getElementById("app-grid");
const emptyState = document.getElementById("empty-state");
const emptyAddBtn = document.getElementById("empty-add-btn");
const fabAdd = document.getElementById("fab-add");
const backBtn = document.getElementById("back-btn");
const appBarTitle = document.getElementById("app-bar-title");
const appFrame = document.getElementById("app-frame");
const renameModal = document.getElementById("rename-modal");
const renameInput = document.getElementById("rename-input");
const renameCancel = document.getElementById("rename-cancel");
const renameSave = document.getElementById("rename-save");

let renameTargetId = null;
let appFileExtension = "resondex"; // Updated from config on init

// ---- View switching ---------------------------------------------------------
function showLibrary() {
  appFrame.src = "about:blank";
  window.launcherAPI.stopApp();
  appView.style.display = "none";
  libraryView.style.display = "flex";
  renderApps();
}

function showApp(port, name) {
  libraryView.style.display = "none";
  appView.style.display = "flex";
  appBarTitle.textContent = name;
  appFrame.src = `http://localhost:${port}`;
}

// ---- Render apps ------------------------------------------------------------
async function renderApps() {
  const apps = await window.launcherAPI.getApps();
  grid.innerHTML = "";

  if (apps.length === 0) {
    emptyState.style.display = "flex";
    fabAdd.style.display = "none";
    grid.style.display = "none";
    return;
  }

  emptyState.style.display = "none";
  fabAdd.style.display = "flex";
  grid.style.display = "grid";

  for (const app of apps) {
    const card = document.createElement("div");
    card.className = "app-card";
    card.dataset.id = app.id;

    const initial = app.name.charAt(0).toUpperCase();
    const dateStr = new Date(app.dateAdded).toLocaleDateString();

    card.innerHTML = `
      <div class="app-actions">
        <button class="rename" title="Rename">&#9998;</button>
        <button class="delete" title="Remove">&times;</button>
      </div>
      <div class="app-icon${app.icon ? '' : ' has-initial'}">${app.icon ? '' : initial}</div>
      <div class="app-name">${escapeHtml(app.name)}</div>
      <div class="app-date">Added ${dateStr}</div>
    `;

    // Load per-app icon if available
    if (app.icon) {
      const iconEl = card.querySelector(".app-icon");
      // Apply per-app icon styling from bundle metadata
      if (app.iconBackground) iconEl.style.background = app.iconBackground;
      if (app.iconBorder) iconEl.style.borderColor = app.iconBorder;

      window.launcherAPI.getAppIcon(app.id).then((dataUrl) => {
        if (dataUrl) {
          iconEl.textContent = "";
          iconEl.innerHTML = `<img src="${dataUrl}" alt="${escapeHtml(app.name)}" style="width:100%;height:100%;object-fit:contain;border-radius:inherit;" />`;
        }
      });
    }

    card.addEventListener("click", async (e) => {
      if (e.target.closest(".app-actions")) return;
      card.classList.add("loading");
      const result = await window.launcherAPI.launchApp(app.id);
      card.classList.remove("loading");
      if (result.error) {
        alert("Failed to launch: " + result.error);
      } else {
        showApp(result.port, result.name);
      }
    });

    card.querySelector(".rename").addEventListener("click", (e) => {
      e.stopPropagation();
      renameTargetId = app.id;
      renameInput.value = app.name;
      renameModal.style.display = "flex";
      renameInput.focus();
      renameInput.select();
    });

    card.querySelector(".delete").addEventListener("click", async (e) => {
      e.stopPropagation();
      if (confirm(`Remove "${app.name}"? This will delete the app files.`)) {
        await window.launcherAPI.removeApp(app.id);
        renderApps();
      }
    });

    grid.appendChild(card);
  }
}

// ---- Add app ----------------------------------------------------------------
async function addApp() {
  const result = await window.launcherAPI.addApp();
  if (!result) return;
  if (result.error) {
    alert("Failed to add app: " + result.error);
    return;
  }
  renderApps();
}

// ---- Rename modal -----------------------------------------------------------
renameSave.addEventListener("click", async () => {
  const newName = renameInput.value.trim();
  if (!newName || !renameTargetId) return;
  await window.launcherAPI.renameApp(renameTargetId, newName);
  renameModal.style.display = "none";
  renameTargetId = null;
  renderApps();
});

renameCancel.addEventListener("click", () => {
  renameModal.style.display = "none";
  renameTargetId = null;
});

renameInput.addEventListener("keydown", (e) => {
  if (e.key === "Enter") renameSave.click();
  if (e.key === "Escape") renameCancel.click();
});

// ---- Event listeners --------------------------------------------------------
emptyAddBtn.addEventListener("click", addApp);
fabAdd.addEventListener("click", addApp);
backBtn.addEventListener("click", showLibrary);

// ---- Drag and drop ----------------------------------------------------------
const dropOverlay = document.getElementById("drop-overlay");
let dragCounter = 0;

document.addEventListener("dragenter", (e) => {
  e.preventDefault();
  dragCounter++;
  if (dragCounter === 1) dropOverlay.classList.add("visible");
});

document.addEventListener("dragleave", (e) => {
  e.preventDefault();
  dragCounter--;
  if (dragCounter === 0) dropOverlay.classList.remove("visible");
});

document.addEventListener("dragover", (e) => {
  e.preventDefault();
  e.dataTransfer.dropEffect = "copy";
});

document.addEventListener("drop", async (e) => {
  e.preventDefault();
  dragCounter = 0;
  dropOverlay.classList.remove("visible");

  const files = Array.from(e.dataTransfer.files);
  const validFiles = files.filter((f) => {
    const name = f.name.toLowerCase();
    return name.endsWith(`.${appFileExtension}`) || name.endsWith(".zip");
  });

  if (validFiles.length === 0) {
    alert(`Only .${appFileExtension} or .zip files can be added.`);
    return;
  }

  for (const file of validFiles) {
    const filePath = window.launcherAPI.getPathForFile(file);
    const result = await window.launcherAPI.addAppByPath(filePath);
    if (result && result.error) {
      alert(`Failed to add "${file.name}": ${result.error}`);
    }
  }

  renderApps();
});

// ---- Helpers ----------------------------------------------------------------
function escapeHtml(str) {
  const div = document.createElement("div");
  div.textContent = str;
  return div.innerHTML;
}

// ---- Handle files opened externally (double-click .resondex in Finder) ------
window.launcherAPI.onFileOpened((result) => {
  if (result && result.error) {
    alert("Failed to add app: " + result.error);
  }
  renderApps();
});

// ---- Apply config (branding, colors) ----------------------------------------
async function applyConfig() {
  const config = await window.launcherAPI.getConfig();

  // Set header text
  document.getElementById("header-title").textContent = config.headerTitle;
  document.getElementById("header-subtitle").textContent = config.headerSubtitle;

  // Set page title
  document.title = config.appName;

  // Apply CSS custom properties for colors
  const root = document.documentElement;
  root.style.setProperty("--accent-color", config.accentColor);

  // Header background: detect color vs image path
  const bg = config.headerBackground;
  const isColor = /^(#|rgb|hsl|hwb|lab|lch|oklch|oklab|transparent|inherit|currentColor)/.test(bg.trim());
  if (isColor) {
    root.style.setProperty("--header-bg", bg);
  } else {
    // Treat as image path (relative to app root or absolute)
    root.style.setProperty("--header-bg", `url("${bg}") center/cover no-repeat`);
  }

  // File extension
  appFileExtension = config.fileExtension || "resondex";

  // Update empty state help text with configured extension
  const emptyHint = document.querySelector("#empty-state p");
  if (emptyHint) {
    emptyHint.textContent = `Drag a .${appFileExtension} file here or click the button below`;
  }
}

// ---- Init -------------------------------------------------------------------
applyConfig();
renderApps();
