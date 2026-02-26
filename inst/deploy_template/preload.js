const { contextBridge, ipcRenderer, webUtils } = require("electron");

contextBridge.exposeInMainWorld("launcherAPI", {
  getConfig: () => ipcRenderer.invoke("get-config"),
  getApps: () => ipcRenderer.invoke("get-apps"),
  getAppIcon: (id) => ipcRenderer.invoke("get-app-icon", id),
  addApp: () => ipcRenderer.invoke("add-app"),
  addAppByPath: (filePath) => ipcRenderer.invoke("add-app-by-path", filePath),
  renameApp: (id, newName) => ipcRenderer.invoke("rename-app", id, newName),
  removeApp: (id) => ipcRenderer.invoke("remove-app", id),
  launchApp: (id) => ipcRenderer.invoke("launch-app", id),
  stopApp: (appId) => ipcRenderer.invoke("stop-app", appId),
  getPathForFile: (file) => webUtils.getPathForFile(file),
  onFileOpened: (callback) => ipcRenderer.on("file-opened", (event, result) => callback(result)),
});
