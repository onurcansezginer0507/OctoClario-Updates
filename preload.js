// preload.js
const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  // ---------- Dialogs ----------
  chooseFolder: () => ipcRenderer.invoke('dialog:openFolder'),

  // Only keep this if you actually implement ipcMain.handle('dialog:openFile') in main.js
  chooseFile: (filters) => ipcRenderer.invoke('dialog:openFile', filters || []),

  saveFile: (defaultName, filters) =>
    ipcRenderer.invoke('dialog:saveFile', defaultName, filters),

  // ---------- Updater (manual) ----------
  checkForUpdates: () => ipcRenderer.invoke('updater:check'),
  downloadUpdate:  () => ipcRenderer.invoke('updater:download'),
  installUpdate:   () => ipcRenderer.invoke('updater:install'),

  // Listen for updater events emitted from main.js
  onUpdaterEvent: (callback) => {
    if (typeof callback !== 'function') return;
    // optional: remove old listeners to avoid duplicates on hot reload
    ipcRenderer.removeAllListeners('updater:event');
    ipcRenderer.on('updater:event', (_event, payload) => callback(payload));
  },

  // ---------- Help / external links ----------
  openExternal: (url) => ipcRenderer.invoke('shell:openExternal', url)
});
