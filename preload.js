// preload.js
const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  // Native folder selection
  chooseFolder: () => ipcRenderer.invoke('dialog:openFolder'),

  // Native file selection (if needed)
  chooseFile: (filters) => ipcRenderer.invoke('dialog:openFile', filters || []),

  // Save file dialog
  saveFile: (defaultName, filters) =>
    ipcRenderer.invoke('dialog:saveFile', defaultName, filters)
});
