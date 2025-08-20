const { app, BrowserWindow, ipcMain, dialog, Menu } = require('electron');
const path = require('path');
const { spawn } = require('child_process');
const http = require('http');
const fs = require('fs');

// ⬇️ NEW: updater + tree-kill
const { autoUpdater } = require('electron-updater');
const treeKill = require('tree-kill');

// ---- Check for dev mode ----
const isDev = process.argv.includes('--dev');
console.log(`[App Mode] Running in ${isDev ? 'DEV' : 'PRODUCTION'} mode`);

let mainWindow = null;
let rProcess = null;
const pidFile = () => path.join(app.getPath('userData'), 'r-pid.txt');

// ---- Wait for Shiny to be ready ----
function waitForShinyServer(url, timeoutMs = 30000) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () => {
      http
        .get(url, (res) => {
          if (res.statusCode === 200 || res.statusCode === 302) return resolve();
          res.resume();
          if (Date.now() - start > timeoutMs) return reject(new Error('Timed out waiting for Shiny'));
          setTimeout(tick, 400);
        })
        .on('error', () => {
          if (Date.now() - start > timeoutMs) return reject(new Error('Timed out waiting for Shiny'));
          setTimeout(tick, 400);
        });
    };
    tick();
  });
}

// ---- Launch R and extract port ----
function launchRAndGetPort() {
  return new Promise((resolve, reject) => {
    const basePath = isDev ? __dirname : process.resourcesPath;
    const rscriptPath = path.join(basePath, 'R-portable', 'bin', 'Rscript.exe');
    const shinyScript = path.join(basePath, 'run-shiny.R');

    if (!fs.existsSync(rscriptPath)) {
      dialog.showErrorBox('Rscript not found', rscriptPath);
      return reject(new Error('Rscript.exe not found'));
    }

    // ⬇️ Important spawn options to prevent orphaning
    const rProc = spawn(rscriptPath, [shinyScript], {
      cwd: basePath,
      windowsHide: true,
      detached: false,
      shell: false,
      stdio: ['ignore', 'pipe', 'pipe']
    });

    // Write PID so uninstaller can kill if needed
    try {
      fs.mkdirSync(app.getPath('userData'), { recursive: true });
      fs.writeFileSync(pidFile(), String(rProc.pid));
    } catch (e) {
      console.warn('[PID] Could not write PID file:', e);
    }

    rProc.stdout.on('data', (data) => {
      const output = data.toString();
      console.log(`[R] ${output}`);

      if (output.includes('>>>ERROR_START<<<')) {
        reject(new Error('R failure: ' + output));
        return;
      }

      const match = output.match(/>>>PORT:\s*(\d+)/);
      if (match) {
        const port = match[1];
        resolve({ port, rProc });
      }
    });

    rProc.stderr.on('data', (data) => {
      console.error(`[R ERR] ${data}`);
    });

    rProc.on('close', (code) => {
      console.log(`[R] exited with code ${code}`);
      if (!mainWindow) {
        reject(new Error(`R exited before Shiny started (exit code ${code})`));
      }
    });
  });
}

// ---- Create Browser Window ----
function createWindow(port) {
  const url = `http://127.0.0.1:${port}`;

  mainWindow = new BrowserWindow({
    width: 1024,
    height: 768,
    webPreferences: {
      preload: path.join(app.getAppPath(), 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false
    }
  });

  waitForShinyServer(url)
    .then(() => mainWindow.loadURL(url))
    .catch((err) => {
      dialog.showErrorBox('Shiny not responding', err.message);
    });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
}

// ---- IPC ----
ipcMain.handle('dialog:openFolder', async () => {
  const result = await dialog.showOpenDialog({ properties: ['openDirectory'] });
  return result.canceled ? null : result.filePaths[0];
});

ipcMain.handle('dialog:saveFile', async (event, defaultName, filters) => {
  const { canceled, filePath } = await dialog.showSaveDialog({
    defaultPath: defaultName,
    filters: filters,
    properties: ['createDirectory']
  });
  return canceled ? null : filePath;
});

// ---- Shutdown handling (tree kill + cleanup) ----
function killRProcessTree(signal = 'SIGTERM') {
  return new Promise((resolve) => {
    if (!rProcess || rProcess.killed) {
      cleanupPidFile();
      return resolve();
    }
    const pid = rProcess.pid;
    console.log(`[Shutdown] Killing R process tree (pid=${pid})...`);
    try {
      treeKill(pid, signal, () => {
        cleanupPidFile();
        resolve();
      });
    } catch (err) {
      console.error('[Shutdown] treeKill failed, fallback:', err);
      try {
        rProcess.kill(signal);
      } catch (_) {}
      cleanupPidFile();
      resolve();
    }
  });
}

function cleanupPidFile() {
  try { fs.existsSync(pidFile()) && fs.unlinkSync(pidFile()); } catch {}
}

// ---- App lifecycle ----
app.whenReady().then(() => {
  Menu.setApplicationMenu(null);

  // ⬇️ Auto-updates only in production, and only if publish is configured
  if (!isDev) {
    autoUpdater.autoDownload = true;

    autoUpdater.on('error', (e) => console.warn('[Updater] error:', e == null ? 'unknown' : (e.stack || e).toString()));
    autoUpdater.on('update-available', () => console.log('[Updater] update available'));
    autoUpdater.on('update-not-available', () => console.log('[Updater] no update'));
    autoUpdater.on('before-quit-for-update', () => {
      // make sure R dies before updater replaces files
      return killRProcessTree('SIGTERM');
    });
    autoUpdater.on('update-downloaded', () => {
      const res = dialog.showMessageBoxSync({
        type: 'info',
        buttons: ['Restart now', 'Later'],
        defaultId: 0,
        cancelId: 1,
        title: 'Update ready',
        message: 'A new version of OctoClario has been downloaded.',
        detail: 'Restart now to install the update.'
      });
      if (res === 0) {
        autoUpdater.quitAndInstall(false, true);
      }
    });

    // fire-and-forget; if publish isn’t set up yet, this will just be a no-op
    try { autoUpdater.checkForUpdatesAndNotify(); } catch {}
  }

  launchRAndGetPort()
    .then(({ port, rProc }) => {
      rProcess = rProc;
      createWindow(port);
    })
    .catch((err) => {
      dialog.showErrorBox('Failed to start Shiny', err.message);
      app.quit();
    });
});

// Ensure we kill R on every exit path
app.on('before-quit', () => { killRProcessTree(); });
app.on('quit', () => { killRProcessTree(); });
app.on('window-all-closed', () => {
  killRProcessTree().then(() => {
    if (process.platform !== 'darwin') app.quit();
  });
});

// Also handle process-level signals just in case
process.on('SIGINT', () => killRProcessTree().then(() => process.exit(0)));
process.on('SIGTERM', () => killRProcessTree().then(() => process.exit(0)));
