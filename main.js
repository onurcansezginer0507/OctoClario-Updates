const { app, BrowserWindow, ipcMain, dialog, Menu } = require('electron');
const path = require('path');
const { spawn } = require('child_process');
const http = require('http');
const fs = require('fs');
const net = require('net');

// ⬇️ NEW: updater + tree-kill
const { autoUpdater } = require('electron-updater');
const treeKill = require('tree-kill');

function getFreePort(){
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.on('error', reject);
    srv.listen(0, '127.0.0.1', () => {
      const p = srv.address().port;
      srv.close(() => resolve(p));
    })
  })
}
if (process.env.OCTO_DISABLE_GPU === '1') app.disableHardwareAcceleration();

const gotLock = app.requestSingleInstanceLock();
if (!gotLock) {
  app.quit()
}
app.on('second-instance', () => {
  if (mainWindow) {
    mainWindow.show();
    mainWindow.focus();
  }
})
// ---- Check for dev mode ----
const isDev = process.argv.includes('--dev');
console.log(`[App Mode] Running in ${isDev ? 'DEV' : 'PRODUCTION'} mode`);

let mainWindow = null;
let rProcess = null;
const pidFile = () => path.join(app.getPath('userData'), 'r-pid.txt');

function cleanupPidFile() {
  try { fs.existsSync(pidFile()) && fs.unlinkSync(pidFile())} catch {}
}

async function ensureNoStaleR() {
  try {
    const pfile = pidFile();
    if (!fs.existsSync(pfile)) return;
    const raw = fs.readFileSync(pfile, 'utf8').trim();
    const pid = parseInt(raw, 10);
    if (pid && !Number.isNaN(pid)) {
      console.log('[Startup] Killing stale R pid', pid);
      await new Promise(res => treeKill(pid, 'SIGKILL', res));
    }
    cleanupPidFile();
  } catch (e) {
    console.warn('[Startup] ensureNoStaleR failed:', e);
  }
}

// ---- Wait for Shiny to be ready ----
function waitForShinyServer(url, timeoutMs = 60000) {
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
async function launchRAndGetPort() {
  const basePath   = isDev ? __dirname : process.resourcesPath;
  const rscriptPath = path.join(basePath, 'R-portable', 'bin', 'Rscript.exe');
  const shinyScript = path.join(basePath, 'run-shiny.R');

  console.log('[Paths]', { basePath, rscriptPath, shinyScript });

  if (!fs.existsSync(rscriptPath)) {
    dialog.showErrorBox('Rscript not found', rscriptPath);
    throw new Error('Rscript.exe not found');
  }
  if (!fs.existsSync(shinyScript)) {
    dialog.showErrorBox('run-shiny.R not found', shinyScript);
    throw new Error('run-shiny.R not found');
  }

  // Electron chooses the port
  const port = await getFreePort();

  const rProc = spawn(rscriptPath, [shinyScript, '--port', String(port)], {
    cwd: basePath,
    windowsHide: true,
    detached: false,
    shell: false,
    stdio: ['ignore', 'pipe', 'pipe']
  });

  try {
    fs.mkdirSync(app.getPath('userData'), { recursive: true });
    fs.writeFileSync(path.join(app.getPath('userData'), 'r-pid.txt'), String(rProc.pid));
  } catch (e) {
    console.warn('[PID] Could not write PID file:', e);
  }

  rProc.stdout.on('data', (data) => console.log('[R]', data.toString()));
  rProc.stderr.on('data', (data) => console.error('[R ERR]', data.toString()));
  rProc.on('close', (code) => console.log('[R] exited with code', code));

  // We already know the port; return it now
  return { port, rProc };
}


// ---- Create Browser Window ----
function createWindowWithSplash(targetUrl) {
  mainWindow = new BrowserWindow({
    width: 1024,
    height: 768,
    show: false,
    backgroundColor: '#ffffff',
    webPreferences: {
      preload: path.join(app.getAppPath(), 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false
    }
  });

  const splash = `
  <!doctype html><html><head><meta charset="utf-8">
  <title>OctoClario</title>
  <style>
    body{margin:0;display:flex;height:100vh;align-items:center;justify-content:center;font-family:Segoe UI, Arial}
    .box{text-align:center}
    .spinner{width:42px;height:42px;border:4px solid #ddd;border-top-color:#e53935;border-radius:50%;animation:spin 1s linear infinite;margin:0 auto 12px}
    @keyframes spin{to{transform:rotate(360deg)}}
    .ver{color:#888;font-size:12px;margin-top:8px}
  </style></head>
  <body><div class="box">
    <div class="spinner"></div>
    <div>Starting analysis server…</div>
    <div class="ver">v${app.getVersion()}</div>
  </div></body></html>`;

  mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(splash));
  mainWindow.once('ready-to-show', () => mainWindow.show());

  // If the final load fails for any reason, show a friendly error
  mainWindow.webContents.on('did-fail-load', (e, code, desc, url) => {
    const html = `
      <h2 style="font-family:Segoe UI, Arial">Could not load UI</h2>
      <p>URL: ${url || targetUrl}</p>
      <p>Error ${code}: ${desc}</p>
      <p>Paths are logged in the console. Try restarting the app.</p>`;
    mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(html));
  });

  // Extra diagnostics
  mainWindow.webContents.on('render-process-gone', (_e, details) => {
    console.warn('[Renderer]', details);
  });

  // Now swap to Shiny when it’s ready
  waitForShinyServer(targetUrl, 60000)
    .then(() => mainWindow.loadURL(targetUrl))
    .catch((err) => {
      console.error('[WaitForShiny] ', err);
      dialog.showErrorBox('Shiny not responding', err.message);
      const html = `<h2 style="font-family:Segoe UI, Arial">OctoClario couldn’t start</h2>
        <p>${err.message}</p>
        <p>Please check that <code>run-shiny.R</code> exists and R can start.</p>`;
      mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(html));
    });

  // Helpful in DEV
  if (isDev || process.env.OCTO_DEVTOOLS === '1') {
    mainWindow.webContents.openDevTools({ mode: 'detach' });
  }

  mainWindow.on('closed', () => { mainWindow = null; });
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
      treeKill(pid, signal, () => { cleanupPidFile(); resolve(); });
    } catch (err) {
      console.error('[Shutdown] treeKill failed, fallback:', err);
      try { rProcess.kill(signal); } catch (_) {}
      cleanupPidFile();
      resolve();
    }
  });
}


// ---- App lifecycle ----
app.whenReady().then(async () => {
  Menu.setApplicationMenu(null);

  // Updater logging (optional but very helpful)
  try {
    const log = require('electron-log');
    autoUpdater.logger = log;
    autoUpdater.logger.transports.file.level = 'info';
    console.log('[Logs]', log.transports.file.getFile().path);
  } catch {}

  if (!isDev) {
    autoUpdater.autoDownload = true;
    autoUpdater.on('before-quit-for-update', () => killRProcessTree('SIGTERM'));
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
      if (res === 0) autoUpdater.quitAndInstall(false, true);
    });
    try { autoUpdater.checkForUpdatesAndNotify(); } catch {}
  }

  await ensureNoStaleR();

  launchRAndGetPort()
    .then(({ port, rProc }) => {
      rProcess = rProc;
      const url = `http://127.0.0.1:${port}`;
      createWindowWithSplash(url);
    })
    .catch((err) => {
      dialog.showErrorBox('Failed to start Shiny', err.message);
      app.quit();
    });
});

app.on('window-all-closed', () => {
  killRProcessTree().then(() => {
    if (process.platform !== 'darwin') app.quit();
  });
});
app.on('before-quit', () => { killRProcessTree(); });
app.on('quit', () => { killRProcessTree(); });

process.on('SIGINT', () => killRProcessTree().then(() => process.exit(0)));
process.on('SIGTERM', () => killRProcessTree().then(() => process.exit(0)));

