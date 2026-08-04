// main.js (DROP-IN REPLACEMENT, CLEAN + MANUAL UPDATER + SPLASH LOGO)
const { app, BrowserWindow, ipcMain, dialog, Menu, shell } = require('electron');
const path = require('path');
const { spawn } = require('child_process');
const http = require('http');
const fs = require('fs');
const net = require('net');

// ---- Updater + logging + tree-kill
const { autoUpdater } = require('electron-updater');
const treeKill = require('tree-kill');
const log = require('electron-log'); // v5 works fine

// ----------------- Logging early -----------------
log.transports.file.level = 'info';
log.info('[Boot] OctoClario v' + app.getVersion());
autoUpdater.logger = log;
log.info('[Logs file]', log.transports.file.getFile().path);

// Optional: disable GPU if you want (set OCTO_DISABLE_GPU=1)
if (process.env.OCTO_DISABLE_GPU === '1') app.disableHardwareAcceleration();

// Dev mode?
const isDev = process.argv.includes('--dev');
log.info(`[App Mode] ${isDev ? 'DEV' : 'PRODUCTION'}`);

// Base path for bundled resources
// - In dev: __dirname
// - In prod: process.resourcesPath (…/OctoClario/resources)
const basePath = isDev ? __dirname : process.resourcesPath;

// ----------------- Splash logo: read once as base64 -----------------
// Expect logo.png in:
// - dev:   <project>/logo.png
// - prod:  <resources>/logo.png  (electron-builder extraResources -> to: logo.png)
let logoBase64 = null;
try {
  const logoPath = path.join(basePath, 'logo.png');
  log.info('[Splash] logo path=' + logoPath);

  if (fs.existsSync(logoPath)) {
    const raw = fs.readFileSync(logoPath);
    logoBase64 = `data:image/png;base64,${raw.toString('base64')}`;
    log.info('[Splash] logo loaded OK');
  } else {
    log.warn('[Splash] logo.png not found');
  }
} catch (e) {
  log.error('[Splash] could not load logo', e);
}

let mainWindow = null;
let rProcess = null;
const pidFile = () => path.join(app.getPath('userData'), 'r-pid.txt');

// ----------------- Helpers -----------------
function getFreePort() {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.on('error', reject);
    srv.listen(0, '127.0.0.1', () => {
      const p = srv.address().port;
      srv.close(() => resolve(p));
    });
  });
}

function cleanupPidFile() {
  try {
    const p = pidFile();
    if (fs.existsSync(p)) fs.unlinkSync(p);
  } catch (_) {}
}

async function ensureNoStaleR() {
  try {
    const p = pidFile();
    if (!fs.existsSync(p)) return;
    const raw = fs.readFileSync(p, 'utf8').trim();
    const pid = parseInt(raw, 10);
    if (pid && !Number.isNaN(pid)) {
      log.info('[Startup] Killing stale R pid ' + pid);
      await new Promise(res => treeKill(pid, 'SIGKILL', res));
    }
    cleanupPidFile();
  } catch (e) {
    log.warn('[Startup] ensureNoStaleR failed', e);
  }
}

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

// ----------------- Kill R tree -----------------
function killRProcessTree(signal = 'SIGTERM') {
  return new Promise((resolve) => {
    if (!rProcess || rProcess.killed) {
      cleanupPidFile();
      return resolve();
    }
    const pid = rProcess.pid;
    log.info(`[Shutdown] Killing R process tree (pid=${pid}) signal=${signal}`);
    try {
      treeKill(pid, signal, () => { cleanupPidFile(); resolve(); });
    } catch (err) {
      log.error('[Shutdown] treeKill failed, fallback', err);
      try { rProcess.kill(signal); } catch (_) {}
      cleanupPidFile();
      resolve();
    }
  });
}

// ----------------- R launcher -----------------
async function launchRAndGetPort() {
  const rscriptPath = path.join(basePath, 'R-portable', 'bin', 'Rscript.exe');
  const shinyScript = path.join(basePath, 'run-shiny.R');

  log.info('[Paths] basePath=' + basePath);
  log.info('[Paths] rscriptPath=' + rscriptPath);
  log.info('[Paths] shinyScript=' + shinyScript);

  if (!fs.existsSync(rscriptPath)) {
    dialog.showErrorBox('Rscript not found', rscriptPath);
    throw new Error('Rscript.exe not found');
  }
  if (!fs.existsSync(shinyScript)) {
    dialog.showErrorBox('run-shiny.R not found', shinyScript);
    throw new Error('run-shiny.R not found');
  }

  const port = await getFreePort();
  log.info('[R] chosen port=' + port);

  const rProc = spawn(rscriptPath, [shinyScript, '--port', String(port)], {
    cwd: basePath,
    windowsHide: true,
    detached: false,
    shell: false,
    stdio: ['ignore', 'pipe', 'pipe']
  });

  // IMPORTANT: capture spawn failures
  rProc.on('error', (err) => {
    log.error('[R] spawn error', err);
  });

  try {
    fs.mkdirSync(app.getPath('userData'), { recursive: true });
    fs.writeFileSync(pidFile(), String(rProc.pid));
    log.info('[PID] wrote r-pid.txt pid=' + rProc.pid);
  } catch (e) {
    log.warn('[PID] Could not write PID file', e);
  }

  rProc.stdout.on('data', (data) => log.info('[R] ' + data.toString().trimEnd()));
  rProc.stderr.on('data', (data) => log.error('[R ERR] ' + data.toString().trimEnd()));
  rProc.on('close', (code) => log.info('[R] exited code=' + code));

  return { port, rProc };
}

// ----------------- Window + Splash -----------------
function createWindowWithSplash(targetUrl) {
  log.info('[UI] createWindowWithSplash url=' + targetUrl);

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
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>OctoClario</title>
<style>
  html,body{height:100%;margin:0;background:#fff;color:#333;font-family:Segoe UI, Arial}
  .wrap{display:flex;height:100%;align-items:center;justify-content:center;text-align:center;flex-direction:column}
  .spinner{width:42px;height:42px;border:4px solid rgba(0,0,0,.2);
           border-top-color:#333;border-radius:50%;animation:spin 1s linear infinite;margin-top:18px}
  @keyframes spin{to{transform:rotate(360deg)}}
  .ver{color:#777;font-size:12px;margin-top:12px}
  .logo{width:160px;margin-bottom:10px;object-fit:contain}
</style>
</head>
<body>
  <div class="wrap">
    ${logoBase64 ? `<img class="logo" src="${logoBase64}" />` : ''}
    <div>Starting analysis server…</div>
    <div class="spinner"></div>
    <div class="ver">v${app.getVersion()}</div>
  </div>
</body>
</html>`;

  mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(splash));
  mainWindow.once('ready-to-show', () => mainWindow.show());

  mainWindow.webContents.on('did-fail-load', (_e, code, desc, url) => {
    const html = `
      <h2 style="font-family:Segoe UI, Arial">Could not load UI</h2>
      <p>URL: ${url || targetUrl}</p>
      <p>Error ${code}: ${desc}</p>
      <p>Check logs and try restarting.</p>`;
    mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(html));
  });

  waitForShinyServer(targetUrl, 60000)
    .then(() => {
      log.info('[UI] Shiny reachable -> loading ' + targetUrl);
      mainWindow.loadURL(targetUrl);
    })
    .catch((err) => {
      log.error('[UI] waitForShinyServer failed', err);
      dialog.showErrorBox('Shiny not responding', err.message);
      const html = `<h2 style="font-family:Segoe UI, Arial">OctoClario couldn’t start</h2>
        <p>${err.message}</p>
        <p>Please check that <code>run-shiny.R</code> exists and R can start.</p>`;
      mainWindow.loadURL('data:text/html;charset=UTF-8,' + encodeURIComponent(html));
    });

  if (isDev || process.env.OCTO_DEVTOOLS === '1') {
    mainWindow.webContents.openDevTools({ mode: 'detach' });
  }

  mainWindow.on('closed', () => { mainWindow = null; });
}

// ----------------- Single-instance lock -----------------
const gotLock = app.requestSingleInstanceLock();
if (!gotLock) {
  app.quit();
}
app.on('second-instance', () => {
  if (mainWindow) {
    mainWindow.show();
    mainWindow.focus();
  }
});

// ----------------- Manual updater wiring (NO startup popups) -----------------
function sendUpdaterEvent(payload) {
  try {
    if (mainWindow && mainWindow.webContents) {
      mainWindow.webContents.send('updater:event', payload);
    }
  } catch (_) {}
}

function setupUpdaterManual() {
  if (isDev) {
    log.info('[Updater] DEV mode -> updater disabled');
    return;
  }

  autoUpdater.autoDownload = false;
  autoUpdater.autoInstallOnAppQuit = false;

  autoUpdater.removeAllListeners();

  autoUpdater.on('checking-for-update', () => {
    log.info('[Updater] checking-for-update');
    sendUpdaterEvent({ type: 'checking' });
  });

  autoUpdater.on('update-available', (info) => {
    log.info('[Updater] update-available ' + (info?.version || ''));
    sendUpdaterEvent({ type: 'available', info });
  });

  autoUpdater.on('update-not-available', (info) => {
    log.info('[Updater] update-not-available');
    sendUpdaterEvent({ type: 'none', info });
  });

  autoUpdater.on('download-progress', (p) => {
    const percent = Math.round((p?.percent || 0) * 10) / 10;
    log.info('[Updater] progress ' + percent + '%');
    sendUpdaterEvent({ type: 'progress', percent, raw: p });
  });

  autoUpdater.on('update-downloaded', (info) => {
    log.info('[Updater] update-downloaded ' + (info?.version || ''));
    sendUpdaterEvent({ type: 'downloaded', info });
  });

  autoUpdater.on('error', (err) => {
    log.error('[Updater] error', err);
    sendUpdaterEvent({ type: 'error', message: err?.message || String(err) });
  });

  log.info('[Updater] manual wiring ready (no auto-check)');
}

// ----------------- IPC -----------------
ipcMain.handle('dialog:openFolder', async () => {
  const result = await dialog.showOpenDialog({ properties: ['openDirectory'] });
  return result.canceled ? null : result.filePaths[0];
});

ipcMain.handle('dialog:saveFile', async (_event, defaultName, filters) => {
  const { canceled, filePath } = await dialog.showSaveDialog({
    defaultPath: defaultName,
    filters,
    properties: ['createDirectory']
  });
  return canceled ? null : filePath;
});

ipcMain.handle('shell:openExternal', async (_event, url) => {
  try {
    if (!url || typeof url !== 'string') return { ok: false, message: 'Invalid URL' };
    await shell.openExternal(url);
    return { ok: true };
  } catch (e) {
    return { ok: false, message: e?.message || String(e) };
  }
});

// Manual updater IPC (called by your Shiny sidebar buttons)
ipcMain.handle('updater:check', async () => {
  if (isDev) return { ok: false, dev: true, message: 'Updater disabled in DEV mode.' };
  try {
    const result = await autoUpdater.checkForUpdates(); // does NOT download (since autoDownload=false)
    return { ok: true, result };
  } catch (e) {
    return { ok: false, message: e?.message || String(e) };
  }
});

ipcMain.handle('updater:download', async () => {
  if (isDev) return { ok: false, dev: true, message: 'Updater disabled in DEV mode.' };
  try {
    await autoUpdater.downloadUpdate(); // progress + downloaded via events
    return { ok: true };
  } catch (e) {
    return { ok: false, message: e?.message || String(e) };
  }
});

ipcMain.handle('updater:install', async () => {
  if (isDev) return { ok: false, dev: true, message: 'Updater disabled in DEV mode.' };
  try {
    await killRProcessTree('SIGTERM');
    await new Promise(r => setTimeout(r, 800));
    await killRProcessTree('SIGKILL');
    autoUpdater.quitAndInstall(false, true);
    return { ok: true };
  } catch (e) {
    return { ok: false, message: e?.message || String(e) };
  }
});


// ----------------- App lifecycle -----------------
app.whenReady().then(async () => {
  log.info('[Lifecycle] whenReady entered');

  Menu.setApplicationMenu(null);
  log.info('[Lifecycle] menu cleared');

  setupUpdaterManual();
  log.info('[Lifecycle] updater wiring done');

  await ensureNoStaleR();
  log.info('[Lifecycle] ensureNoStaleR done');

  log.info('[Lifecycle] about to launch R');
  try {
    const { port, rProc } = await launchRAndGetPort();
    rProcess = rProc;

    const url = `http://127.0.0.1:${port}`;
    log.info('[Lifecycle] R launched, creating window url=' + url);

    createWindowWithSplash(url);
  } catch (err) {
    log.error('[Lifecycle] launchRAndGetPort failed', err);
    dialog.showErrorBox('Failed to start Shiny', err.message || String(err));
    app.quit();
  }
});

app.on('window-all-closed', () => {
  killRProcessTree().then(() => {
    if (process.platform !== 'darwin') app.quit();
  });
});

app.on('before-quit', () => { killRProcessTree(); });
app.on('quit', () => { killRProcessTree(); });

// Process-level safety
process.on('SIGINT', () => killRProcessTree().then(() => process.exit(0)));
process.on('SIGTERM', () => killRProcessTree().then(() => process.exit(0)));
