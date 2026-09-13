(() => {
  const cfg = window.EPIQUANTA_SITE || {};
  const local = cfg.mode === 'local';
  document.body.classList.toggle('is-local', local);
  let repository = cfg.repository || '';
  const host = location.hostname;
  if (!repository && host.endsWith('.github.io')) {
    const owner = host.slice(0, -10);
    const project = location.pathname.split('/').filter(Boolean)[0];
    repository = owner + '/' + (project && !project.endsWith('.html') ? project : host);
  }
  if (/^[a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.-]+$/.test(repository)) {
    const base = 'https://github.com/' + repository;
    document.querySelectorAll('[data-download]').forEach(a => {
      a.href = base + '/releases/latest/download/' + encodeURIComponent(cfg.asset || 'Epiquanta_Windows.zip');
      a.textContent = '下載 Windows 完整套件 ↓';
    });
    document.querySelectorAll('[data-releases]').forEach(a => { a.href = base + '/releases/latest'; });
    document.getElementById('release-status').textContent = '從 GitHub Releases 取得完整套件，內含程式與 Windows R 套件。';
  }
  const pause = document.getElementById('pause-motion');
  pause.addEventListener('click', () => {
    const paused = document.getElementById('brand-scene').classList.toggle('paused');
    pause.setAttribute('aria-pressed', String(paused));
    pause.textContent = paused ? '播放 Logo 動畫' : '暫停 Logo 動畫';
  });
})();
