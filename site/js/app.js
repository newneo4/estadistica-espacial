/* ========================================================
   ESTADÍSTICA ESPACIAL - Portfolio App
   Dynamic task loading, canvas animation, filters & search
   ======================================================== */

(() => {
  'use strict';

  /* --------------------------------------------------
     CONFIG
  -------------------------------------------------- */
  const MANIFEST_URL = 'manifest.json';

  /* --------------------------------------------------
     DOM REFS
  -------------------------------------------------- */
  const $ = (sel) => document.querySelector(sel);
  const $$ = (sel) => document.querySelectorAll(sel);

  const navbar       = $('#navbar');
  const heroCanvas   = $('#hero-canvas');
  const tasksGrid    = $('#tasks-grid');
  const loadingGrid  = $('#loading-grid');
  const emptyState   = $('#empty-state');
  const filtersDiv   = $('#filters');
  const searchInput  = $('#search-input');
  const statTotal    = $('#stat-total');
  const statCompleted = $('#stat-completed');
  const statUpdated  = $('#stat-updated');

  /* --------------------------------------------------
     STATE
  -------------------------------------------------- */
  let allTasks = [];
  let activeFilter = 'all';
  let activeUnitFilter = 'all';
  let searchQuery = '';

  /* --------------------------------------------------
     INIT
  -------------------------------------------------- */
  document.addEventListener('DOMContentLoaded', () => {
    initNavbar();
    initCanvas();
    loadTasks();
    initSearch();
  });

  /* --------------------------------------------------
     NAVBAR — scroll-aware background
  -------------------------------------------------- */
  function initNavbar() {
    const onScroll = () => {
      navbar.classList.toggle('scrolled', window.scrollY > 40);
    };
    window.addEventListener('scroll', onScroll, { passive: true });
    onScroll();
  }

  /* --------------------------------------------------
     CANVAS — Spatial point pattern animation
     Evocates geographic data points and spatial networks
  -------------------------------------------------- */
  function initCanvas() {
    const ctx = heroCanvas.getContext('2d');
    let width, height, particles, animId;
    const PARTICLE_COUNT = 90;
    const CONNECTION_DIST = 140;
    const PARTICLE_SPEED = 0.25;

    const accentColors = [
      'rgba(56, 189, 248, ',   // sky
      'rgba(129, 140, 248, ',  // indigo
      'rgba(52, 211, 153, ',   // emerald
    ];

    class Particle {
      constructor() {
        this.reset();
        // Start from random position
        this.x = Math.random() * width;
        this.y = Math.random() * height;
      }

      reset() {
        this.x = Math.random() * width;
        this.y = Math.random() * height;
        this.vx = (Math.random() - 0.5) * PARTICLE_SPEED;
        this.vy = (Math.random() - 0.5) * PARTICLE_SPEED;
        this.radius = Math.random() * 2 + 1;
        this.colorIdx = Math.floor(Math.random() * accentColors.length);
        this.alpha = Math.random() * 0.5 + 0.2;
      }

      update() {
        this.x += this.vx;
        this.y += this.vy;

        // Wrap around edges
        if (this.x < -10) this.x = width + 10;
        if (this.x > width + 10) this.x = -10;
        if (this.y < -10) this.y = height + 10;
        if (this.y > height + 10) this.y = -10;
      }

      draw() {
        ctx.beginPath();
        ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
        ctx.fillStyle = accentColors[this.colorIdx] + this.alpha + ')';
        ctx.fill();
      }
    }

    function resize() {
      width = heroCanvas.width = heroCanvas.offsetWidth;
      height = heroCanvas.height = heroCanvas.offsetHeight;
    }

    function createParticles() {
      particles = [];
      for (let i = 0; i < PARTICLE_COUNT; i++) {
        particles.push(new Particle());
      }
    }

    function drawConnections() {
      for (let i = 0; i < particles.length; i++) {
        for (let j = i + 1; j < particles.length; j++) {
          const dx = particles[i].x - particles[j].x;
          const dy = particles[i].y - particles[j].y;
          const dist = Math.sqrt(dx * dx + dy * dy);

          if (dist < CONNECTION_DIST) {
            const opacity = (1 - dist / CONNECTION_DIST) * 0.12;
            ctx.beginPath();
            ctx.moveTo(particles[i].x, particles[i].y);
            ctx.lineTo(particles[j].x, particles[j].y);
            ctx.strokeStyle = `rgba(56, 189, 248, ${opacity})`;
            ctx.lineWidth = 0.6;
            ctx.stroke();
          }
        }
      }
    }

    function animate() {
      ctx.clearRect(0, 0, width, height);
      drawConnections();
      particles.forEach(p => {
        p.update();
        p.draw();
      });
      animId = requestAnimationFrame(animate);
    }

    // Init
    resize();
    createParticles();
    animate();

    window.addEventListener('resize', () => {
      resize();
      // Reinitialize if particles went off the new bounds
      particles.forEach(p => {
        if (p.x > width || p.y > height) {
          p.x = Math.random() * width;
          p.y = Math.random() * height;
        }
      });
    });

    // Pause animation when hero is not visible
    const heroObserver = new IntersectionObserver(([entry]) => {
      if (entry.isIntersecting) {
        if (!animId) animate();
      } else {
        cancelAnimationFrame(animId);
        animId = null;
      }
    }, { threshold: 0 });
    heroObserver.observe($('#hero'));
  }

  /* --------------------------------------------------
     LOAD TASKS — fetch manifest.json
  -------------------------------------------------- */
  async function loadTasks() {
    try {
      const res = await fetch(MANIFEST_URL);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      allTasks = await res.json();

      // Sort by date descending
      allTasks.sort((a, b) => new Date(b.date) - new Date(a.date));

      updateStats();
      buildFilters();
      renderCards();
    } catch (err) {
      console.warn('No se pudo cargar manifest.json:', err.message);
      // Show empty state with helpful message
      loadingGrid.style.display = 'none';
      tasksGrid.style.display = 'none';
      emptyState.querySelector('p').textContent = 'Aún no hay tareas publicadas';
      emptyState.querySelector('.empty-hint').textContent = 'Las tareas aparecerán aquí automáticamente al hacer push';
      emptyState.style.display = 'block';
    }
  }

  /* --------------------------------------------------
     STATS
  -------------------------------------------------- */
  function updateStats() {
    const total = allTasks.length;
    const completed = allTasks.filter(t => t.status === 'completada').length;
    const dates = allTasks.map(t => new Date(t.date)).filter(d => !isNaN(d));
    const latest = dates.length > 0
      ? dates.sort((a, b) => b - a)[0].toLocaleDateString('es', { day: 'numeric', month: 'short' })
      : '—';

    animateNumber(statTotal, total);
    animateNumber(statCompleted, completed);
    statUpdated.textContent = latest;
  }

  function animateNumber(el, target) {
    const duration = 800;
    const start = performance.now();
    const from = 0;

    function tick(now) {
      const elapsed = now - start;
      const progress = Math.min(elapsed / duration, 1);
      // Ease out cubic
      const ease = 1 - Math.pow(1 - progress, 3);
      el.textContent = Math.round(from + (target - from) * ease);
      if (progress < 1) requestAnimationFrame(tick);
    }
    requestAnimationFrame(tick);
  }

  /* --------------------------------------------------
     FILTERS — build from unique tags
  -------------------------------------------------- */
  function buildFilters() {
    const allTags = new Set();
    allTasks.forEach(t => (t.tags || []).forEach(tag => allTags.add(tag)));

    const sorted = [...allTags].sort();
    sorted.forEach(tag => {
      const btn = document.createElement('button');
      btn.className = 'filter-btn';
      btn.dataset.filter = tag;
      btn.textContent = tag.charAt(0).toUpperCase() + tag.slice(1);
      filtersDiv.appendChild(btn);
    });

    // Delegate clicks for tags
    filtersDiv.addEventListener('click', (e) => {
      const btn = e.target.closest('.filter-btn');
      if (!btn) return;
      filtersDiv.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      activeFilter = btn.dataset.filter;
      renderCards();
    });

    // Delegate clicks for units
    const unitFiltersDiv = $('#unit-filters');
    if (unitFiltersDiv) {
      unitFiltersDiv.addEventListener('click', (e) => {
        const btn = e.target.closest('.unit-tab');
        if (!btn) return;
        unitFiltersDiv.querySelectorAll('.unit-tab').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        activeUnitFilter = btn.dataset.unit;
        renderCards();
      });
    }
  }

  /* --------------------------------------------------
     SEARCH
  -------------------------------------------------- */
  function initSearch() {
    let debounceTimer;
    searchInput.addEventListener('input', () => {
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(() => {
        searchQuery = searchInput.value.trim().toLowerCase();
        renderCards();
      }, 200);
    });
  }

  /* --------------------------------------------------
     RENDER CARDS
  -------------------------------------------------- */
  function renderCards() {
    loadingGrid.style.display = 'none';

    // Filter
    let filtered = allTasks;
    if (activeUnitFilter !== 'all') {
      filtered = filtered.filter(t => t.unidad === activeUnitFilter);
    }
    if (activeFilter !== 'all') {
      filtered = filtered.filter(t => (t.tags || []).includes(activeFilter));
    }
    if (searchQuery) {
      filtered = filtered.filter(t =>
        t.title.toLowerCase().includes(searchQuery) ||
        t.description.toLowerCase().includes(searchQuery) ||
        (t.tags || []).some(tag => tag.toLowerCase().includes(searchQuery))
      );
    }

    // Empty?
    if (filtered.length === 0) {
      tasksGrid.style.display = 'none';
      emptyState.style.display = 'block';
      return;
    }

    emptyState.style.display = 'none';
    tasksGrid.style.display = 'grid';
    tasksGrid.innerHTML = '';

    filtered.forEach((task, i) => {
      const card = createCard(task, i);
      tasksGrid.appendChild(card);
    });

    // Observe cards for entrance animation
    observeCards();
  }

  /* --------------------------------------------------
     CREATE CARD
  -------------------------------------------------- */
  function createCard(task, index) {
    const card = document.createElement('article');
    card.className = 'task-card';
    card.style.transitionDelay = `${index * 80}ms`;

    const reportUrl = `tareas/${task.folder}/${task.report || 'analisis.html'}`;

    const thumbnailHTML = task.thumbnail
      ? `<img src="tareas/${task.folder}/${task.thumbnail}" alt="${task.title}" loading="lazy">`
      : `<div class="card-thumbnail-placeholder">
           <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">
             <path d="M3 3h18v18H3z"/><path d="M3 9h18"/><path d="M9 21V9"/>
           </svg>
         </div>`;

    const tagsHTML = (task.tags || [])
      .map(tag => `<span class="tag">${tag}</span>`)
      .join('');

    const statusClass = (task.status || 'proxima')
      .toLowerCase()
      .replace(/\s+/g, '-')
      .normalize('NFD')
      .replace(/[\u0300-\u036f]/g, '');

    const statusLabel = task.status
      ? task.status.charAt(0).toUpperCase() + task.status.slice(1)
      : 'Próxima';

    const dateFormatted = task.date
      ? new Date(task.date + 'T00:00:00').toLocaleDateString('es', {
          day: 'numeric', month: 'short', year: 'numeric'
        })
      : '';
      
    const pdfButtonHTML = task.pdf 
      ? `<a href="tareas/${task.folder}/${task.pdf}" target="_blank" rel="noopener" class="pdf-btn" title="Ver PDF" aria-label="Ver PDF de la tarea">
           <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
             <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path><polyline points="14 2 14 8 20 8"></polyline><line x1="16" y1="13" x2="8" y2="13"></line><line x1="16" y1="17" x2="8" y2="17"></line><polyline points="10 9 9 9 8 9"></polyline>
           </svg>
           PDF
         </a>`
      : '';

    card.innerHTML = `
      <a href="${reportUrl}" class="card-link" aria-label="Ver reporte: ${task.title}"></a>
      <div class="card-accent"></div>
      <div class="card-thumbnail">
        ${thumbnailHTML}
        <span class="card-number">${task.id || ''}</span>
      </div>
      <div class="card-body">
        <h3 class="card-title">${task.title}</h3>
        <p class="card-description">${task.description || ''}</p>
        <div class="card-tags">${tagsHTML}</div>
        <div class="card-footer">
          <span class="card-date">
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <rect width="18" height="18" x="3" y="4" rx="2" ry="2"/><line x1="16" x2="16" y1="2" y2="6"/>
              <line x1="8" x2="8" y1="2" y2="6"/><line x1="3" x2="21" y1="10" y2="10"/>
            </svg>
            ${dateFormatted}
          </span>
          <div class="card-actions">
            ${pdfButtonHTML}
            <span class="status-badge ${statusClass}">${statusLabel}</span>
          </div>
        </div>
      </div>
    `;

    return card;
  }

  /* --------------------------------------------------
     INTERSECTION OBSERVER — entrance animations
  -------------------------------------------------- */
  function observeCards() {
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add('visible');
          observer.unobserve(entry.target);
        }
      });
    }, {
      threshold: 0.08,
      rootMargin: '0px 0px -40px 0px'
    });

    $$('.task-card').forEach(card => observer.observe(card));
  }

})();
