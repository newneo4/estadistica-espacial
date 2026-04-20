# 📊 Estadística Espacial — Portafolio de Tareas

[![Deploy Portfolio](https://github.com/newneo4/estadistica-espacial/actions/workflows/deploy.yml/badge.svg)](https://github.com/newneo4/estadistica-espacial/actions/workflows/deploy.yml)

> Portafolio web del curso de Estadística Espacial. Cada tarea se organiza en su propia carpeta con datos, código R y reportes renderizados.

🌐 **[Ver portafolio →](https://newneo4.github.io/estadistica-espacial/)**

---

## Estructura del Repositorio

```
├── site/               # Código fuente de la página web
│   ├── index.html      # Landing page
│   ├── css/            # Estilos
│   └── js/             # JavaScript
│
├── tareas/             # Tareas del curso
│   ├── tarea-01-.../   # Una carpeta por tarea
│   │   ├── README.md
│   │   ├── metadata.json
│   │   ├── analisis.Rmd
│   │   ├── analisis.html  ← renderizado local
│   │   ├── datos/
│   │   └── figuras/
│   └── ...
│
├── scripts/            # Scripts de automatización
│   └── build.sh        # Construye el sitio para deploy
│
└── .github/workflows/  # GitHub Actions
    └── deploy.yml      # Deploy automático a GitHub Pages
```

## ¿Cómo agregar una nueva tarea?

### 1. Crear la carpeta

```bash
mkdir -p tareas/tarea-02-autocorrelacion/{datos,figuras}
```

### 2. Crear `metadata.json`

```json
{
  "id": "tarea-02",
  "folder": "tarea-02-autocorrelacion",
  "title": "Autocorrelación Espacial",
  "description": "Análisis de autocorrelación espacial usando el índice de Moran y LISA.",
  "date": "2026-05-01",
  "tags": ["autocorrelación", "moran", "LISA"],
  "thumbnail": null,
  "status": "completada", "en progreso", 
  "report": "analisis.html"
}
```

### 3. Escribir y renderizar el análisis

- Escribir tu `.Rmd` con el análisis
- **Knit** en RStudio → genera `analisis.html`

### 4. Push

```bash
git add .
git commit -m "Agregar tarea 02: Autocorrelación Espacial"
git push
```

GitHub Actions se encargará automáticamente de construir y desplegar el sitio actualizado. ☕

---

## Tecnologías

| Componente | Tecnología |
|:---|:---|
| Landing page | HTML, CSS, JavaScript vanilla |
| Análisis | R + R Markdown |
| Deploy | GitHub Actions → GitHub Pages |

---

## Desarrollo local

```bash
# Construir el sitio
bash scripts/build.sh

# Servir localmente
python3 -m http.server -d docs
# → Abrir http://localhost:8000
```

---
