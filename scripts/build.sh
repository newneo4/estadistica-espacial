#!/bin/bash
# ========================================================
# Build Script — Estadística Espacial Portfolio
# Copia el sitio estático y los reportes HTML renderizados
# a la carpeta docs/ para deploy en GitHub Pages.
# ========================================================

set -e

SITE_DIR="site"
TASKS_DIR="tareas"
OUT_DIR="docs"

echo "🔧 Construyendo sitio..."

# 1. Limpiar directorio de salida
rm -rf "$OUT_DIR"

# 2. Copiar sitio estático
cp -r "$SITE_DIR" "$OUT_DIR"
echo "   ✓ Sitio estático copiado"

# 3. Crear directorio de tareas en el output
mkdir -p "$OUT_DIR/tareas"

# 4. Copiar reportes HTML y assets de cada tarea
task_count=0
for dir in "$TASKS_DIR"/*/; do
  [ -d "$dir" ] || continue
  task_name=$(basename "$dir")
  dest="$OUT_DIR/tareas/$task_name"
  mkdir -p "$dest"

  # Copiar archivos HTML renderizados
  if ls "$dir"*.html 1>/dev/null 2>&1; then
    cp "$dir"*.html "$dest/"
  fi

  # Copiar archivos PDF
  if ls "$dir"*.pdf 1>/dev/null 2>&1; then
    cp "$dir"*.pdf "$dest/"
  fi

  # Copiar figuras/imágenes si existen
  if [ -d "$dir/figuras" ]; then
    cp -r "$dir/figuras" "$dest/"
  fi

  # Copiar archivos adicionales que el reporte pueda necesitar
  # (e.g., _files/ generados por R Markdown)
  for subdir in "$dir"*_files/; do
    if [ -d "$subdir" ]; then
      cp -r "$subdir" "$dest/"
    fi
  done

  task_count=$((task_count + 1))
done
echo "   ✓ $task_count tarea(s) copiada(s)"

# 5. Generar manifest.json a partir de los metadata.json
MANIFEST="$OUT_DIR/manifest.json"
echo "[" > "$MANIFEST"
first=true
for dir in "$TASKS_DIR"/*/; do
  [ -d "$dir" ] || continue
  if [ -f "${dir}metadata.json" ]; then
    if [ "$first" = true ]; then
      first=false
    else
      printf "," >> "$MANIFEST"
    fi
    cat "${dir}metadata.json" >> "$MANIFEST"
  fi
done
echo -e "\n]" >> "$MANIFEST"
echo "   ✓ manifest.json generado"

echo ""
echo "✅ Sitio construido exitosamente en $OUT_DIR/"
echo "   Tareas: $task_count"
echo "   Puedes servir localmente con: python3 -m http.server -d docs"
