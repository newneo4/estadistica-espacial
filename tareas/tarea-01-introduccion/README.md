# Tarea 01 — Introducción a la Estadística Espacial

## Objetivo

Explorar los conceptos fundamentales de la estadística espacial y familiarizarse con las herramientas de R para el manejo de datos geográficos.

## Contenido

- Tipos de datos espaciales (puntos, líneas, polígonos, rasters)
- Sistemas de coordenadas de referencia (CRS)
- Paquetes principales: `sf`, `sp`, `terra`
- Visualización básica con `ggplot2` + `geom_sf()`

## Archivos

| Archivo | Descripción |
|:---|:---|
| `analisis.Rmd` | Código fuente del análisis en R Markdown |
| `analisis.html` | Reporte renderizado (knit desde RStudio) |
| `datos/` | Datos de entrada utilizados |
| `figuras/` | Gráficos exportados |

## Instrucciones para renderizar

```r
# En RStudio:
# 1. Abrir analisis.Rmd
# 2. Click en "Knit" → genera analisis.html
# O desde la consola:
rmarkdown::render("analisis.Rmd")
```
