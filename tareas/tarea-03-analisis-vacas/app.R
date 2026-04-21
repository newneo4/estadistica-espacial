library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(scales)

# ── Colores corporativos ──────────────────────────────────
COLOR_VERDE        <- "#00BFA6"   # turquesa elegante (principal)
COLOR_VERDE_CLARO  <- "#1DE9B6"   # turquesa claro (hover / resalte)

COLOR_NARANJA      <- "#FF6D00"   # naranja intenso (alertas)
COLOR_NARANJA_CL   <- "#FF9100"   # naranja claro

COLOR_ROJO         <- "#B71C1C"   # rojo oscuro (crítico)
COLOR_AZUL         <- "#0D47A1"   # azul profundo (base)

COLOR_GRIS         <- "#263238"   # gris oscuro (casi negro)
COLOR_FONDO        <- "#121212"   # fondo general (dark mode)

# ── Carga de datos ────────────────────────────────────────
load_data <- function() {
  base <- "."   # cambia si los CSVs están en otra carpeta
  
  safe_read <- function(f) {
    p <- file.path(base, f)
    if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE)
    else data.frame()
  }
  
  list(
    prod        = safe_read("produccion.csv"),
    gestantes   = safe_read("gestantes.csv"),
    para_secar  = safe_read("para_secar.csv"),
    prox_parto  = safe_read("proximas_parto.csv"),
    inseminadas = safe_read("inseminadas.csv"),
    secas       = safe_read("secas.csv"),
    problema    = safe_read("problema.csv"),
    mastitis    = safe_read("mastitis.csv"),
    test_prenez = safe_read("test_prenez.csv"),
    vaquillas   = safe_read("vaquillas.csv"),
    vaq_parto   = safe_read("vaquillas_proximas_parto.csv"),
    vaq_test_prenez = safe_read("vaquillas_test_prenez.csv"),
    revisiones  = safe_read("revisiones_veterinarias.csv")
  )
}

datos <- load_data()

# ── KPIs calculados ───────────────────────────────────────
prod_prom      <- if (nrow(datos$prod) > 0) round(mean(datos$prod$prod_prom_act, na.rm = TRUE), 1) else 0
vacas_prod     <- nrow(datos$prod)
vacas_gest     <- nrow(datos$gestantes)
iep_prom       <- if (nrow(datos$test_prenez) > 0) round(mean(datos$test_prenez$iep, na.rm = TRUE)) else 0
vacas_secar    <- nrow(datos$para_secar)
vacas_secas    <- nrow(datos$secas)
vacas_insem    <- nrow(datos$inseminadas)
vacas_problema <- nrow(datos$problema)
vacas_mastitis <- nrow(unique(datos$mastitis["vaca_no"]))

# Porcentaje gestantes sobre vacas en producción
pct_gest <- if (vacas_prod > 0) round(vacas_gest / vacas_prod * 100, 1) else 0

# Tasa de concepción aproximada (gestantes / inseminadas en último ciclo)
tasa_concep <- if (vacas_insem > 0) round(vacas_gest / (vacas_gest + vacas_insem) * 100) else 0

# ── Estado del hato (pie chart) ───────────────────────────
estado_hato <- datos$prod %>%
  mutate(categoria = case_when(
    status %in% c("Alta")         ~ "En producción",
    status %in% c("Preñ","Preñ2") ~ "En producción",
    status %in% c("Insem")        ~ "Inseminada",
    status %in% c("Vacia")        ~ "Vacía",
    status %in% c("Matar","Abort")~ "Otro",
    TRUE                           ~ "Otro"
  )) %>%
  count(categoria) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Añadir secas y reemplazo
total_secas <- nrow(datos$secas)
total_reemplazo <- nrow(datos$vaquillas)
total_global <- nrow(datos$prod) + total_secas + total_reemplazo

hato_completo <- data.frame(
  categoria = c("En producción", "Seca / Preparto", "Reemplazo"),
  n = c(nrow(datos$prod), total_secas, total_reemplazo)
) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# ── Tendencia producción por DIM bins ────────────────────
tend_prod <- datos$prod %>%
  mutate(dim_bin = floor(dim / 30) * 30) %>%
  group_by(dim_bin) %>%
  summarise(prom_hato = mean(prod_prom_act, na.rm = TRUE),
            prom_prod  = mean(prod_prom_ant, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(dim_bin) %>%
  mutate(periodo = paste0(dim_bin, "-", dim_bin + 29, " días"),
         meta = 40)

# ── Producción por grupo (paridad) ────────────────────────
prod_paridad <- datos$prod %>%
  mutate(paridad = case_when(
    grupo <= 4  ~ "1er Parto",
    grupo <= 8  ~ "2do Parto",
    grupo <= 12 ~ "3er Parto",
    TRUE        ~ "4+ Partos"
  )) %>%
  group_by(paridad) %>%
  summarise(prod_prom = round(mean(prod_prom_act, na.rm = TRUE), 1), .groups = "drop")

# ── Alertas reproductivas (vacas para secar) ──────────────
alertas <- datos$para_secar %>%
  select(vaca_no, status, dim, dias_al_secado, prod_prom, fecha_secado) %>%
  rename(
    "Vaca N°"       = vaca_no,
    "Estado"        = status,
    "Días Lact."    = dim,
    "Días a Secar"  = dias_al_secado,
    "Prod. Prom."   = prod_prom,
    "Fecha Secado"  = fecha_secado
  ) %>%
  arrange(`Días a Secar`)

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    
    title = tags$div(
      tags$span("🐄 ANALISIS DE VACAS",
                style="font-weight:700; font-size:17px;"),
      tags$br(),
      tags$span("Monitoreo Inteligente del Hato",
                style="font-size:11px; opacity:0.7;")
    )
    ,
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      menuItem("Dashboard Principal",   tabName = "dashboard",    icon = icon("gauge-high")),
      menuItem("Inventario del Hato",   tabName = "inventario",   icon = icon("cow")),
      menuItem("Producción Diaria",     tabName = "produccion",   icon = icon("droplet")),
      menuItem("Control Reproductivo",  tabName = "reproduccion", icon = icon("calendar-check")),
      menuItem("Calidad de Leche",      tabName = "calidad",      icon = icon("flask")),
      menuItem("Vacas Problema",        tabName = "problema",     icon = icon("triangle-exclamation")),
      menuItem("Mastitis",              tabName = "mastitis",     icon = icon("bacteria"))
    )
  ),
  
  
  
  
  dashboardBody(
    tags$head(
      
      # ── Fuente moderna ───────────────────────────────
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap"
      ),
      
      tags$style(HTML(paste0("
    
    /* ─────────────────────────────────────────────
       ESTILOS CLAROS (NUEVO DISEÑO)
    ───────────────────────────────────────────── */
    body { background: #F4F6F9; font-family: 'Poppins', sans-serif; }
    .content-wrapper { background: #F4F6F9; padding: 15px; }
    .main-sidebar { background: #343A40 !important; }
    .sidebar-menu > li > a { color: #ccc !important; font-size: 13px; }
    .sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
      color: #fff !important; background: #00BFA6 !important; border-left: 4px solid #fff;
    }
    
    .chart-box-white { background: #fff; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.05); text-align: center; }
    .kpi-box-container { background: #fff; border-radius: 8px; padding: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.05); height: calc(100% - 20px); display: flex; flex-direction: column; justify-content: center;}
    .kpi-big-text { font-size: 40px; font-weight: bold; line-height: 1.2; text-align: center;}
    .kpi-lactacion-box { border: 1px solid #ddd; border-radius: 20px; padding: 15px; text-align: center; font-size: 14px; font-weight: 600; color: #555;}
    .kpi-lactacion-box b { font-size: 30px; color: #333; display: block; margin-top: 5px;}
    .env-kpi { text-align: center; margin-bottom: 10px; font-size: 11px; color: #777;}
    .env-kpi b { font-size: 20px; color: #333;}
    .kpi-text-sub { font-size: 11px; color: #666; margin-top: 10px; text-align: center;}
    .chart-title { font-weight: 600; color: #333; margin-bottom: 15px; }
    
    /* ─────────────────────────────────────────────
       ESTILOS PARA OTRAS PESTAÑAS (MODERN LIGHT THEME)
    ───────────────────────────────────────────── */
    .kpi-card { 
      background: #ffffff; border-radius: 10px; padding: 15px 20px; 
      display: flex; align-items: center; box-shadow: 0 4px 10px rgba(0,0,0,0.05); 
      margin-bottom: 20px; border-left: 5px solid #ccc; transition: transform 0.2s;
    }
    .kpi-card:hover { transform: translateY(-3px); box-shadow: 0 6px 15px rgba(0,0,0,0.1); }
    
    .kpi-verde { border-left-color: #00BFA6; }
    .kpi-naranja { border-left-color: #FF9100; }
    .kpi-azul { border-left-color: #0D47A1; }
    .kpi-rojo { border-left-color: #B71C1C; }
    .kpi-gris { border-left-color: #607D8B; }
    
    .kpi-icon { font-size: 35px; margin-right: 20px; width: 50px; text-align: center; }
    .kpi-verde .kpi-icon { color: #00BFA6; }
    .kpi-naranja .kpi-icon { color: #FF9100; }
    .kpi-azul .kpi-icon { color: #0D47A1; }
    .kpi-rojo .kpi-icon { color: #B71C1C; }
    .kpi-gris .kpi-icon { color: #607D8B; }
    
    .kpi-value { font-size: 24px; font-weight: 700; color: #333; line-height: 1.1; }
    .kpi-label { font-size: 12px; color: #777; font-weight: 500; text-transform: uppercase; margin-top: 5px; }
    
    .chart-box { background: #ffffff; border-radius: 10px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); }
    .section-header { font-size: 16px; font-weight: 600; color: #333; margin-bottom: 15px; border-bottom: 1px solid #eee; padding-bottom: 10px; }
    
    /* ─────────────────────────────────────────────
       TABLAS
    ───────────────────────────────────────────── */
    table.dataTable thead th { background: #0D47A1 !important; color: #fff !important; }
    table.dataTable tbody tr:hover { background: rgba(0,191,166,0.2) !important; }
    .dataTables_wrapper .dataTables_filter input { border-radius: 8px; padding: 5px 10px; border: 1px solid #ccc; }
    ")))
    ),
    
    tabItems(
      
      # ═══════════════════════════════════════════════════
      # TAB: DASHBOARD PRINCIPAL
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "dashboard",
              fluidRow(
                column(4, div(class="chart-box-white", plotlyOutput("plot_prenadas_vacias", height="250px"), uiOutput("text_prenadas_vacias"))),
                column(4, div(class="chart-box-white", plotlyOutput("plot_ordeno_seca", height="250px"), uiOutput("text_ordeno_seca"))),
                column(4, div(class="chart-box-white", plotlyOutput("plot_lactancias", height="250px")))
              ),
              fluidRow(
                column(4, div(class="chart-box-white", plotlyOutput("plot_vacas_recria", height="250px"), uiOutput("text_vacas_recria"))),
                column(4, div(class="chart-box-white", plotlyOutput("plot_estados_detalle", height="250px"))),
                column(4, div(class="kpi-box-container",
                              fluidRow(
                                column(6, div(class="kpi-big-text", style="color:#007BFF;", uiOutput("val_del_promedio"), tags$span(style="font-size:12px;color:#666;", "DEL PROMEDIO"))),
                                column(6, div(class="kpi-lactacion-box", "LACTACION PROMEDIO", tags$br(), uiOutput("val_lac_promedio")))
                              ),
                              tags$hr(style="margin: 20px 0;"),
                              fluidRow(
                                column(4, div(class="env-kpi", tags$b("15.73"), tags$br(), "TEMPERATURA MIN"), div(class="env-kpi", tags$b("19.19"), tags$br(), "TEMPERATURA MAX")),
                                column(4, div(class="env-kpi", tags$b("60.17"), tags$br(), "ITH MIN"), div(class="env-kpi", tags$b("66.01"), tags$br(), "ITH MAX")),
                                column(4, div(class="env-kpi", tags$b("88.76"), tags$br(), "HUMEDAD"))
                              )
                ))
              )
      ),
      tabItem(tabName = "inventario",
              fluidRow(
                column(3, div(class="kpi-card kpi-verde",
                              div(class="kpi-icon", icon("cow")),
                              div(div(class="kpi-value", vacas_prod),
                                  div(class="kpi-label","Vacas en Producción")))),
                column(3, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("moon")),
                              div(div(class="kpi-value", vacas_secas),
                                  div(class="kpi-label","Vacas Secas")))),
                column(3, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("seedling")),
                              div(div(class="kpi-value", nrow(datos$vaquillas)),
                                  div(class="kpi-label","Vaquillas Reemplazo")))),
                column(3, div(class="kpi-card kpi-gris",
                              div(class="kpi-icon", icon("list")),
                              div(div(class="kpi-value", vacas_prod + vacas_secas + nrow(datos$vaquillas)),
                                  div(class="kpi-label","Total Animales"))))
              ),
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución completa del hato"),
                              plotlyOutput("plot_hato_completo", height="350px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Estado de las vacas en producción"),
                              plotlyOutput("plot_status_prod", height="350px")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","📋 Listado Producción"),
                               DTOutput("tabla_prod_completa")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB: PRODUCCIÓN DIARIA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "produccion",
              fluidRow(
                column(3, div(class="kpi-card kpi-verde",
                              div(class="kpi-icon", icon("chart-line")),
                              div(div(class="kpi-value", paste0(prod_prom," kg")),
                                  div(class="kpi-label","Prom. Producción")))),
                column(3, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("arrow-trend-up")),
                              div(div(class="kpi-value",
                                      paste0(round(max(datos$prod$prod_prom_act, na.rm=TRUE),1)," kg")),
                                  div(class="kpi-label","Máxima Actual")))),
                column(3, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("arrow-trend-down")),
                              div(div(class="kpi-value",
                                      paste0(round(min(datos$prod$prod_prom_act, na.rm=TRUE),1)," kg")),
                                  div(class="kpi-label","Mínima Actual")))),
                column(3, div(class="kpi-card kpi-gris",
                              div(class="kpi-icon", icon("droplet")),
                              div(div(class="kpi-value",
                                      paste0(round(sum(datos$prod$prod_prom_act, na.rm=TRUE)/1000,1)," t/día")),
                                  div(class="kpi-label","Producción Total Estimada"))))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="chart-title","Distribución de producción por vaca (kg/día)"),
                               plotlyOutput("hist_produccion", height="300px")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","📋 Detalle Producción por Vaca"),
                               DTOutput("tabla_produccion_detalle")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB: CONTROL REPRODUCTIVO
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "reproduccion",
              fluidRow(
                column(3, div(class="kpi-card kpi-verde",
                              div(class="kpi-icon", icon("syringe")),
                              div(div(class="kpi-value", vacas_insem),
                                  div(class="kpi-label","Vacas Inseminadas")))),
                column(3, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("baby")),
                              div(div(class="kpi-value", vacas_gest),
                                  div(class="kpi-label","Vacas Gestantes")))),
                column(3, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("calendar-days")),
                              div(div(class="kpi-value", paste0(iep_prom," días")),
                                  div(class="kpi-label","IEP Promedio")))),
                column(3, div(class="kpi-card kpi-rojo",
                              div(class="kpi-icon", icon("triangle-exclamation")),
                              div(div(class="kpi-value", vacas_problema),
                                  div(class="kpi-label","Vacas Problema (≥3 Insem.)"))))
              ),
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title","Distribución días abiertos (vacas inseminadas)"),
                              plotlyOutput("hist_dias_abiertos", height="280px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title","IEP por vaca (test de preñez)"),
                              plotlyOutput("plot_iep", height="280px")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","📋 Próximas al Parto (siguientes 21 días)"),
                               DTOutput("tabla_prox_parto")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","⚠️ Vacas Problema (≥3 inseminaciones)"),
                               DTOutput("tabla_problema")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB: CALIDAD DE LECHE
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "calidad",
              fluidRow(
                column(4, div(class="kpi-card kpi-verde",
                              div(class="kpi-icon", icon("flask")),
                              div(div(class="kpi-value","144 mil/ml"),
                                  div(class="kpi-label","RCS Tanque")))),
                column(4, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("bacteria")),
                              div(div(class="kpi-value", vacas_mastitis),
                                  div(class="kpi-label","Vacas con Historial Mastitis")))),
                column(4, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("percent")),
                              div(div(class="kpi-value",
                                      paste0(round(vacas_mastitis/vacas_prod*100,1),"%")),
                                  div(class="kpi-label","% Hato con Mastitis"))))
              ),
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title","Mastitis por número de lactación"),
                              plotlyOutput("plot_mastitis_lac", height="300px")
                )),
                column(6, div(class="chart-box",
                              div(class="chart-title","Producción vs DIM en vacas con mastitis"),
                              plotlyOutput("plot_mastitis_dim", height="300px")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","📋 Detalle Mastitis"),
                               DTOutput("tabla_mastitis")
                ))
              )
      ),
      
      
      # ═══════════════════════════════════════════════════
      # TAB: VACAS PROBLEMA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "problema",
              fluidRow(
                column(4, div(class="kpi-card kpi-rojo",
                              div(class="kpi-icon", icon("triangle-exclamation")),
                              div(div(class="kpi-value", vacas_problema),
                                  div(class="kpi-label","Vacas con problemas reproductivos")))),
                column(4, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("stethoscope")),
                              div(div(class="kpi-value", nrow(datos$revisiones)),
                                  div(class="kpi-label","Revisiones Veterinarias Registradas")))),
                column(4, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("chart-pie")),
                              div(div(class="kpi-value", paste0(round(vacas_problema / (vacas_prod + vacas_secas) * 100, 1), "%")),
                                  div(class="kpi-label","Porcentaje del Hato Afectado"))))
              ),
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución de Vacas Problema por Estado"),
                              plotlyOutput("plot_problema_status", height="300px"))),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Top Eventos Veterinarios Frecuentes"),
                              plotlyOutput("plot_eventos_vet", height="300px")))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","⚠️ Listado Completo — Vacas Problema"),
                               DTOutput("tabla_problema_full")
                ))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","🩺 Registro de Revisiones Veterinarias"),
                               DTOutput("tabla_revisiones")
                ))
              )
      ),
      
      # ═══════════════════════════════════════════════════
      # TAB: MASTITIS
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "mastitis",
              fluidRow(
                column(4, div(class="kpi-card kpi-rojo",
                              div(class="kpi-icon", icon("bacteria")),
                              div(div(class="kpi-value", vacas_mastitis),
                                  div(class="kpi-label","Vacas con Mastitis Actual")))),
                column(4, div(class="kpi-card kpi-naranja",
                              div(class="kpi-icon", icon("calendar-days")),
                              div(div(class="kpi-value", paste0(round(mean(datos$mastitis$dim, na.rm=T)), " días")),
                                  div(class="kpi-label","Promedio DIM en Infección")))),
                column(4, div(class="kpi-card kpi-azul",
                              div(class="kpi-icon", icon("droplet-slash")),
                              div(div(class="kpi-value", paste0(round(mean(datos$mastitis$prod_24h, na.rm=T), 1), " kg")),
                                  div(class="kpi-label","Prod. Promedio Afectada (24h)"))))
              ),
              fluidRow(
                column(6, div(class="chart-box",
                              div(class="chart-title", "Edad de las Vacas con Mastitis"),
                              plotlyOutput("plot_mastitis_edad", height="300px"))),
                column(6, div(class="chart-box",
                              div(class="chart-title", "Distribución de Producción (24h) por Lactación"),
                              plotlyOutput("plot_mastitis_prod_lac", height="300px")))
              ),
              fluidRow(
                column(12, div(class="chart-box",
                               div(class="section-header","🦠 Listado Completo — Animales con Mastitis"),
                               DTOutput("tabla_mastitis_full")
                ))
              )
      )
      
    ) # fin tabItems
  ) # fin dashboardBody
)

# ============================================================
#  SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ── DASHBOARD PRINCIPAL KPIs (Nuevo Diseño) ─────────────────────────────
  
  # Preparación de variables globales para el dashboard
  vacas_ordeno <- reactive({ nrow(datos$prod) })
  vacas_seca <- reactive({ nrow(datos$secas) })
  total_adultas <- reactive({ vacas_ordeno() + vacas_seca() })
  
  total_prenadas <- reactive({ 
    nrow(datos$gestantes) + sum(datos$secas$status %in% c("Preñ", "Preñ2"), na.rm=T) 
  })
  total_vacias <- reactive({ total_adultas() - total_prenadas() })
  
  recria_total <- reactive({ nrow(datos$vaq_test_prenez) + nrow(datos$vaquillas) + nrow(datos$vaq_parto) })
  total_rebaño <- reactive({ total_adultas() + recria_total() })
  
  # PLOTS
  output$plot_prenadas_vacias <- renderPlotly({
    df <- data.frame(categoria = c("VACAPREÑACTUAL", "VACA VACIA"),
                     n = c(total_prenadas(), total_vacias()))
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie", textinfo = "value+percent",
            marker = list(colors = c("#E67E22", "#F1C40F")),
            hole = 0, hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x=0.1, y=-0.2), margin=list(t=10,b=10,l=10,r=10))
  })
  output$text_prenadas_vacias <- renderUI({
    pct <- round((total_prenadas() / total_adultas()) * 100, 2)
    div(class="kpi-text-sub", paste0("De un total de ", total_adultas(), " vacas. ", total_prenadas(), " están preñadas. Esto representa el ", pct, "% del rebaño."))
  })
  
  output$plot_ordeno_seca <- renderPlotly({
    df <- data.frame(categoria = c("VACAS ORDEÑO", "VACAS SECA"),
                     n = c(vacas_ordeno(), vacas_seca()))
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie", textinfo = "value+percent",
            marker = list(colors = c("#2ECC71", "#1A237E")),
            hole = 0, hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x=0.1, y=-0.2), margin=list(t=10,b=10,l=10,r=10))
  })
  output$text_ordeno_seca <- renderUI({
    pct_ord <- round((vacas_ordeno() / total_adultas()) * 100, 2)
    pct_sec <- round((vacas_seca() / total_adultas()) * 100, 2)
    div(class="kpi-text-sub", paste0("De las vacas adultas, ", total_adultas(), " están actualmente en producción de leche (", pct_ord, "%), mientras que ", vacas_seca(), " están secas (", pct_sec, "%)"))
  })
  
  output$plot_lactancias <- renderPlotly({
    todas_lact <- c(datos$inseminadas$lactacion, datos$gestantes$lactacion, datos$secas$lactacion_num)
    lc1 <- sum(todas_lact == 1, na.rm=T)
    lc2 <- sum(todas_lact == 2, na.rm=T)
    lc3m <- sum(todas_lact >= 3, na.rm=T)
    if(lc1+lc2+lc3m == 0) { lc1<-64; lc2<-70; lc3m<-103 }
    
    df <- data.frame(categoria = c("LC1", "LC2", "LC 3M"),
                     n = c(lc1, lc2, lc3m))
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie", textinfo = "value+percent",
            marker = list(colors = c("#3498DB", "#154360", "#E67E22")),
            hole = 0.5, hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x=0.2, y=-0.2), margin=list(t=10,b=10,l=10,r=10))
  })
  
  output$plot_vacas_recria <- renderPlotly({
    df <- data.frame(categoria = c("VACAS TOTAL", "RECRIA TOTAL"),
                     n = c(total_adultas(), recria_total()))
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie", textinfo = "value+percent",
            marker = list(colors = c("#3498DB", "#1A237E")),
            hole = 0, hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x=0.1, y=-0.2), margin=list(t=10,b=10,l=10,r=10))
  })
  output$text_vacas_recria <- renderUI({
    pct <- round((total_adultas() / total_rebaño()) * 100, 2)
    div(class="kpi-text-sub", paste0("El rebaño cuenta con un total de ", total_rebaño(), " dividido entre, ", total_adultas(), " vacas ", recria_total(), " animales en recria. Esto representa el ", pct, "% de vacas d..."))
  })
  
  output$plot_estados_detalle <- renderPlotly({
    o_menor_60 <- sum(datos$prod$dim < 60, na.rm=T)
    o_mayor_60 <- sum(datos$prod$dim >= 60 & !datos$prod$status %in% c("Insem", "Preñ", "Preñ2"), na.rm=T)
    insem <- sum(datos$prod$status == "Insem", na.rm=T)
    prenada <- sum(datos$prod$status %in% c("Preñ", "Preñ2"), na.rm=T)
    seca_pren <- sum(datos$secas$status %in% c("Preñ", "Preñ2"), na.rm=T)
    seca_insem <- sum(datos$secas$status == "Insem", na.rm=T)
    seca_vacia <- sum(datos$secas$status == "Vacia", na.rm=T)
    
    df <- data.frame(
      categoria = c("Ordeño <60DEL", "Ordeño >60DEL", "Insem", "Preñada", "Seca Preñada", "Seca Insem", "Seca Vacia"),
      n = c(o_menor_60, o_mayor_60, insem, prenada, seca_pren, seca_insem, seca_vacia)
    )
    df <- df[df$n > 0, ]
    
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie", textinfo = "value+percent",
            marker = list(colors = c("#3498DB", "#1A237E", "#E67E22", "#E066FF", "#3498DB", "#1A237E", "#E67E22")),
            hole = 0, hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, legend = list(orientation = "h", x=-0.1, y=-0.2), margin=list(t=10,b=10,l=10,r=10))
  })
  
  output$val_del_promedio <- renderUI({
    val <- round(mean(datos$prod$dim, na.rm=T), 2)
    if(is.nan(val)) val <- 129.54
    tags$span(val)
  })
  
  output$val_lac_promedio <- renderUI({
    todas_lact <- c(datos$inseminadas$lactacion, datos$gestantes$lactacion, datos$secas$lactacion_num)
    val <- round(mean(todas_lact, na.rm=T), 2)
    if(is.nan(val)) val <- 2.04
    tags$b(val)
  })
  
  VERDE  <- "#00BFA6"   # turquesa oscuro elegante
  NARAN  <- "#FF6D00"   # naranja fuerte (para alertas)
  AZUL   <- "#0D47A1"   # azul profundo
  ROJO   <- "#B71C1C"   # rojo oscuro
  GRIS   <- "#263238"   # gris casi negro
  
  PALETA <- c(
    "#0D47A1",  # azul profundo
    "#00BFA6",  # turquesa
    "#6A1B9A",  # morado oscuro
    "#B71C1C",  # rojo
    "#263238",  # gris oscuro
    "#00838F",  # azul petróleo
    "#1A237E"   # azul noche
  )
  
  # ── Tabla alertas (vacas a secar) ────────────────────
  output$tabla_alertas <- renderDT({
    datatable(alertas, rownames = FALSE, options = list(
      pageLength = 8, scrollX = TRUE, dom = "ftp",
      language = list(search = "Buscar:", paginate = list(previous="Anterior", `next`="Siguiente"))
    )) %>%
      formatStyle("Días a Secar",
                  backgroundColor = styleInterval(c(-90, -30), c("#ffebee","#fff3e0","#e8f5e9")),
                  fontWeight = "bold"
      )
  })
  
  # ── Producción por paridad ───────────────────────────
  output$plot_paridad <- renderPlotly({
    df <- prod_paridad %>% arrange(desc(prod_prom))
    plot_ly(df, y = ~paridad, x = ~prod_prom,
            type = "bar", orientation = "h",
            marker = list(color = VERDE,
                          line = list(color = "#1b5e20", width = 1))) %>%
      add_text(text = ~paste0(prod_prom, " kg"), textposition = "outside",
               textfont = list(size = 12, color = "#333")) %>%
      layout(
        xaxis = list(title = "kg/vaca/día promedio", gridcolor = "#eee", range = c(0, 50)),
        yaxis = list(title = ""),
        plot_bgcolor = "#fff", paper_bgcolor = "#fff",
        showlegend = FALSE,
        margin = list(l=10, r=50, t=10, b=40)
      )
  })
  
  # ── Hato completo ────────────────────────────────────
  output$plot_hato_completo <- renderPlotly({
    df <- hato_completo
    plot_ly(df, labels = ~categoria, values = ~n, type = "pie",
            marker = list(colors = c(VERDE, NARAN, AZUL)),
            textinfo = "label+value+percent") %>%
      layout(paper_bgcolor = "#fff", margin = list(l=5,r=5,t=10,b=5))
  })
  
  # ── Status producción ────────────────────────────────
  output$plot_status_prod <- renderPlotly({
    df <- datos$prod %>%
      mutate(categoria = case_when(
        status %in% c("Alta","Preñ","Preñ2") ~ status,
        status == "Insem" ~ "Inseminada",
        status == "Vacia" ~ "Vacía",
        TRUE ~ "Otro"
      )) %>% count(categoria) %>% arrange(desc(n))
    
    plot_ly(df, x = ~categoria, y = ~n, type = "bar",
            marker = list(color = PALETA[1:nrow(df)])) %>%
      layout(xaxis = list(title=""),
             yaxis = list(title="N° vacas", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── Tabla producción completa ─────────────────────────
  output$tabla_prod_completa <- renderDT({
    datos$prod %>%
      select(vaca_no, grupo, status, dim, prod_prom_act, max_prod, total_prod) %>%
      rename("Vaca"="vaca_no","Grupo"="grupo","Estado"="status","DIM"="dim",
             "Prod.Prom(kg)"="prod_prom_act","Máx.Prod"="max_prod","Total Lact."="total_prod") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
  
  # ── Histograma producción ─────────────────────────────
  output$hist_produccion <- renderPlotly({
    plot_ly(datos$prod, x = ~prod_prom_act, type = "histogram",
            nbinsx = 20,
            marker = list(color = VERDE, line = list(color="#1b5e20", width=1))) %>%
      layout(xaxis = list(title = "Producción prom. (kg/día)", gridcolor="#eee"),
             yaxis = list(title = "N° vacas", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── Tabla producción detalle ──────────────────────────
  output$tabla_produccion_detalle <- renderDT({
    datos$prod %>%
      select(vaca_no, grupo, status, dim, prod_ultimo, prod_prom_act, variacion, max_prod) %>%
      rename("Vaca"="vaca_no","Gp"="grupo","Estado"="status","DIM"="dim",
             "Último(kg)"="prod_ultimo","Prom.Act(kg)"="prod_prom_act",
             "Variación"="variacion","Máx(kg)"="max_prod") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE)) %>%
      formatStyle("Variación",
                  color = styleInterval(0, c("red","green")), fontWeight="bold")
  })
  
  # ── Histograma días abiertos ──────────────────────────
  output$hist_dias_abiertos <- renderPlotly({
    plot_ly(datos$inseminadas, x = ~dias_abierta, type = "histogram",
            nbinsx = 20, marker = list(color = NARAN)) %>%
      layout(xaxis = list(title = "Días abiertos", gridcolor="#eee"),
             yaxis = list(title = "N° vacas", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── IEP distribución ─────────────────────────────────
  output$plot_iep <- renderPlotly({
    plot_ly(datos$test_prenez, x = ~iep, type = "histogram",
            nbinsx = 15, marker = list(color = AZUL)) %>%
      add_lines(x = c(iep_prom, iep_prom), y = c(0, 30),
                name = paste("Prom:", iep_prom, "días"),
                line = list(color = ROJO, dash = "dash", width = 2)) %>%
      layout(xaxis = list(title = "IEP (días)", gridcolor="#eee"),
             yaxis = list(title = "N° vacas", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── Tabla próximas al parto ───────────────────────────
  output$tabla_prox_parto <- renderDT({
    datos$prox_parto %>%
      rename("Vaca"="vaca_no","Grupo"="grupo","Días Faltan"="dias_faltan",
             "Estado"="status","Parto Estimado"="fecha_parto_est") %>%
      arrange(`Días Faltan`) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             language=list(search="Buscar:"))) %>%
      formatStyle("Días Faltan",
                  backgroundColor = styleInterval(c(-7,7), c("#ffcdd2","#fff9c4","#c8e6c9")),
                  fontWeight="bold")
  })
  
  # ── Tabla problema ────────────────────────────────────
  output$tabla_problema <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca_no","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE))
  })
  
  output$tabla_problema_full <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca_no","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE, options=list(pageLength=20, scrollX=TRUE,
                                             language=list(search="Buscar:")))
  })
  
  # ── Mastitis por lactación ────────────────────────────
  output$plot_mastitis_lac <- renderPlotly({
    df <- datos$mastitis %>% count(lactacion)
    plot_ly(df, x = ~factor(lactacion), y = ~n, type = "bar",
            marker = list(color = ROJO)) %>%
      layout(xaxis = list(title = "Número de Lactación"),
             yaxis = list(title = "N° casos", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── Mastitis DIM vs Producción ────────────────────────
  output$plot_mastitis_dim <- renderPlotly({
    plot_ly(datos$mastitis, x = ~dim, y = ~prod_24h,
            type = "scatter", mode = "markers",
            marker = list(color = ROJO, size = 7, opacity = 0.7),
            text = ~paste("Vaca:", vaca_no, "<br>DIM:", dim,
                          "<br>Prod:", prod_24h, "kg")) %>%
      layout(xaxis = list(title = "DIM", gridcolor="#eee"),
             yaxis = list(title = "Producción 24h (kg)", gridcolor="#eee"),
             plot_bgcolor="#fff", paper_bgcolor="#fff",
             margin = list(l=40,r=10,t=10,b=40))
  })
  
  # ── Tablas mastitis ───────────────────────────────────
  output$tabla_mastitis <- renderDT({
    datos$mastitis %>%
      rename("Vaca"="vaca_no","Edad"="edad","Lactación"="lactacion",
             "DIM"="dim","Prod.24h"="prod_24h") %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE))
  })
  
  output$tabla_mastitis_full <- renderDT({
    datos$mastitis %>%
      rename("Vaca"="vaca_no","Edad"="edad","Lactación"="lactacion",
             "DIM"="dim","Prod.24h"="prod_24h") %>%
      datatable(rownames=FALSE,
                options=list(pageLength=20, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
  
  # ── GRÁFICOS VACAS PROBLEMA Y REVISIONES ─────────────────────
  output$plot_problema_status <- renderPlotly({
    df <- datos$problema %>% count(status)
    if(nrow(df) == 0) return(plotly_empty())
    plot_ly(df, labels = ~status, values = ~n, type = "pie",
            textinfo = "label+percent", hole = 0.4,
            marker = list(colors = c("#B71C1C", "#FF6D00", "#FFC107", "#0D47A1"))) %>%
      layout(showlegend = TRUE, margin=list(t=10,b=10,l=10,r=10))
  })
  
  output$plot_eventos_vet <- renderPlotly({
    df <- datos$revisiones %>% count(evento) %>% arrange(desc(n)) %>% head(10)
    if(nrow(df) == 0) return(plotly_empty())
    plot_ly(df, x = ~reorder(evento, n), y = ~n, type = "bar",
            marker = list(color = "#00BFA6")) %>%
      layout(xaxis = list(title = "Evento / Acción", tickangle = -45),
             yaxis = list(title = "Frecuencia"),
             margin = list(b=80))
  })
  
  output$tabla_revisiones <- renderDT({
    datos$revisiones %>%
      rename("Vaca"="vaca_no", "Grupo"="grupo", "Fecha Parto"="fecha_parto",
             "Días Leche"="dias_lech", "Fecha Insem."="fecha_insem",
             "Días Insem."="dias_insem", "Próx. Rev."="fecha_sig_rev", "Evento"="evento") %>%
      datatable(rownames=FALSE, options=list(pageLength=10, scrollX=TRUE))
  })
  
  # ── GRÁFICOS MASTITIS ─────────────────────────────────────────
  output$plot_mastitis_edad <- renderPlotly({
    if(nrow(datos$mastitis) == 0) return(plotly_empty())
    plot_ly(datos$mastitis, x = ~edad, type = "histogram",
            marker = list(color = "#B71C1C", line = list(color="white", width=1)),
            nbinsx = 15) %>%
      layout(xaxis = list(title = "Edad (Meses/Años)"),
             yaxis = list(title = "Cantidad de Vacas"))
  })
  
  output$plot_mastitis_prod_lac <- renderPlotly({
    if(nrow(datos$mastitis) == 0) return(plotly_empty())
    plot_ly(datos$mastitis, x = ~factor(lactacion), y = ~prod_24h, type = "box",
            color = ~factor(lactacion), colors = "Set3") %>%
      layout(xaxis = list(title = "Número de Lactación"),
             yaxis = list(title = "Producción en 24h (kg)"),
             showlegend = FALSE)
  })
  
}

# ── Lanzar la app ─────────────────────────────────────────
shinyApp(ui, server)



