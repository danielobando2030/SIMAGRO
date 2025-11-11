################################################################################
# Proyecto FAO - VP - 2025
# UI - Variación porcentual mensual del precio promedio
################################################################################

pacman::p_load(shiny, plotly, dplyr, zoo, lubridate)
options(scipen = 999)

# --- Cargar datos para las opciones del selector ---
data <- readRDS("base_precios_mayorista_mes_filtrados_3_7.rds")
productos <- sort(unique(data$producto))
anios <- sort(unique(format(as.Date(paste0(data$mes_y_ano, "-01")), "%Y")))

ui <- fluidPage(
  tags$head(
    tags$title("Variación mensual del precio promedio - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h1 { color: #6A1B9A; font-weight: bold; font-size: 32px; text-align: center; }
      h2 { color: #8E24AA; text-align: center; margin-bottom: 25px; }
      .btn-faoc {
        background-color: #6A1B9A; border-color: #6A1B9A; color: white; font-weight: 500;
        border-radius: 6px; margin-right: 5px;
      }
      .btn-faoc:hover {
        background-color: #500985; border-color: #500985; color: white;
      }
      .well-panel-fao {
        background-color: #6A1B9A; color: white; border-radius: 8px;
        padding: 15px; font-weight: 500; font-size: 14px;
      }
      .panel-cambio p { margin-bottom: 8px; line-height: 1.4; }
    "))
  ),
  
  # --- Logo superior ---
  div(class = "logo-header", img(src = "logo_3.png", height = "90px")),
  
  # --- Título principal ---
  h1("Variación porcentual mensual del precio promedio"),
  h2("Identificación de los meses con mayor variación intermensual"),
  
  # --- Filtros de selección ---
  fluidRow(
    column(6,
           selectInput("producto", "Seleccione producto:",
                       choices = productos, selected = "Aguacate")),
    column(6,
           selectInput("anio", "Seleccione año:",
                       choices = anios, selected = "2014"))
  ),
  
  br(),
  
  # --- Layout principal ---
  fluidRow(
    column(
      9,
      plotlyOutput("grafico", height = "600px"),
      br(),
      downloadButton("descargarPDF", "Generar informe PDF", class = "btn-faoc"),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc")
    ),
    column(
      3,
      wellPanel(uiOutput("mayorCambio"), class = "well-panel-fao")
    )
  ),
  
  br(),
  div(style="font-size:12px; color:#5A5A5A; text-align:left; line-height:1.4;",
      HTML("Fuente: cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).")),
  
  # --- Logo inferior ---
  br(), div(class = "logo-footer", img(src = "logo_2.png", height = "80px"))
)
