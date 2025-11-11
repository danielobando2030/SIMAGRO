################################################################################
# Proyecto FAO - VP - 2025
# UI - Matriz de correlación de precios entre productos
################################################################################

pacman::p_load(shiny, plotly, dplyr, lubridate)
options(scipen = 999)

# Cargar datos para opciones de menú
data <- readRDS("precios_bogota_balanceado_3_8.rds")
data$anio <- lubridate::year(data$mes_y_ano)

################################################################################
ui <- fluidPage(
  tags$head(
    tags$title("Correlación de precios - FAO VP 2025"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h1 { color: #6A1B9A; font-weight: bold; font-size: 32px; text-align: center; margin-bottom: 10px; }
      h2 { color: #8E24AA; text-align: center; margin-bottom: 25px; font-size: 18px; }
      .btn-faoc {
        background-color: #6A1B9A; border-color: #6A1B9A; color: white; font-weight: 500;
        border-radius: 6px; margin-right: 5px;
      }
      .btn-faoc:hover {
        background-color: #500985; border-color: #500985; color: white;
      }
      .panel-fao {
        background-color: #6A1B9A;
        color: white;
        border-radius: 8px;
        padding: 15px;
        font-size: 14px;
        margin-bottom: 12px;
      }
    "))
  ),
  
  # Logo superior
  div(class = "logo-header", img(src = "logo_3.png", height = "90px", style = "display:block; margin:auto;")),
  
  h1("Matriz de correlación de precios entre productos"),
  h2("Identificación de relaciones entre precios mayoristas por producto"),
  
  fluidRow(
    column(
      width = 8,
      plotlyOutput("grafico", height = "700px"),
      br(),
      actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn-faoc"),
      downloadButton("descargarPDF", "Generar informe PDF", class = "btn-faoc")
    ),
    column(
      width = 4,
      div(class = "panel-fao", uiOutput("topPositivas")),
      div(class = "panel-fao", uiOutput("topNegativas"))
    )
  ),
  
  br(),
  div(style="font-size:12px; color:#5A5A5A; text-align:left; line-height:1.4;",
      HTML("Fuente: cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA) – DANE.")),
  
  # Logo inferior
  br(),
  div(class = "logo-footer", img(src = "logo_2.png", height = "80px", style = "display:block; margin:auto;"))
)
