################################################################################-
# Proyecto FAO - VP - 2025
# Interfaz UI - Mapa de Rutas (con logos FAO)
################################################################################-

library(shiny)
library(leaflet)

ui <- fluidPage(
  tags$head(
    tags$title("Mapa de rutas de abastecimiento - FAO 2025"),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      .main-title { color: #5A189A; font-size: 32px; font-weight: bold; text-align:center; }
      .sub-header { color: #7B2CBF; font-size: 18px; text-align:center; }
      .stat-box {
        background-color: #f3e8ff;
        border-left: 5px solid #5A189A;
        padding: 12px; border-radius: 8px; margin-bottom: 12px;
      }
      .stat-value { color: #3C096C; font-size: 16px; font-weight: bold; }
      .btn-danger {
        background-color: #7B2CBF !important; border-color: #7B2CBF !important;
      }
      .btn-danger:hover {
        background-color: #5A189A !important; border-color: #5A189A !important;
      }
    "))
  ),
  
  # ---- Logo superior ----
  div(img(src = "logo_3.png", style = "width:100%; margin-bottom:20px;")),
  
  # ---- Títulos ----
  titlePanel(div("Mapa de Rutas de Abastecimiento", class = "main-title")),
  h4("Proyecto FAO - VP - 2025", class = "sub-header"),
  
  # ---- Filtros ----
  fluidRow(
    column(3, selectInput("anio", "Año:", choices = NULL)),
    column(3, selectInput("mes", "Mes:", choices = NULL)),
    column(3, selectInput("producto", "Producto:", choices = NULL)),
    column(3,
           checkboxGroupInput(
             "rutas", "Selecciona las regiones a mostrar:",
             choices = c(
               "Noroccidente" = "Noroccidente",
               "Nororiente"   = "Nororiente",
               "Norte"        = "Norte",
               "Oriente"      = "Oriente",
               "Suroriente"   = "Suroriente",
               "Sur"          = "Sur",
               "Suroccidente" = "Suroccidente",
               "Occidente"    = "Occidente"
             ),
             selected = c(
               "Noroccidente","Nororiente","Norte","Oriente",
               "Suroriente","Sur","Suroccidente","Occidente"
             )
           )
    )
  ),
  
  # ---- Mapa + Panel lateral ----
  fluidRow(
    column(9, leafletOutput("grafico", height = 600)),
    column(3,
           br(),
           div(class = "stat-box", textOutput("municipio_mas_importante")),
           div(class = "stat-box", textOutput("ranking_rutas")),
           br(),
           downloadButton("descargarPDF", "Descargar informe PDF", class = "btn btn-danger"),
           br(), br(),
           downloadButton("descargarDatos", "Descargar datos CSV"),
           br(), br(),
           actionButton("reset", "Restablecer filtros", icon = icon("refresh"))
    )
  ),
  
  # ---- Pie de página ----
  br(),
  tags$p("Fuente: Cálculos propios con base en datos SIPSA. Proyecto FAO - VP - 2025.",
         style = "font-size:13px;color:gray;text-align:center;"),
  div(img(src = "logo_2.png", style = "width:100%; margin-top:20px;"))
)
