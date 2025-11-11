################################################################################-
# Interfaz UI - Mapa de Rutas por Región (versión con logos)
################################################################################-


library(shiny)
library(leaflet)


ui <- fluidPage(
  tags$head(
    tags$title("Mapa de rutas de abastecimiento - FAO 2025"),
    tags$style(HTML("
body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
.main-title { color: #5A189A; font-size: 32px; font-weight: bold; text-align: center; }
.sub-header { color: #7B2CBF; font-size: 18px; text-align: center; margin-bottom: 20px; }
.stat-box { background-color: #f3e8ff; border-left: 5px solid #5A189A; padding: 12px; margin-bottom: 12px; border-radius: 8px; }
.stat-title { color: #5A189A; font-weight: bold; font-size: 15px; }
.stat-value { color: #3C096C; font-size: 18px; font-weight: bold; }
#titulo_mapa { color: #5A189A; font-size: 18px; font-weight: bold; text-align: center; }
#mensaje-estado { text-align: center; font-size: 20px; color: #5A189A; font-weight: bold; margin-top: 200px; }
.btn-primary { background-color: #5A189A !important; border-color: #5A189A !important; }
.btn-danger { background-color: #9D4EDD !important; border-color: #9D4EDD !important; }
"))
  ),
  div(img(src="logo_3.png", style="width:100%; margin-bottom:20px;")),
  titlePanel(div("Mapa de rutas de abastecimiento por región", class="main-title")),
  h4("Proyecto FAO - VP - 2025", class="sub-header"),
  fluidRow(
    column(4, selectInput("anio", "Año:", choices=NULL)),
    column(4, selectInput("mes", "Mes:", choices=NULL)),
    column(4, selectInput("producto", "Producto:", choices=NULL))
  ),
  fluidRow(
    column(9, br(), h5(textOutput("titulo_mapa"), id="titulo_mapa"), br(), leafletOutput("grafico", height=600)),
    column(3, br(),
           div(class="stat-box", div(class="stat-title","Municipio con mayor importancia"), div(class="stat-value", textOutput("region_mas_importante"))),
           div(class="stat-box", div(class="stat-title","Municipio con menor importancia"), div(class="stat-value", textOutput("region_menos_importante"))),
           br(), downloadButton("descargarDatos", "Descargar datos CSV", class="btn btn-primary"), br(), br(),
           downloadButton("descargarPDF", "Descargar informe PDF", class="btn btn-danger"), br(), br(),
           actionButton("reset", "Restablecer filtros", icon=icon("refresh"))
    )
  ),
  br(),
  tags$p("Fuente: Cálculos propios con base en datos SIPSA. Proyecto FAO - VP - 2025.", style="font-size: 13px; color: gray; text-align: center;"),
  div(img(src="logo_2.png", style="width:100%; margin-top:20px;"))
)