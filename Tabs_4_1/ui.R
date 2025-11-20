################################################################################-
# UI – diseño estilo FAO (paneles ROJOS como módulo 3_9, botones sin fondo)
################################################################################-

mes_nombre_a_numero <- c(
  "Enero"="01","Febrero"="02","Marzo"="03","Abril"="04","Mayo"="05",
  "Junio"="06","Julio"="07","Agosto"="08","Septiembre"="09",
  "Octubre"="10","Noviembre"="11","Diciembre"="12"
)

library(leaflet)
meses_nombres <- setNames(mes_nombre_a_numero, names(mes_nombre_a_numero))

ui <- fluidPage(
  
  tags$head(
    tags$title("Mapa de Rutas – FAO 2025"),
    tags$link(
      rel="stylesheet", type="text/css",
      href="https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    tags$style(HTML("
      body { font-family:'Prompt', sans-serif; background-color:#fafafa; }

      .main-header { 
        font-size: 40px; 
        color:#743639; 
        text-align:left; 
        font-weight:bold; 
        margin-left:10px;
      }

      .main-header_2 {
        font-size:20px;
        color:#743639;
        font-weight:500;
        margin-left:10px;
        margin-top:-10px;
        margin-bottom:20px;
      }

      .panel-981338 {
        background-color: #981338 !important;
        color: white !important;
        font-weight: bold;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      
      .panel-332728 {
        background-color: #332728 !important;
        color: white !important;
        font-weight: bold;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      
      .panel-4F3032 {
        background-color: #4F3032 !important;
        color: white !important;
        font-weight: bold;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }

      .btn-faoc,
      .shiny-action-button,
      .shiny-download-link {
        background-color: transparent !important;
        border: 1.6px solid #cccccc !important;
        color: #4E4D4D !important;
        font-weight:500 !important;
        border-radius:6px !important;
        height:36px !important;
        padding:6px 14px !important;
        margin-right:6px !important;
      }

      .btn-faoc:hover,
      .shiny-action-button:hover,
      .shiny-download-link:hover {
        background-color:#f2f2f2 !important;
        color:#333333 !important;
      }
    "))
  ),
  
  tags$h1("Mapa de rutas de abastecimiento por municipio", class="main-header"),
  tags$h1("Visualización de rutas de abastecimiento desde el municipio de origen",
          class="main-header_2"),
  
  fluidRow(
    column(4, selectInput("anio", "Año:", choices = c())),
    column(4, selectInput("mes", "Mes:", choices = meses_nombres, selected="Diciembre")),
    column(4, selectInput("producto", "Producto:", choices = c()))
  ),
  
  br(),
  
  fluidRow(
    column(
      9,
      leafletOutput("grafico", height=480),
      br(),
      actionButton("descargar", "Gráfica", icon=icon("download"), class="btn-faoc"),
      downloadButton("descargarDatos", "Datos", class="btn-faoc"),
      a("GitHub", href="https://github.com/danielobando2030/SIMAGRO",
        target="_blank", class="btn-faoc", icon("github")),
      actionButton("reset2", "Restablecer", icon=icon("refresh"), class="btn-faoc"),
      downloadButton("descargarPDF", "Generar informe", class="btn-faoc")
    ),
    
    column(
      3,
      
      div(class="panel-981338",
          div("Municipio con mayor importancia"),
          div(textOutput("region_mas_importante"))
      ),
      
      div(class="panel-332728",
          div("Municipio con menor importancia"),
          div(textOutput("region_menos_importante"))
      ),
      
      # PANEL 3: SOLO EL MENSAJE — SIN TÍTULO
      div(class="panel-4F3032",
          div(textOutput("mensaje_interpretativo"))
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA)."),
      style="font-size:12px; color:#4E4D4D; font-family:'Prompt';"
    )
  ),
  
  div(img(src="logo_4.png", style="width:100%; margin-top:20px;"))
)
