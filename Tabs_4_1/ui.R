################################################################################-
# UI – diseño estilo FAO (paneles ROJOS como módulo 3_9, botones sin fondo)
################################################################################-|

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

      /* TITULOS */
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

      /* PANEL ROJO OSCURO */
      .panel-rojo-oscuro {
        background-color:#8A171C !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
      }

      /* PANEL ROJO CLARO */
      .panel-rojo-claro {
        background-color:#BC222A !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
      }

      /* ------------------------------------------
         BOTONES SIN FONDO (estilo FAO limpio)
         ------------------------------------------ */
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

      /* hover */
      .btn-faoc:hover,
      .shiny-action-button:hover,
      .shiny-download-link:hover {
        background-color:#f2f2f2 !important;
        color:#333333 !important;
      }
    "))
  ),
  
  # ---------------- TITULOS ----------------
  tags$h1("Mapa de rutas de abastecimiento por región", class="main-header"),
  tags$h1("Visualización de rutas desde municipios origen hacia el destino seleccionado",
          class="main-header_2"),
  
  # ---------------- FILTROS ----------------
  fluidRow(
    column(
      4,
      selectInput("anio", "Año:", choices = c())
    ),
    column(
      4,
      selectInput("mes", "Mes:", choices = meses_nombres, selected="Diciembre")
    ),
    column(
      4,
      selectInput("producto", "Producto:", choices = c())
    )
  ),
  
  br(),
  
  # ---------------- MAPA + PANEL DERECHO ----------------
  fluidRow(
    
    # MAPA
    column(
      9,
      leafletOutput("grafico", height=480),
      br(),
      actionButton("descargar", "Gráfica", icon=icon("download"), class="btn-faoc"),
      downloadButton("descargarDatos", "Datos", class="btn-faoc"),
      a("GitHub",
        href="https://github.com/danielobando2030/SIMAGRO",
        target="_blank",
        class="btn-faoc",
        icon("github")),
      actionButton("reset2", "Restablecer", icon=icon("refresh"), class="btn-faoc"),
      downloadButton("descargarPDF", "Generar informe", class="btn-faoc")
    ),
    
    # PANEL DERECHO (dos tonos rojos estilo 3_9)
    column(
      3,
      
      div(class="panel-rojo-oscuro",
          div(class="stat-title","Municipio con mayor importancia"),
          div(textOutput("region_mas_importante"))
      ),
      
      div(class="panel-rojo-claro",
          div(class="stat-title","Municipio con menor importancia"),
          div(textOutput("region_menos_importante"))
      ),
      
      div(class="panel-rojo-claro",
          div(class="stat-title","Mensaje interpretativo"),
          div(textOutput("mensaje1"))
      )
    )
  ),
  
  br(),
  
  # ---------------- FUENTE ----------------
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir de datos del SIPSA."),
      style="font-size:12px; color:#4E4D4D; font-family:'Prompt';"
    )
  ),
  
  # ---------------- LOGO FINAL ----------------
  div(img(src="logo_2.png", style="width:100%; margin-top:20px;"))
)
