################################################################################-
# UI – FAO 2025 | Mapa de Rutas (OPTIMIZADO SEGURO)
################################################################################-
library(leaflet)

meses_nombres <- c(
  "Enero"="01","Febrero"="02","Marzo"="03","Abril"="04","Mayo"="05",
  "Junio"="06","Julio"="07","Agosto"="08","Septiembre"="09",
  "Octubre"="10","Noviembre"="11","Diciembre"="12"
)

ui <- fluidPage(
  
  tags$head(
    tags$title("Mapa de Rutas – FAO 2025"),
    
    # Fuente FAO
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    
    # ========================= CSS FAO =========================
    tags$style(HTML("
      :root {
        --fao-red-1: #981338;
        --fao-red-2: #332728;
        --fao-red-3: #4F3032;
        --fao-gray:  #4E4D4D;
        --fao-title: #743639;
      }

      body {
        font-family: 'Prompt', sans-serif;
        background-color: #fafafa;
      }

      /* Títulos */
      .main-header {
        font-size: 40px;
        color: var(--fao-title);
        font-weight: bold;
        margin: 0 0 4px 10px;
      }

      .main-header_2 {
        font-size: 20px;
        color: var(--fao-title);
        font-weight: 500;
        margin: 0 0 20px 10px;
      }

      /* Paneles FAO */
      .panel-fao {
        color: white;
        font-weight: bold;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }

      .panel-red-1 { background-color: var(--fao-red-1); }
      .panel-red-2 { background-color: var(--fao-red-2); }
      .panel-red-3 { background-color: var(--fao-red-3); }

      /* Botones FAO */
      .btn-faoc,
      .shiny-action-button,
      .shiny-download-link {
        background-color: transparent !important;
        border: 1.6px solid #cccccc !important;
        color: var(--fao-gray) !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        height: 36px !important;
        padding: 6px 14px !important;
        margin-right: 6px !important;
      }

      .btn-faoc:hover {
        background-color: #f2f2f2 !important;
        color: #333333 !important;
      }
    "))
  ),
  
  # ============================================================
  # TÍTULOS
  # ============================================================
  tags$h1("Mapa de rutas de abastecimiento por municipio", class = "main-header"),
  tags$h2(
    "Visualización de rutas de abastecimiento desde el municipio de origen",
    class = "main-header_2"
  ),
  
  # ============================================================
  # FILTROS
  # ============================================================
  fluidRow(
    column(4, selectInput("anio", "Año:", choices = c())),
    column(4, selectInput("mes", "Mes:", choices = meses_nombres, selected = "Diciembre")),
    column(4, selectInput("producto", "Producto:", choices = c()))
  ),
  
  # ============================================================
  # CONTENIDO PRINCIPAL
  # ============================================================
  fluidRow(
    
    # ---------------- MAPA ----------------
    column(
      9,
      leafletOutput("grafico", height = 480),  # ⬅️ NO TOCADO
      div(style = "margin-top:12px;",
          downloadButton("descargar", "Gráfica", class = "btn-faoc"),
          downloadButton("descargarDatos", "Datos", class = "btn-faoc"),
          tags$a(
            "GitHub",
            href = "https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_4_1",
            target = "_blank",
            class = "btn-faoc",
            shiny::icon("github")
          ),
          actionButton(
            "reset2",
            "Restablecer",
            icon = shiny::icon("refresh"),
            class = "btn-faoc"
          ),
          downloadButton("descargarPDF", "Generar informe", class = "btn-faoc")
      )
    ),
    
    # ---------------- PANELES ----------------
    column(
      3,
      div(
        class = "panel-fao panel-red-1",
        div("Municipio con mayor importancia"),
        div(textOutput("region_mas_importante"))
      ),
      
      div(
        class = "panel-fao panel-red-2",
        div("Municipio con menor importancia"),
        div(textOutput("region_menos_importante"))
      ),
      
      div(
        class = "panel-fao panel-red-3",
        div(textOutput("mensaje_interpretativo"))
      )
    )
  ),
  
  # ============================================================
  # FUENTE
  # ============================================================
  fluidRow(
    column(
      12,
      HTML(
        "<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información
        de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>
        Esta visualización permite identificar las principales rutas por las cuales
        se transportan los alimentos hacia Bogotá y Cundinamarca desde los municipios
        de origen del abastecimiento, donde un tono más rojizo y mayor grosor de la ruta
        indican mayor importancia."
      ),
      style = "font-size:12px; color:var(--fao-gray);"
    )
  ),
  
  div(
    img(src = "logo_4.png", style = "width:100%; margin-top:20px;")
  )
)
