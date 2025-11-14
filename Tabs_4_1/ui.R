################################################################################-
# UI – diseño estilo FAO antiguo (sin botón duplicado)
################################################################################-

mes_nombre_a_numero <- c(
  "Enero"="01","Febrero"="02","Marzo"="03","Abril"="04","Mayo"="05",
  "Junio"="06","Julio"="07","Agosto"="08","Septiembre"="09",
  "Octubre"="10","Noviembre"="11","Diciembre"="12"
)

meses_nombres <- setNames(mes_nombre_a_numero, names(mes_nombre_a_numero))

ui <- fluidPage(
  
  tags$head(
    tags$title("Mapa de Rutas – FAO 2025"),
    tags$link(
      rel="stylesheet", 
      type="text/css",
      href="https://fonts.googleapis.com/css2?family=Prompt&display=swap"
    ),
    tags$style(HTML("
      body { font-family:'Prompt', sans-serif; background-color:#fafafa; }
      .main-header { font-size:34px; color:#134174; text-align:center; font-weight:bold; }

      .stat-box { 
        background-color:#134174; 
        padding:12px; 
        border-radius:8px; 
        margin-bottom:10px; 
        color:white; 
      }

      .stat-title { 
        font-size:14px; 
        font-weight:bold; 
      }

      .stat-value { 
        font-size:14px;    
        font-weight:normal; 
      }

      /* --------------------------
         BOTONES UNIFICADOS
         -------------------------- */

      .btn,
      .btn-primary,
      .btn-danger,
      .btn-default,
      .shiny-download-link,
      .shiny-action-button {
        background-color:#134174 !important;
        border-color:#134174 !important;
        color:white !important;
        font-weight:bold;
      }

      /* Color hover */
      .btn:hover,
      .btn-primary:hover,
      .btn-danger:hover,
      .btn-default:hover,
      .shiny-download-link:hover,
      .shiny-action-button:hover {
        background-color:#0F355F !important;
        border-color:#0F355F !important;
        color:white !important;
      }
"))
  ),
  
  div(img(src="logo_3.png", style="width:100%; margin-bottom:20px;")),
  
  tags$h1("Mapa de rutas de abastecimiento por región", class="main-header"),
  
  # ---------------- Filtros ----------------
  fluidRow(
    column(4, selectInput("anio", "Año:", sort(unique(data_merged$anio)), selected="2024")),
    column(4, selectInput("mes", "Mes:", meses_nombres, selected="Diciembre")),
    column(4, selectInput("producto", "Producto:", sort(unique(data_merged$producto)), selected="Aguacate Hass"))
  ),
  
  br(),
  
  # ---------------- Mapa + Panel derecho ----------------
  fluidRow(
    
    # Mapa + Botones
    column(
      9,
      leafletOutput("grafico", height=480),
      br(),
      actionButton("descargar", "Gráfica", icon=icon("download"), class="btn btn-primary"),
      downloadButton("descargarDatos", "Datos", class="btn btn-primary"),
      a("GitHub", href="https://github.com/danielobando2030/SIMAGRO", target="_blank",
        class="btn btn-default shiny-action-button", icon("github")),
      actionButton("reset2", "Restablecer", icon=icon("refresh"), class="btn btn-primary"),
      downloadButton("descargarPDF", "Generar informe", class="btn btn-danger")
    ),
    
    # Panel Estadísticas
    column(
      3,
      
      div(class="stat-box",
          div(class="stat-title","Municipio con mayor importancia"),
          div(class="stat-value", textOutput("region_mas_importante"))
      ),
      
      div(class="stat-box",
          div(class="stat-title","Municipio con menor importancia"),
          div(class="stat-value", textOutput("region_menos_importante"))
      ),
      
      div(class="stat-box",
          div(class="stat-title","Mensaje interpretativo"),
          div(class="stat-value", textOutput("mensaje1"))   # ← CORREGIDO
      )
    )
  ),
  
  br(),
  
  # ---------------- Fuente ----------------
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir de datos del SIPSA."),
      style = "font-size:12px; color:#4E4D4D; font-family:'Prompt';"
    )
  ),
  
  div(img(src="logo_2.png", style="width:100%; margin-top:20px;"))
)
