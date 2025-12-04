################################################################################-
# Proyecto FAO - VP - 2025
# UI – Mapa de Rutas por Regiones (versión FINAL con TRES MENSAJES)
################################################################################-

library(shiny)
library(leaflet)

source("4_2b_cierres_rutas_abastecimiento.R")  # contiene graficar_rutas_color_importancia

ui <- fluidPage(
  
  ###########################################################################
  # SCRIPTS JS PARA CHECKBOXES (con bloqueo y SIN LOOP)
  ###########################################################################
  
  tags$script("window.blockCheckboxEvents = false;"),
  
  tags$script("
    Shiny.addCustomMessageHandler('block_checkbox_events', function(value){
      window.blockCheckboxEvents = value;
    });
  "),
  
  tags$script("
    $(document).on('change', 'input[type=checkbox]', function() {
      if (window.blockCheckboxEvents) return;
      Shiny.setInputValue(this.id, this.checked, {priority: 'defer'});
    });
  "),
  
  ###########################################################################
  # HEAD + ESTILOS
  ###########################################################################
  tags$head(
    tags$title("Mapa de rutas por cierres – FAO 2025"),
    
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    
    tags$style(HTML("
      body { 
        font-family: 'Prompt', sans-serif; 
        background-color: #fafafa;
        color:#4E4D4D;
      }

      /* TITULOS */
      .main-header {
        font-size: 40px;
        color: #743639;
        font-weight: 700;
        margin-top: 10px;
      }
      .main-header_2 {
        font-size: 20px;
        color: #743639;
        font-weight: 500;
      }

      /* PANEL PRIMERO (mensaje3) — NUEVO COLOR */
      .panel-rojo-oscuro {
        background-color:#332728 !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
        min-height:120px;
      }

      /* PANEL ROJO CLARO (mensaje2) */
      .panel-rojo-claro {
        background-color:#BC222A !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
        min-height:120px;
      }

      /* PANEL ROJO OSCURO 2 (mensaje1) */
      .panel-rojo-oscuro-2 {
        background-color:#8A171C !important;
        color:white !important;
        font-weight:bold;
        border-radius:8px;
        padding:15px;
        margin-bottom:15px;
        min-height:120px;
      }

      /* BOTONES */
      .btn-faoc,
      .shiny-download-link,
      .shiny-action-button,
      .btn {
        background-color: transparent !important;
        border: 1px solid #bfbfbf !important;
        color: #4E4D4D !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        margin-right: 6px !important;
        height: 36px !important;
      }
      .btn-faoc:hover, .btn:hover {
        background-color: #e6e6e6 !important;
      }

      /* CHECKBOX GRID 4x2 */
      .rutas-container {
        display: flex;
        flex-wrap: wrap;
        width: 100%;
      }
      .ruta-item {
        width: 25%;
        display: flex;
        align-items: center;
        gap: 3px;
        margin-bottom: 6px;
      }
      .ruta-item input[type='checkbox'] {
        width: 14px;
        height: 14px;
      }
      .ruta-item label {
        font-size: 11px;
        line-height: 12px;
      }

      /* Pie de página */
      .footer-text { 
        font-size: 12px; 
        color: #4E4D4D;
      }
    "))
  ),
  
  ###########################################################################
  # TITULOS
  ###########################################################################
  tags$h1("Impacto del cierre de vías de acceso en el abastecimiento de Bogotá", class="main-header"),
  tags$h2("Importancia de la ruta por región ante un posible cierre de vías", class="main-header_2"),
  
  ###########################################################################
  # FILTROS
  ###########################################################################
  div(
    fluidRow(
      
      column(
        3,
        selectInput("anio", "Año:",
                    choices = sort(unique(data_cierres_final$anio)),
                    selected = 2024)
      ),
      
      column(
        3,
        selectInput(
          "mes", "Mes:",
          choices = setNames(
            sprintf("%02d", 1:12),
            c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
          ),
          selected = "12"
        )
      ),
      
      column(
        3,
        selectInput("producto", "Producto:",
                    choices = sort(unique(data_cierres_final$producto)),
                    selected = "Aguacate Hass")
      ),
      
      # CHECKBOXES
      column(
        3,
        tags$label("Regiones a mostrar:"),
        div(
          class="rutas-container",
          
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Noroccidente", checked="checked",
                         style="accent-color:#e31a1c;"),
              tags$label("Noroccidente")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Nororiente", checked="checked",
                         style="accent-color:#ff7f00;"),
              tags$label("Nororiente")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Norte", checked="checked",
                         style="accent-color:#6a3d9a;"),
              tags$label("Norte")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Oriente", checked="checked",
                         style="accent-color:#1f78b4;"),
              tags$label("Oriente")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Suroriente", checked="checked",
                         style="accent-color:#b2df8a;"),
              tags$label("Suroriente")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Sur", checked="checked",
                         style="accent-color:#b15928;"),
              tags$label("Sur")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Suroccidente", checked="checked",
                         style="accent-color:#a6cee3;"),
              tags$label("Suroccidente")
          ),
          div(class="ruta-item",
              tags$input(type="checkbox", id="r_Occidente", checked="checked",
                         style="accent-color:#33a02c;"),
              tags$label("Occidente")
          )
        )
      )
    )
  ),
  
  ###########################################################################
  # MAPA + PANEL DERECHO (3 MENSAJES)
  ###########################################################################
  fluidRow(
    
    column(
      9,
      div(
        leafletOutput("grafico", height="480px"),
        br(),
        
        downloadButton("descargar", "Gráfica", class="btn-faoc"),
        downloadButton("descargarDatos", "Datos", class="btn-faoc"),
        
        tags$a(
          href="https://github.com/danielobando2030/SIMAGRO",
          target="_blank",
          class="btn-faoc",
          icon("github"),
          tags$span("GitHub")
        ),
        
        actionButton("reset", "Restablecer", icon=icon("refresh"), class="btn-faoc"),
        
        downloadButton("descargarPDF", "Generar informe", class="btn-faoc")
      )
    ),
    
    column(
      3,
      
      # --- PRIMER PANEL: AHORA mensaje3 (COLOR NUEVO) ---
      div(class="panel-rojo-oscuro",
          htmlOutput("impacto_cierre")
      ),
      
      # --- SEGUNDO PANEL: mensaje2 ---
      div(class="panel-rojo-claro",
          htmlOutput("ranking_rutas")
      ),
      
      # --- TERCER PANEL: mensaje1 ---
      div(class="panel-rojo-oscuro-2",
          htmlOutput("municipio_mas_importante")
      )
    )
  ),
  
  br(),
  
  ###########################################################################
  # FUENTE
  ###########################################################################
  fluidRow(
    column(
      12,
      HTML("<b>Fuente:</b> Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br><br>
           Esta visualización permite ver las rutas más importantes por donde se transportan los alimentos hacia Bogotá y Cundinamarca. 
           Las líneas más gruesas indican mayor importancia de la ruta de abastecimiento."),
      class="footer-text"
    )
  ),
  
  ###########################################################################
  # LOGO INFERIOR
  ###########################################################################
  fluidRow(
    tags$div(
      tags$img(src="logo_4.png", style="width:100%; margin:0; padding:0;"),
      style="width:100%; margin:0;"
    )
  )
)

################################################################################-
# FIN UI
################################################################################-
