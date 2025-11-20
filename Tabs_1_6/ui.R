#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Modificado: Cristian Daniel Obando, Luis Miguel Garcia
#Fecha de ultima modificacion: 10/11/2025
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-

source("1_6b_porcentaje_productos_salen_funciones.R")

library(shiny)

# Define la interfaz de usuario
ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh; padding-bottom: 100px;",  # Añade un margen inferior
    tags$head(
      tags$title("Productos_salen"),  # Añade esta línea
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),  # Importa la fuente Prompt
      tags$style(HTML("
      #grafico {
             display: block;
             margin: auto;
             color: #266486;
           }
      .selectize-dropdown {
      z-index: 10000 !important;
      }
     .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #266486;
      }
      body {
        overflow-x: hidden;
      }
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #266486;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #4E4D4D;
      }
      .sub-header2 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
        color: #4E4D4D;
      }
    "))
  ),
  tags$h1("Cundinamarca en los mercados: Principales productos por volumen", class = "main-header"),
  tags$h1("Principales productos de Cundinamarca en otros mercados: volumen y composición de la canasta.", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),  
  div(class = "scrollable-content",
      fluidRow(
        column(4,
               selectInput("año", "Selecciones el año:", c("Todos los años" = "todo", sort(unique(salen$anio))))),
        column(4,
               selectInput("mes", "Selecciones el mes:", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5,
                                                           "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11,
                                                           "Diciembre" = 12), selected="")),
        column(4,
               selectInput("depto", "Seleccione el destino", c("Total nacional" = "todo", sort(unique(salen$mpio_destino)))))
      )),
  div(
    fluidRow(
      column(10,
             highchartOutput("grafico",height = "300px"),
             downloadButton("descargar_", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_6", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer", icon = icon("refresh")),
             downloadButton("report", "Generar informe")
      ),
      column(2, 
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #0087CF; color: #FFFFFF;"),
             wellPanel(textOutput("mensaje2"),
                      style = "background-color: #2A4E61; color: #FFFFFF;")
          
      )
    ),
    fluidRow(
      column(
        12,
        align = "left",
        HTML("<b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br>
         <br>
         Solo se muestran los productos que representan al menos un 0,5% del volumen total de productos de origen cundinamarqués reportado en las centrales de abasto del SIPSA."),
        style = "font-size:12px; color:#4E4D4D; text-align:left; font-family:'Prompt', sans-serif; margin-top:20px;"
      )
    )),
  fluidRow(
    tags$div(
      tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin: 0;"
    )
  )
  ) 
)