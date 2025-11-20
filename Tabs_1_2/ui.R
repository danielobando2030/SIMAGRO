#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Modificado: Cristian Daniel Obando,m Luis Miguel Garcia
#Fecha de ultima modificacion: 10/11/2025
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
library(sf)
################################################################################-
options(scipen = 999)
source("1_2b_Indices_envian_cundinamarca_cadena_funciones.R")
################################################################################-

# Cargar los paquetes necesarios
library(shiny)
library(dplyr)
library(ggplot2)

# Definir la interfaz de usuario

ui <- fluidPage(
  tags$div(
    style = "position: relative; min-height: 100vh; padding-bottom: 100px;",  # Añade un margen inferior
    tags$head(
      tags$title("Importancia Cundinamarca"),  # Añade esta línea
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),  # Importa la fuente Prompt
      tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #134174;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #134174;
      }
      .sub-header2 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
        color: #4E4D4D;
      }
      .sub-header3 {
        font-family: 'Prompt', sans-serif;
        font-size: 15px;
        color: #4E4D4D;
      }
      .center {
        display: flex;
        justify-content: center;
        color: #4E4D4D;
      }
      .scrollable-content {
        overflow-y: auto;
        overflow-x: hidden;
        height: auto;
        color: #4E4D4D;
      }
      
    "))
  ),
  tags$h1("Cundinamarca y su importancia en la recepción de alimentos", class = "main-header"),
  tags$h1("Descubre la importancia de Cundinamarca como receptor de alimentos desde otros departamentos del país.", class = "main-header_2"),
#  div(
#    textOutput("subtitulo"),
#    class = "sub-header2",
#    style = "margin-bottom: 20px;"
#  ),
  div(
      fluidRow(
        column(4,
               selectInput("anio", "Año", c("Todos los años" = "todo", sort(as.character(unique(cundinamarca$anio)))))),
        column(4,
               selectInput("mes", "Mes", c("Todos los meses" = "todo", "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8, "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12), selected="")),
        column(4,
               selectInput("producto", "Producto",c("Todos los productos" = "todo", sort(as.character(unique(cundinamarca$producto))))))
      )),
  div(
    fluidRow(
      column(9,
             leafletOutput("grafico",  height = "500px"),
             downloadButton("descargar_", "Gráfica", icon = icon("download")),
             downloadButton("descargarDatos", "Datos"),
             shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_1_2", target="_blank",
                      class = "btn btn-default shiny-action-button", icon("github")),
             actionButton("reset", "Restablecer", icon = icon("refresh")),
             downloadButton("report", "Generar informe")
        
      ),
      column(3, 
             wellPanel(textOutput("mensaje1"),
                       style = "background-color: #134174; color: #FFFFFF;"),
             wellPanel(textOutput("subtitulo"),
                       style = "background-color: #D3E7FF; color: #000000;")
      )
    ),
  
),
fluidRow(
  column(12, align = "left",
         HTML("<b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br>
               <br>
               Esta visualización muestra el porcentaje de alimentos enviados a Cundinamarca desde cada departamento, incluyendo productos de origen local. 
               Permite apreciar la importancia de Cundinamarca como receptor de alimentos provenientes de otros territorios del país.<br>
               Los departamentos en color gris indican la ausencia de reportes de ingresos de productos provenientes de esas áreas en las principales centrales de abasto.
               "),
         style = "font-size:12px; color:#4E4D4D; text-align:left; font-family: 'Prompt', sans-serif; margin-top:15px;"
  )
),

# Logo institucional
fluidRow(
  tags$div(
    tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),
    style = "width: 100%; margin: 0;"
  )
)

  )
)
