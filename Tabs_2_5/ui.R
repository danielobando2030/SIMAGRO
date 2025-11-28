# Proyecto FAO
# Visualizacion de DATOS   - abastecimeinto en bogota 
################################################################################-
#Autores: Cristian Daniel Obando, Luis Miguel Garcia,Juan Carlos Muñoz, MoraJuliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 03/04/2024
#Fecha de ultima modificacion: 28/11/2025
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse); library(shiny); library(lubridate);library(shinythemes);
options(scipen = 999)
################################################################################
rm(list = ls())

source("2_5b_funciones_participacion_destino.R")
productos <- unique(IHH_anual_producto$producto)

ui <- fluidPage(
  #theme = shinythemes::shinytheme("default"),
  tags$head(
    tags$title("Índice concentración del origen de los alimentos - Índice"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Prompt&display=swap"),
    tags$style(HTML("
      .main-header {
        font-family: 'Prompt', sans-serif;
        font-size: 40px;
        color: #743639;
      }
      .sub-header {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
      }
      .main-header_2 {
        font-family: 'Prompt', sans-serif;
        font-size: 20px;
        color: #743639;
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
      }
      .scrollable-content {
        overflow-y: auto;
        overflow-x: hidden;
        height: auto;
      }
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  tags$h1("Índice de diversidad de destino de los alimentos", class = "main-header"),
  tags$h1("Análisis de la diversidad de municipios destino del volumen alimentos que salen de Cundinamarca", class = "main-header_2"),
  div(
    textOutput("subtitulo"),
    class = "sub-header2",
    style = "margin-bottom: 20px;"
  ),
    div(
        fluidRow(
          column(4,
                 selectInput("tipo", "Seleccione el tipo:", 
                             choices = list("Anual" = 1, 
                                            "Anual por producto" = 2, 
                                            "Mensual" = 3, 
                                            "Mensual por producto" = 4))
        ),
        column(4,
               conditionalPanel(
                 condition = "input.tipo == 2 || input.tipo == 4",
                 selectInput("producto", "Seleccione los productos:", 
                             choices = c("Todos los productos" = NULL , unique(IHH_anual_producto$producto)), multiple = TRUE)
               )
        ),
        column(4,
               conditionalPanel(
                 condition = "input.tipo == 3 || input.tipo == 4",
                 selectInput("anio", "Seleccione el año:", 
                             choices = c("Todos los años" = "todo", unique(IHH_mensual_producto$year)))
               )
        )
      )
  ),
 
    fluidRow(
        column(9,
               plotlyOutput("grafico"),
               downloadButton("descargar_", "Gráfica", icon = icon("download")),
               downloadButton("descargarDatos", "Datos"),
               shiny::a("GitHub", href="https://github.com/danielobando2030/SIMAGRO/tree/main/Tabs_2_5", target="_blank",
                        class = "btn btn-default shiny-action-button", icon("github")),
               actionButton("reset", "Restablecer", icon = icon("refresh")),
               downloadButton("report", "Generar informe")
        ),
        column(3, 
               div(
                 wellPanel(textOutput("mensaje1"),
                           style = "background-color: #BC222A; color: #FFFFFF;"),
                 wellPanel(uiOutput("mensaje2"),
                           style = "background-color: #983136; color: #FFFFFF;")
               ))
    ),
  fluidRow(
    column(
      12,
      align = "left",
      HTML('
      <b>Fuente:</b> Elaboración propia con base en datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario – SIPSA (DANE).<br><br>

      Este gráfico se calcula con base en el índice de Herfindahl-Hirschman.<br><br>

      <b>La fórmula del índice de Herfindahl-Hirschman es:</b><br><br>

      $$IHH = \\sum_{i=1}^{n} s_i^2$$

      <br>

      Donde S<sub>i</sub> es la participación que tiene cada municipio (destino) en el total del volumen de alimentos que salen de cundinamarca.

      <script>
        MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);
      </script>
            <br><br>

      <table style="border-collapse: collapse; width: 100%; max-width: 800px;">
        <thead>
<tr style="background: #f2f2f2;">
  <th style="text-align: left; padding: 8px; border: 1px solid #ddd;"><strong>Rango HHI</strong></th>
  <th style="text-align: left; padding: 8px; border: 1px solid #ddd;"><strong>Interpretación</strong></th>
  <th style="text-align: left; padding: 8px; border: 1px solid #ddd;"><strong>Significado en términos de diversidad de destino</strong></th>
</tr>
</thead>
<tbody>
<tr>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>0 – 1.500</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>Baja concentración</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;">
    <strong>Alta diversidad:</strong> el volumen de alimentos se distribuye de manera equilibrada entre muchos municipios que reciben productos desde Cundinamarca.
  </td>
</tr>
<tr>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>1.500 – 2.500</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>Concentración moderada</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;">
    <strong>Diversidad media:</strong> un grupo de municipios empieza a concentrar una proporción relevante del volumen de alimentos enviado desde Cundinamarca.
  </td>
</tr>
<tr>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>&gt; 2.500</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;"><strong>Alta concentración</strong></td>
  <td style="padding: 8px; border: 1px solid #ddd;">
    <strong>Baja diversidad:</strong> pocos municipios concentran la mayor parte del volumen de alimentos recibido desde Cundinamarca.
  </td>
</tr>
</tbody>
</table>
      '),
      style = "font-size:12px; color:#4E4D4D;
             text-align:left; font-family:'Prompt', sans-serif;
             margin-top:20px;"
    )
  ),
    
  fluidRow(
      tags$div(
        tags$img(src = 'logo_2.png', style = "width: 100%; margin: 0;"),  
        style = "width: 100%; margin:0;"  
      )
    ) 
    
  )

