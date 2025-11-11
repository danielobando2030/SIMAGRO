################################################################################
# Proyecto FAO - VP - 2025
# Interfaz UI - Módulo 3_1: Precios Mayoristas (Bogotá)
################################################################################
# Autores: Luis Miguel García, Laura Quintero, Daniel Obando
# Última edición: 2025/11/07
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(stringr)

ui <- fluidPage(
  
  # ------------------------------------------------------------------
  # Encabezado
  # ------------------------------------------------------------------
  tags$head(
    tags$title("Comportamiento de precios mayoristas - Bogotá"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h2, h4, h5 { color: #3D3D6B; }
      .btn-faoc {
        background-color: #6A0DAD;
        border-color: #6A0DAD;
        color: white;
        font-weight: 500;
      }
      .btn-faoc:hover {
        background-color: #500985;
        border-color: #500985;
        color: white;
      }
    "))
  ),
  
  div(
    h2("Comportamiento de los precios en el tiempo",
       style = "font-weight:600; color:#3D3D6B; text-align:center;"),
    h4("Análisis histórico de precios de alimentos en las centrales de abasto de Bogotá.",
       style = "color:#5A5A5A; font-weight:400; text-align:center; margin-top:-10px; margin-bottom:5px;")
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Filtros
  # ------------------------------------------------------------------
  fluidRow(
    column(3,
           selectInput("temporalidad", "Seleccione temporalidad:",
                       choices = c("Mensual" = "mensual", "Diaria" = "diaria"),
                       selected = "mensual")
    ),
    column(3, uiOutput("productoUI")),
    column(3, uiOutput("anioUI")),
    column(3,
           selectInput("variable", "Variable a graficar:",
                       choices = c("Precio promedio" = "precio_prom",
                                   "Cambio porcentual mensual" = "cambio_pct",
                                   "Cambio porcentual anual" = "cambio_pct_anual"),
                       selected = "precio_prom")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Subtítulo dinámico
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           h5(textOutput("subtitulo"),
              style = "color:#3D3D6B; margin-bottom:10px;")
    )
  ),
  
  # ------------------------------------------------------------------
  # Gráfico + Panel lateral
  # ------------------------------------------------------------------
  fluidRow(
    column(9, align = "center",
           plotlyOutput("grafico", height = "450px")
    ),
    column(3,
           uiOutput("texto_volatil"),
           uiOutput("texto_promedio_cambio"),
           uiOutput("texto_mes_max_anual"),
           style = "padding:0px; margin-top:20px;"
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Botones de acción (FAO estilo institucional)
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           downloadButton("descargar", "Gráfica", class = "btn btn-faoc"),
           downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
           shiny::a("GitHub",
                    href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_1",
                    target = "_blank",
                    class = "btn btn-faoc",
                    icon("github")),
           actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn btn-faoc"),
           downloadButton("descargarInforme", "Generar informe", class = "btn btn-faoc")
    )
  ),
  
  br(),
  
  # ------------------------------------------------------------------
  # Fuente de datos
  # ------------------------------------------------------------------
  fluidRow(
    column(12, align = "left",
           HTML("Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
               La información solo se muestra para los precios en el centro de acopio de Bogotá.<br>
               Para los productos fríjol verde, tomate, aguacate, banano, guayaba, mandarina, naranja, piña, arracacha, papa negra y yuca, los precios reportados corresponden a la variedad predominante en el mercado al momento de la recolección de la información.<br>
               De acuerdo con el SIPSA, el valor reportado corresponde al precio mayorista por kilogramo de producto de primera calidad en la Central Mayorista de Corabastos."),
           style = "font-size:12px; color:#5A5A5A; text-align:left;"
    )
  ),
  
  # ------------------------------------------------------------------
  # Logo institucional al final
  # ------------------------------------------------------------------
  fluidRow(
    tags$div(
      tags$img(src = "logo_2.png", style = "width: 100%; margin: 0;"),
      style = "width: 100%; margin:0;"
    )
  )
)
