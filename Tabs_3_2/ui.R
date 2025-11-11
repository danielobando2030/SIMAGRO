################################################################################-
# Proyecto FAO - VP - 2025
# Visualización de datos - Comparación con Bogotá
################################################################################-
# Autores: Luis Miguel García, Laura Quintero, Juliana Lalinde
# Última modificación: 07/11/2025
################################################################################-

library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Cargar datos y funciones
source("3_2b_precios_diferencias_mapa_funciones.R")

productos_filtrados <- data_global %>%
  count(producto, name = "Freq") %>%
  filter(Freq > 11) %>%
  arrange(producto) %>%
  pull(producto)

################################################################################-
# Interfaz de usuario
################################################################################-
ui <- fluidPage(
  tags$head(
    tags$title("Comparación de precios con Bogotá"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Prompt:wght@400;600&display=swap"),
    tags$style(HTML("
      body { font-family: 'Prompt', sans-serif; background-color: #fafafa; }
      h1, h2, h3, h4 { color: #0D8D38; }
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
      .sub-header2 { font-size: 15px; color: #3D3D6B; }
      .selectize-dropdown { z-index: 10000 !important; }
    "))
  ),
  
  # Encabezado
  h1("Diferencia de precios de alimentos por departamento", class = "main-header"),
  h4("Comparación de precios promedio con respecto a Bogotá D.C.", style = "color:#0D8D38;"),
  
  br(),
  
  # Controles
  fluidRow(
    column(4,
           selectInput("anio", "Año:",
                       choices = c("Todos los años" = "todo", sort(unique(data_global$year))),
                       selected = 2024)
    ),
    column(4,
           selectInput("mes", "Mes:",
                       choices = c("Todos los meses" = "todo",
                                   "Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4,
                                   "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8,
                                   "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12),
                       selected = 1)
    ),
    column(4,
           selectInput("producto", "Producto:",
                       choices = c("Todos los productos" = "todo",
                                   setNames(productos_filtrados, productos_filtrados)),
                       selected = "Aguacate")
    )
  ),
  
  br(),
  
  # Mapa y panel lateral
  fluidRow(
    column(9,
           leafletOutput("grafico", height = "500px"),
           br(),
           downloadButton("descargar", "Gráfica", class = "btn btn-faoc"),
           downloadButton("descargarDatos", "Datos", class = "btn btn-faoc"),
           shiny::a("GitHub",
                    href = "https://github.com/Simonaa-Antioquia/Tableros/tree/main/Tabs_3_2",
                    target = "_blank",
                    class = "btn btn-faoc",
                    icon("github")),
           actionButton("reset", "Restablecer", icon = icon("refresh"), class = "btn btn-faoc"),
           downloadButton("descargarInforme", "Generar informe", class = "btn btn-faoc")
    ),
    column(3,
           wellPanel(textOutput("mensaje1"),
                     style = "background-color: #0D8D38; color: white; font-weight: bold;"),
           wellPanel(textOutput("mensaje2"),
                     style = "background-color: #13A756; color: white; font-weight: bold;")
    )
  ),
  
  br(),
  
  # ---------------------------------------------------------------------------------
  # Bloque institucional FAO estándar (idéntico al del módulo anterior)
  # ---------------------------------------------------------------------------------
  fluidRow(
    column(12, align = "left",
           HTML("Fuente: Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).<br>
               La información solo se muestra para los precios en el centro de acopio de Bogotá.<br>
               Para los productos fríjol verde, tomate, aguacate, banano, guayaba, mandarina, naranja, piña, arracacha, papa negra y yuca, los precios reportados corresponden a la variedad predominante en el mercado al momento de la recolección de la información.<br>
               De acuerdo con el SIPSA, el valor reportado corresponde al precio mayorista por kilogramo de producto de primera calidad en la Central Mayorista de Corabastos."),
           style = "font-size:12px; color:#5A5A5A; text-align:left;"
    )
  ),
  
  br(), br(),
  
  # Logo institucional FAO
  tags$div(
    tags$img(src = 'logo_2.png', 
             style = "width: 100%; display: block; margin: 0 auto;"),
    style = "position: relative; bottom: 0; width: 100%; background-color: white;"
  )
)
