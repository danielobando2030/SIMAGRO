################################################################################-
# Proyecto FAO - VP - 2025
# Servidor - Mapa de Rutas (versión corregida y FINAL)
################################################################################-
# Autores: Juliana Lalinde, Laura Quintero, Germán Angulo,
#          Luis Miguel García, Daniel Obando
# Fecha: 13/11/2025
################################################################################-

rm(list = ls())

library(readr); library(lubridate); library(dplyr); library(ggplot2); library(zoo); library(readxl)
library(glue); library(tidyverse); library(gridExtra); library(corrplot)
library(shiny); library(shinydashboard)
library(htmlwidgets); library(webshot2); library(magick)
library(knitr); library(rmarkdown)
library(leaflet); library(scales); library(stringr); library(purrr)

options(scipen = 999)

# -----------------------------------------------------------
# Formato porcentaje español
# -----------------------------------------------------------
formato_pct_es <- function(x, digits = 1) {
  x <- x * 100
  if (abs(x - round(x)) < 1e-8) {
    paste0(formatC(round(x), format = "f", digits = 0), "%")
  } else {
    paste0(formatC(x, format = "f", digits = digits, decimal.mark = ","), "%")
  }
}

# -----------------------------------------------------------
# Cargar funciones y datos
# -----------------------------------------------------------
source("4_1b_rutas_abastecimiento.R")   # graficar_rutas()

mes_nombre_a_numero <- c(
  "Enero"="01","Febrero"="02","Marzo"="03","Abril"="04",
  "Mayo"="05","Junio"="06","Julio"="07","Agosto"="08",
  "Septiembre"="09","Octubre"="10","Noviembre"="11","Diciembre"="12"
)

################################################################################-
# SERVER
################################################################################-
server <- function(input, output, session) {
  
  # -----------------------------------------------------------
  # Inicializar filtros
  # -----------------------------------------------------------
  observe({
    updateSelectInput(session, "anio",
                      choices = sort(unique(data_merged$anio)),
                      selected = "2024")
    
    updateSelectInput(session, "mes",
                      choices = names(mes_nombre_a_numero),
                      selected = "Diciembre")
    
    updateSelectInput(session, "producto",
                      choices = sort(unique(data_merged$producto)),
                      selected = "Aguacate Hass")
  })
  
  # -----------------------------------------------------------
  # Objeto maestro
  # -----------------------------------------------------------
  resultado <- reactive({
    req(input$anio, input$mes, input$producto)
    
    mes_num <- mes_nombre_a_numero[input$mes]
    
    df <- data_merged %>%
      filter(
        anio == as.character(input$anio),
        mes == mes_num,
        producto == input$producto
      )
    
    if (nrow(df) == 0) return("No hay información disponible")
    
    grafico_leaf <- graficar_rutas(
      df,
      Año = input$anio,
      Mes = mes_num,
      Producto = input$producto
    )
    
    list(
      datos = df,
      grafico_leaf = grafico_leaf
    )
  })
  
  # -----------------------------------------------------------
  # 👉 NUEVO: gráfico estático GGplot (OBJETO)
  # -----------------------------------------------------------
  plot_estatico <- reactive({
    res <- resultado()
    if (is.character(res)) return(NULL)
    
    graficar_rutas_estatico_ggplot(
      res$datos,
      Año = input$anio,
      Mes = mes_nombre_a_numero[input$mes],
      Producto = input$producto
    )
  })
  
  # -----------------------------------------------------------
  # Panel derecho: mayor importancia
  # -----------------------------------------------------------
  output$region_mas_importante <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos
    
    top <- df %>%
      group_by(mpio_origen) %>%
      summarise(valor = sum(importancia_ruta, na.rm = TRUE), .groups="drop") %>%
      arrange(desc(valor)) %>%
      slice_head(n = 1)
    
    if (nrow(top) == 0) return("—")
    
    paste0(
      str_to_title(str_to_lower(top$mpio_origen)),
      " (", formato_pct_es(top$valor), ")"
    )
  })
  
  # -----------------------------------------------------------
  # Panel derecho: menor importancia
  # -----------------------------------------------------------
  output$region_menos_importante <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos %>% filter(importancia_ruta > 0)
    
    bottom <- df %>%
      group_by(mpio_origen) %>%
      summarise(valor = sum(importancia_ruta, na.rm = TRUE), .groups="drop") %>%
      arrange(valor) %>%
      slice_head(n = 1)
    
    if (nrow(bottom) == 0) return("—")
    
    paste0(
      str_to_title(str_to_lower(bottom$mpio_origen)),
      " (", formato_pct_es(bottom$valor), ")"
    )
  })
  
  # -----------------------------------------------------------
  # Panel derecho: mensaje interpretativo
  # -----------------------------------------------------------
  output$mensaje_interpretativo <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos
    
    n_mpios <- df %>%
      filter(importancia_ruta > 0) %>%
      summarise(n = n_distinct(mpio_origen)) %>%
      pull(n)
    
    if (n_mpios == 0) {
      "No se registran municipios aportando este alimento."
    } else {
      paste0(
        "Este alimento es abastecido por ",
        n_mpios,
        " municipios diferentes."
      )
    }
  })
  
  # -----------------------------------------------------------
  # Mapa interactivo
  # -----------------------------------------------------------
  output$grafico <- leaflet::renderLeaflet({
    res <- resultado()
    if (is.character(res)) return(NULL)
    res$grafico_leaf
  })
  
  # -----------------------------------------------------------
  # PDF FAO
  # -----------------------------------------------------------
  escape_latex <- function(x) {
    x %>%
      str_replace_all("%", "\\\\%") %>%
      str_replace_all("_", "\\\\_") %>%
      str_replace_all("\\$", "\\\\$")
  }
  
  output$descargarPDF <- downloadHandler(
    filename = function(){
      paste0("informe_rutas_", input$anio, "_",
             mes_nombre_a_numero[input$mes], "_",
             input$producto, ".pdf")
    },
    content = function(file){
      
      res <- resultado()
      df  <- res$datos
      
      mensaje1 <- df %>%
        group_by(mpio_origen) %>%
        summarise(v = sum(importancia_ruta, na.rm = TRUE), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n=1) %>%
        mutate(txt = paste0(
          str_to_title(str_to_lower(mpio_origen)),
          " (", formato_pct_es(v), ")"
        )) %>% pull(txt)
      
      mensaje2 <- df %>%
        filter(importancia_ruta > 0) %>%
        group_by(mpio_origen) %>%
        summarise(v = sum(importancia_ruta, na.rm = TRUE), .groups="drop") %>%
        arrange(v) %>%
        slice_head(n=1) %>%
        mutate(txt = paste0(
          str_to_title(str_to_lower(mpio_origen)),
          " (", formato_pct_es(v), ")"
        )) %>% pull(txt)
      
      mensaje3 <- paste0(
        "Este alimento es abastecido por ",
        n_distinct(df$mpio_origen[df$importancia_ruta > 0]),
        " municipios diferentes."
      )
      
      rmarkdown::render(
        "informe.Rmd",
        output_file = file,
        params = list(
          datos    = df,
          plot     = plot_estatico(),   # ✅ OBJETO GGPLOT
          anio     = input$anio,
          mes      = mes_nombre_a_numero[input$mes],
          producto = input$producto,
          mensaje1 = escape_latex(mensaje1),
          mensaje2 = escape_latex(mensaje2),
          mensaje3 = escape_latex(mensaje3)
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

################################################################################-
# FIN
################################################################################-
