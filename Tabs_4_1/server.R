################################################################################-
# Proyecto FAO - VP - 2025
# Servidor - Mapa de Rutas (versión corregida y final)
################################################################################-
# Autores: Luis Miguel García, Juliana Lalinde, Laura Quintero, Germán Angulo
# Fecha: 13/11/2025
################################################################################-

rm(list = ls())

library(readr); library(lubridate); library(dplyr); library(ggplot2); library(zoo); library(readxl)
library(glue); library(tidyverse); library(gridExtra); library(corrplot); library(shiny); library(shinydashboard)
library(htmlwidgets); library(webshot); library(magick); library(shinyscreenshot); library(webshot2)
library(knitr); library(rmarkdown); library(leaflet); library(scales); library(stringr); library(purrr)

options(scipen = 999)

# -----------------------------------------------------------
# Cargar función y datos 
# -----------------------------------------------------------
source("4_1b_rutas_abastecimiento.R")

# -----------------------------------------------------------
# Conversión de meses nombre → número
# -----------------------------------------------------------
mes_nombre_a_numero <- c(
  "Enero"="01","Febrero"="02","Marzo"="03","Abril"="04",
  "Mayo"="05","Junio"="06","Julio"="07","Agosto"="08",
  "Septiembre"="09","Octubre"="10","Noviembre"="11","Diciembre"="12"
)

################################################################################-
# SERVIDOR PRINCIPAL
################################################################################-
server <- function(input, output, session) {
  
  # -----------------------------------------------------------
  # Inicializar selectInput
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
  # Objeto maestro reactivo
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
    
    # Top municipio
    top_row <- df %>%
      group_by(mpio_origen) %>%
      summarise(importancia_total = sum(importancia_ruta, na.rm = TRUE), .groups="drop") %>%
      arrange(desc(importancia_total)) %>%
      slice_head(n = 1)
    
    list(
      datos = df,
      grafico_leaf = grafico_leaf,
      top = top_row
    )
  })
  
  # -----------------------------------------------------------
  # Render mapa
  # -----------------------------------------------------------
  output$grafico <- leaflet::renderLeaflet({
    res <- resultado()
    if (is.character(res)) return(NULL)
    res$grafico_leaf
  })
  
  # -----------------------------------------------------------
  # Descargar gráfica
  # -----------------------------------------------------------
  grafico_plano <- reactive({
    res <- resultado()
    if (is.character(res)) return(NULL)
    
    tmp_html <- tempfile(fileext=".html")
    tmp_png  <- tempfile(fileext=".png")
    
    saveWidget(res$grafico_leaf, tmp_html, selfcontained=TRUE)
    webshot2::webshot(tmp_html, tmp_png, vwidth=1600, vheight=1000)
    
    tmp_png
  })
  
  output$descargar <- downloadHandler(
    filename = function(){
      paste0("grafica_rutas_", input$anio, "_", mes_nombre_a_numero[input$mes], "_", input$producto, ".png")
    },
    content = function(file){
      file.copy(grafico_plano(), file)
    }
  )
  
  # -----------------------------------------------------------
  # Descargar datos CSV
  # -----------------------------------------------------------
  output$descargarDatos <- downloadHandler(
    filename = function(){
      paste0("datos_rutas_", input$anio, "_", mes_nombre_a_numero[input$mes], "_", input$producto, ".csv")
    },
    content = function(file){
      res <- resultado()
      write.csv(res$datos, file, row.names = FALSE, fileEncoding="UTF-8")
    }
  )
  
  # -----------------------------------------------------------
  # Reset
  # -----------------------------------------------------------
  observeEvent(input$reset2, {
    updateSelectInput(session, "anio", selected="2024")
    updateSelectInput(session, "mes", selected="Diciembre")
    updateSelectInput(session, "producto", selected="Aguacate Hass")
  })
  
  # -----------------------------------------------------------
  # Informe PDF
  # -----------------------------------------------------------
  escape_latex <- function(x) {
    x <- as.character(x)
    x <- stringr::str_replace_all(x, "%", "\\\\%")
    x <- stringr::str_replace_all(x, "_", "\\\\_")
    x <- stringr::str_replace_all(x, "\\$", "\\\\$")
    x <- stringr::str_replace_all(x, "#", "\\\\#")
    x <- stringr::str_replace_all(x, "\\{", "\\\\{")
    x <- stringr::str_replace_all(x, "\\}", "\\\\}")
    x
  }
  
  output$descargarPDF <- downloadHandler(
    filename = function(){
      paste0("informe_rutas_", input$anio, "_", mes_nombre_a_numero[input$mes], "_", input$producto, ".pdf")
    },
    content = function(file){
      
      res <- resultado()
      df <- res$datos
      
      # MENSAJE 1 — mayor
      mensaje1 <- {
        top <- df %>% group_by(mpio_origen) %>%
          summarise(valor=sum(importancia_ruta, na.rm=TRUE), .groups="drop") %>%
          arrange(desc(valor)) %>% slice_head(n=1)
        
        if(nrow(top)==0) "—"
        else paste0(
          str_to_title(str_to_lower(top$mpio_origen)),
          " (", sprintf('%.1f%%', top$valor*100), ")"
        )
      }
      
      # MENSAJE 2 — menor
      mensaje2 <- {
        bottom <- df %>% 
          filter(importancia_ruta > 0) %>%
          group_by(mpio_origen) %>%
          summarise(valor=sum(importancia_ruta, na.rm=TRUE), .groups="drop") %>%
          arrange(valor) %>% slice_head(n=1)
        
        if(nrow(bottom)==0) "—"
        else paste0(
          str_to_title(str_to_lower(bottom$mpio_origen)),
          " (", sprintf('%.1f%%', bottom$valor*100), ")"
        )
      }
      
      # MENSAJE 3 — municipios
      mensaje3 <- {
        n_mpios <- df %>% 
          filter(importancia_ruta > 0) %>%
          summarise(n=n_distinct(mpio_origen)) %>% pull(n)
        
        if(n_mpios==0) "No se registran municipios aportando este alimento."
        else paste0("Este alimento es abastecido por ", n_mpios, " municipios diferentes.")
      }
      
      rmarkdown::render(
        "informe.Rmd",
        output_file = file,
        params = list(
          datos       = df,
          grafico_png = grafico_plano(),
          anio        = input$anio,
          mes         = mes_nombre_a_numero[input$mes],
          producto    = input$producto,
          mensaje1    = escape_latex(mensaje1),
          mensaje2    = escape_latex(mensaje2),
          mensaje3    = escape_latex(mensaje3)
        ),
        envir = new.env(parent=globalenv())
      )
    }
  )
  # -----------------------------------------------------------
  # Estadísticas laterales (con formato corregido)
  # -----------------------------------------------------------
  output$region_mas_importante <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos
    
    top <- df %>%
      group_by(mpio_origen) %>%
      summarise(valor=sum(importancia_ruta, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(valor)) %>%
      slice_head(n=1)
    
    if (nrow(top)==0) return("—")
    
    nombre <- str_to_title(str_to_lower(top$mpio_origen))
    
    paste0(nombre, " (", sprintf("%.1f%%", top$valor*100), ")")
  })
  
  output$region_menos_importante <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos %>% filter(importancia_ruta > 0)
    
    bottom <- df %>%
      group_by(mpio_origen) %>%
      summarise(valor=sum(importancia_ruta, na.rm=TRUE), .groups="drop") %>%
      arrange(valor) %>%
      slice_head(n=1)
    
    if (nrow(bottom)==0) return("—")
    
    nombre <- str_to_title(str_to_lower(bottom$mpio_origen))
    
    paste0(nombre, " (", sprintf("%.1f%%", bottom$valor*100), ")")
  })
  
  output$mensaje_interpretativo <- renderText({
    res <- resultado()
    if (is.character(res)) return("—")
    
    df <- res$datos
    
    # Número de municipios distintos que aportan (con importancia > 0)
    n_mpios <- df %>%
      filter(importancia_ruta > 0) %>%
      summarise(n = n_distinct(mpio_origen)) %>%
      pull(n)
    
    if (n_mpios == 0) {
      return("No se registran municipios aportando este alimento.")
    }
    
    paste0("Este alimento es abastecido por ", n_mpios, 
           " municipios diferentes.")
  })
}

################################################################################-
# FIN DEL SERVIDOR
################################################################################-
