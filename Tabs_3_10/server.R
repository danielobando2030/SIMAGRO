################################################################################
# Proyecto FAO - VP - 2025
# Servidor - Módulo 3_10: Precios de Insumos Agrícolas
################################################################################
# Autores: Luis Miguel García, Laura Quintero, Daniel Obando
# Última edición: 2025/11/11
################################################################################

library(shiny)
library(plotly)
library(dplyr)
library(glue)
library(rmarkdown)
library(webshot2)
library(htmlwidgets)

# -------------------------------------------------------------------------------
# Cargar función principal
# -------------------------------------------------------------------------------
source("3_10b_boxplot_insumos.R")
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# -------------------------------------------------------------------------------
# Cargar base de datos
# -------------------------------------------------------------------------------
base_precios <- readRDS("data_precios_insumos_3_10.rds")

# -------------------------------------------------------------------------------
# Servidor principal
# -------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- 1. Actualizar selectInput dinámico de presentación según subgrupo ---
  observeEvent(input$subgrupo_sel, {
    presentaciones_disp <- base_precios %>%
      filter(subgrupos == input$subgrupo_sel) %>%
      distinct(presentacion) %>%
      arrange(presentacion) %>%
      pull(presentacion)
    
    updateSelectInput(session, "presentacion_sel",
                      choices = presentaciones_disp,
                      selected = presentaciones_disp[1])
  })
  
  # --- 2. Generar gráfico reactivo ---
  grafico_reactivo <- reactive({
    req(input$subgrupo_sel, input$presentacion_sel)
    
    boxplot_interactivo(
      data = base_precios,
      presentacion_sel = input$presentacion_sel,
      subgrupo_sel = input$subgrupo_sel
    )
  })
  
  # --- 3. Renderizar gráfico en pantalla ---
  output$grafico_boxplot <- renderPlotly({
    grafico_reactivo()
  })
  
  # --- 4. Descargar gráfico (PNG) ---
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste0("grafico_precios_insumos_", input$subgrupo_sel, "_", input$presentacion_sel, ".png")
    },
    content = function(file) {
      g <- grafico_reactivo()
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(as_widget(g), temp_html, selfcontained = TRUE)
      webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
    }
  )
  
  # --- 5. Descargar datos filtrados (CSV) ---
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste0("datos_precios_insumos_", input$subgrupo_sel, "_", input$presentacion_sel, ".csv")
    },
    content = function(file) {
      df <- base_precios %>%
        filter(subgrupos == input$subgrupo_sel,
               presentacion == input$presentacion_sel)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # --- 6. Descargar informe PDF institucional ---
  output$descargarPDF <- downloadHandler(
    filename = function() {
      paste0("informe_precios_insumos_",
             gsub("\\s+", "_", input$subgrupo_sel), "_",
             gsub("\\s+", "_", input$presentacion_sel), ".pdf")
    },
    content = function(file) {
      req(input$subgrupo_sel, input$presentacion_sel)
      
      # 1. Gráfico temporal
      g <- grafico_reactivo()
      tmp_dir  <- tempdir()
      tmp_html <- file.path(tmp_dir, "grafico_tmp.html")
      tmp_png  <- file.path(tmp_dir, "grafico_tmp.png")
      tmp_rmd  <- file.path(tmp_dir, "informe.Rmd")
      
      # 2. Copiar Rmd base al temp
      if (!file.copy("informe.Rmd", tmp_rmd, overwrite = TRUE)) {
        stop("No se encontró 'informe.Rmd' en el directorio de la app.")
      }
      
      # 3. Guardar gráfico como HTML y luego a PNG
      htmlwidgets::saveWidget(as_widget(g), tmp_html, selfcontained = TRUE)
      webshot2::webshot(tmp_html, tmp_png, vwidth = 1200, vheight = 800, zoom = 2)
      
      # 4. Logos temporales
      logo_sup <- file.path(tmp_dir, "logo_3.png")
      logo_inf <- file.path(tmp_dir, "logo_2.png")
      if (!file.copy("www/logo_3.png", logo_sup, overwrite = TRUE)) {
        stop("No se encontró 'www/logo_3.png'.")
      }
      if (!file.copy("www/logo_2.png", logo_inf, overwrite = TRUE)) {
        stop("No se encontró 'www/logo_2.png'.")
      }
      
      # 5. Datos filtrados
      df <- base_precios %>%
        filter(
          subgrupos == input$subgrupo_sel,
          presentacion == input$presentacion_sel
        )
      
      # 6. Renderizar el PDF dentro de tempdir
      owd <- setwd(tmp_dir)
      on.exit(setwd(owd), add = TRUE)
      
      out_file <- rmarkdown::render(
        input = tmp_rmd,
        output_file = "informe_final.pdf",
        output_format = "pdf_document",
        params = list(
          datos        = df,
          grafico      = tmp_png,
          subgrupo     = input$subgrupo_sel,
          presentacion = input$presentacion_sel,
          logo_sup     = logo_sup,
          logo_inf     = logo_inf
        ),
        envir = new.env(parent = globalenv())
      )
      
      # 7. Enviar el PDF al usuario
      file.copy(out_file, file, overwrite = TRUE)
    }
  )
  
  # --- 7. Botones de restablecer ---
  observeEvent(input$reset, {
    updateSelectInput(session, "subgrupo_sel", selected = "Fungicidas")
  })
  
  observeEvent(input$reset2, {
    updateSelectInput(session, "subgrupo_sel", selected = "Fungicidas")
  })
}

################################################################################
# Fin del servidor
################################################################################
