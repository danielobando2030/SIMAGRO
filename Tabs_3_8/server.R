################################################################################
# Proyecto FAO - VP - 2025
# SERVER - Matriz de correlación de precios entre productos
################################################################################

pacman::p_load(
  shiny, plotly, dplyr, lubridate, tidyr, stringr,
  htmlwidgets, webshot2, rmarkdown
)
options(scipen = 999)

# --- Cargar función de correlaciones ---
source("3_8b_correlaciones_precios.R")

# --- Cargar datos ---
data <- readRDS("precios_bogota_balanceado_3_8.rds")
data <- data %>%
  mutate(
    mes_y_ano = as.Date(as.yearmon(mes_y_ano)),
    anio = year(mes_y_ano),
    mes  = month(mes_y_ano)
  )

################################################################################
# DEFINICIÓN DEL SERVER
################################################################################
server <- function(input, output, session) {
  
  # --- Inicializar selector ---
  observe({
    anios <- sort(unique(data$anio))
    updateSelectInput(session, "anio", choices = anios, selected = max(anios))
  })
  
  # --- Cálculo de correlaciones ---
  resultado_correlacion <- reactive({
    anio_sel <- if (!is.null(input$anio)) as.numeric(input$anio) else max(data$anio, na.rm = TRUE)
    res <- correlacion_precios(data, anio_sel)
    return(res)
  })
  
  # --- Gráfico principal ---
  output$grafico <- renderPlotly({
    res <- resultado_correlacion()
    
    if (is.null(res) || is.null(res$grafico)) {
      return(plotly_empty(type = "heatmap"))
    }
    
    res$grafico
  })
  
  # --- Panel 1: Correlaciones más altas ---
  output$topPositivas <- renderUI({
    res <- resultado_correlacion()
    m <- res$matriz
    if (is.null(m)) return(NULL)
    
    cor_df <- as.data.frame(as.table(m)) %>%
      filter(Var1 != Var2) %>%
      mutate(
        Var1 = as.character(Var1),
        Var2 = as.character(Var2),
        pair = paste(pmin(Var1, Var2), pmax(Var1, Var2), sep = "_")
      ) %>%
      distinct(pair, .keep_all = TRUE) %>%
      arrange(desc(Freq)) %>%
      slice_head(n = 5)
    
    HTML(paste0(
      "<b> Correlaciones más altas</b><br>",
      paste0("<br>", cor_df$Var1, " – ", cor_df$Var2, 
             ": <b>", sprintf('%.2f', cor_df$Freq), "</b>", collapse = "")
    ))
  })
  
  # --- Panel 2: Correlaciones más bajas ---
  output$topNegativas <- renderUI({
    res <- resultado_correlacion()
    m <- res$matriz
    if (is.null(m)) return(NULL)
    
    cor_df <- as.data.frame(as.table(m)) %>%
      filter(Var1 != Var2) %>%
      mutate(
        Var1 = as.character(Var1),
        Var2 = as.character(Var2),
        pair = paste(pmin(Var1, Var2), pmax(Var1, Var2), sep = "_")
      ) %>%
      distinct(pair, .keep_all = TRUE) %>%
      arrange(Freq) %>%
      slice_head(n = 5)
    
    HTML(paste0(
      "<b> Correlaciones más bajas</b><br>",
      paste0("<br>", cor_df$Var1, " – ", cor_df$Var2,
             ": <b>", sprintf('%.2f', cor_df$Freq), "</b>", collapse = "")
    ))
  })
  
  # --- Botón reset ---
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = max(data$anio))
  })
  
  # --- Descarga de PDF (FAO 2025 versión estable) ---
  output$descargarPDF <- downloadHandler(
    filename = function() {
      anio_sel <- if (!is.null(input$anio)) input$anio else max(data$anio)
      paste0("informe_correlacion_", anio_sel, ".pdf")
    },
    content = function(file) {
      
      res <- resultado_correlacion()
      g <- res$grafico
      m <- res$matriz
      
      # asegurar que m sea matriz numérica
      if (!is.null(m) && !is.matrix(m)) m <- as.matrix(m)
      mode(m) <- "numeric"
      
      # ---- Extraer top 5 correlaciones (ya limpiadas) ----
      cor_tab <- as.data.frame(as.table(m)) %>%
        filter(Var1 != Var2) %>%
        mutate(
          Var1 = as.character(Var1),
          Var2 = as.character(Var2),
          pair = paste(pmin(Var1, Var2), pmax(Var1, Var2), sep = "_")
        ) %>%
        distinct(pair, .keep_all = TRUE)
      
      top5_pos <- cor_tab %>% arrange(desc(Freq)) %>% slice_head(n = 5)
      top5_neg <- cor_tab %>% arrange(Freq) %>% slice_head(n = 5)
      
      # ---- Crear mensajes para el PDF ----
      mensaje_altas <- paste(
        apply(top5_pos, 1, function(x) {
          glue("{x[['Var1']]} – {x[['Var2']]}: {sprintf('%.2f', as.numeric(x[['Freq']]))}")
        }),
        collapse = "\n"
      )
      
      mensaje_bajas <- paste(
        apply(top5_neg, 1, function(x) {
          glue("{x[['Var1']]} – {x[['Var2']]}: {sprintf('%.2f', as.numeric(x[['Freq']]))}")
        }),
        collapse = "\n"
      )
      
      # ---- Carpeta temporal ----
      tmp_dir <- tempfile("informe_corr_")
      dir.create(tmp_dir)
      
      tmp_html <- file.path(tmp_dir, "grafico_tmp.html")
      tmp_png  <- file.path(tmp_dir, "grafico_tmp.png")
      tmp_rmd  <- file.path(getwd(), "informe.Rmd")
      tmp_pdf  <- file.path(tmp_dir, "informe_correlacion.pdf")
      
      # ---- Guardar gráfico Plotly como PNG ----
      htmlwidgets::saveWidget(as_widget(g), tmp_html, selfcontained = TRUE)
      Sys.sleep(1.2)
      webshot2::webshot(tmp_html, tmp_png, vwidth = 1600, vheight = 1000)
      
      # ---- Render PDF ----
      rmarkdown::render(
        input = tmp_rmd,
        output_format = rmarkdown::pdf_document(latex_engine = "xelatex", toc = FALSE),
        output_file = tmp_pdf,
        params = list(
          datos          = m,
          grafico        = tmp_png,
          anio           = if (!is.null(input$anio)) input$anio else max(data$anio),
          mensaje_altas  = mensaje_altas,
          mensaje_bajas  = mensaje_bajas
        ),
        envir = new.env(parent = globalenv()),
        clean = TRUE
      )
      
      # ---- Copiar PDF final ----
      if (file.exists(tmp_pdf)) {
        file.copy(tmp_pdf, file, overwrite = TRUE)
      } else {
        stop("⚠️ No se generó el PDF final. Revisa TinyTeX.")
      }
    }
  )
}
