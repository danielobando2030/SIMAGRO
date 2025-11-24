################################################################################
# Proyecto FAO - VP - 2025
# Servidor - Módulo 3_10: Precios de Insumos Agrícolas
################################################################################
# Autores: Cristian Daniel Obando, Luis Miguel García, Laura Quintero
# Última edición: 2025/11/11
################################################################################

# Paquetes
library(shiny)
library(plotly)
library(dplyr)
library(glue)
library(rmarkdown)
library(webshot2)
library(htmlwidgets)

# -------------------------------------------------------------------------------
# Cargar función principal (boxplot_interactivo)
# -------------------------------------------------------------------------------
# Asegúrate de que el archivo exista y exporte la función boxplot_interactivo()
if (!file.exists("3_10b_boxplot_insumos.R")) {
  stop("No se encontró '3_10b_boxplot_insumos.R' en el directorio de la app. Ajusta la ruta.")
}
source("3_10b_boxplot_insumos.R")

# Ajuste de locale para fechas en español (opcional)
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# -------------------------------------------------------------------------------
# Cargar base de datos
# -------------------------------------------------------------------------------
if (!file.exists("data_precios_insumos_3_10.rds")) {
  stop("No se encontró 'data_precios_insumos_3_10.rds'. Coloca el archivo en el directorio de la app.")
}

# -------------------------------------------------------------------------------
# Servidor principal
# -------------------------------------------------------------------------------
server <- function(input, output, session) {

output$ui1=renderUI({
  selectInput(
    "presentacion_sel",
    "Presentación:",
    choices =sort(unique(base_precios$presentacion[base_precios$subgrupos==input$subgrupo_sel]))
  )
  })
resultado <- reactive({
    req(input$subgrupo_sel, input$presentacion_sel)
    tryCatch(
      {
        boxplot_interactivo(
          data = base_precios,
          presentacion_sel = input$presentacion_sel,
          subgrupo_sel = input$subgrupo_sel
        )
      },
      error = function(e) {
        return("No hay información disponible")
      }
    )
  })

output$grafico <- renderPlotly({
res=resultado()  
res$grafico_plotly
})

output$grafico <- plotly::renderPlotly({
  res <- resultado()
  if (is.character(res) || length(res) == 0) {
    return(NULL)  #  No hay gráfico para mostrar
  } else {
    res$grafico_plotly  # Devuelve el gráfico Plotly
  }
})

# Se genera el grafico plano

grafico_plano <- reactive({
  res <- resultado()
  if (is.character(res) || length(res) == 0 || is.null(input$municipios) || input$municipios < 1) {
    return(NULL)  # No hay gráfico para guardar
  } else {
    res$grafico_plano  # Guarda solo el gráfico 'grafico_plano'
  }
})  



output$descargar <- downloadHandler(
  filename = function() {
    paste("grafica-", Sys.Date(), ".png", sep="")
  },
  content = function(file) {
    tempFile <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(as_widget(resultado()$grafico_plano), tempFile, selfcontained = FALSE)
    webshot::webshot(tempFile, file = file, delay = 2)
  }
)

output$descargarDatos <- downloadHandler(
  filename = function() {
    paste("datos-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(resultado()$datos, file)
  }
)
values <- reactiveValues(subtitulo = NULL)  

output$subtitulo <- renderText({
  values$subtitulo= "Descubre cómo se comportan a lo largo del tiempo los precios de los insumos que llegan a Cundinamarca para la producción de alimentos."
  values$subtitulo
})



# Mensajes 
values <- reactiveValues(mensaje1 = NULL)
output$mensaje1 <- renderText({
  resultado_data <- resultado()
  if (nrow(resultado_data$datos) == 0) {
    validate("No hay información disponible")
  } else {
    
    values$mensaje1=resultado_data$Text_
    
    return(values$mensaje1)
  }})


# Boton de reset

observeEvent(input$reset, {
  updateSelectInput(session, "subgrupo_sel", selected = "Fungicidas")
  updateSelectInput(session, "presentacion_sel", selected = "1 kilogramo")

})


# Descagamos el grafico

observeEvent(input$descargar, {
  screenshot("#grafico", scale = 5, file = "grafico_1.png")
}) 

# Generamos el informe

output$report <- downloadHandler(
  filename = 'informe.pdf',
  content = function(file) {
    # Ruta al archivo RMarkdown
    rmd_file <- "informe.Rmd"
    
    # Renderizar el archivo RMarkdown a PDF
    rmarkdown::render(rmd_file, output_file = file, params = list(
      datos = resultado()$datos, 
      plot = resultado()$grafico_plano,# Accede al gráfico 'grafico_plano'
      subtitulo = values$subtitulo,
      mensaje1 = values$mensaje1,
      subgrupo_sel = input$subgrupo_sel,
      presentacion_sel = input$presentacion_sel
      
    ))
    
    
  },
  contentType = 'application/pdf'
)

  
  
    
}