#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 21/04/2024
#Fecha de ultima modificacion: 21/04/2024
################################################################################
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################
library(shiny);library(htmlwidgets);library(webshot);library(magick);library(shinyscreenshot);library(webshot2)
################################################################################
server <- function(input, output, session) {
  
  resultado<-reactive({
    if(input$año=="todo"&&input$mes=="todo"&&input$depto=="todo"){
      salen_prod()
    }else if(input$año=="todo"&&input$mes=="todo"){
      salen_prod(depto = input$depto)
    }else if(input$año=="todo"){
      validate(
        need(input$año != "todo", "Debe seleccionar un año.")
      )
    }else if(input$año=="todo"&&input$depto=="todo"){
      validate(
        need(input$año != "todo", "Debe seleccionar un año.")
      )
    }else if(input$mes=="todo"&&input$depto=="todo"){
      salen_prod(año = input$año)
    }else if(input$depto=="todo"){
      salen_prod(año = input$año, Mes = input$mes)
    }else if(input$mes=="todo"){
      salen_prod(año = input$año, depto = input$depto)
    }else {
      salen_prod(año = input$año, Mes = input$mes, depto = input$depto) 
    }
  })
  
  # Generacion del grafico
  output$grafico <- renderHighchart({
    res<-resultado()
    if(nrow(res$datos)==0){
      validate(
        ("No hay datos disponibles")
      )
    }else{
      res$grafico
    }
    
  })
  
  
  # Generacion del grafico plano
  
  grafico_plano <- reactive({
    res<-resultado()
    if(nrow(res$datos)==0){
      validate(
        ("No hay datos disponibles")
      )
    }else{
      res$grafico_plano
    }
  })
  
  # En caso de que se quiera mostrar el head de los datos
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  
  
  # Boton de reset
  observeEvent(input$reset, {
    updateSelectInput(session, "año", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "depto", selected = "todo")
  })
  
  # Descarga de datos
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
  # Descargar Imagen
  output$descargar_ <- downloadHandler(
    filename = function() {
      paste("grafica_productos_salen_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    }
  )
  
  
  # Generamos el subtitulo
  
  output$subtitulo <- renderText({
    res <- resultado()
    if(nrow(res$datos) == 0){
      validate(
        need(FALSE, "No hay datos disponibles.")
      )
    } else {
      resultado <- resultado()
      producto_max <- resultado$producto_max
      porcentaje_max <- resultado$porcentaje_max
      depto <- input$depto
      if(depto != "todo"){
        values$subtitulo <- (paste0("El producto de origen cundinamarques con mayor volumen reportado en la central de abasto de ", depto, " fue ", producto_max, " con un porcentaje de: ",porcentaje_max,"%"))
      } else if (depto == "todo") {
        values$subtitulo  <- (paste0("El producto de origen cundinamarques con mayor volumen reportado fue ", producto_max, " con un porcentaje de: ",porcentaje_max,"%"))
      }
    }
    return(values$subtitulo)
  })
  
  
  
  # Mensajes
  
  values <- reactiveValues(mensaje1 = NULL, mensaje2 = NULL, mensaje3 = NULL)
  
  output$mensaje1 <- renderText({
    values$mensaje1 <- " Este gráfico muestra la importancia en volumen de cada alimento entre los productos de origen cundinamarques reportado en las centrales de abasto del SIPSA, componente abastecimiento."
    values$mensaje1 
    })
  
  output$mensaje2 <- renderText({
    values$mensaje2 <- "Cada rectángulo en el gráfico representa un tipo de alimento, y el tamaño de cada rectángulo es  proporcional al volumen de ese alimento en comparación con los demás ingresados."
    values$mensaje2 
    })
  

  
  
  # Generamos el Informe
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
        anio = input$año,
        mes = input$mes,
        depto = input$depto,
        subtitulo = values$subtitulo,
        producto_max = resultado()$producto_max,
        porcentaje_max = resultado()$porcentaje_max,
        plot = grafico_plano(),
        mensaje1 = values$mensaje1,
        mensaje2 = values$mensaje2
      ))  
    },
    
    
    contentType = 'application/pdf'
  )  
  

  

}