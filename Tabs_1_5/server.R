#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 21/04/2024
#Fecha de ultima mo dificacion: 21/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################

server <- function(input, output, session) {
  
  resultado<-reactive({
    if(input$año=="todo"&&input$mes!="todo"&&input$depto=="todo"){
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    }else if(input$año=="todo"&&input$mes=="todo"&&input$depto=="todo"){
      entran_prod()
    }else if(input$año=="todo"&&input$mes=="todo"){
      entran_prod(depto = input$depto)
    }else if(input$año=="todo"&&input$mes!="todo"&&input$depto!="todo"){
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    }else if(input$año=="todo"&&input$depto=="todo"){
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    }else if(input$mes=="todo"&&input$depto=="todo"){
      entran_prod(año = input$año)
    }else if(input$depto=="todo"){
      entran_prod(año = input$año, Mes = input$mes)
    }else if(input$mes=="todo"){
      entran_prod(año = input$año, depto = input$depto)
    }else {
      entran_prod(año = input$año, Mes = input$mes, depto = input$depto) 
    } 
  })
  
# IMAGEN PLOTLY  
output$grafico <- renderHighchart({
    res<-resultado()
    if(nrow(res$datos)==0){
      validate(
        ("Debe seleccionar un año.")
      )
    }else{
      res$grafico
    }
  })

# IMAGEN PLANA

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

  
# Para ver el head de los datos
  output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  
# Descargar el grafico 
  output$descargar_ <- downloadHandler(
    filename = function() {
      paste("grafica_productos_ingresan_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Forzar la ejecución de la función reactiva
      res <- resultado()
      
      # Usa ggsave para guardar el gráfico
      ggplot2::ggsave(filename = file, plot = res$grafico_plano, width = 13, height = 7, dpi = 200)
    }
)
  


# Descargar la base de datos
output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )

  
# Boton de reset 
observeEvent(input$reset, {
    updateSelectInput(session, "año", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "depto", selected = "todo")
  })


#Subtitulo


values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL, mensaje2 = NULL)

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
        values$subtitulo <- (paste0("El Producto procedente de ", depto, " con mayor volumen reportado en las centrales de abasto de Bogotá fue ", producto_max))
      } else if (depto == "todo") {
        values$subtitulo <- (paste0("El prodcuto con mayor volumen reportado en las centrales de abasto de Bogotá fue ", producto_max))
      }
    }
    return(values$subtitulo)
  })
  
  
  
# Mensajes
output$mensaje1 <- renderText({
    values$mensaje1 <-("Este gráfico visualiza los alimentos que llegan a las centrales de abasto de Bogotá, destacando los productos principales por volumen.")
    values$mensaje1
    })
  output$mensaje2 <- renderText({
    values$mensaje2 <- ("Cada rectángulo en el gráfico representa un tipo de alimento, y el tamaño de cada rectángulo es  proporcional al volumen de ese alimento en comparación con los demás ingresados")
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
