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
  
  output$ubicacionInput <- renderUI({
    req(input$algo)
    if(input$algo == "Neto_sale") {
      selectInput("ubicacion", "Seleccione el destino:", choices = c("Todos las ciudades" ="todo", sort(lugar_sale)))
    } else if (input$algo == "Neto_entra"){
      selectInput("ubicacion", "Seleccione el origen:", choices = c("Todos los departamentos" ="todo",sort(lugar_entra)))
    }else if (input$algo == "Neto_entra_exter"){
      selectInput("ubicacion", "Seleccione el origen:", choices = c("Todos los departamentos" ="todo",sort(lugar_entra_exter)))
    }else{
      selectInput("ubicacion", "Seleccione el origen:", choices = c("Todos los departamentos" ="todo"))
    }
  })
  
  resultado <- reactive({
    req(input$algo, input$año, input$mes, input$ubicacion)
    if(input$año=="todo"&&input$mes=="todo"&&input$ubicacion=="todo"){
      pareto_graf(pareto = input$algo)
    }else if(input$año=="todo"&&input$mes=="todo"){
      pareto_graf(pareto = input$algo,sitio = input$ubicacion)
    }else if(input$año=="todo"){
      validate(
        need(input$año != "todo", "Debe seleccionar un año.")
      )
    }else if(input$año=="todo"&&input$ubicacion=="todo"){
      validate(
        need(input$año != "todo", "Debe seleccionar un año.")
      )
    }else if(input$mes=="todo"&&input$ubicacion=="todo"){
      pareto_graf(pareto = input$algo,año = input$año)
    }else if(input$ubicacion=="todo"){
      pareto_graf(pareto = input$algo,año = input$año, Mes = input$mes)
    }else if(input$mes=="todo"){
      pareto_graf(pareto = input$algo,año = input$año, sitio = input$ubicacion)
    }else {
      pareto_graf(pareto = input$algo,año = input$año, Mes = input$mes, sitio = input$ubicacion) 
    }
  })
  
  output$grafico <- plotly::renderPlotly({
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
  

  observeEvent(input$reset, {
    updateSelectInput(session, "año", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "ubicacion", selected = "todo")
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  
  # Subtitulo
  # Creamos un vector de valores para llevar al rmd
  
  values <- reactiveValues(subtitulo = NULL)
  
  output$subtitulo <- renderText({
    res<-resultado()
    if(nrow(res$datos)==0){
     (
        values$subtitulo<-  "No hay datos disponibles"
      )
    }else{res <- resultado() 
    req(res)
    porcent_prod <- round(res$porcent_prod,digits = 1)
    acumulado <- res$acumulado
    prod_neces <- res$prod_neces
    "Para los filtros seleccionados, aproximadamente el 80% del volumen total se concentra en {N} productos, que representan el {X%} del total de productos registrados"
    values$subtitulo <- (paste0("Para los filtros seleccionados, aproximadamente el 80% del volumen total de alimentos que ",
                                ifelse(input$algo=="Neto_entra", " ingresa ",
                                       ifelse(input$algo == "Neto_sale"," sale ",
                                              ifelse(input$algo == "Neto_entra_local"," ingresa local "," ingresa externo "))),
                                "se concentran en ", prod_neces," productos que representa el ",porcent_prod,"% del total de productos registrados."))}
    return(values$subtitulo)
  })
  
  
  # Mensajes
  
  values <- reactiveValues(mensaje1 = NULL, mensaje2 = NULL, mensaje3 = NULL)
  
  output$mensaje1 <- renderText({
    values$mensaje1 <- "Este gráfico ordena los alimentos según su volumen y muestra cómo se acumula el total. La línea vertical indica el punto en el que se alcanza alrededor del 80 % del volumen acumulado. El producto que aparece cuando pasas el cursor sobre ese punto es el que completa ese porcentaje y define cuántos productos concentran la mayor parte del volumen en el periodo seleccionado."
    values$mensaje1
  })
  
  output$mensaje2 <- renderText({
    values$mensaje2 <- "El gráfico permite identificar cuáles son los alimentos que más pesan en el abastecimiento asociado a Cundinamarca. Cada barra representa un producto y su altura muestra el volumen; la línea acumulada indica qué proporción del volumen total se va sumando producto a producto, para las entradas (locales o externas) o las salidas, según los filtros seleccionados."
    values$mensaje2
  })
  
  output$mensaje3 <- renderText({
    values$mensaje3 <- "Este gráfico muestra la importancia que tiene cada alimento en las entradas (locales o externas) y las salidas de los alimentos de Cundinamarca."
    values$mensaje3
  })
  
  
# Descargar grafica 
  
  observeEvent(input$descargar, {
    screenshot("#grafico", scale = 5)
  })

# Generamos el Informe
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
        datos = resultado()$datos, 
        porcent_prod = resultado()$porcent_prod,
        acumulado=resultado()$acumulado,
        prod_neces=resultado()$prod_neces,
        num_productos = resultado()$num_productos,
        plot = grafico_plano(),
        subtitulo = values$subtitulo,
        mensaje1 = values$mensaje1,
        mensaje2 = values$mensaje2,
        mensaje3 = values$mensaje3,
        algo = input$algo,
        anio = input$año,
        mes = input$mes,
        ubicacion = input$ubicacion
      ))  
    },
    
    contentType = 'application/pdf'
  )  
  

}