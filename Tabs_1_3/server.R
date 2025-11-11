

# Definir la función de servidor
server <- function(input, output, session) {
  
  resultado<-reactive({
    # Comprobar si solo se ha seleccionado un producto
    if (input$producto != "todo" && input$anio == "todo" && input$mes == "todo") {
      col_en_cun(Producto = input$producto)
    } else if (input$mes != "todo" && input$anio == "todo") {
      validate(
        need(input$anio != "todo", "Debe seleccionar un año.")
      )
    } else if(input$anio == "todo" && input$producto == "todo" && input$mes == "todo"){
      col_en_cun()
    } else if(input$producto == "todo" && input$mes == "todo" ){
      col_en_cun(Año = input$anio)
    } else if(input$producto == "todo"){
      col_en_cun(Año = input$anio, Mes = input$mes)
    } else if(input$mes == "todo" ){
      col_en_cun(Año = input$anio, Producto = input$producto)
    } else if(input$anio == "todo" && input$mes == "todo"){
      col_en_cun(Producto = input$producto)
    } else{
      col_en_cun(Año = input$anio, Mes = input$mes ,Producto = input$producto)
    }  
  })
  
# Boton reset  
  observeEvent(input$reset, {
    updateSelectInput(session, "producto", selected = "todo")
    updateSelectInput(session, "mes", selected = "todo")
    updateSelectInput(session, "anio", selected = "todo")
  })
  

# Grafico plotly
  
output$grafico <- renderLeaflet({
    res <- resultado()
    if (is.data.frame(res) || is.list(res)) {
      if (nrow(res$datos) == 0) {
        print("No hay datos disponibles")
        leaflet()  # Devuelve un mapa de Leaflet vacío
      } else {
        res$grafico %>%
          setView(lng = -75.5, lat = 3.9, zoom = 5) 
      }
    } else {
      print("No hay datos disponibles")
      leaflet()  # Devuelve un mapa de Leaflet vacío
    }
  })
  
  
  
# Se genera el grafico plano
grafico_plano <- reactive({
  res <- resultado()
  if (nrow(res$datos) == 0) {
    return(NULL)  
  } else {
    res$grafico_plano  
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

  
# Ver datos en el tablero (head)
    output$vistaTabla <- renderTable({
    if (!is.null(resultado()$datos)) {
      head(resultado()$datos, 5)
    }
  })
  

# Descargar un excel con los datos  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(resultado()$datos, file)
    }
  )
  

# Subtitulo
  
values <- reactiveValues(subtitulo = NULL, mensaje1 = NULL, mensaje2 = NULL)   
  
  output$subtitulo <- renderText({
    res <- resultado()
    if (is.data.frame(res) || is.list(res)) {
      if(nrow(res$datos) == 0) {
        values$subtitulo <- ("No hay datos disponibles")
      }else{
        porcentaje_max <- res$porcentaje_max
        dpto_max <- res$dpto_max
        
        values$subtitulo <- (paste0(dpto_max," recibe el ", porcentaje_max, "% de sus alimentos de Cundinamarca, siendo el departamento más dependiente."))
      }
    } else {
      values$subtitulo <- ("No hay datos disponibles")
    }
    return(values$subtitulo)
  })
  
# Mensajes: Mensaje 1   
  output$mensaje1 <- renderText({
   values$mensaje1 <- ("Cada porcentaje representa la proporción de productos que cada departamento recibe de Cundinamarca en relación con el total de productos que ingresan a dicho departamento.")
  return(values$mensaje1)
   })
#Mensaje 2  
  output$mensaje2 <- renderText({
    resultado <- resultado()
    porcentaje_max_1<-resultado$porcentaje_max
    dpto_max <- resultado$dpto_max
    values$mensaje2 <-(paste0("Para el producto y periodo de tiempo  seleccionado, " ,dpto_max, " recibió el ",porcentaje_max_1, "% del total de volumen registrado con origen Cundinamarca." ))
    return(values$mensaje2)
     })

# Generamos el Informe
  output$report <- downloadHandler(
    filename = 'informe.pdf',
    
    content = function(file) {
      # Ruta al archivo RMarkdown
      rmd_file <- "informe.Rmd"
      
      # Renderizar el archivo RMarkdown a PDF
      rmarkdown::render(rmd_file, output_file = file, params = list(
        anio = input$anio,
        mes = input$mes,
        producto = input$producto,
        subtitulo = values$subtitulo,
        maximo = resultado()$porcentaje_max_1,
        plot = grafico_plano(),
        mensaje1 = values$mensaje1,
        mensaje2 = values$mensaje2
      ))  
    },
    contentType = 'application/pdf'
  )    

  
  
}

