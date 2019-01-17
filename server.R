# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output, session) {
  
  # Importation du jeu de données
  #data_set <- mpg
  
  data_set <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else {
      return(read.csv(inFile$datapath))
    }
  })
  
  
  # Variables réactives ####
  observe({
    if(!is.null(data_set())) {
      updateSelectInput(session, inputId = "var1", 
                        choices = colnames(data_set()))
    }
  })
  if (is.null(data_set)) {
  var1 <- reactive({paste("data_set$", input$var1) %>% parse(text=.) %>% eval()})
  eval(var_type(var = "var1"))
  outputOptions(output, "var1_type", suspendWhenHidden = FALSE)
  
  # Outputs ####
  
  # – Nombre de catégories ####
  output$number_of_levels <- renderText({
    t <- paste("Il y a",
               factor(var1()) %>% levels() %>% length(),
               "catégories pour la variable selectionnée.")
    t
  })
  
  # – Résumé ####
  output$summary <- renderPrint({
    summary(data_set)
  })
  
  # – Graphique ####
  output$graph1 <- renderPlotly({
    g <- ggplot(data = data_set) +
      graph_aes(var = var1(), disc = input$disc_var1) +
      (graph_type(type = input$gtype) %>% parse(text=.) %>% eval())
      
    ggplotly(g)
  })
  
  observe({
    if(is.character(var1())|input$disc_var1) {
      updateSelectInput(session, inputId = "stat", selected = "count")
    } else {
      updateSelectInput(session, inputId = "stat", selected = "bin")
    }
  })
  }
  
  # – Table de données ####
  output$confirmation <- renderText({
    if (is.null(data_set())) {
      return(NULL)
    } else {
      return("Table de toutes les données :")
    }
  })
  outputOptions(output, "confirmation", suspendWhenHidden = FALSE)
  output$data_table <- renderDataTable({
    data_set()
  })
})