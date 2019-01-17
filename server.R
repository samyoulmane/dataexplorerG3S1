# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output) {
  
  # Variables réactives ####
  variable <- reactive({
    paste("mpg$", input$var1) %>% parse(text=.) %>% eval()
  })
  
  # Outputs ####
  
  # – Nombre de catégories ####
  output$number_of_levels <- renderText({
    t <- paste("Il y a",
               factor(variable()) %>% levels() %>% length(),
               "categories pour la variable selectionnee.")
    t
  })
  
  # – Résumé ####
  output$summary <- renderPrint({
    summary(mpg)
  })
  
  # – Graphique ####
  output$graph1 <- renderPlotly({
    g <- ggplot(data = mpg, aes_string(input$var1)) + 
      (graph_type(type = input$gtype) %>% parse(text=.) %>% eval())
    ggplotly(g)
  })
  
  # – Table de données ####
  output$table <- renderDataTable({
    datatable(mpg)
  })

})