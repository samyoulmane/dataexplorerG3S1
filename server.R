# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output) {
  
  variable <- reactive({
    paste("mpg$", input$var1) %>% parse(text=.) %>% eval()
  })
  
  output$number_of_levels <- renderText({
    t <- paste("Il y a",
               factor(variable()) %>% levels() %>% length(),
               "categories pour la variable selectionnee.")
    t
  })
  
  output$summary <- renderPrint({
    summary(mpg)
  })
  
  output$graph1 <- renderPlotly({
    g <- ggplot(data = mpg, aes_string(input$var1)) + 
      (graph_type(type = input$gtype) %>% parse(text=.) %>% eval())
    ggplotly(g)
  })
  
  output$table <- renderDataTable({
    datatable(mpg)
  })

})