# Serveur pour l'application Explorateur de données

# Source du script commun
source(file = "script.R")

# Serveur

shinyServer(function(input, output) {
  variable <- reactive({
    paste("mpg$", input$var1) %>% parse(text=.) %>% eval()
  })
  output$structure <- renderPrint({
    str(variable())
  })
  output$number_of_levels <- renderText({
    t <- paste("Il y a",
               factor(variable()) %>% levels() %>% length(),
               "categories pour la variable selectionnee.")
    t
  })
  output$factors <- renderTable({
    dt <- data.frame(table(variable())) %>% 
      `colnames<-`(c(input$var1, "Frequence"))
    dt
  })
  output$summary <- renderPrint({
    summary(mpg)
  })
  output$graph1 <- renderPlotly({
    g <- ggplot(data = mpg, aes_string(input$var1)) + 
      eval(parse(text=input$gtype))
    ggplotly(g)
  })
  output$table <- renderDataTable({
    datatable(mpg)
  })
  
})