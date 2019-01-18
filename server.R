# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output, session) {
  
  # Importation du jeu de données
  data_set <- reactive({
    inFile <- input$file_af
    if (is.null(inFile)) {
      inFile <- input$file_be
      if (is.null(inFile)) {
        return(NULL) # J'ai mis ce dataset par défaut pour nous faciliter le développement
      } else {read.csv(inFile$datapath)}
    } else {read.csv(inFile$datapath)}
  })
  
  # Variables réactives ####
  
  # Remplissage des menus déroulants pour variables
  observe({
    if(!is.null(data_set())) {
      for (i in c("var1", "var2", "var3")) {
        updateSelectInput(session, inputId = i, 
                          choices = colnames(data_set()))
      }
    }
  })
  
  var1 <- reactive({
    req(input$var1)
    data_set <- data_set()
    var <- input$var1
    paste("data_set$", var) %>% parse(text=.) %>% eval()
  })
  
  var2 <- reactive({
    req(input$var2)
    data_set <- data_set()
    var <- input$var2
    paste("data_set$", var) %>% parse(text=.) %>% eval()
  })
  
  var3 <- reactive({
    req(input$var3)
    data_set <- data_set()
    var <- input$var3
    paste("data_set$", var) %>% parse(text=.) %>% eval()
  })
  
  # Determination des types de variables
  observe({
    req(input$var1)
    eval(var_type(variable = input$var1, "var1"))
    outputOptions(output, "var1_type", suspendWhenHidden = FALSE)
  })
  
  observe({
    req(input$var2)
    eval(var_type(variable = input$var2, "var2"))
    outputOptions(output, "var2_type", suspendWhenHidden = FALSE)
  })
  
  observe({
    req(input$var3)
    eval(var_type(variable = input$var3, "var3"))
    outputOptions(output, "var3_type", suspendWhenHidden = FALSE)
  })
  
  # Outputs ####
  
  # – Résumé ####
  output$summary <- renderPrint({
    c(mode(var1()), typeof(var1()), str(data_set()))
  })
  
  # # – Nombre de catégories ####
  #  output$number_of_levels <- renderText({
  #    req(input$var1, cancelOutput = T)
  #    t <- paste("Il y a",
  #               factor(var1()) %>% levels() %>% length(),
  #               "catégories pour la variable selectionnée.")
  #    t
  #  })
  
  # – Graphique ####
  output$graph1 <- renderPlotly({
    req(input$var1, cancelOutput = T)
    data_set <- data_set()
    g <- ggplot(data = data_set) +
      graph_aes(var = var1(), disc = input$disc_var1) +
      (graph_type(type = input$gtype) %>% parse(text=.) %>% eval()) +
      (paste0("theme_", input$theme,"()") %>% parse(text=.) %>% eval()) +
      labs(x=str_to_title(input$var1))
      
    ggplotly(g)
  })
  
  observe({
    req(input$var1)
    if(is.character(var1())|is.factor(var1())|input$disc_var1) {
      updateSelectInput(session, inputId = "stat", selected = "count")
      updateSelectInput(session, inputId = "gtype", selected = "geom_histogram")
    } else {
      updateSelectInput(session, inputId = "stat", selected = "bin")
      updateSelectInput(session, inputId = "gtype", selected = "geom_density")
    }
    if(is.character(var1())|is.factor(var1())) {
      updateCheckboxInput(session, inputId = "disc_var1", value = T)
    }
  })
  
  observeEvent(input$var1, {
    if(typeof(var1()) == "integer") {
      updateCheckboxInput(session, inputId = "disc_var1", value = T)
    } else {
      updateCheckboxInput(session, inputId = "disc_var1", value = F)}
  })
  
  # – Table de données ####
  output$confirmation <- renderText({
    inFile <- input$file_be
    if (is.null(inFile)) {
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