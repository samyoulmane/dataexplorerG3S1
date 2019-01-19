# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output, session) {
  
  # Fonctions devant ête définines dans le serveur ####
  
  # Fonction pour créer les variables
  create_variable <- function(variable) {
    reactive({
      vari <- eval(to_eval_text(c("input$", variable)))
      req(vari)
      data_set <- data_set()
      return(eval(to_eval_text(c("data_set$", vari))))
    })
  }
  
  # Fonction pour déterminer le type de variable
  type_variable <- function(variable) {
    observe({
      req(eval(to_eval_text(c("input$", variable))))
      if (is.factor(eval(to_eval_text(c(variable, "()")))) | is.character(eval(to_eval_text(c(variable, "()"))))) {
        eval(to_eval_text(c("output$", variable, "_type <- renderText('factor')")))
      } else {
        eval(to_eval_text(c("output$", variable, "_type <- renderText({typeof(",variable,"())})")))
      }
      outputOptions(output, paste0(variable, "_type"), suspendWhenHidden = FALSE)
    })
  }
  
  # Importation du jeu de données ####
  data_set <- reactive({
    inFile <- input$file_af
    if (is.null(inFile)) {
      inFile <- input$file_be
      if (is.null(inFile)) {
        return(mpg) # J'ai mis ce dataset par défaut pour nous faciliter le développement
      } else {read.csv(inFile$datapath)}
    } else {read.csv(inFile$datapath)}
  })
  
  # Variables réactives ####
  
  # Remplissage des menus déroulants pour variables
  observe({
    req(data_set(), cancelOutput = T)
    for (i in c("var1", "var2", "var3")) {
      updateSelectInput(session, inputId = i, choices = colnames(data_set()))
    }
  })
  
  # Création des variables réactives - liaison entre input$variable et data_set
  var1 <- create_variable("var1")
  var2 <- create_variable("var2")
  var3 <- create_variable("var3")
  
  # Determination du type des variables
  type_variable("var1")
  type_variable("var2")
  type_variable("var3")
  
  # Outputs ####
  
  # – Résumé ####
  output$summary <- renderPrint({
    c(mode(var1()), typeof(var1()), is.factor(var1()), str(data_set()))
  })
  
  # – Graphique à une variable ####
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
  
  # Changement du type de graphique en fonction du type de la variable 1
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
      #return(NULL)
      return("Table de toutes les données :") # Mettre NULL quand on aura finit le développement
    } else {
      return("Table de toutes les données :")
    }
  })
  outputOptions(output, "confirmation", suspendWhenHidden = FALSE)
  output$data_table <- renderDataTable({
    data_set()
  })
})