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
  
  # Fonction qui détermine stat
  graph_stat <- function(presence_var2 = input$presence_var2, gtype = input$gtype) {
    if (gtype %in% c("geom_freqpoly", "geom_area")) {
      return(input$stat)
    } else if (presence_var2) {
      return("identity")
    } else if(gtype %in% c("geom_histogram", "geom_bar")) {
      return("count")
    }
  }
  
  # data à utiliser pour geom_col, à optimiser
  data_to_use <- function(gtype) {
    if (!input$disc_var2) {
      var1_label <- input$var1
      data_set <- data_set()
      data_to_use <- data_set %>% group_by(eval(parse(text=input$var1)))
      data_to_use <- eval(to_eval_text(c("summarise", "(data_to_use, ", input$fct_tri, "(", input$var2, "))"))) %>% 
        `colnames<-`(., c(input$var1, input$var2))
      data_to_use <- eval(to_eval_text(c("arrange(data_to_use, ", input$var2, ")"))) %>% 
        mutate(var1_label=factor(eval(parse(text=input$var1)),
                                 levels=eval(parse(text=input$var1)))) %>% 
        select(!!input$var2, var1_label) %>% 
        `colnames<-`(., c(input$var2, input$var1))
      return(data_to_use)
    } else {
      return(data_set())
    }
  }
  
  # Retourne les abscisses ordonées
  graph_aes <- function (x, Xdisc = F, func = length) {
    if (is.character(x)|Xdisc|is.factor(x)) {
      x <- reorder(x = x, X = x, FUN=func)
      return(aes(x=x))
    } else {
      return(aes(x=x))
    }
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
  
  # – Résumés ####
  output$typeandmode1 <- renderPrint(c(input$var1, typeof(var1()), mode(var1())))
  output$typeandmode2 <- renderPrint(c(input$var2, typeof(var2()), mode(var2()), is.factor(var2())))
  output$summary <- renderPrint(summary(data_set()))
  output$structure <- renderPrint(str(data_set()))
   # à modifier
  # – Graphique à une variable ####
  output$graph1 <- renderPlotly({
    if (!input$presence_var2) {
      req(input$var1, input$gtype, cancelOutput = T)
      data_set <- data_set()
      g <- ggplot(data = data_set) +
        graph_aes(x = var1(), Xdisc = input$disc_var1) +
        graph_type(type = input$gtype, percent = input$percent, disc_var1 = input$disc_var1) %>% parse(text=.) %>% eval() +
        paste0("theme_", input$theme,"()") %>% parse(text=.) %>% eval() +
        theme(axis.text.x = element_text(angle = input$Angle))
      if(input$percent) {
        g <- g +
          labs(x=str_to_title(input$var1), y="%")
      } else {
        g <- g +
          labs(x=str_to_title(input$var1))
      }
      ggplotly(g)
    }
  })
  
  # –– Changement du type de graphique en fonction du type de la variable 1 ####
  observe({
    if (!input$presence_var2) { # Si la deuxième variable n'est pas là :
      updateSelectInput(session, inputId = "gtype", choices = types_onevar)
      if(is.character(var1())|is.factor(var1())|input$disc_var1) {
        updateSelectInput(session, inputId = "stat", selected = "count")
        updateSelectInput(session, inputId = "gtype", selected = "geom_histogram")
      } else {
        updateSelectInput(session, inputId = "stat", selected = "bin")
        updateSelectInput(session, inputId = "gtype", selected = "geom_density")
      }
    } else { # Si la deuxième variable est là :
      updateSelectInput(session, inputId = "stat", selected = "identity")
      updateSelectInput(session, inputId = "gtype", choices = types_morevar)
      if(is.character(var2())|is.factor(var2())) {
        updateCheckboxInput(session, inputId = "disc_var2", value = T)
      }
    }
    if(is.character(var1())|is.factor(var1())) {
      updateCheckboxInput(session, inputId = "disc_var1", value = T)
    }
  })
  
  # – Graphique à deux variables ####
  output$graph2 <- renderPlotly({
    if (input$presence_var2) {
      data_set <- data_set()
      req(input$var1, input$var2, cancelOutput = T)
      if (input$disc_var1 & !input$disc_var2 & input$gtype != 'geom_boxplot') {
        data_set <- data_to_use()
        x <- input$var1
        y <- input$var2
        g <- ggplot(data = data_set) +
          aes_string(x = x, y = y)
      } else if (input$gtype == 'geom_boxplot') {
        x <- reorder(x = var1(), X = var2(), FUN=eval(parse(text = input$fct_tri)))
        y <- var2()
        g <- ggplot(data = data_set) +
          aes(x = x, y = y)
      } else {
        x <- var1()
        y <- var2()
        g <- ggplot(data = data_set) +
          aes(x = x, y = y)
      }
      g <- g +
        graph_type(type = input$gtype, disc_var1 = input$disc_var1) %>% parse(text=.) %>% eval() +
        paste0("theme_", input$theme, "()") %>% parse(text=.) %>% eval() +
        labs(x=str_to_title(input$var1), y=str_to_title(input$var2)) +
        theme(axis.text.x = element_text(angle = input$Angle))
      gg <- ggplotly(g)
      layout(gg, boxgroupgap=1-input$largeur, 1-input$largeur)
    } else {return(NULL)}
  })
  
  # –– Changement du type de graphique en fonction du type des variables 1 et 2 ####
  observe({
    req(input$var1, input$var2, input$presence_var2)
    if (input$disc_var1 == F & input$disc_var2 == F) {
      updateSelectInput(session, "gtype", selected = "geom_jitter")
    } else if (input$disc_var1 == T & input$disc_var2 == F) {
      updateSelectInput(session, "gtype", selected = "geom_boxplot")
    } else if (input$disc_var1 == T & input$disc_var2 == T) {
      updateSelectInput(session, "gtype", selected = "geom_count")
    } else if (input$disc_var1 == F & input$disc_var2 == T) {
      updateSelectInput(session, "gtype", selected = "geom_line")
    }
  })
  
  # Changement de la fonction de tri 
  observe({
    if(input$gtype == "geom_boxplot") {
      updateSelectInput(session, inputId = "fct_tri", selected = "median")
    }
  })
  
  # Changements en fonction d'événements particuliers
  observeEvent(input$var1, {
    if(typeof(var1()) == "integer") {
      updateCheckboxInput(session, inputId = "disc_var1", value = T)
      updateSliderInput(session, inputId = "Angle", value = 0)
    } else if(typeof(var1()) == "double") {
      updateCheckboxInput(session, inputId = "disc_var1", value = F)
      updateSliderInput(session, inputId = "Angle", value = 0)
      }
  })
  observeEvent(input$var2, {
    if(typeof(var2()) == "integer") {
      updateCheckboxInput(session, inputId = "disc_var2", value = T)
    } else if(typeof(var2()) == "double") {
      updateCheckboxInput(session, inputId = "disc_var2", value = F)}
  })
  observeEvent(input$gtype, {
    if (input$gtype %in% types_onevar$`Variable continue`) {
      updateSelectInput(session, "stat", choices = c('bin', 'count'))
    }
  })
  
  # – Table de données ####
  output$confirmation <- renderText({
    inFile <- input$file_be
    if (is.null(inFile)) {
      if (input$button == 0) {
        return(NULL)
      } else {
        return("Table de toutes les données :")
      }
    } else {
      return("Table de toutes les données :")
    }
  })
  
  outputOptions(output, "confirmation", suspendWhenHidden = FALSE)
  output$data_table <- renderDataTable({
    data_set()
  })
})
