Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8")
# Serveur pour l'application Explorateur de données

# Serveur

shinyServer(function(input, output, session) {
  
  # Fonctions devant ête définines dans le serveur ####
  
  # Fonction pour créer les variables réactives
  create_variable <- function(variable) {
    reactive({
      vari <- eval(to_eval_text(c("input$", variable)))
      req(vari)
      if (variable == "var3" & input$gtype == "geom_col" & input$var3 != input$var1) {
        # Parce que dans ce cas, il une agrégation qui est faite donc pour pouvoir utiliser var3() il faut l'adapter à data_to_use
        data_set <- data_to_use()
        return(eval(to_eval_text(c("data_set$", vari))))
      } else {
        data_set <- data_set()
        return(eval(to_eval_text(c("data_set$", vari))))
      }
    })
  }
  
  # Fonction pour déterminer le type de variable (renvoie un output utilisé dans l'UI uniquement)
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
  
  # Agrégation des données en fonction de la moyenne ou de la médiane
  fct_tri <- function(x, fct) {
    switch(fct,
      "mean" = summarise(x, "mean" = mean(!!sym(input$var2))),
      "median" = summarise(x, "median" = median(!!sym(input$var2)))
    )
  }
  
  # Données triée à utiliser pour les aes
  data_to_use <- function(gtype = input$gtype) {
    req(input$var1)
    if (gtype == "geom_col" & (!input$presence_var3 | input$var3 == input$var1)) {
      data_set <- data_set()
      data_to_use <- data_set %>% 
        group_by(!!sym(input$var1)) %>%
        fct_tri(input$fct_tri) %>%
        arrange(!!sym(input$fct_tri)) %>%
        mutate(var1_label=factor(!!sym(input$var1), levels=!!sym(input$var1))) %>% # Création d'une nouvelle colonne avec les levels ordonnés
        select(!!input$fct_tri, var1_label) %>%
        `colnames<-`(., c(input$var2, input$var1))
      return(data_to_use)
    } else if (gtype == "geom_col" & input$var3 != input$var1) {
      data_set <- data_set()
      data_to_use <- data_set %>% 
        group_by(!!sym(input$var1), !!sym(input$var3)) %>%
        fct_tri(input$fct_tri) %>%
        arrange(!!sym(input$fct_tri)) %>%
        select(!!input$var1, !!input$var3, !!input$fct_tri) %>%
        `colnames<-`(., c(input$var1, input$var3, input$var2))
    } else {
      return(data_set())
    }
  }
  
  # Retourne les abscisses ordonnées
  graph_aes <- function (x, Xdisc = F, func = length) {
    if (is.character(x)|Xdisc|is.factor(x)) {
      x <- reorder(x = x, X = x, FUN=func)
      return(aes(x=x))
    } else {
      return(aes_string(x=input$var1))
    }
  }
  
  # Pour le tri des abscisses dans un graphique à boites à moustaches
  aes_to_use <- function (a = input$disc_var1, b = input$disc_var2, g = input$gtype) {
    if (a & !b & g!='geom_boxplot') {
      data_set <- data_to_use()
      ggplot(data = data_set) +
        aes_string(x = input$var1, y = input$var2)
    } else if (g == 'geom_boxplot') {
      data_set <- data_set()
      x <- reorder(x = var1(), X = var2(), FUN=eval(parse(text = input$fct_tri)))
      ggplot(data = data_set) +
        aes(x = x, y = var2())
    } else {
      data_set <- data_set()
      ggplot(data = data_set) +
        aes_string(x = input$var1, y = input$var2)
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
      updateSelectInput(session, inputId = i, choices = c("Choisir" = "", colnames(data_set())))
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
  
  # – Graphiques ####
  
  # –– Graphique à une variable ####
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
      if (input$coordflip) {
        g <- g + coord_flip()
      }
      if(is.numeric(var1())|is.integer(var1())){
        if (input$stat_mean) {
          g <- g + geom_vline(xintercept=mean(var1(), na.rm=T), 
                              linetype="dashed", 
                              color="red")
        }
        if (!is.null(input$perc1)) {
          g <- g + geom_vline(xintercept=quantile(var1(), c(input$perc1, input$perc2)), 
                              linetype="twodash", 
                              color="blue")
        }
      } 
      
      ggplotly(g)
    }
  })
  
  # ––– Changement du type de graphique en fonction du type de la variable 1 ####
  observe({
    if (!input$presence_var2) { # Si la deuxième variable n'est pas là :
      updateSelectInput(session, inputId = "gtype", choices = types_onevar)
      if(input$disc_var1) {
        updateSelectInput(session, inputId = "stat", selected = "count")
        updateSelectInput(session, inputId = "gtype", selected = "geom_bar")
      } else {
        updateSelectInput(session, inputId = "stat", selected = "bin")
        updateSelectInput(session, inputId = "gtype", selected = "geom_density")
      }
    } else { # Si la deuxième variable est là :
      updateSelectInput(session, inputId = "stat", selected = "identity")
      if(is.character(var2())|is.factor(var2())) {
        updateCheckboxInput(session, inputId = "disc_var2", value = T)
      }
    }
  })
  
  # –– Graphique à deux variables ####
  output$graph2 <- renderPlotly({
    if (input$presence_var2) {
      data_set <- data_set()
      data_to_use <- data_to_use()
      req(input$var1, input$var2, cancelOutput = T)
      g <- aes_to_use() +
        graph_type(type = input$gtype, disc_var1 = input$disc_var1) %>% parse(text=.) %>% eval() +
        paste0("theme_", input$theme, "()") %>% parse(text=.) %>% eval() +
        labs(x=str_to_title(input$var1), y=str_to_title(input$var2)) +
        theme(axis.text.x = element_text(angle = input$Angle))
      if (input$presence_var3) {
        req(input$var3)
        if (input$disc_var3) { # Pour prendre en compte var3
          g <- g + aes(fill = factor(var3())) + 
            scale_fill_discrete(name = input$var3) # Pour renommer l'étiquette de var3
        } else {
          g <- g + aes_string(fill = input$var3)
        }
      }
      if (input$coordflip) { # Pour inverser les axes
        g <- g + coord_flip()
      }
      if (input$trend_line & input$gtype != "geom_smooth") {
        g <- g + geom_smooth() # Pour ajouter une ligne de tendence
      }
      if (is.numeric(var2())|is.integer(var2())) {
        if (input$stat_mean) {
          g <- g + geom_hline(yintercept=mean(var2(), na.rm=T), 
                              linetype="dashed", 
                              color="red")
        }
        if (!is.null(input$perc)) {
          g <- g + geom_hline(yintercept=quantile(var2(), input$perc), 
                              linetype="twodash", 
                              color="blue")
        }
      }
      gg <- ggplotly(g)
      if (input$gtype == "geom_boxplot") {
        gg <- layout(gg, boxgap=1-input$largeur, boxmode = "group") # Le graphique étant produit avec plotly, la largeur des boites à moustaches se modifie ainsi.
      }
      if (input$var1 == input$var3) {
        gg <- layout(gg, showlegend = FALSE) # Effacement de la légende si var1 = var3, permet un simple coloriage
      }
      gg
    } else {return(NULL)}
  })
  
  # ––– Switcher
  
  observeEvent(input$switcher, {
      a <- input$var1
      b <- input$var2
      updateSelectInput(session, "var1", selected = b)
      updateSelectInput(session, "var2", selected = a)
    })
  
  # ––– Changement du type de graphique en fonction du type des variables 1 et 2 ####
  observeEvent(c(input$var1, input$var2, input$switcher, input$disc_var1, input$disc_var2, input$presence_var2),{
    req(input$var1, input$var2, input$presence_var2)
    if (input$disc_var1 == F  & !is.factor(var1()) & input$disc_var2 == F) {
      updateSelectInput(session, "gtype", selected = "geom_jitter")
    } else if ((input$disc_var1 == T | is.factor(var1()) | is.character(var1())) & input$disc_var2 == F) {
      updateSelectInput(session, "gtype", selected = "geom_boxplot")
    } else if (input$disc_var1 == T & input$disc_var2 == T) {
      updateSelectInput(session, "gtype", selected = "geom_count")
    } else if (input$disc_var1 == F & input$disc_var2 == T) {
      updateSelectInput(session, "gtype", selected = "geom_line")
    } else {
      updateSelectInput(session, "gtype", selected = "geom_count")
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
    if(typeof(var1()) %in% c("integer", "double")) {
      updateCheckboxInput(session, inputId = "disc_var1", value = F)
      updateSliderInput(session, inputId = "Angle", value = 0)
    }
    if(is.character(var1())|is.factor(var1())) {
      updateCheckboxInput(session, inputId = "disc_var1", value = T)
      updateSliderInput(session, inputId = "Angle", value = 45)
    }
  })
  observeEvent(input$var2, {
    if(typeof(var2()) == "integer") {
      updateCheckboxInput(session, inputId = "disc_var2", value = F)
    } else if(typeof(var2()) == "double") {
      updateCheckboxInput(session, inputId = "disc_var2", value = F)}
  })
  observeEvent(input$gtype, {
    if (input$gtype %in% types_onevar$`Variable continue`) {
      updateSelectInput(session, "stat", choices = c('bin', 'count'))
    }
  })
  observeEvent(input$presence_var2, {
    if (input$presence_var2) {updateSelectInput(session, inputId = "gtype", choices = types_morevar)}
  })
  
  # – Tableaux ####
  
  # –– Résumé ####
  output$description <- renderDataTable({
    data_set <- data_set()
    datatable(data_set %>% 
                desctable(stats = list("N"= length,
                                       "%/Mean" = is.factor ~ percent | mean,
                                       "sd"     = is.factor ~ NA | sd,
                                       "Med"    = is.normal ~ NA | median,
                                       "IQR"    = is.normal ~ NA | IQR,
                                       "Min"    = is.factor ~ NA | min,
                                       "Max"    = is.factor ~ NA | max)) %>% 
                as.data.frame(), filter = 'top') %>% 
      formatRound(columns=c("%/Mean", "sd", "Med", "Min", "Max"), digits=3)
  })
  
  # –– Table de données ####
  output$confirmation <- renderText({
    inFile <- input$file_be
    if (is.null(inFile)) {
      if (input$button == 0) {
        return(NULL)
      } else {
        return("Données chargées")
      }
    } else {
      return("Données chargées")
    }
  })
  
  outputOptions(output, "confirmation", suspendWhenHidden = FALSE)
  output$data_table <- renderDataTable({
    DT::datatable(data_set(), filter = 'top')
  })
})