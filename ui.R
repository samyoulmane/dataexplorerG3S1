Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8")

# UI pour l'application Explorateur de données

shinyUI(fluidPage(
  includeCSS("style.css"),
  titlePanel("Explorateur de données"),
  hr(),
  conditionalPanel("output.confirmation == ''", # Ecran d'accueil (importation de fichier) ####
    wellPanel(style = "text-align:center; width: 50%; margin: auto; margin-top: 15%;",
      fileInput(inputId = "file_be", 
                accept ="text/csv", 
                label = "Veuillez importer un jeu de données", 
                buttonLabel = "Parcourir...", 
                placeholder = "Pas de fichier selectionné")
    )
  ),
  
  conditionalPanel("output.confirmation !== ''", # Ecran principal (visualisation) #####
    sidebarLayout( # sidebar ####
                   
      sidebarPanel(width = 3,
                   
        fileInput("file_af", 
                  accept ="text/csv", 
                  label = "Importer un autre fichier", 
                  buttonLabel = "Parcourir...", 
                  placeholder = "Pas de fichier selectionné"),
        
        selectInput("gtype", "Type de graphique", 
                    choices = types_onevar, 
                    selected = "geom_bar"),
        
        conditionalPanel("input.gtype == 'geom_bar' || input.gtype == 'geom_histogram'",
          checkboxInput("percent", "Pourcentages")),
        
        # – Options du graph ####
                   
          conditionalPanel("input.gtype == 'geom_density'",
            option_to_add("kernel")),
          
          checkboxInput("coordflip", "Inverser les axes"),
        
          conditionalPanel("input.presence_var2 & !input.disc_var1 & input.gtype !=='geom_smooth'",
                           checkboxInput("trend_line", "Ligne de tendance")),
        
          conditionalPanel("input.gtype == 'geom_freqpoly' || input.gtype == 'geom_area'",
                           option_to_add("stat")),
          
          conditionalPanel("input.disc_var1 == true & input.disc_var2 == false & input.presence_var2 == true", 
            selectInput("fct_tri", "Trier les abscisses en fonction de", choices = fonctions_tri)),
          
          conditionalPanel("(output.var1_type !== 'factor' & !input.presence_var2 & !input.disc_var1) || input.presence_var2 & output.var2_type !=='factor'",
                           checkboxInput("stat_mean", "Moyenne"),
                           fluidRow(
                             column(6, numericInput("perc1", "Percentile 1", NULL, min=0, max=1, step = 0.01)),
                             column(6, numericInput("perc2", "Percentile 2", NULL, min=0, max=1, step = 0.01))
                           )
          ), 
          
          conditionalPanel("input.gtype !=='geom_smooth'",
          option_to_add("Transparence")),
          
          conditionalPanel("output.var1_type == 'factor'",
            option_to_add("Angle")),
          
          conditionalPanel("input.gtype == 'geom_line' || input.gtype =='geom_density'",
            option_to_add("linetype")),

          conditionalPanel("input.gtype == 'geom_col' || input.gtype == 'geom_bar' || input.gtype == 'geom_boxplot'",
            option_to_add("largeur")),
        
          option_to_add("theme")

       ), # Fin de sidebar

      mainPanel(width = 9, # mainPanel ####
        wellPanel(fluidRow(id = "variables_selector", # – Panel de selection des variables ####
                           
          column(width = 4, 
            selectInput("var1", "Variable 1 - X", c()),
            checkboxInput("disc_var1", "Discrète"),
            conditionalPanel("!input.disc_var1 & input.presence_var2",
                             checkboxInput("log_var1", "Logarithmique"))
          ), # fin de variable 1

          column(id = "blockswitcher", width = 1,
            conditionalPanel("input.presence_var2 == true",
              actionButton("switcher", "", icon("exchange"))
            )
          ), # fin du switcher

          column(width = 4,checkboxInput("presence_var2", "Variable 2 - Y", value = F),
            conditionalPanel("input.presence_var2 == true",
              selectInput(inputId = "var2", NULL, c()),
              checkboxInput("disc_var2", "Discrète"),
              conditionalPanel("!input.disc_var2",
                checkboxInput("log_var2", "Logarithmique"))
            )
          ), # fin de variable 2

          column(width = 3,
            conditionalPanel("input.presence_var2 == true",
              checkboxInput("presence_var3", "Variable 3 - couleur"),
              conditionalPanel("input.presence_var3 == true",
                selectInput(inputId = "var3", NULL, c()),
                checkboxInput("disc_var3", "Discrète")
              ) # fin du deuxième conditionalPanel
            ) # fin du premier conditionalPanel
          ) # fin de variable 3

        )), # fin du panel de selection des variables

        # – Outputs ####

        conditionalPanel("input.presence_var2 == false",
          plotlyOutput("graph1", height = "510px")
        ),

        conditionalPanel("input.presence_var2 == true",
          plotlyOutput("graph2", height = "510px")
        ),
        
        br(),
        
        tabsetPanel(type = "pills",
        
          tabPanel("Résumé des variables",
            br(),
            dataTableOutput("description")
          ),
        
          tabPanel("Données",
            br(),
            dataTableOutput("data_table")
          )
        )
        
      ) # fin de mainPanel
    ) # fin de sidebarLayout
  ) # fin de Ecran principal (visualisation)
))# fin de shinyUI et de de fluidPage
