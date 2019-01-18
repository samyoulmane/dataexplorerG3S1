# UI pour l'application Explorateur de données

# UI

shinyUI(fluidPage(
  includeCSS("style.css"),
  titlePanel("Explorateur de données"),
  hr(),
  conditionalPanel("output.confirmation == ''",
                   wellPanel(fileInput(inputId = "file_be", 
                                       label = "Veuillez importer un jeu de données",
                                       buttonLabel = "Parcourir...",
                                       placeholder = "Pas de fichier selectionné"),
                              style = "text-align:center; width: 50%; margin: auto; margin-top: 15%;"
                             )
                  ),
  conditionalPanel("output.confirmation !== ''", sidebarLayout( # Sidebar ####
    sidebarPanel(width = 3,
                 fileInput(inputId = "file_af", 
                           label = "Importer un autre fichier",
                           buttonLabel = "Parcourir...",
                           placeholder = "Pas d'autre fichier selectionné"),
                 selectInput(inputId = "gtype",
                             label = "Type de graph",
                             choices = types_onevar),
                 inputPanel(h5("OPTIONS"), # – Options du graph ####
                            conditionalPanel(condition = "input.gtype == 'geom_density'",
                                             option_to_add("kernel")),
                            conditionalPanel(condition = "input.gtype != 'geom_density'",
                                             option_to_add("stat")),
                            option_to_add("theme"),
                            option_to_add("alpha"),
                            option_to_add("linetype")
                            )
    ), # fin de sidebarPanel
    mainPanel(width = 9, # mainPanel ####
              wellPanel(flowLayout(id = "variables_selector", # – Panel de selection des variables ####
                div(
                  selectInput(inputId = "var1",
                              label = "Variable 1 - X",
                              choices = c()),
                  conditionalPanel("output.var1_type != 'character' || output.var1_type != 'factor'",
                                   checkboxInput(inputId = "disc_var1", label = "Disrète")
                                   )
                ), # fin de variable 1
                
                div(
                  checkboxInput(inputId="presence_var2",
                                label="Variable 2 - Y"),
                  conditionalPanel(condition = "input.presence_var2 == true",
                                   selectInput(inputId = "var2",
                                               label = NULL,
                                               choices = c()))
                ), # fin de variable 2
                
                div(
                  conditionalPanel(condition = "input.presence_var2 == true",
                                   checkboxInput(inputId="presence_var3",
                                                 label="Variable 3 - couleur"),
                                   conditionalPanel(condition = "input.presence_var3 == true",
                                                    selectInput(inputId = "var3",
                                                                label= NULL,
                                                                choices = c())))
                ) # fin de variable 3 - last
              )), # fin de flowLayout
              
              # – Outputs ####
              plotlyOutput("graph1"),
              br(),
              tags$p("Résumé des variables du jeu de données :"),
              verbatimTextOutput("summary"),
              hr(),
              textOutput("confirmation"),
              br(),
              dataTableOutput("data_table")
              
    ) # fin de mainPanel
  )) # fin de sidebarLayout
    
)) # fin de fluidPage