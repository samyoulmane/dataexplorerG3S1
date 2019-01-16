# UI pour l'application Explorateur de données

# Source du script
source(file = "script.R")

# UI

shinyUI(fluidPage(
  includeCSS("style.css"),         
  titlePanel("Explorateur de données"),
  hr(),
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput(inputId = "file", 
                           label = "Importer un fichier",
                           buttonLabel = "Parcourir...",
                           placeholder = "Pas de fichier selectionné"),
                 
                 selectInput(inputId = "gtype",
                             label = "Type de graph",
                             choices = types)
    ),
    mainPanel(width = 9,
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "var1",
                                   label = "Variable 1",
                                   choices = colnames(mpg),
                                   selected = "qsec")
                ),
                
                column(width=4,
                       checkboxInput(inputId="Presence_var2",
                                     label="Variable 2"),
                       conditionalPanel(condition = "input.Presence_var2 == true",
                                        selectInput(inputId = "var2",
                                                    label = NULL,
                                                    choices = colnames(mpg),
                                                    selected = "qsec"))
                ),
                
                
                column(width=4,
                       conditionalPanel(condition = "input.Presence_var2 == true",
                                        checkboxInput(inputId="Presence_var3",
                                                      label="Variable 3"),
                                        conditionalPanel(condition = "input.Presence_var3 == true",
                                                         selectInput(inputId = "var3",
                                                                     label= NULL,
                                                                     choices = colnames(mpg),
                                                                     selected = "qsec")
                                        )
                       )
                )
              ),
              plotlyOutput("graph1"),
              br(),
              fluidRow(
                column(width = 4,
                       textOutput("number_of_levels"),
                       br(),
                       tableOutput("factors"),
                       br()),
                column(width = 8,
                       tags$p("Structure de la variable selectionnee :"),
                       verbatimTextOutput("structure"),
                       br(),
                       tags$p("Résumé des variables du jeu de données :"),
                       verbatimTextOutput("summary"))
              )
    )
    
  )
))