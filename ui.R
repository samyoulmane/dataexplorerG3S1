# UI pour l'application Explorateur de données

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
                             choices = types),
                 inputPanel(h5("OPTIONS"),
                            conditionalPanel(condition = "input.gtype == 'geom_density'",
                                             option_to_add("kernel")),
                            conditionalPanel(condition = "input.gtype != 'geom_density'",
                                             option_to_add("stat")),
                            option_to_add("alpha"),
                            option_to_add("linetype")
                            )
    ), # fin de sidebarPanel
    mainPanel(width = 9,
              wellPanel(flowLayout(id = "variables_selector", # Panel de selection des variables
                div(
                  selectInput(inputId = "var1",
                              label = "Variable 1",
                              choices = colnames(mpg))
                ), # fin de variable 1
                
                div(
                  checkboxInput(inputId="presence_var2",
                                label="Variable 2"),
                  conditionalPanel(condition = "input.presence_var2 == true",
                                   selectInput(inputId = "var2",
                                               label = NULL,
                                               choices = colnames(mpg)))
                ), # fin de variable 2
                
                div(
                  conditionalPanel(condition = "input.presence_var2 == true",
                                   checkboxInput(inputId="presence_var3",
                                                 label="Variable 3"),
                                   conditionalPanel(condition = "input.presence_var3 == true",
                                                    selectInput(inputId = "var3",
                                                                label= NULL,
                                                                choices = colnames(mpg))))
                ) # fin de variable 3 - last
              )), # fin de flowLayout
              plotlyOutput("graph1"),
              br(),
              textOutput("number_of_levels"),
              br(),
              tags$p("Résumé des variables du jeu de données :"),
              verbatimTextOutput("summary")
    ) # fin de mainPanel
  ) # fin de sidebarLayout
    
)) # fin de fluidPage