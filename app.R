# Packages

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

# Objets utiles plus tard ####

types <- c("Barres"='geom_bar()',
           "Aire"='stat_bin()',
           "Densite de Gauss"='geom_density(kernel = "gaussian")',
           "Polygone des frequences"='geom_freqpoly()',
           "Histogramme"='geom_histogram(stat = "count")')

# Fonctions ####

# UI ####

ui <- fluidPage(
  navbarPage(title = "Explorateur de données", # Titre de l'application
             # Premier onglet
             tabPanel(title = "Graph - une variable",
                      br(),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          fileInput(inputId = "file", 
                                    label = "Importer un fichier",
                                    buttonLabel = "Parcourir...",
                                    placeholder = "Pas de fichier selectionné"),
                          selectInput(inputId = "var1",
                                      label = "Choisir une variable",
                                      choices = colnames(mpg),
                                      selected = "qsec"),
                          selectInput(inputId = "gtype",
                                      label = "Type de graph",
                                      choices = types)
                        ),
                        mainPanel(width = 9,
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
             ),
             # Deuxième onlet
             tabPanel(title = "Graph - deux variables",
                      tags$p("Graphique avec deux variables prévu ici.")
             ),
             # Troisième onglet
             tabPanel(title = "Table de données",
                      dataTableOutput("table")
             )
  )
)

# Serveur ####
server <- function(input, output, session) {
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
}

# Lancement
shinyApp(ui = ui, server = server)

