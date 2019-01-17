# Script pou définir les éléments commun à l'UI et au serveur

# Packages ####

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(stringi)
library(stringr)

# Objets utiles plus tard ####

# – Options du graphique ####

# Vecteur avec les types de graphiques pour une variable
types_onevar <- c("Barres"='geom_bar',
                  "Aire"='geom_area',
                  "Densite de Gauss"='geom_density',
                  "Polygone des frequences"='geom_freqpoly',
                  "Histogramme"='geom_histogram')

# Options avec à utiliser avec le panel selectInput
options_select <- c("stat", "color", "fill", "linetype", "group", "shape", "kernel")

# Options avec à utiliser avec le panel sliderInput
options_slider <- c("alpha", "size", "lower", "middle", "upper", "ymax", "ymin")

# Options spécifiques du graph boxplot
options_boxplot <- c("lower", "middle", "upper", "ymax", "ymin")

# Vecteur avec les options, à intégrer dans graph_type
# Une fonction serait-elle plus adaptée ? #
options_graph <- c("alpha = input$alpha, stat = input$stat, linetype = input$linetype")

# Fonctions ####

# – Fonctions relatives au graphique ####

# Retourne un panel en fonction du type d'option
panel_option_to_add <- function (option) {
  if (option %in% options_select) {type <- "select"}
  if (option %in% options_slider) {type <- "slider"}
  paste0(type,"Input(inputId = \'", option, "\',label = \'", str_to_title(option), "\',")
}

# Complète le panel en fonction de l'option choisie
option_to_add <- function (option) {
  
  # Text options
  if (option == "stat") {
    b <- "choices = c('count', 'bin'))"
  }
  if (option == "kernel") {
    b <- 'choices = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight","cosine", "optcosine"))'
  }
  
  if (option == "linetype") {
    b <- 'choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))'
  }
  
  # Numeric options
  if (option == "alpha") {
    b <- "value = 1, min = 0, max = 1, step = 0.1)"
  }
  
  return(eval(paste(panel_option_to_add(option),b) %>% parse(text = .)))
}

# Retourne le type de graph à afficher avec les options
graph_type <- function (type) {
  if (type == "geom_density") {
    options_graph <- gsub("stat = input$stat", "kernel = input$kernel", options_graph, fixed = T)
  }
  return(paste0(type, '(', options_graph, ')'))
}
  