# Packages

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(stringi)
library(stringr)

# Objets utiles plus tard ####

types <- c("Barres"='geom_bar',
           "Aire"='geom_area',
           "Densite de Gauss"='geom_density',
           "Polygone des frequences"='geom_freqpoly',
           "Histogramme"='geom_histogram')

options_select <- c("stat", "color", "fill", "linetype", "group", "shape", "kernel")

options_slider <- c("alpha", "size", "stroke")

options_boxplot <- c("lower", "middle", "upper", "ymax", "ymin")

options_graph <- c("alpha = as.numeric(input$alpha)",
                   "stat = input$stat")

# Fonctions ####

panel_option_to_add <- function (option) {
  if (option %in% options_select) {type <- "select"}
  if (option %in% options_slider) {type <- "slider"}
  paste0(type,"Input(inputId = \'", option, "\',label = \'", str_to_title(option), "\',")
}

option_to_add <- function (option) {
  
  if (option == "stat") {
    b <- "choices = c('count', 'bin'))"
  }
  if (option == "kernel") {
    b <- 'choices = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight","cosine", "optcosine"))'
  }
  
  if (option == "alpha") {
    b <- "value = 0.5, min = 0, max = 1, step = 0.1)"
  }
  
  return(eval(paste(panel_option_to_add(option),b) %>% parse(text = .)))
}

graph_type <- function(type) {
  if (type == "geom_density") { # Condition mise dans la fonction pour répondre à la réactivité
    append(options_graph, "kernel = input$kernel")
  }
  return(paste0(type, '(', options_graph, ')'))
}
  


#(paste0(input$gtype, '(', 'alpha=', input$alpha,')' %>% parse(text=.) %>% eval())
  