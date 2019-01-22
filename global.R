# Script pour définir les éléments communs à l'UI et au serveur
Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8") # Encodage
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

# Vecteurs avec les types de graphiques
types_onevar <- list("Variable discrète" = list("Barres"='geom_bar', "Histogramme"='geom_histogram'),
                     "Variable continue" = list("Densité"='geom_density', "Aire"='geom_area', "Polygone des fréquences"='geom_freqpoly'))

types_morevar <- list("Deux variables continues" = list("Jitter" = 'geom_jitter', "Points" = 'geom_point', "Courbe" = 'geom_smooth'),
                      "x discrète, y continue"   = list("Colonnes" = 'geom_col', "Boites à moustaches" = 'geom_boxplot'),
                      "Deux variables discrètes" = list("Count" = 'geom_count'),
                      "Autres"                   = list("Ligne" = 'geom_line'))

# Options avec à utiliser avec le panel selectInput
options_select <- c("stat", "color", "fill", "linetype", "group", "shape", "kernel", "theme")

# Options avec à utiliser avec le panel sliderInput
options_slider <- c("Transparence", "size", "Angle", "largeur")

# Vecteur avec les options, à intégrer dans graph_type
# Une fonction serait-elle plus adaptée ? #
options_graph <- c("alpha = input$Transparence, stat = graph_stat(), linetype = input$linetype")

# Vecteur avec les thèmes
themes_graph <- c("classic", "light", "linedraw", "minimal")

# Vecteur avec les fonctions de tri
fonctions_tri <- c("Moyenne" = "mean", 
                   "Médiane" = "median")

# Fonctions ####

to_eval_text <- function (x) {
  x %>% paste0(., collapse = "") %>% parse(text=.)
}

# – Fonctions relatives au graphique ####


# Retourne un panel en fonction du type d'option
panel_option_to_add <- function (option) {
  if (option %in% options_select) {type <- "select"}
  if (option %in% options_slider) {type <- "slider"}
  paste0(type,"Input(\'", option, "\',label = \'", str_to_title(option), "\',")
}

# Complète le panel en fonction de l'option choisie (UI)
option_to_add <- function (option) {
  
  # Text options
  if (option == "stat") {
    b <- "choices = c('')"
  }
  if (option == "kernel") {
    b <- 'choices = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight","cosine", "optcosine")'
  }
  if (option == "linetype") {
    b <- 'choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")'
  }
  if (option == "theme") {
    b <- 'choices = c("Sans quadrillage"="classic", "Léger"="light", "Marqué"="linedraw", "Minimal"="minimal"), selected = "minimal"'
  }
  
  # Numeric options
  if (option == "Transparence") {
    b <- "value = 1, min = 0, max = 1, step = 0.1"
  }
  if (option == "Angle") {
    b <- "value = 0, min = 0, max = 90, step = 15"
  }
  if (option == "largeur") {
    b <- "value = 0.9, min = 0.1, max = 1, step = 0.1"
  }
  
  return(eval(paste(panel_option_to_add(option),b,")") %>% parse(text = .)))
}

# Retourne les options adaptées au type de graph (server)
graph_options <- function(type) {
  if (!is.null(type)) {
    if (type %in% c("geom_density", "geom_boxplot", "geom_col")) {
      options_graph <- gsub("stat = graph_stat(), ", "", options_graph, fixed = T)
    }
    if (type == "geom_density") {
      options_graph <- paste(options_graph, "kernel = input$kernel", sep = ", ", collapse = "")
    }
    if (type == "geom_smooth") {
      options_graph <- c("")
    }
    if (type %in% c("geom_count", "geom_point", "geom_jitter")) {
      options_graph <- gsub(", linetype = input$linetype", "", options_graph, fixed = T)
    }
    if (type %in% c("geom_bar", "geom_boxplot", "geom_col")) {
      options_graph <- paste(options_graph, "width = input$largeur", sep = ", ", collapse = "")
    }
    return(options_graph)
  }
}

# Retourne le type de graph à afficher avec les options
graph_type <- function (type) {
  if (!is.null(type)) {
    return(paste0(type, '(', graph_options(type), ')'))
  }
}





