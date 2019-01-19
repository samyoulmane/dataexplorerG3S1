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

# Vecteur avec les types de graphiques pour une variable
types_onevar <- c("Histogramme"='geom_histogram',
                  "Barres"='geom_bar',
                  "Aire"='geom_area',
                  "Densite"='geom_density',
                  "Polygone des frequences"='geom_freqpoly')
types_morevar <- c("Ligne" = 'geom_line',
                  "Points" = 'geom_point',
                  "Jitter" = 'geom_jitter',
                  "Colonnes" = 'geom_col',
                  "Smooth" = 'geom_smooth',
                  "Boxplot" = 'geom_boxplot',
                  "Count" = 'geom_count')

# Options avec à utiliser avec le panel selectInput
options_select <- c("stat", "color", "fill", "linetype", "group", "shape", "kernel", "theme")

# Options avec à utiliser avec le panel sliderInput
options_slider <- c("alpha", "size", "lower", "middle", "upper", "ymax", "ymin", "anglex")

# Options spécifiques du graph boxplot
options_boxplot <- c("lower", "middle", "upper", "ymax", "ymin")

# Vecteur avec les options, à intégrer dans graph_type
# Une fonction serait-elle plus adaptée ? #
options_graph <- c("alpha = input$alpha, stat = input$stat, linetype = input$linetype")

# Vecteur avec les thèmes
themes_graph <- c("bw", "gray", "dark", "classic", "light", "linedraw", "minimal", "void")

# Fonctions ####

to_eval_text <- function (x) {
  x %>% paste0(., collapse = "") %>% parse(text=.)
}

# – Fonctions relatives au graphique ####


# Retourne un panel en fonction du type d'option
panel_option_to_add <- function (option) {
  if (option %in% options_select) {type <- "select"}
  if (option %in% options_slider) {type <- "slider"}
  paste0(type,"Input(inputId = \'", option, "\',label = \'", str_to_title(option), "\',")
}

# Complète le panel en fonction de l'option choisie (UI)
option_to_add <- function (option) {
  
  # Text options
  if (option == "stat") {
    b <- "choices = c('count', 'bin', 'identity')"
  }
  if (option == "kernel") {
    b <- 'choices = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight","cosine", "optcosine")'
  }
  if (option == "linetype") {
    b <- 'choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")'
  }
  if (option == "theme") {
    b <- 'choices = c("Noir et blanc"="bw", "Gris"="gray", "Foncé"="dark", "Classique"="classic", "Clair"="light", "Linedraw"="linedraw", "Minimal"="minimal", "Void"="void"), selected = "minimal"'
  }
  
  # Numeric options
  if (option == "alpha") {
    b <- "value = 1, min = 0, max = 1, step = 0.1"
  }
  if (option == "anglex") {
    b <- "value = 0, min = 0, max = 90, step = 15"
  }
  
  return(eval(paste(panel_option_to_add(option),b,")") %>% parse(text = .)))
}

# Retourne les options adaptées au type de graph (server)
graph_options <- function(type) {
  if (type %in% c("geom_density", "geom_boxplot")) {
    options_graph <- gsub("stat = input$stat, ", "", options_graph, fixed = T)
  }
  if (type == "geom_density") {
    options_graph <- paste(options_graph, "kernel = input$kernel", sep = ", ", collapse = "")
  }
  if (type == "geom_smooth") {
    options_graph <- c("")
  }
  return(options_graph)
}

# Retourne le type de graph à afficher avec les options
graph_type <- function (type) {
  return(paste0(type, '(', graph_options(type), ')'))
}

# Retourne les abscisses ordonées
graph_aes <- function (x, y = NULL, Xdisc = F, func = function(x)-length(x)) {
  if (is.character(x)|Xdisc|is.factor(x)) {
    if (is.null(y)) {
      return(aes(x=reorder(x = x, X = x, FUN=func)))
    } else {
      return(aes(x=reorder(x = x, X = y, FUN=func), y=y))
    }
  } else if (is.null(y)) {
    return(aes(x=x))
    } else {
      return(aes(x=x, y=y))
    }
}



