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