# Global - Helper functions and importing of other scripts

##################################################################
##                          Libraries:                          ##
##                 tidyverse: Data manipulation                 ##
##                       rio: data import                       ##
##             netmeta: meta analysis (frequentist)             ##
##    yaml: loading config file with common static variables    ##
##################################################################
library(tidyverse)
library(rio)
library(netmeta)
library(ggplot2)
library(yaml)

##################################################################
##                         UI Libraries                         ##
##################################################################
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(leaflet)
library(collapsibleTree)
library(shinycssloaders)
library(shinyjs)

# Global Reactive Values
data <- reactiveValues()
freq <- reactiveValues()