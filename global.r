library(shiny)
library(leaflet)
library(shinydashboard)
require(shinyjs)
require(opendapR)

src <- read.csv('opendap_srcs.csv',stringsAsFactors=F)
src