library(shiny)
library(leaflet)
library(shinydashboard)
require(shinyjs)
require(opendapR)

src <- read.csv('data/opendap_srcs.csv',stringsAsFactors=F)
src
