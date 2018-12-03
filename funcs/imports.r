# imports.r - migrando todos los imports aca

library(tidyr)#install.packages("tidyr")
library(dplyr)#install.packages("dplyr")
library(purrr)#install.packages("purrr")
library(stringr)#install.packages("stringr")
library(stringi)#install.packages("stringi")
library(lubridate)#install.packages("lubridate")
library(RSQLite)#install.packages("RSQLite")
library(DBI)#install.packages("DBI")
library(igraph)#install.packages("igraph")
library(ggplot2)#install.packages("ggplot2")
# library(ggraph)#install.packages("ggraph")
library(RColorBrewer)#install.packages("RColorBrewer")
library(visNetwork)#install.packages("visNetwork")
library(shiny)# install.packages("shiny")
library(DT)#install.packages("DT")
library(shinyWidgets)#install.packages("shinyWidgets")
library(bsplus)#install.packages("bsplus")
library(plotly)#install.packages("plotly")
library(shinydashboard)# install.packages("shinydashboard")
library(ndtv)#install.packages("ndtv")
library(intergraph)#install.packages("intergraph")
library(assertive)#install.packages("assertive")
library(testthat)#install.packages("testthat")
library(igraphdata)#install.packages("igraphdata")
library(forcats)#install.packages("forcats")
library(visdat)#install.packages("visdat")

library(skimr)#install.packages("skimr")
# https://github.com/timelyportfolio/RBioFabric/issues/1

library(Rcpp)#install.packages("Rcpp")
library(devtools)#install.packages("devtools")

library(htmlwidgets)#install.packages("htmlwidgets")

devtools::install_github("jas1/RBioFabric")
library(RBioFabric)
# devtools::install_github()RBioFabric_0.4