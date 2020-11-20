# imports.r - migrando todos los imports aca



shiny_apps_import_flag <- TRUE
log_prefix <- "imports.r"
if (!shiny_apps_import_flag) {
    flog.info(paste0(log_prefix,"  - NO SHINY APPS"))
    library(pacman)
    packages_instalar <-c("rlang","tidyr","dplyr","purrr","stringr","stringi","lubridate","RSQLite","DBI",
                          "ggplot2","RColorBrewer","shiny","DT","shinyWidgets","bsplus","plotly","shinydashboard","shinydashboardPlus",
                          "igraph","visNetwork","ndtv","intergraph","igraphdata","qgraph","forcats","visdat","ggridges",
                          "skimr","ggrepel","tm", "tidytext","Rcpp","devtools","htmlwidgets","jsonlite","assertive","testthat")
    pacman::p_load(char = packages_instalar)
    pacman::p_load_gh(char= "jas1/RBioFabric",dependencies = FALSE) #devtools::install_github("jas1/RBioFabric")
}else{
    flog.info(paste0(log_prefix,"  - SHINY APPS"))
    library(rlang)
    library(tidyr)#install.packages("tidyr")
    library(dplyr)#install.packages("dplyr")
    library(purrr)#install.packages("purrr")
    library(stringr)#install.packages("stringr")
    library(stringi)#install.packages("stringi")
    library(lubridate)#install.packages("lubridate")
    library(RSQLite)#install.packages("RSQLite")
    library(DBI)#install.packages("DBI")

    library(ggplot2)#install.packages("ggplot2")

    library(RColorBrewer)#install.packages("RColorBrewer")
    library(shiny)# install.packages("shiny")
    library(DT)#install.packages("DT")
    library(shinyWidgets)#install.packages("shinyWidgets")
    library(bsplus)#install.packages("bsplus")
    library(plotly)#install.packages("plotly")
    library(shinydashboard)# install.packages("shinydashboard")
    library(shinydashboardPlus)# install.packages("shinydashboardPlus")
    # library(shinyjs)# install.packages("shinyjs")

    library(igraph)#install.packages("igraph")
    # library(ggraph)#install.packages("ggraph")
    # library(tidygraph)#install.packages("tidygraph")
    library(visNetwork)#install.packages("visNetwork")
    # library(network)#install.packages("network")
    # library(networkDynamic)#install.packages("networkDynamic")
    library(ndtv)#install.packages("ndtv")
    library(intergraph)#install.packages("intergraph")
    library(igraphdata)#install.packages("igraphdata")
    library(qgraph)#install.packages("qgraph")

    library(forcats)#install.packages("forcats")
    library(visdat)#install.packages("visdat")
    library(ggridges)#install.packages("ggridges")
    library(skimr)#install.packages("skimr")
    library(ggrepel)#install.packages("ggrepel")

    # install.packages("survival")
    # install.packages("cluster")
    # install.packages("rpart")
    # install.packages("Hmisc")
    # install.packages("qgraph")
    # install.packages("ggridges")

    library(tm) # install.packages("tm")
    library(tidytext)# install.packages("tidytext")

    # https://github.com/timelyportfolio/RBioFabric/issues/1
    library(Rcpp)#install.packages("Rcpp")
    library(devtools)#install.packages("devtools")
    library(htmlwidgets)#install.packages("htmlwidgets")
    library(jsonlite)#install.packages("jsonlite")
    library(RBioFabric) # devtools::install_github("jas1/RBioFabric")
    # devtools::install_github()RBioFabric_0.4
    library(assertive)#install.packages("assertive")
    library(testthat)#install.packages("testthat")
}
flog.info(paste0(log_prefix,"  - LOADED"))