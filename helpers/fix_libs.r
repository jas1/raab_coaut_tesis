# imports.r - migrando todos los imports aca

# install.packages("dplyr")
# install.packages("stringi")
# install.packages("stringr")
# install.packages("rebus")
library(stringi);library(stringr);library(rebus);
all_libs <- c('library(Rcpp)#install.packages("Rcpp")
library(devtools)#install.packages("devtools")
              library(htmlwidgets)#install.packages("htmlwidgets")
              library(jsonlite)#install.packages("jsonlite")
              library(RBioFabric) # devtools::install_github("jas1/RBioFabric")
              # devtools::install_github()RBioFabric_0.4
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
              library(qgraph)#install.packages("qgraph")
              
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
              library(ggridges)#install.packages("ggridges")
              library(skimr)#install.packages("skimr")
              library(ggrepel)#install.packages("ggrepel")
              library(shinydashboardPlus)
              # install.packages("survival")
              # install.packages("cluster")
              # install.packages("rpart")
              # install.packages("Hmisc")
              # install.packages("qgraph")
              # install.packages("ggridges")')

between_pattern <- rebus::OPEN_PAREN %R% "(.*?)"%R% rebus::CLOSE_PAREN
libs_filtered <- all_libs %>% 
    str_match_all(pattern = between_pattern) %>% 
    unlist() %>% 
    str_replace_all('"','') %>% 
    str_replace_all(rebus::OPEN_PAREN,'') %>% 
    str_replace_all(rebus::CLOSE_PAREN,'') %>% 
    unique() 

# RBioFabric = no , porque uso el de jas1
# "" = no , porque no.
libs_para_bajar <- libs_filtered[which(!(libs_filtered%in%c("","RBioFabric")))]
evaluate_txt_template <-'if(!require("RPL")) {install.packages("RPL"); require("RPL");}'
evaluate_txt_template_2 <-'install.packages("RPL")'
reqs <- purrr::map(libs_para_bajar,~gsub(pattern="RPL",
                                 x = evaluate_txt_template,
                                 replacement =.)) %>% unlist()

reqs_2 <- purrr::map(libs_para_bajar,~gsub(pattern="RPL",
                                         x = evaluate_txt_template_2,
                                         replacement =.)) %>% 
    unlist() 
# purrr::map(reqs,.f = function(param_parse){
#     parse(text=param_parse)
# })

write.table(reqs ,file = here::here('tmp','instalar_libs.r'),
            row.names = FALSE,
            quote = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")
write.table(reqs_2 ,file = here::here('tmp','instalar_libs_2.r'),
          row.names = FALSE,
          quote = FALSE,
          col.names = FALSE,
          fileEncoding = "UTF-8")
