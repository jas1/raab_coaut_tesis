para_citar <- c('base', 'tidyr', 'dplyr', 'purrr', 'stringr', 'stringi', 'lubridate',
                'RSQLite', 'DBI', 'igraph',  'ggplot2',  'qgraph',  'RColorBrewer',
                'visNetwork',  'shiny',  'DT',  'shinyWidgets',  'bsplus',  'plotly',
                'shinydashboard',  'ndtv',  'intergraph',  'assertive',  'testthat',
                'igraphdata',  'forcats',  'visdat',  'ggridges',  'skimr', 'ggrepel',
                'Rcpp',  'devtools',  'htmlwidgets',  'jsonlite',  'RBioFabric','here',
                'RefManageR','rvest','pdftools','tesseract','network','networkDynamic',
                'shinydashboardPlus')
# out <- purrr::map(para_citar,~citation(package = ., lib.loc = NULL))
# write(as.character(out), file = here::here("tmp",'citations.txt'), append = TRUE)

install.packages('pdftools')      
citation(package = 'pdftools', lib.loc = NULL)
install.packages('tesseract')      
citation(package = 'tesseract', lib.loc = NULL)
citation(package = 'network', lib.loc = NULL)    
citation(package = 'networkDynamic', lib.loc = NULL)
citation(package = 'shinydashboardPlus', lib.loc = NULL)


# 'network','networkDynamic'