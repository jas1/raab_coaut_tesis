# IMPORT LIBS ----------------------------------------------------------------------------
library(here)#install.packages("here")
source(here:::here("funcs","imports.r"),encoding = "UTF-8")

# FUNCIONES -------------------------------------------------------------------

# exportado a archivo funcs/funciones.r

source(here:::here("funcs","funciones.r"),encoding = "UTF-8")

# GLOBALS ---------------------------------------------------------------------

source(here:::here("funcs","globals.r"),encoding = "UTF-8")

# SHINY: UI -------------------------------------------------------------------

source(here:::here("funcs","ui.r"),encoding = "UTF-8")

# SHINY: SERVER ---------------------------------------------------------------

source(here:::here("funcs","server.r"),encoding = "UTF-8")

# SHINY: app invocation -------------------------------------------------------


shinyApp(ui = ui, server = server)
