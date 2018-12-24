# IMPORT LIBS ----------------------------------------------------------------------------
library(here)#install.packages("here")
source(here:::here("funcs","imports.r"),encoding = "UTF-8")

# FUNCIONES -------------------------------------------------------------------

# exportado a archivo funcs/funciones.r

source(here:::here("funcs","funciones.r"),encoding = "UTF-8")

# GLOBALS ---------------------------------------------------------------------

source(here:::here("funcs","globals.r"),encoding = "UTF-8")

# SHINY: MODULOS  -------------------------------------------------------------

# EDA 
source(here:::here("funcs","shiny_module_exploratorio_tablas.r"),encoding = "UTF-8")

# estructura nodos
source(here:::here("funcs","shiny_module_estructura_nodos.r"),encoding = "UTF-8")

# componentes
source(here:::here("funcs","shiny_module_subgrafos.r"),encoding = "UTF-8")

# modelos
source(here:::here("funcs","shiny_module_estructura_modelos.r"),encoding = "UTF-8")

# SHINY: UI -------------------------------------------------------------------

source(here:::here("funcs","ui.r"),encoding = "UTF-8")

# SHINY: SERVER ---------------------------------------------------------------

source(here:::here("funcs","server.r"),encoding = "UTF-8")

# SHINY: app invocation -------------------------------------------------------


shinyApp(ui = ui, server = server)
