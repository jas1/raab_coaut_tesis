# IMPORT LIBS ----------------------------------------------------------------------------

source(here("funcs","imports.r"),encoding = "UTF-8")

# FUNCIONES -------------------------------------------------------------------

# exportado a archivo funcs/funciones.r

source(here("funcs","funciones.r"),encoding = "UTF-8")

# GLOBALS ---------------------------------------------------------------------

source(here("funcs","globals.r"),encoding = "UTF-8")

# SHINY: UI -------------------------------------------------------------------

source(here("funcs","ui.r"),encoding = "UTF-8")

# SHINY: SERVER ---------------------------------------------------------------

source(here("funcs","server.r"),encoding = "UTF-8")

# SHINY: app invocation -------------------------------------------------------


shinyApp(ui = ui, server = server)
