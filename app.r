


# IMPORT LIBS ----------------------------------------------------------------------------
library("here")#install.packages("here")
library("futile.logger")
log_file_name <- here::here("logs",format(x=Sys.time(),format="%Y%m%d_%H%M%S_%Z_-_log.log"))
logger_name <- 'raab_coaut_tesis'
flog.appender(appender.file(log_file_name), name=logger_name)
flog.threshold(ERROR,name=logger_name)
flog.info("LOGGER INICIADO")
#install.packages("futile.logger")
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

# comunidades
source(here:::here("funcs","shiny_module_comunidades.r"),encoding = "UTF-8")

# comunidades
source(here:::here("funcs","shiny_module_articulos_text_mining.r"),encoding = "UTF-8")


# SHINY: UI -------------------------------------------------------------------

source(here:::here("funcs","ui.r"),encoding = "UTF-8")

# SHINY: SERVER ---------------------------------------------------------------

source(here:::here("funcs","server.r"),encoding = "UTF-8")

# SHINY: app invocation -------------------------------------------------------
#options(shiny.fullstacktrace = TRUE)
#options(shiny.reactlog=TRUE) 
shinyApp(ui = ui, server = server)
