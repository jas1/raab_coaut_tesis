# shiny module: explorar componentes / subgrafos: 
# similar a lo de comunidades: 
# current grafo + current db articulos 
# extraer subgrafos segun criterio ( componentes :p )
# - metricas de cada subgrafo
# -- listado metricas
# -- data table metricas
# -- exportar metricas
# - detalle autores de cada subgrafo
# -- listado autores x subgrafo
# -- exportar detalles
# - detalle componente seleccionado
# -- exportar articulos asociados 

# se podria reciclar este modulo en la parte de comunidades
# la cosa es que tendria que parametrizar todos los titulos, 
# y pasarle un vector de titulos para las partes en particular de la UI / graficos

# aca lo que interesa mas alla de los componentes en general es EL COMPONENTE MAS GRANDE
# las metricas del mismo , los autores que participan, los articulos que participan 
# eventualmente las tematicas.

subgrafos_ui <- function(id, # escencial para poder armar el componente
                           label = "Análisis de subgrafo") {
    
    ns <- NS(id) # namespace para como se va a llamar 
    # recordar todos los ids dben ponerse como ns('id_de_cosa')
    tagList( # dentro de taglist puedo escribir libremente como lo hice antes
        # - metricas de cada subgrafo
        # -- listado metricas
        # -- data table metricas
        # -- exportar metricas
        # - detalle autores de cada subgrafo
        # -- listado autores x subgrafo
        # -- exportar detalles
        # - detalle componente seleccionado
        # -- exportar articulos asociados
    ) 
}

subgrafos_server <- function(input, output, session, # parametros de shiny
                             current_grafo, # parametros del componente: grafo
                             current_db_articulos # parametros del componente: base articulos
                             ) {
    
    # - metricas de cada subgrafo
    # -- listado metricas
    # -- data table metricas
    # -- exportar metricas
    # - detalle autores de cada subgrafo
    # -- listado autores x subgrafo
    # -- exportar detalles
    # - detalle componente seleccionado
    # -- exportar articulos asociados
}