# helper_app_shiny_nodos.r

# objetivo
# hacer esto esta para ayudarme a entender donde ando parado y que me falta
# hacer esto mientras proecso en mi cabeza como voy a responder 
# y donde tengo cabos sueltos
# para ahcerlo use parte de la app de demo
# agarre las seccioens que tenia tirados por todos lados. 
# hipotesis, secciones, 
# https://github.com/datastorm-open/visNetwork/blob/master/inst/shiny/src/server/manip_server.R
# https://github.com/datastorm-open/visNetwork/blob/master/inst/shiny/src/ui/manip_ui.R
# https://github.com/datastorm-open/visNetwork/blob/master/inst/shiny/server.R
# https://github.com/datastorm-open/visNetwork/blob/master/inst/shiny/ui.R

# escala de colores: 
# https://www.color-hex.com/color-palette/82472

# imports -----------------------------------------------------------------

require(shiny)
require(visNetwork)

# tratamiento -------------------------------------------------------------

partes_tesis <- tibble::tribble(
                    ~codigo, ~x, ~y,   ~color,                                                                                                                                                                                                                                                                                                                           ~nodo_desc,          ~tarea,             ~origen,                                                                                 ~detalle,
                       "h1",  1,  5, "ed705a",                                                                                                                                                                      "La red agregada total (desde 1996 a 2016) es de mundo pequeño puesto que es una característica presente en la mayoría de las redes de coautorías científicas.",     "hipotesis",        "Plan tesis",                                                          "mundo pequenio y libre escala",
                       "h2",  1, 10, "ed705a",                                                                                                                                                                  "La cantidad de autores aumenta en el tiempo debido al crecimiento de la disciplina en el país, lo que se refleja en un aumento en la cantidad de nodos en la red.",     "hipotesis",        "Plan tesis",                                                  "cuidado con crecimiento de disciplina",
                       "h3",  1, 15, "ed705a",                                                                                                                                                                                  "Los autores fundadores se refuerzan en el tiempo, lo cual se verá reflejado en mayores medidas de grado, intermediación y fuerza de colaboración.",     "hipotesis",        "Plan tesis",                                                "verificar en diferentes T las metricas.",
                       "h4",  1, 20, "ed705a",                                                                                                                                                                  "Los componentes más grandes absorben a los más pequeños a lo largo del tiempo, dado que autores periféricos pasan a trabajar en conjunto con autores principales.",     "hipotesis",        "Plan tesis",                        "si se abosrben componentes en tiempo, los links que se generan.",
                       "t1",  3,  1, "6e7dda",                                     "Desambiguación de nombres de autor, se considera un proceso necesario para el armado de la base de datos. Esto puede verse reflejado en el trabajo de Morel et al. (2009) [ 9 ] y una fundamentación más extensa de por qué realizar esta tarea se observa en el trabajo de Tang (2010) [ 10 ]",         "tarea",             "kumar",                                                                                     "--",
                       "t2",  3,  3, "6e7dda",                                                                                                                                                                                    "Visualización de la red. Se plantea tener diferentes puntos de vista que pueden ayudar con el análisis de la red complementario a las métricas.",         "tarea",             "kumar",                                                                                     "--",
                       "t3",  3,  5, "6e7dda",                                                                                                         "Análisis de mundo pequeño y red de libre escala (Small World & scale free) para la red agregada total y para cada instante temporal, para poder comparar la red obtenida con simulaciones basadas en sus particularidades.",         "tarea",             "kumar",                                                                                     "--",
                       "t4",  3,  7, "6e7dda",                                                                                                                                                                                 "El tamaño del componente gigante y su evolución en el tiempo. Este análisis brinda la posibilidad de ver qué tan cohesiva o fragmentada es la red.",         "tarea",             "kumar",                                                                                     "--",
                       "t5",  3,  9, "6e7dda",                                                                                                                           "El componente gigante podría significar la principal actividad de investigación; mientras que los otros componentes pueden ser agrupamientos especializados o sub-comunidades. (Fatt et al., 2010) [ 8 ]",         "tarea",             "kumar",                                                                                     "--",
                       "t6",  3, 11, "6e7dda",                                                                                                   "Búsqueda de Comunidades.  Este análisis permite evaluar la ubicación e interacciones entre los distintos grupos de investigación dentro de la red y buscarles un sentido a las comunidades dentro del contexto de la disciplina.",         "tarea",           "koseglu",                                                                                     "--",
                       "t7",  3, 13, "6e7dda",   "Métricas generales a fin de analizar por períodos de agregación y la red completa. De las métricas que se tendrán en cuenta se destacan: (a) artículos con más de un autor, (b) autores que participan en artículos con más de un autor, (c) índice de colaboración, siendo este último una relación entre las medidas (b) / (c)",         "tarea",           "koseglu",                                                                                     "--",
                       "t8",  3, 15, "6e7dda",                "Patrones de autoría, es decir, cuántos artículos por cantidad de autores existen en los diferentes periodos, dejando expuesto cuantos participan individualmente, en colaboración de a 2 autores, o más. Este análisis es útil para evaluar si existen tendencias de mayor trabajo en equipo a lo largo del tiempo.",         "tarea",           "koseglu",                                                                                     "--",
                       "t9",  3, 17, "6e7dda", "Comparación con otras redes de coautoría realizadas en otros trabajos utilizando métricas que están presentes en todos ellos por ejemplo: (a) artículos por autor, (b) coeficiente de clustering (transitividad), (c) tamaño del componente principal, (d) componente principal representado en porcentaje, y (e) distancia media.",         "tarea",           "koseglu",                                                                                     "--",
                       "s1",  5,  1, "21dfb9",                                                                                                                                                                                                                                                                                                           "Capitulo 1: introduccion", "seccion tesis", "Escritura – tesis",    "Establecer contexto, plantear problema, plantear plan de solucion, pie a capitulo 2",
                       "s2",  5,  3, "21dfb9",                                                                                                                                                                                                                                                                                                       "Capitulo 2: generacion Datos", "seccion tesis", "Escritura – tesis",     "intro de capitulo, materiales y metodos, resultados de procesos, resumen, pie al 3",
                       "s3",  5,  5, "21dfb9",                                                                                                                                                                                                                                                                                                 "Capitulo 3: generacion herramienta", "seccion tesis", "Escritura – tesis",               "intro del capitulo, materiales y metodos, resumen del capitulo, pie al 4",
                       "s4",  5,  7, "21dfb9",                                                                                                                                                                                                                                                                                           "Capitulo 4: analisis de red de coautoria", "seccion tesis", "Escritura – tesis", "intro del capitulo contestar hipotesis. Contestar objetivos. Resumen capitulo pie al 5",
                       "s5",  5,  9, "21dfb9",                                                                                                                                                                                                                                                                                                     "Capitulo 5: discucion & cierre", "seccion tesis", "Escritura – tesis",                          "Intro, discucion, cierre, trabajos futuros.  Agradecimientos.",
                       "s6",  5, 11, "21dfb9",                                                                                                                                                                                                                                                                                             "ANEXO: software y librerías utilizadas", "seccion tesis", "Escritura – tesis",                                                                   "creditos al software",
                       "s7",  5, 13, "21dfb9",                                                                                                                                                                                                                                                                                             "ANEXO: Detalle estandarización autores", "seccion tesis", "Escritura – tesis",                               "detalle de como se realizo la estandarizacion de autores",
                       "s8",  5, 15, "21dfb9",                                                                                                                                                                                                                                                                                                 "ANEXO: Detalle consideraciones PDF", "seccion tesis", "Escritura – tesis",                                                                   "detalle de los pdf\\",
                       "s9",  5, 17, "21dfb9",                                                                                                                                                                                                                                                                                                "ANEXO: Detalle procesamiento bibTEX", "seccion tesis", "Escritura – tesis",                                                                  "detalle de los bibtex",
                      "s10",  5, 19, "21dfb9",                                                                                                                                                                                                                                                                          "ANEXO: Detalle de algoritmos para búsqueda de comunidades", "seccion tesis", "Escritura – tesis",                                  "detalle de algoritmos para la busqueda de comunidades",
                       "a1",  7,  1, "e3f57a",                                                                                                                                                                                                                                                                                                          "cosideraciones: generales",   "seccion app",    "desarrollo app",                                                                        "consideraciones",
                       "a2",  7,  3, "e3f57a",                                                                                                                                                                                                                                                                                                               "consideraciones: EDA",   "seccion app",    "desarrollo app",                                                                  "analisis exploratorio",
                       "a3",  7,  5, "e3f57a",                                                                                                                                                                                                                                                                                                   "analisis estatico: visualizacion",   "seccion app",    "desarrollo app",                                                                          "visualizacion",
                       "a4",  7,  7, "e3f57a",                                                                                                                                                                                                                                                                                             "analisis estatico: articulos asociados",   "seccion app",    "desarrollo app",                                                                           "datos crudos",
                       "a5",  7,  9, "e3f57a",                                                                                                                                                                                                                                                                                                  "analisis estatico: estructura red",   "seccion app",    "desarrollo app",                                                                       "estructura grafo",
                       "a6",  7, 11, "e3f57a",                                                                                                                                                                                                                                                                                             "analisis estatico: comparacion modelos",   "seccion app",    "desarrollo app",                                                                                "modelos",
                       "a7",  7, 13, "e3f57a",                                                                                                                                                                                                                                                                                                     "analisis estatico: comunidades",   "seccion app",    "desarrollo app",                                                                            "comunidades",
                       "a8",  7, 15, "e3f57a",                                                                                                                                                                                                                                                                                          "analisis temporal: visualizacion dinamica",   "seccion app",    "desarrollo app",                                                                     "temporal evolucion",
                       "a9",  7, 17, "e3f57a",                                                                                                                                                                                                                                                                                           "analisis temporal: metricas red por anio",   "seccion app",    "desarrollo app",                                                                       "metricas anuales",
                      "a10",  7, 19, "e3f57a",                                                                                                                                                                                                                                                                                         "analisis temporal: metricas red acumuladas",   "seccion app",    "desarrollo app",                                                                    "metricas acumuladas",
                      "a11",  7, 21, "e3f57a",                                                                                                                                                                                                                                                                                     "analisis temporal: metricas nodos top N tiempo",   "seccion app",    "desarrollo app",                                                                         "metricas top n",
                      "a12",  7, 23, "e3f57a",                                                                                                                                                                                                                                                                          "analisis temporal: metricas nodos top N tiempo acumuladas",   "seccion app",    "desarrollo app",                                                                 "metricas top n totales"
                    ) %>% mutate(color=paste0("#",color))

inicial_edges <- tibble::tribble(
    ~from,   ~to, ~realizado,
     "a1",  "s3",          1,
     "a2",  "s3",          1,
     "a3",  "s3",          1,
     "a4",  "s3",          1,
     "a5",  "s3",          1,
     "a6",  "s3",          1,
     "a7",  "s3",          1,
     "a8",  "s3",          1,
     "a9",  "s3",          1,
    "a10",  "s3",          1,
    "a11",  "s3",          1,
    "a12",  "s3",          1,
     "a5",  "s4",          1,
     "a6",  "s4",          1,
     "a8",  "s4",          1,
     "a9",  "s4",          1,
    "a10",  "s4",          1,
    "a11",  "s4",          1,
     "h4",  "a8",          1,
     "h4",  "a5",          1,
     "h3",  "a5",          1,
     "h3", "a11",          1,
     "h2", "a10",          1,
     "h2",  "a9",          1,
     "h1",  "t3",          1,
     "h1",  "a6",          1,
     "h1",  "s1",          1,
     "h2",  "a2",          1,
     "h2",  "s1",          0,
     "h3",  "s1",          0,
     "h4",  "s1",          0,
     "h1",  "s4",          0,
     "h2",  "s4",          1,
     "h3",  "s4",          1,
     "h4",  "s4",          1,
     "t1",  "s2",          1,
     "t1",  "s7",          1,
     "t2",  "s3",          1,
     "t3",  "a6",          1,
     "t3",  "s4",          1,
     "t4",  "a8",          1,
     "t4",  "s4",          1,
     "t4",  "h4",          1,
     "t5",  "s4",          0,
     "t5",  "h4",          0,
     "t5",  "a4",          0,
     "t6", "s10",          0,
     "t6",  "a7",          0,
     "t7",  "a5",          1,
     "t7",  "h3",          1,
     "t8",  "a2",          1,
     "t8",  "h2",          1,
     "t8",  "s4",          1,
     "t9",  "s4",          1,
     "t5",  "t4",          0,
     "t6",  "t5",          0
    )

grafo_vacio <- make_empty_graph(n = 0, directed = FALSE) 

hipotesis <- partes_tesis %>% filter(str_detect(codigo,pattern = 'h')) %>% pull(codigo)
tareas <- partes_tesis %>% filter(str_detect(codigo,pattern = 't')) %>% pull(codigo)
secciones <- partes_tesis %>% filter(str_detect(codigo,pattern = 's')) %>% pull(codigo)
secciones_app <- partes_tesis %>% filter(str_detect(codigo,pattern = 'a')) %>% pull(codigo)

nodos <- partes_tesis %>% select(codigo,x,y,color,nodo_desc) %>% 
    mutate(title=nodo_desc,
           label=codigo)


# conexiones <- 
# grafo_resultado <- grafo_vacio +
#     igraph::vertices(hipotesis,color='red') +
#     igraph::vertices(tareas,color='blue') +
#     igraph::vertices(secciones,color='green')

grafo_resultado <- graph_from_data_frame(vertices = nodos,
                                         d= inicial_edges,
                                         directed = FALSE)
# plot(grafo_resultado)
# plot(grafo_resultado,layout=layout_manual)
# layout_manual <- partes_tesis %>%
#     select(x,y) %>% as.matrix()



# SHINY RECICLADO ---------------------------------------------------------


ui <- shiny::shinyUI(shiny::navbarPage(
    title = "Examples",
    
    shiny::tabPanel(
        title = "Manipulation in shiny",
        fluidRow(
            column(
                width = 12,
                visNetworkOutput("network_manip",height = "600px"),
                fluidRow(
                    column(
                        width = 6,
                        verbatimTextOutput("code_network_manip")
                    ),
                    column(
                        width = 3,
                        HTML("Using manipulation option, you can view current action in shiny
               with input$networkid_graphChange")
                    ),
                    column(
                        width = 3,
                        verbatimTextOutput('view_manip')
                    )
                )
            )
        )
    )# fin manipulation
))# fin UI

server <- shinyServer(function(input, output) {

        output$network_manip <- renderVisNetwork({
            # nodes <- data.frame(id = 1:3)
            # edges <- data.frame(from = c(1,2), to = c(1,3))
            
            visNetwork::visIgraph(grafo_resultado,idToLabel = FALSE) %>%
                visOptions(manipulation = TRUE) %>% 
                visIgraphLayout(layoutMatrix = layout_manual)
        })
        
        output$view_manip <- renderPrint({
            input$network_manip_graphChange
        })
        
        output$code_network_manip <- renderText({
            '
      visNetwork(nodes, edges)
        visOptions(manipulation = TRUE))
     '
        })
    })# FIN SERVER

shinyApp(ui = ui, server = server)