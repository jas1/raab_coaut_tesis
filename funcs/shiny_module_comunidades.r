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

comunidades_ui <- function(id, # escencial para poder armar el componente
                           label = "Análisis de Comunidades") {
    
    ns <- NS(id) # namespace para como se va a llamar 
    
    # dt_autores_subgrafos_name_select <- paste0("input.","dt_autores_subgrafos_rows_selected" )
    # dt_autores_subgrafos_name_select_cond <- paste0('( ',dt_autores_subgrafos_name_select,'!= null && ',dt_autores_subgrafos_name_select,' != "" )')
    # dt_autores_subgrafos_name_select_cond_not <- paste0('!',dt_autores_subgrafos_name_select_cond)
    # 
    # dt_autores_subgrafos_rows_selected
    # 
    # print(dt_autores_subgrafos_name_select)
    # print(dt_autores_subgrafos_name_select_cond)
    # print(dt_autores_subgrafos_name_select_cond_not)
    # 
    # recordar todos los ids dben ponerse como ns('id_de_cosa')
    tagList( # dentro de taglist puedo escribir libremente como lo hice antes
        # - metricas de cada subgrafo
        br(),
        htmlOutput(outputId = ns('comunidades_result')),
        br(),
        bs_accordion(id = ns("estructura_subgrafo")) %>%
            # subgrafo estructura --------------------------------------------
        bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
            bs_append(title = "Estructura Comunidades", content = div(
                downloadButton (outputId = ns("dw_estructura_subgrafos"),
                                label = "Bajar métricas"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_estructura_subgrafos'))
            )) %>%
            # subgrafo - autores ---------------------------------------------------
        bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
            bs_append(title = "Autores por Comunidad", content = div(
                br(),
                downloadButton (outputId = ns("dw_autores_subgrafos"),
                                label = "Bajar autores"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_autores_subgrafos'))
            )) %>%
            # subgrafos - detalle ---------------------------------------------------
        bs_set_opts(panel_type = "info") %>%
            bs_append(title = "Detalle Comunidad seleccionada", content = div(
                conditionalPanel(
                    '!( input.dt_autores_subgrafos_rows_selected!= null && input.dt_autores_subgrafos_rows_selected != "" )',
                    ns=ns,
                    # dt_autores_subgrafos_name_select_cond_not, 
                                 br(),
                                 div(p("Debe seleccionar una comunidad en 'Autores por Comunidad'"))
                ), # fin debe seleccionar comunidad
                conditionalPanel(
                    '( input.dt_autores_subgrafos_rows_selected!= null && input.dt_autores_subgrafos_rows_selected != "" )',
                     ns=ns, 
                                 div(
                                     # TEXTMINING ARTICULOS - TITULOS ------------------------------------------
                                     
                                     textmining_ui(ns("textmining"))
                                     # h2('Artículos Comunidad'),
                                     # br(),
                                     # downloadButton (outputId = ns("dw_articulos"),
                                     #                 label = "Bajar artículos"),
                                     # br(),
                                     # br(),
                                     # DT::dataTableOutput(ns('dt_articulos'))
                                 ) # fin div detalle comunidad  
                )# fin conditional comunidad seleccionada
            )) %>% # fin bs_append detalle comunidad seleccionada. 
        # subgrafos - detalle nodos ---------------------------------------------------
        bs_set_opts(panel_type = "info") %>%
            bs_append(title = "Detalle nodos de la comunidad seleccionada", content = div(
                conditionalPanel(
                    '!( input.dt_autores_subgrafos_rows_selected!= null && input.dt_autores_subgrafos_rows_selected != "" )',
                    ns=ns,
                    # dt_autores_subgrafos_name_select_cond_not, 
                    br(),
                    div(p("Debe seleccionar un subgrafo en 'Autores por Subgrafo'"))
                ), # fin debe seleccionar comunidad
                conditionalPanel(
                    '( input.dt_autores_subgrafos_rows_selected!= null && input.dt_autores_subgrafos_rows_selected != "" )',
                    ns=ns, 
                    div(
                        # h2('Nodos Subgrafo'),
                        # br(),
                        # downloadButton (outputId = ns("dw_nodos_subgrafos"),
                        #                 label = "Bajar nodos"),
                        # br(),
                        # br(),
                        # DT::dataTableOutput(ns('dt_nodos_subgrafos'))
                        estructura_nodos_ui(ns("est_nodos_subgrafos"))
                        
                    ) # fin div detalle comunidad  
                )# fin conditional seleccion
                ) # fin del div del append
            )# fin bs_append detalle seleccion
    ) # fin taglist del componente
}

comunidades_server <- function(input, output, session, # parametros de shiny
                             current_grafo, # parametros del componente: grafo
                             current_db_articulos, # parametros del componente: base articulos
                             current_comunidad # parametro del componente: resultado de la ejecucion de comunidades
                             ) {
# ARMADO SUBGRAFOS ---------------------------------------------------------------    
    subgrafos_reactive <- reactive({
        # en el caso de comunidades me viene el resultado , tengo que armar algo que termine dando un listado ed subgrafos

        listado_comunidades <- list()
        for(i in 1:length(current_comunidad)){
            current_group <- igraph::groups(current_comunidad)[[i]]
            current_subgraph <- igraph::induced_subgraph(current_grafo,current_group) 
            listado_comunidades[[i]] <- current_subgraph
        }

        listado_comunidades #<- igraph::decompose.graph(current_grafo)
    })


# si no tengo la modularidad no puedo medir la calidad para comparar con otros algoritmos.
# calculo de modularidad --------------------------------------------------

    current_modularidad_reactive <- reactive({
        modularidad <- modularity(current_grafo, membership(current_comunidad))
        modularidad
    })
    
    current_modularidad_reactive_con_pesos <- reactive({
        modularidad <- modularity(current_grafo, membership(current_comunidad),E(current_grafo)$weight)
        modularidad
    })
    

    
    
# resultado info ----------------------------------------------------------
    
    output$comunidades_result <- renderUI ({
        
        # current_comunidad <- current_comunidad()
        
        mensaje_algo <- paste0('Para la búsqueda de comunidades se usó el Algoritmo: ', algorithm(current_comunidad))
        mensaje_cantidad <- paste0('<p>Se generaron: ', length(current_comunidad), ' comunidades.</p>')
        mensaje_modularidad <- paste0("<p>Modularidad del resultado es: ",current_modularidad_reactive(),'</p>')
        
        mensaje_modularidad_pesos <- paste0("<p>Modularidad considerando pesos es: ",current_modularidad_reactive_con_pesos(),'</p>',
                                            "<p>Recordar que los pesos estan dados por la fuerza de colaboración</p>")
        
        # infomap muestra diferente.
        if(str_detect(algorithm(current_comunidad),'Info')){
            mensaje_cantidad <- paste0('<p>Se generaron: ', code_len(current_comunidad), ' comunidades.</p>')
        }
        # recordar hay un comparador de comunidades, por si quiero comparar resultados de algoritmos.
        # se puede comparar quienes estan en las comunidaes.
        mensaje_final <- paste0(mensaje_algo,mensaje_cantidad,mensaje_modularidad,mensaje_modularidad_pesos)
        
        HTML(mensaje_final)
    }) 
# ESTRUCTURA ---------------------------------------------------------------

    
    subgrafos_estr_listado_reactive <- reactive({
        listados_componentes <- purrr::map(subgrafos_reactive(),calcular_estructura_sobre_grafo)
        names(listados_componentes) <- 1:length(listados_componentes)
        estr_componentes_list <- dplyr::bind_rows(listados_componentes, .id = 'names') %>% rename(componente_id=names)

        # sacamos las columnas de componentes, salvo la de componente_id, porque no tienen sentido
        # dado que cada componente tiene 1 solo componete xD , el mismo :p
        listado <- estr_componentes_list %>%  select(- dplyr::starts_with("componentes"))
        listado
    })

    output$dw_estructura_subgrafos <- downloadHandler( 
        filename = paste("comu_","estructura","_", Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(subgrafos_estr_listado_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$dt_estructura_subgrafos <- DT::renderDataTable({
        dt_out <- DT::datatable(
            subgrafos_estr_listado_reactive(),
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
# AUTORES ---------------------------------------------------------------
    autores_subgrafos_reactive <- reactive({
        listado_autores <- purrr::map(subgrafos_reactive(),function(x){ paste0(V(x)$label,collapse = ";") })
        names(listado_autores) <- 1:length(listado_autores) 
        
        autores_componentes_listado <- listado_autores%>% unlist() %>% as_tibble() %>% 
            tibble::rowid_to_column("componente_id") %>% 
            rename(autores=value) %>% 
            mutate(cantidad_autores=str_count(autores,";")+1) %>%
            select(componente_id,cantidad_autores,autores)
        
        autores_componentes_listado
    })
    
    output$dw_autores_subgrafos <- downloadHandler( 
        filename = paste("comu_","autores","_", Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(autores_subgrafos_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$dt_autores_subgrafos <- DT::renderDataTable({
        dt_out <- DT::datatable(
            autores_subgrafos_reactive(),
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="single")
        dt_out
    })

# ARTICULOS ---------------------------------------------------------------

    articulos_reactive <- reactive({

        current_com_selected <- autores_subgrafos_reactive()[input$dt_autores_subgrafos_rows_selected,]
        autores_comunidad <- str_split(current_com_selected[3],pattern = ';') %>% unlist()

        # print(autores_comunidad)
        # print(current_db_articulos)
        # write.csv(current_db_articulos, "articulos_ahora",row.names = FALSE,fileEncoding = "UTF-8")
        filtro_coautores <- current_db_articulos %>% 
            filter(autor %in% autores_comunidad) %>%
            select(autores,anio,titulo,url,cant_autores,fuerza_colaboracion) %>%
            mutate(articulo=paste0("<p><a target='_blank' href='",url,"'>",titulo,"</a></p>")) %>% 
            group_by(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% tally() %>% 
            select(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% 
            mutate(parsed_data=purrr::map(articulo,.f = xml2::read_html)) %>% 
            mutate(df_parsed=purrr::map(parsed_data,.f = function(x){
                url <- x %>% 
                    rvest::html_nodes(xpath = '//a') %>% 
                    rvest::html_attr("href")
                titulos <- x %>% 
                    rvest::html_nodes(xpath = '//a') %>% 
                    rvest::html_text()
                data.frame(url,titulos,stringsAsFactors = FALSE)
                
            })) %>% 
            unnest(df_parsed) %>% 
            select(-parsed_data)
        filtro_coautores
    })
    
    output$dw_articulos <- downloadHandler( 
        filename = paste("comu_","autores","_", Sys.Date(), '.csv', sep=''), content = function(file) {
            # write.csv(articulos_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
            
            data_dl_art <- articulos_reactive() %>% select(-articulo)
            
            write.csv(data_dl_art, file,row.names = FALSE,fileEncoding = "UTF-8")
            
        })
    
    output$dt_articulos <- DT::renderDataTable({
        dt_data <- articulos_reactive() %>% select(-url,-titulos)
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
    
# TEXT MINING -------------------------------------------------------------
    
    # textmining_ui(ns("textmining"))
    observeEvent(input$dt_autores_subgrafos_rows_selected,{
        
        current_com_selected <- autores_subgrafos_reactive()[input$dt_autores_subgrafos_rows_selected,]
        datos_seleccion <- current_com_selected
        
        callModule(textmining_server, "textmining",
                   articulos_reactive(),# parametros del componente: grafo
                   datos_seleccion # datos autores
        )
    })
    
# NODOS -------------------------------------------------------------------
    
    current_grafo_selected <- reactive({
        current_com_selected_id <- input$dt_autores_subgrafos_rows_selected
        grafo_reactive_tmp <- subgrafos_reactive()[[current_com_selected_id]]
        
        # print(igraph::vcount(grafo_reactive_tmp))

        grafo_reactive_tmp
    })

    observeEvent(input$dt_autores_subgrafos_rows_selected,{
        callModule(estructura_nodos_server, "est_nodos_subgrafos",
                   current_grafo_selected()# parametros del componente: grafo
        )
    })
    
}