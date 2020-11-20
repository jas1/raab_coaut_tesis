log_prefix <- "shiny_module_estructura_nodos.r"
# shiny_module_estructura_nodos
# bajar datos
# listado en crudo
# densidad de grado
# histogramas todas vars
# densidades todas vars
estructura_nodos_ui <- function(id, # escencial para poder armar el componente
                         label = "Estructura de Nodos") {
    
    ns <- NS(id) # namespace para como se va a llamar 
    
    # recordar todos los ids dben ponerse como ns('id_de_cosa')
    tagList( # dentro de taglist puedo escribir libremente como lo hice antes
        # - metricas de cada subgrafo
 
        tabsetPanel(type = "tabs",
                    id=ns("estructura_nodos_tabs"),
                    # bajar datos
                    # listado en crudo

# UI - tab DATOS ----------------------------------------------------------
                    
                    tabPanel("Datos",div(
                        br(),
                        downloadButton (outputId = ns("est_nodos_download"),
                                        label = "Bajar est. nodos"),
                        br(),
                        br(),
                        DT::dataTableOutput(ns('est_nodos_table'))
                    )),

# UI - Distribución de grado --------------------------------------------------

                    # densidad de grado
                    tabPanel("Distribución de grado",div(
                        plotlyOutput(ns('est_nodos_densidad'))
                    )),

                    # densidad de grado
                    tabPanel("Distribución de Grado ( log / log )",div(
                        plotlyOutput(ns('est_nodos_densidad_log_log'))
                    )),


# UI - histogramas variables ----------------------------------------------

                    # histogramas todas vars
                    tabPanel("Histograma variables",div(
                        pickerInput(
                            inputId = ns("est_nodos_histo_vars"), 
                            label = "Variables",
                            choices = estructura_red_nodos_vars_compara_simu_list,
                            options = list(
                                placeholder = 'Seleccionar Variable',
                                onInitialize = I('function() { this.setValue(""); }'),
                                'actions-box' = TRUE, 
                                size = 10,
                                'selected-text-format' = "count > 3",
                                'deselect-all-text' = "Ninguno",
                                'select-all-text' = "Todos",
                                'none-selected-text' = "Sin Selección",
                                'count-selected-text' = "{0} seleccionados."
                            ), 
                            multiple = FALSE
                        ),
                        br(),
                        plotlyOutput(ns('est_nodos_histogramas_vars'))
                    )),


# UI - densidades variables -----------------------------------------------
                    # densidades todas vars
                    tabPanel("Densidades variables",div(
                        pickerInput(
                            inputId = ns("est_nodos_densidades_vars"), 
                            label = "Variables",
                            choices = estructura_red_nodos_vars_compara_simu_list,
                            options = list(
                                placeholder = 'Seleccionar Variable',
                                onInitialize = I('function() { this.setValue(""); }'),
                                'actions-box' = TRUE, 
                                size = 10,
                                'selected-text-format' = "count > 3",
                                'deselect-all-text' = "Ninguno",
                                'select-all-text' = "Todos",
                                'none-selected-text' = "Sin Selección",
                                'count-selected-text' = "{0} seleccionados."
                            ), 
                            multiple = FALSE
                        ),
                        br(),
                        plotOutput(ns('est_nodos_densidades_vars'))
                    ))
        )# fin tabset panel
        
       
    ) # fin taglist del componente
}


estructura_nodos_server <- function(input, output, session, # parametros de shiny
                             current_grafo#, # grafo actual
) {

# SERVER: estructura nodos - DATOS ------------------------------------------------

    est_nodos_list_reactive <- reactive({
        grafo_reactive_tmp <- current_grafo
        
        # estructura_red_df <- metricas_nodos_grafo(grafo_reactive_tmp) %>%
        #     
        #     rename('Autor' = autor,
        #            'Grado' = degree,
        #            'Betweeness' = betweeness,
        #            'Eigen Centrality' = eigen_centrality,
        #            'Closeness' = closeness,
        #            'Page Rank' = page_rank,
        #            '# Triangulos' = count_triangles,
        #            'Fuerza Colaboración' = fuerza_colaboracion)
        estructura_red_df <- metricas_nodos_grafo(grafo_reactive_tmp)
        # print(igraph::vcount(current_grafo))
        
        
        estructura_red_df
    })

    estructura_red_grafo_dt <- reactive({
        
        nodos_reactive_renamed <- est_nodos_list_reactive() %>% 
                rename('Autor' = autor,
                       'Grado' = degree,
                       'Intermediación' = betweeness,
                       'Centrarlidad Autovector' = eigen_centrality,
                       'Cercanía' = closeness,
                       'Page Rank' = page_rank,
                       '# Triángulos' = count_triangles,
                       'Fuerza Colaboración' = fuerza_colaboracion)
        
        dt_return <- DT::datatable(options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                                   nodos_reactive_renamed,
                                   escape = FALSE,
                                   rownames = FALSE,
                                   selection = 'none') %>%
            formatRound('Intermediación',4) %>%
            formatRound('Centrarlidad Autovector',4) %>%
            formatRound('Cercanía',7) %>%
            formatRound('Page Rank',4) %>%
            formatRound('Fuerza Colaboración',4) 
        
        dt_return
    })
    
    output$est_nodos_table <- DT::renderDataTable({ 
        # ver inconexos, ver, top 10 , ver last 10 o last N , sin contar inconexos. 
        # todo eso sale de la tabla. 
        # hay que agregar tooltips de inteprretacion.
        estructura_red_grafo_dt()
    })
    
    # exportar nodos
    output$est_nodos_download <- downloadHandler( 
        filename = paste('est_nodos_data-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(est_nodos_list_reactive(), file,row.names = FALSE)
        })

# SERVER: estructura nodos - Distribución de Grado -------------------------------------
    
    est_nodos_dist_grados_reactive <- reactive({
        degree_distrib <- degree_distribution(current_grafo) %>% 
            as_tibble() %>% 
            rename(degree_distribution=value) %>% 
            tibble::rownames_to_column("grado") %>% 
            mutate(grado=as.integer(grado)) %>% 
            arrange(grado)
    })
    
    # est_nodos_densidad 
    output$est_nodos_densidad <- renderPlotly({
        pl <- est_nodos_dist_grados_reactive() %>% 
                ggplot(aes(x=grado,y=degree_distribution)) + 
                geom_col() +
                labs(title="Distribución de Grado",
                     y="probabilidad de grado")+
                theme_light()
        # pl
        ggplotly(pl)
    })
    
    # est_nodos_densidad_log_log 
    output$est_nodos_densidad_log_log <- renderPlotly({
        pl <- est_nodos_dist_grados_reactive() %>%
                ggplot(aes(x=grado,y=degree_distribution+1)) +
                geom_point() +
                scale_x_log10()+
                scale_y_log10()+
                labs(title="Distribución de Grado ( log / log )",
                     subtitle="distribución+1 para arreglo de infinito",
                     x="grado",
                     y="probabilidad de grado")+
                theme_light()
        # pl
        ggplotly(pl)
    })

# SERVER: estructura nodos - histogramas vars -----------------------------------
    
    est_nodos_histo_vars_opts_reactive <- reactive({
        # no toma globales, hay que pasarlo por parametro
        ret <- estructura_red_nodos_vars_compara_simu_list # es global
        # ret <- listado_variables_seleccion
        ret 
    })
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "est_nodos_histo_vars", 
                             choices = est_nodos_histo_vars_opts_reactive())
    })
    
    est_nodos_list_long_reactive <- reactive({
        metricas_nodos_long <- est_nodos_list_reactive() %>% 
            tidyr::gather(metrica,valor,-autor) %>% 
            as_tibble() 
        metricas_nodos_long
    })
    
    # est_nodos_histogramas_vars 
    
    output$est_nodos_histogramas_vars <- renderPlotly({

        # metrica_seleccionada <- "degree"
        # metrica_seleccionada <- "fuerza_colaboracion"
        # metrica_seleccionada <- "closeness"
        # metrica_seleccionada <- "count_triangles"
        metrica_seleccionada <- input$est_nodos_histo_vars
        
        pl <- est_nodos_list_long_reactive() %>% 
            filter(metrica==metrica_seleccionada) %>% 
            ggplot(aes(valor)) +
            geom_histogram() +
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 #x= metrica_seleccionada,
                 y= "cantidad nodos")+
            theme_light()
        # pl
        ggplotly(pl)
    })
# SERVER: estructura nodos - densidades vars ------------------------------------
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "est_nodos_densidades_vars", 
                             choices = est_nodos_histo_vars_opts_reactive())
    })
    
    # est_nodos_densidades_vars    
    output$est_nodos_densidades_vars <- renderPlot({
        
        # metrica_seleccionada <- "degree"
        # metrica_seleccionada <- "fuerza_colaboracion"
        # metrica_seleccionada <- "closeness"
        # metrica_seleccionada <- "count_triangles"
        metrica_seleccionada <- input$est_nodos_densidades_vars
        
        pl <-  est_nodos_list_long_reactive() %>% 
            filter(metrica==metrica_seleccionada) %>% 
            ggplot(aes(valor)) +
            geom_density_line() +
            labs(title=paste0("Densidad de ",metrica_seleccionada),
                # x= metrica_seleccionada,
                 y= "cantidad nodos")+
            theme_light()
        pl
        # ggplotly(pl)
    })
}
flog.info(paste0(log_prefix,"  - LOADED"))