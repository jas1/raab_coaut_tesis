#shiny module estructura - simulacion / modelos

estructura_modelos_ui <- function(id, # escencial para poder armar el componente
                                label = "Comparación con modelos") {
    
    ns <- NS(id) # namespace para como se va a llamar 
    
    # recordar todos los ids dben ponerse como ns('id_de_cosa')
    tagList(
        # 1 parrafo superior que recuerde los valores especificos para este grafo
        
        # 1 tabset panel
        # 1 panel random
        # 1 panel smallworld
        # 1 panel free scale ( barabasi albert)
        bs_accordion(id = ns("main_accordeon")) %>%
# UI - DATOS RED ---------------------------------------------------------------
        bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
        bs_append(title = "Datos Red", content = div(
            # htmlOutput(ns('datos_red'))
            p("Datos de la red actual"),
            DT::dataTableOutput(ns('datos_red'))
            )) %>%  # fin div datos de la red / append
# UI - MODELOS ---------------------------------------------------------------
        bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
        bs_append(title = "Modelos", content = div(
            tabsetPanel(type = "tabs",
                        id=ns("estructura_modelos_tabs"),
                        
                        # UI - MODELO ERDOS_RENYI - RANDOM NET -----------------------------------------
                        tabPanel("Red aleatoria",div(
                            br(),
                            div(
                                p("Para la ejecución de una red aleatoria se usa el modelo de Erdos-Renyi basado en nodos y aristas.")
                            ),# fin div disclaimer
                            br(),
                            bs_accordion(id = ns("modelo_random_accordeon")) %>%
                                # modelo parametros --------------------------------------------
                            bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Párametros", content = div(
                                    numericInput(inputId = ns("random_param_iter"),
                                                 label="Iteraciones",min=1,max=1200,value = 100),
                                    numericInput(inputId=ns("random_param_vcount"),
                                                 label="Cant. Vértices.",min=0,max=1200,value = 100),
                                    numericInput(inputId=ns("random_param_ecount"),
                                                 label="Cant. Aristas",min=0,max=10000,value = 100),
                                    actionButton(inputId=ns("random_param_update"),label="Ejecutar modelo"),
                                    actionButton(inputId=ns("random_param_sugerencia"),label="Parámetros sugeridos")
                                )) %>% # fin parametros 
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Histogramas", content = div(
                                    pickerInput(
                                        inputId = ns("modelo_random_histo_vars_sel"), 
                                        label = "Variables",
                                        choices = '',
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
                                    plotOutput(ns("modelo_random_histo_vars"))
                                )) %>% # fin histogramas
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Datos simulación", content = div(
                                    br(),
                                    downloadButton (outputId = ns("modelo_random_datos_simu_DL"), label = "Bajar simulación"),
                                    br(),
                                    DT::dataTableOutput(ns('modelo_random_datos_simu_DT'))
                                ))# fin bajada datos
                        )),# fin tab panel / div de aleatoria
                        # UI - MODELO Watts-Strogatz - Small world -----------------------------------------
                        tabPanel("Mundo pequeño",div(
                            
                            br(),
                            div(
                                p("Para la ejecución de una red mundo pequeño se usa el modelo de Watts-Strogatz")
                            ),# fin div disclaimer
                            br(),
                            bs_accordion(id = ns("modelo_sw_accordeon")) %>%
                                # modelo parametros --------------------------------------------
                            bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Párametros", content = div(
                                    numericInput(inputId = ns("sw_param_iter"),
                                                 label="Iteraciones",min=1,max=1200,value = 100),
                                    numericInput(inputId=ns("sw_param_vcount"),
                                                 label="Cant. Vértices.",min=0,max=1200,value = 100),
                                    numericInput(inputId=ns("sw_vecindad"),
                                                 label="Vecindad",min=0,max=100,value = 5),
                                    sliderInput(inputId=ns("sw_prob_reescritura"),
                                                label="Prob reescritura vértices",
                                                min=0,max=1, step = 0.005,
                                                value = 0.05),
                                    actionButton(inputId=ns("sw_param_update"),label="Ejecutar modelo"),
                                    actionButton(inputId=ns("sw_param_sugerencia"),label="Parámetros sugeridos")
                                )) %>% # fin parametros 
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Histogramas", content = div(
                                    pickerInput(
                                        inputId = ns("modelo_sw_histo_vars_sel"), 
                                        label = "Variables",
                                        choices = '',
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
                                    plotOutput(ns("modelo_sw_histo_vars"))
                                )) %>% # fin histogramas
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Datos simulación", content = div(
                                    br(),
                                    downloadButton (outputId = ns("modelo_sw_datos_simu_DL"), label = "Bajar simulación"),
                                    br(),
                                    DT::dataTableOutput(ns('modelo_sw_datos_simu_DT'))
                                ))# fin bajada datos
                            
                            
                        )),# fin tab panel / div mundo pequeño
                        # UI - MODELO Barabasi-Albert - SCALE FREE  -----------------------------------------
                        tabPanel("Libre escala",div(
                            
                            br(),
                            div(
                                p("Para la ejecución de una red libre escala se usa el modelo de Barabasi-Albert")
                            ),# fin div disclaimer
                            br(),
                            bs_accordion(id = ns("modelo_ba_accordeon")) %>%
                                # modelo parametros --------------------------------------------
                            bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Párametros", content = div(
                                    numericInput(inputId = ns("ba_param_iter"),
                                                 label="Iteraciones",min=1,max=1200,value = 100),
                                    numericInput(inputId=ns("ba_param_vcount"),
                                                 label="Cant. Vértices.",min=0,max=1200,value = 100),
                                    numericInput(inputId=ns("ba_param_edges_step"),
                                                 label="Aristas por paso",min=0,max=100,value = 5),
                                    sliderInput(inputId=ns("ba_param_pow_pref_att"),
                                                label="Exponente conexión preferencial",
                                                min=0,max=10, step = 0.01,
                                                value = 0.05),
                                    actionButton(inputId=ns("ba_param_update"),label="Ejecutar modelo"),
                                    actionButton(inputId=ns("ba_param_sugerencia"),label="Parámetros sugeridos")
                                )) %>% # fin parametros 
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Histogramas", content = div(
                                    pickerInput(
                                        inputId = ns("modelo_ba_histo_vars_sel"), 
                                        label = "Variables",
                                        choices = '',
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
                                    plotOutput(ns("modelo_ba_histo_vars"))
                                )) %>% # fin histogramas
                                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                bs_append(title = "Datos simulación", content = div(
                                    br(),
                                    downloadButton (outputId = ns("modelo_ba_datos_simu_DL"), label = "Bajar simulación"),
                                    br(),
                                    DT::dataTableOutput(ns('modelo_ba_datos_simu_DT'))
                                ))# fin bajada datos
                            
                            
                        ))# fin tab panel / div mundo pequeño
            )# fin  tabsetPanel
        )) %>%  # fin div modelos / append
    # UI - Verificaciones ---------------------------------------------------------------
        bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
        bs_append(title = "Validaciones", content = div(
            
            tabsetPanel(type = "tabs",
                        id=ns("estructura_modelos_tabs"),
                        
# UI - VERIFICACION - SMALL WORLD -----------------------------------------
                        tabPanel("Mundo Pequeño",div(
                            
                            conditionalPanel("input.random_param_update",
                                             div(
# UI - small world - Parametros -------------------------------------------

                                                 bs_accordion(id = ns("small_world_verificacion")) %>%
                                                     bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                                     bs_append(title = "Parámetros", content = div(
                                                         uiOutput(ns("small_world_params"))
                                                     )) %>%
# UI - small world - Verificacion 1 -------------------------------------------
                                                     bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                                     bs_append(title = "Validación 1", content = div(
                                                         uiOutput(ns("small_world_validacion_1"))
                                                     )) %>%
# UI - small world - Verificacion 2 -------------------------------------------
                                                    bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                                    bs_append(title = "Validación 2", content = div(
                                                        uiOutput(ns("small_world_validacion_2"))
                                                    )) %>%
# UI - small world - Comparacion -------------------------------------------
                                                    bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                                    bs_append(title = "Comparación de Validaciones", content = div(
                                                        uiOutput(ns("small_world_comparacion_valores_validaciones"))
                                                    ))
                                                 
                                                 # uiOutput(ns("small_world"))                                                 
                                             ),ns=ns),
                            conditionalPanel("!input.random_param_update",
                                             div(
                                                 p("para validar Mundo pequeño se debe ejecutar la simulación de redes aleatorias.(En el tab de Modelos / Aleatorio). ")
                                             ),ns=ns)
                        )) ,# fin small world
# UI - VERIFICACION - SCALE FREE -----------------------------------------
                        tabPanel("Libre Escala",div(

                            uiOutput(ns("scale_free"))
                            
                        )) # fin scale free
            )# fin tabset panel verificaciones
        ))# fin verificaciones div / append
    )# fin taglist del modulo

}# fun funcion UI


estructura_modelos_server <- function(input, output, session, # parametros de shiny
                                    current_grafo,#, # grafo actual
                                    model_list_global_vars # variable global lista para selecciones
) {

    
# SERVER - DATOS RED ---------------------------------------------------------------
    # reactive para luego poder extraer aca los valores default para los paramtetros de los modelos
    valores_para_red_reactive <- reactive({
        valores_para_red <- calcular_metricas(current_grafo)
        valores_para_red
    })
    # para mostrar los datos default de la red
    output$datos_red <- DT::renderDataTable ({

        mostrar <- valores_para_red_reactive() %>% 
            rename("transitividad"=transitivity, 
                   "diámetro"=diameter,
                   "dist. camino medio"=avg_path,
                   "ley pot. alfa"=pow_law_alpha,
                   "ley pot. signif. test KS"=pow_law_ks_p,
                   "alfa > 1"=pow_law_greater_than_2,
                   "test KS significativo"=pow_law_ks_p_is_signif)

        DT::datatable(
            options = list(dom = 't',
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            mostrar,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none')

    })
# SERVER - MODELO ERDOS_RENYI - RANDOM NET -----------------------------------------
    # observeEvent(input$random_param_update,{
    observeEvent(input$random_param_sugerencia,{
        # current_grafo <- g_aut
        n_v <- igraph::vcount(current_grafo)
        n_e <- igraph::ecount(current_grafo)
        
        updateNumericInput(session, "random_param_iter", value = 1000)
        updateNumericInput(session, "random_param_vcount", value = n_v)
        updateNumericInput(session, "random_param_ecount", value = n_e)    
    })

    modelo_random_semilla_reactive <- reactive({
        semilla <- 12345
        semilla
    })
    
    
    modelo_random_ejecucion <- reactive({
        req(input$random_param_update)
        
        # ret <- simular_random(iter=100,vertex_count=n_v,edge_count=n_e,semilla=12345)
        ret <- simular_random(iter=input$random_param_iter,
                       vertex_count=input$random_param_vcount,
                       edge_count=input$random_param_ecount,
                       semilla=modelo_random_semilla_reactive())# arreglar la semilla 
        ret
    })
    modelo_random_params_reactive <- reactive({
        param_list <- data.frame(param_iter=input$random_param_iter,
                                 param_vertex_count=input$random_param_vcount,
                                 param_edge_count=input$random_param_ecount,
                                 param_semilla=modelo_random_semilla_reactive())
        param_list
    })
    
    modelo_random_resumen_reactive <- reactive({
        param_list <- modelo_random_params_reactive()
        simulacion_random <- modelo_random_ejecucion()
        simulaciones_resumen <- simulaciones_to_resumen_ejecucion(simulacion_random,param_list)
        simulaciones_resumen
    })
    
# 
#     output$modelo_random <- renderPlot({
#         modelo_random_ejecucion
#     })
#     
    modelo_random_histo_vars_opts_reactive <- reactive({
        # no toma globales, hay que pasarlo por parametro
        # ret <- estructura_red_nodos_vars_compara_simu_list # es global
        # ret <- colnames(valores_para_red_reactive())
        ret <- model_list_global_vars
        # ret <- listado_variables_seleccion
        ret 
    })
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "modelo_random_histo_vars_sel", 
                             choices = modelo_random_histo_vars_opts_reactive())
    })
    
    modelo_random_ejecucion_long <- reactive({
        metricas_nodos_long <- modelo_random_ejecucion() %>%
        # ret2 <- ret %>% 
            tidyr::gather(metrica,valor,-param_iter,-param_vertex_count,-param_edge_count) %>% 
            as_tibble() 
        metricas_nodos_long
    })

    output$modelo_random_histo_vars <- renderPlot({
        req(input$modelo_random_histo_vars_sel)
        
        # metrica_seleccionada <- "transitivity"
        metrica_seleccionada <- input$modelo_random_histo_vars_sel

        data_red <- modelo_random_ejecucion_long() %>% 
            #ret2 %>% 
            filter(metrica==metrica_seleccionada)

        data_current <- valores_para_red_reactive() %>% 
            tidyr::gather(metrica,valor) %>% 
            filter(metrica==metrica_seleccionada)
        
        pl_to_build <- data_red %>% ggplot(aes(valor)) +
            geom_histogram()+ 
            geom_vline(data=data_current,
                       aes(xintercept=valor,colour="red"),
                       show.legend = FALSE) + 
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "cantidad de simulaciones")+
            theme_light()
        
        pl_df <- ggplot_build(pl_to_build)
        y_scale <- pl_df$layout$panel_scales_y[[1]]  # Extrae la escala en y
        y_limits <- y_scale$get_limits()   # Se fija los límites
        y_mid <- max(y_limits)/2           # Agarra el punto medio.

        pl <- pl_to_build+geom_text(data=data_current, 
                  aes(x=valor, 
                      label=paste0(metrica_seleccionada,": ",round(valor,5),"\n"), 
                      y=y_mid), 
                  colour="red", 
                  angle=90)
        
        # ggplotly(pl)
        pl
        
    })
    
    output$modelo_random_datos_simu_DL <- downloadHandler( 
        filename = paste("modelos_","random","_", Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(modelo_random_ejecucion(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$modelo_random_datos_simu_DT <- DT::renderDataTable({
        
        mostrar <- modelo_random_ejecucion() %>% 
            select(-starts_with("param_")) %>% 
            rename("transitividad."=transitivity, 
                   "diámetro"=diameter,
                   "dist.camino medio"=avg_path,
                   "ley pot. alfa"=pow_law_alpha,
                   "ley pot. signif. test KS"=pow_law_ks_p,
                   "alfa > 1"=pow_law_greater_than_2,
                   "test KS significativo"=pow_law_ks_p_is_signif)
        
        DT::datatable(
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            mostrar,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none')
    })

# SERVER - MODELO Watts-Strogatz - Small world -----------------------------------------
    
    # observeEvent(input$random_param_update,{
    observeEvent(input$sw_param_sugerencia,{
        # current_grafo <- g_aut
        n_v <- igraph::vcount(current_grafo)

        updateNumericInput(session, "sw_param_iter", value = 1000)    
        updateNumericInput(session, "sw_param_vcount", value = n_v)
        updateNumericInput(session, "sw_vecindad", value = 5)
        updateSliderInput(session, "sw_prob_reescritura", value = 0.05)  
    })

    # 
    #     output$modelo_random <- renderPlot({
    #         modelo_random_ejecucion
    #     })
    #     
    modelo_sw_histo_vars_opts_reactive <- reactive({
        # no toma globales, hay que pasarlo por parametro
        # ret <- estructura_red_nodos_vars_compara_simu_list # es global
        # ret <- colnames(valores_para_red_reactive())
        ret <- model_list_global_vars
        # ret <- listado_variables_seleccion
        ret 
    })
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "modelo_sw_histo_vars_sel", 
                             choices = modelo_sw_histo_vars_opts_reactive())
    })
    
    modelo_sw_ejecucion <- reactive({
        req(input$sw_param_update)
        
        # ret <- simular_random(iter=100,vertex_count=n_v,edge_count=n_e,semilla=12345)
        ret <- simular_small_world(iter = input$sw_param_iter,
                                   vertex_count = input$sw_param_vcount,
                                   vecindad = input$sw_vecindad,
                                   prob_reescritura = input$sw_prob_reescritura,
                                   semilla = 12345)# arreglar la semilla 
        ret
    })

    modelo_sw_ejecucion_long <- reactive({
        metricas_nodos_long <- modelo_sw_ejecucion() %>%
            # ret2 <- ret %>% 
            tidyr::gather(metrica,valor,
                          -param_iter,-param_vertex_count,
                          -param_vecindad,-param_prob_reescritura) %>% 
            as_tibble() 
        metricas_nodos_long
    })
    
    output$modelo_sw_histo_vars <- renderPlot({
        req(input$modelo_sw_histo_vars_sel)
        
        # metrica_seleccionada <- "transitivity"
        metrica_seleccionada <- input$modelo_sw_histo_vars_sel
        
        # print(metrica_seleccionada)
        
        data_red <- modelo_sw_ejecucion_long() %>% 
            #ret2 %>% 
            filter(metrica==metrica_seleccionada)
        
        data_current <- valores_para_red_reactive() %>% 
            tidyr::gather(metrica,valor) %>% 
            filter(metrica==metrica_seleccionada)
        
        pl_to_build <- data_red %>% ggplot(aes(valor)) +
            geom_histogram()+ 
            geom_vline(data=data_current,
                       aes(xintercept=valor,colour="red"),
                       show.legend = FALSE) + 
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "cantidad de simulaciones")+
            theme_light()
        
        pl_df <- ggplot_build(pl_to_build)
        y_scale <- pl_df$layout$panel_scales_y[[1]]  # Extrae la escala en y
        y_limits <- y_scale$get_limits()   # Se fija los límites
        y_mid <- max(y_limits)/2           # Agarra el punto medio.
        
        pl <- pl_to_build+geom_text(data=data_current, 
                                    aes(x=valor, 
                                        label=paste0(metrica_seleccionada,": ",round(valor,5),"\n"), 
                                        y=y_mid), 
                                    colour="red", 
                                    angle=90)
        # ggplotly(pl)
        pl
        
    })
    
    output$modelo_sw_datos_simu_DL <- downloadHandler( 
        filename = paste("modelos_","small_world","_", Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(modelo_sw_ejecucion(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$modelo_sw_datos_simu_DT <- DT::renderDataTable({
        
        mostrar <- modelo_sw_ejecucion() %>% 
            select(-starts_with("param_")) %>% 
            rename("transitividad."=transitivity, 
                   "diámetro"=diameter,
                   "dist.camino medio"=avg_path,
                   "ley pot. alfa"=pow_law_alpha,
                   "ley pot. signif. test KS"=pow_law_ks_p,
                   "alfa > 1"=pow_law_greater_than_2,
                   "test KS significativo"=pow_law_ks_p_is_signif)
        
        DT::datatable(
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            mostrar,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none')
    })
 
# SERVER - MODELO Barabasi-Albert - SCALE FREE  -----------------------------------------
    observeEvent(input$ba_param_sugerencia,{
        # current_grafo <- g_aut
        n_v <- igraph::vcount(current_grafo)
        pl_alfa <- valores_para_red_reactive()$pow_law_alpha
        
        updateNumericInput(session, "ba_param_iter", value = 1000)    
        updateNumericInput(session, "ba_param_vcount", value = n_v)
        updateNumericInput(session, "ba_param_edges_step", value = 5)
        updateSliderInput(session, "ba_param_pow_pref_att", value = pl_alfa)  
    })

    modelo_ba_histo_vars_opts_reactive <- reactive({
        # no toma globales, hay que pasarlo por parametro
        # ret <- estructura_red_nodos_vars_compara_simu_list # es global
        # ret <- colnames(valores_para_red_reactive())
        ret <- model_list_global_vars
        # ret <- listado_variables_seleccion
        ret 
    })
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "modelo_ba_histo_vars_sel", 
                             choices = modelo_ba_histo_vars_opts_reactive())
    })
    
     modelo_ba_ejecucion <- reactive({
        req(input$ba_param_update)
        
        # ret <- simular_random(iter=100,vertex_count=n_v,edge_count=n_e,semilla=12345)
        ret <- simular_scale_free(iter =input$ba_param_iter ,
                                  vertex_count = input$ba_param_vcount,
                                  poder_pref_attach = input$ba_param_pow_pref_att,
                                  m_edges_per_step = input$ba_param_edges_step,
                                  semilla = 12345)# arreglar la semilla 

        ret
    })  
     
     modelo_ba_ejecucion_long <- reactive({
         metricas_nodos_long <- modelo_ba_ejecucion() %>%
             # ret2 <- ret %>% 
             tidyr::gather(metrica,valor,
                           -param_iter,-param_vertex_count,
                           -param_poder_pref_attach,-param_m_edges_per_step) %>% 
             as_tibble() 
         metricas_nodos_long
     })
     
     output$modelo_ba_histo_vars <- renderPlot({
         req(input$modelo_ba_histo_vars_sel)
         
         # metrica_seleccionada <- "transitivity"
         metrica_seleccionada <- input$modelo_ba_histo_vars_sel
         
         # print(metrica_seleccionada)
         
         data_red <- modelo_ba_ejecucion_long() %>% 
             #ret2 %>% 
             filter(metrica==metrica_seleccionada)
         
         data_current <- valores_para_red_reactive() %>% 
             tidyr::gather(metrica,valor) %>% 
             filter(metrica==metrica_seleccionada)
         
         pl_to_build <- data_red %>% ggplot(aes(valor)) +
             geom_histogram()+ 
             geom_vline(data=data_current,
                        aes(xintercept=valor,colour="red"),
                        show.legend = FALSE) + 
             labs(title=paste0("Histograma de ",metrica_seleccionada),
                  x= metrica_seleccionada,
                  y= "cantidad de simulaciones")+
             theme_light()
         
         pl_df <- ggplot_build(pl_to_build)
         y_scale <- pl_df$layout$panel_scales_y[[1]]  # Extrae la escala en y
         y_limits <- y_scale$get_limits()   # Se fija los límites
         y_mid <- max(y_limits)/2           # Agarra el punto medio.
         
         pl <- pl_to_build+geom_text(data=data_current, 
                                     aes(x=valor, 
                                         label=paste0(metrica_seleccionada,": ",round(valor,5),"\n"), 
                                         y=y_mid), 
                                     colour="red", 
                                     angle=90)
         # ggplotly(pl)
         pl
         
     })
     
     output$modelo_ba_datos_simu_DL <- downloadHandler( 
         filename = paste("modelos_","scale_free","_", Sys.Date(), '.csv', sep=''), content = function(file) {
             write.csv(modelo_ba_ejecucion(), file,row.names = FALSE,fileEncoding = "UTF-8")
         })
     
     output$modelo_ba_datos_simu_DT <- DT::renderDataTable({
         
         mostrar <- modelo_ba_ejecucion() %>% 
             select(-starts_with("param_")) %>% 
             rename("transitividad."=transitivity, 
                    "diámetro"=diameter,
                    "dist.camino medio"=avg_path,
                    "ley pot. alfa"=pow_law_alpha,
                    "ley pot. signif. test KS"=pow_law_ks_p,
                    "alfa > 1"=pow_law_greater_than_2,
                    "test KS significativo"=pow_law_ks_p_is_signif)
         
         DT::datatable(
             options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
             mostrar,
             escape = FALSE,
             rownames = FALSE,
             selection = 'none')
     })
# SERVER - SMALL WORLD ---------------------------------------------------------------


# SERVER - SMALL WORLD - DATOS ------------------------------------------

     small_world_reactive_data <- reactive({
         req(input$random_param_update)
         valores_para_red <- valores_para_red_reactive()
         params <-  modelo_random_params_reactive()
         simulaciones_resumen_random <- modelo_random_resumen_reactive()
         
         param_long <- params %>% 
             tidyr::gather(parametro,valor) %>% 
             as_tibble() 
         # texto_params <-param_long %>% mutate(texto = paste("<p><strong>",str_replace(parametro,"param_",""),"</strong>",valor,"</p>")) %>% pull(texto) %>% paste(collapse = "\n")
         
         delta_net <- valores_para_red$avg_path / simulaciones_resumen_random$mean_avg_path_length
         gamma_net <- valores_para_red$transitivity / simulaciones_resumen_random$mean_transitivity
         
         es_small_world <- round(delta_net) == 1  &  gamma_net > 1 
         
         paste0(param_long$parametro,": ",param_long$valor,collapse = "\n")
         
         # validacion 2. solo control interaciones y  lo & hi confidencia. que le dejamos lo default
         # para generar los 1000 el de smallworldness utiliza: degree.sequence.game
         # https://www.rdocumentation.org/packages/igraph/versions/0.4.1/topics/degree.sequence.game
         smallworldness_grafo <- qgraph::smallworldness(x=current_grafo,B = params %>% pull(param_iter) )
         es_mayor_a_1 <- smallworldness_grafo > 1
         es_mayor_a_3 <- smallworldness_grafo > 3
         parametros_sw <- paste0(param_long$parametro,": ",param_long$valor,collapse = "\n")
         
         ret_list <- list('parametros_sw'=parametros_sw,
                          'delta_net'=delta_net,
                          'gamma_net'=gamma_net,
                          'es_small_world'=es_small_world,
                          'smallworldness_grafo'=smallworldness_grafo,
                          'es_mayor_a_1'=es_mayor_a_1,
                          'es_mayor_a_3'=es_mayor_a_3,
                          'valores_para_red'=valores_para_red,
                          'simulaciones_resumen_random'=simulaciones_resumen_random)
         ret_list
         
     })
     
# SERVER - SMALL WORLD - Parametros ------------------------------------------
     
     output$small_world_params <- renderUI({
         resultado <- div(
             p("comparado con los parametros de simulación aleatoria: "),
             # p(a("para recordar",href="https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network")),
             p("(",em("se sugiere configurar el modelo en Modelos: Red aleatoria"),")"),
             br(),
             div(small_world_reactive_data()$parametros_sw),
             br())
         resultado
     })
     
# SERVER - SMALL WORLD - validacion 1 ------------------------------------------
     
     output$small_world_validacion_1 <- renderUI({
         resultado <- div(
             h2("Validación 1:"),
             p("para validar small world se tomó el criterio de comparar la red actual contra redes aleatorias ( lo seleccionado en el tab de Modelos / Aleatorio). "),
             p("la comparación se lleva a cabo en el promedio del camino mas corto VS la media del promedio del camino más corto de las N redes simuladas."),             
             p("para mas detalles ver: ",a(href="https://doi.org/10.1063/1.3360561","Bialonski (2010)")," donde se detalla el procedimiento."),
             
             p(strong("Mundo pequeño: ")," si Delta ~=1 y Gamma > 1"),
             p(strong('Delta = '),'L/Lr. y ',strong('Gamma = '),"C/Cr."),
             p("L: promedio camino más corto "),
             p("Lr: promedio de promedio camino más corto de las simulaciones"),
             p("C: transitivity "),
             p("Cr: promedio de transitivity de las simulaciones"),
             p(strong("Delta: "),small_world_reactive_data()$delta_net),
             p(strong("Gamma: "),small_world_reactive_data()$gamma_net),
             p(strong("Es mundo pequeño: "),if_else(small_world_reactive_data()$es_small_world,"SI","NO")),
             br())
         resultado
     })
     
# SERVER - SMALL WORLD - Validacion 2 ------------------------------------------
     
     output$small_world_validacion_2 <- renderUI({
         resultado <- div(
             h2("Validación 2: "),
             p("Una red puede ser considerada 'mundo pequeño ' si la 'smallworldness' es mayor a 1 ",a(href="https://www.rdocumentation.org/packages/qgraph/versions/1.5/topics/smallworldness","(qgraph::smallworldness)")),
             p("Un punto de vista más estricto dice de la red llevarla a 'smallworldness' >= 3 ",a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0002051","(Humphries & Gurney, 2008)")),
             br(),
             p(strong("smallworldness: "), small_world_reactive_data()$smallworldness_grafo["smallworldness"]),
             p(strong("mayor a 1: "), if_else(small_world_reactive_data()$es_mayor_a_1,"SI","NO")),
             p(strong("mayor a 3: "), if_else(small_world_reactive_data()$es_mayor_a_3,"SI","NO")),
             br())
         resultado
     })
     
# SERVER - SMALL WORLD - comparacion validaciones ------------------------------------------
     
     output$small_world_comparacion_valores_validaciones <- renderUI({
         
         # medidas_a_mostrar <- data.frame('transitivity [red]'=c(small_world_reactive_data()$valores_para_red$transitivity,
         #                                   small_world_reactive_data()$smallworldness_grafo["trans_target"]),
         #            "transitivity [promedio simulaciones]"=c(small_world_reactive_data()$simulaciones_resumen_random$mean_transitivity,
         #                                                     small_world_reactive_data()$smallworldness_grafo["trans_rnd_M"]),
         #            'promedio distancia del camino mas corto [red]'=c(small_world_reactive_data()$valores_para_red$avg_path,
         #                                                              small_world_reactive_data()$smallworldness_grafo["averagelength_target"]),
         #            "promedio distancia del camino mas corto [promedio simulaciones]"=c(small_world_reactive_data()$simulaciones_resumen_random$mean_avg_path_length,
         #                                                                                small_world_reactive_data()$smallworldness_grafo["averagelength_rnd_M"])
         #            )
         
         # dataTableOutput(renderDataTable(datatable(medidas_a_mostrar)))
         
         
         # medidas_a_mostrar
         
         resultado <- div(
             p("Recordar que para Validación 2, se vuelven a ejecutar simulaciones."),
             p("Sobre las simulaciones de Validación 1, se puede ver su detalle en su pestaña"),
             p("Sobre las simulaciones de Validación 2, se puede ver su detalle en su pestaña"),
             p(strong("para cada variable: "),'valor validación 1 | ','valor validación 2'),
             p(strong("transitivity [red]:"), small_world_reactive_data()$valores_para_red$transitivity,
               " | ",small_world_reactive_data()$smallworldness_grafo["trans_target"]),
             p(strong("transitivity [promedio simulaciones]:"),
               small_world_reactive_data()$simulaciones_resumen_random$mean_transitivity,
               " | ", small_world_reactive_data()$smallworldness_grafo["trans_rnd_M"] ),
             br(),
             # FIXME: revisar lo de smallworldness hasta donde debatirlo un poco.
             p(strong("promedio distancia del camino mas corto [red]:"),
               small_world_reactive_data()$valores_para_red$avg_path,
               " | ",small_world_reactive_data()$smallworldness_grafo["averagelength_target"]),
             p(strong("promedio distancia del camino más corto [promedio simulaciones]:"),
               small_world_reactive_data()$simulaciones_resumen_random$mean_avg_path_length,
               " | ",small_world_reactive_data()$smallworldness_grafo["averagelength_rnd_M"] )
         )
         resultado
     })

     
     output$small_world <- renderUI({
         req(input$random_param_update)
         valores_para_red <- valores_para_red_reactive()
         params <-  modelo_random_params_reactive()
         simulaciones_resumen_random <- modelo_random_resumen_reactive()

         param_long <- params %>% 
             tidyr::gather(parametro,valor) %>% 
             as_tibble() 
         # texto_params <-param_long %>% mutate(texto = paste("<p><strong>",str_replace(parametro,"param_",""),"</strong>",valor,"</p>")) %>% pull(texto) %>% paste(collapse = "\n")
         
         delta_net <- valores_para_red$avg_path / simulaciones_resumen_random$mean_avg_path_length
         gamma_net <- valores_para_red$transitivity / simulaciones_resumen_random$mean_transitivity
         
         es_small_world <- round(delta_net) == 1  &  gamma_net > 1 
         
         paste0(param_long$parametro,": ",param_long$valor,collapse = "\n")

         smallworldness_grafo <- qgraph::smallworldness(current_grafo)
         es_mayor_a_1 <- smallworldness_grafo > 1
         es_mayor_a_3 <- smallworldness_grafo > 3
         wasd <- paste0(param_long$parametro,": ",param_long$valor,collapse = "\n")
         
         resultado <- div(
             p("comparado con los parametros de simulación aleatoria: "),
             # p(a("para recordar",href="https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network")),
             p("(",em("se sugiere configurar el modelo en Modelos: Red aleatoria"),")"),
             br(),
             div(wasd),
             br(),
             h2("Validación 1:"),
             p("por comparación con redes aleatorias"),
             p("Mundo pequeño: si delta ~=1 & gamma > 1  "),
             p("#delta:=L/Lr. and gamma:=C/Cr."),
             p("L: promedio camino mas corto "),
             p("Lr: promedio de promedio camino mas corto de las simulaciones"),
             p("C: transitivity "),
             p("Cr: promedio de transitivity de las simulaciones"),
             p(strong("Delta: "),delta_net),
             p(strong("Gamma: "),gamma_net),
             p("Es mundo pequeño: ",es_small_world),
             br(),
             h2("Validación 2: "),
             p("Una red puede ser considerada 'mundo pequeño ' si la 'smallworldness' es mayor a 1 ",a(href="https://www.rdocumentation.org/packages/qgraph/versions/1.5/topics/smallworldness","(qgraph::smallworldness)")),
             p("Un punto de vista mas estricto dice de la red llevarla a 'smallworldness' >= 3 ",a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0002051","(Humphries & Gurney, 2008)")),
             p("Para considerar una red  'mundo pequeño', Es sugerido inspeccionar transitivity substancialmente mas grande que las simulaciones aleatorias, "),
             p("y que el el promedio de distancia del camino más corto es similar o mayor (pero no mucho mayor)"),
             br(),
             p("smallworldness: ", smallworldness_grafo["smallworldness"]),
             p("mayor a 1: ", es_mayor_a_1),
             p("mayor a 3: ", es_mayor_a_3),
             br(),
             p("transitivity [red]:", valores_para_red$transitivity, " | ",smallworldness_grafo["trans_target"]),
             p("transitivity [promedio simulaciones]:", simulaciones_resumen_random$mean_transitivity, " | ", smallworldness_grafo["trans_rnd_M"] ),
             br(),
             # FIXME: revisar lo de smallworldness hasta donde debatirlo un poco.
             p("promedio distancia del camino más corto [red]:", valores_para_red$avg_path, " | ",smallworldness_grafo["averagelength_target"]),
             p("promedio distancia del camino más corto [promedio simulaciones]:", simulaciones_resumen_random$mean_avg_path_length, " | ",smallworldness_grafo["averagelength_rnd_M"] )#,
             
             
             # FIXME: SACAR ESTO:
             #p("otro para ver:", "SAND - 5.5.2 Assessing Small World Properties", "de csardi")
         )
         
         resultado
     }) 
# SERVER - SCALE FREE ---------------------------------------------------------------
     output$scale_free <- renderUI({
         
         valores_para_red <- valores_para_red_reactive()
         # para valores 2 : ver http://networksciencebook.com/chapter/4 donde habla de < 2 no free scale
         # por el tema del test de KS, leer el help del paquete, la funcion fit_power_law
         
         resultado <- div(
             br(),
             p(" Una red Libre escala es una red en la cual su distribución de grado sigue la ley de potencia."),
             p(a(" Barabasi (2015) ",href="http://networksciencebook.com/chapter/4")),# Agregar el link al capitulo 4 del libro de network science.
             p(strong(" Verificamos por la ley de potencia:")),
             p(" Luego de ajustar los grados de la red a la ley de potencia, tenemos que verificar:"),
             # ley de potencia ( power fit law)
             # resultado_fit_ley_potencia$alpha > 2  # 2.515571             
             p(strong("El parámetro alfa de la función es mayor que 2?")),
             p("Exponente alfa: ",valores_para_red["pow_law_alpha"]),
             p("El exponente es mayor a 2: ", valores_para_red["pow_law_greater_than_2"]),
             
             # p("Dada la hipotesis:"),
             # the original data could have been drawn from the fitted power-law distribution.
             p("Los datos originales pueden ser obtenidos dada la distribucion de ley de potencia obtenida."),
             # Small p-values (less than 0.05) indicate that the test rejected the hypothesis
             p("P-Valores pequenios ( Menores a 0.05 ) Indican que el test rechaza la hipotesis "),
             br(),
             p(strong("El test de ajuste de Kolmogorov-Smirnov significativo?")),
             p("p-value test KS: ",valores_para_red["pow_law_ks_p"]),
             p("menor a 0.05:",valores_para_red["pow_law_ks_p_is_signif"]),
             
             p(if_else(!valores_para_red["pow_law_ks_p_is_signif"] & valores_para_red["pow_law_greater_than_2"],"Es de Libre Escala.","No es de Libre Escala."))

         )
         resultado
     }) 
}