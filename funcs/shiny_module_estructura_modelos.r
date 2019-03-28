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
                                p("Para la ejecución de una red aleatoria se usa el modelo de Erdos-Renyi basado en nodos y vertices.")
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
        bs_append(title = "Verificaciones", content = div(
            
            tabsetPanel(type = "tabs",
                        id=ns("estructura_modelos_tabs"),
                        
# UI - VERIFICACION - SMALL WORLD -----------------------------------------
                        tabPanel("Mundo Pequeño",div(
                            
                            conditionalPanel("input.random_param_update",
                                             div(
                                                 p("para validar small world se tomo el criterio de comparar la red actual contra redes aleatorias ( lo seleccionado en el tab de Modelos / Aleatorio). "),
                                                 p("la comparacion se lleva a cabo en el promedio del camino mas corto VS la media del promedio del camino mas corto de las N redes simuladas."),
                                                 uiOutput(ns("small_world"))                                                 
                                             ),ns=ns),
                            conditionalPanel("!input.random_param_update",
                                             div(
                                                 p("para validar Mundo pequeño se debe ejecutar la simulacion de redes aleatorias.(En el tab de Modelos / Aleatorio). ")
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
                                    current_grafo#, # grafo actual
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
                   "alfa > 1"=pow_law_greater_than_1,
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
        ret <- colnames(valores_para_red_reactive())
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
        
        pl <- data_red %>% ggplot(aes(valor)) +
            geom_histogram()+ 
            geom_vline(data=data_current,
                       aes(xintercept=valor,colour="red"),
                       show.legend = FALSE) + 
            geom_text(data=data_current, 
                      aes(x=valor, 
                          label=paste0(metrica_seleccionada," de la red: ",round(valor,5),"\n"), 
                          y=20), 
                      colour="red", angle=90, 
                      text=element_text(size=11))+
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "cantidad de simulaciones")+
            theme_light()
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
                   "alfa > 1"=pow_law_greater_than_1,
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
        ret <- colnames(valores_para_red_reactive())
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
        
        pl <- data_red %>% ggplot(aes(valor)) +
            geom_histogram()+ 
            geom_vline(data=data_current,
                       aes(xintercept=valor,colour="red"),
                       show.legend = FALSE) + 
            geom_text(data=data_current, 
                      aes(x=valor, 
                          label=paste0(metrica_seleccionada," de la red: ",round(valor,5),"\n"), 
                          y=20), 
                      colour="red", angle=90, 
                      text=element_text(size=11))+
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "cantidad de simulaciones")+
            theme_light()
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
                   "alfa > 1"=pow_law_greater_than_1,
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
        ret <- colnames(valores_para_red_reactive())
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
         
         pl <- data_red %>% ggplot(aes(valor)) +
             geom_histogram()+ 
             geom_vline(data=data_current,
                        aes(xintercept=valor,colour="red"),
                        show.legend = FALSE) + 
             geom_text(data=data_current, 
                       aes(x=valor, 
                           label=paste0(metrica_seleccionada," de la red: ",round(valor,5),"\n"), 
                           y=20), 
                       colour="red", angle=90, 
                       text=element_text(size=11))+
             labs(title=paste0("Histograma de ",metrica_seleccionada),
                  x= metrica_seleccionada,
                  y= "cantidad de simulaciones")+
             theme_light()
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
                    "alfa > 1"=pow_law_greater_than_1,
                    "test KS significativo"=pow_law_ks_p_is_signif)
         
         DT::datatable(
             options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
             mostrar,
             escape = FALSE,
             rownames = FALSE,
             selection = 'none')
     })
# SERVER - SMALL WORLD ---------------------------------------------------------------
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
         
         # wasd <- for (row1 in seq_along(1:nrow(param_long))) {
         #     p(strong(param_long[row1,1]),": ",param_long[row1,2])
         #     paste0(param_long[row1,1]),": ",param_long[row1,2])
         #     # print(row1$parametro)
         #     # print(row1$valor)
         # }
         # tmp <- qgraph::smallworldness(g_aut)
         # 
         # sw_df <- data.frame(propiedad=tmp %>% names(),
         #            valor=tmp,
         #            stringsAsFactors = FALSE)
         # sw_df %>% filter(propiedad=="smallworldness") %>% pull(valor)

         
         smallworldness_grafo <- qgraph::smallworldness(current_grafo)
         es_mayor_a_1 <- smallworldness_grafo > 1
         es_mayor_a_3 <- smallworldness_grafo > 3
         wasd <- paste0(param_long$parametro,": ",param_long$valor,collapse = "\n")
         
         resultado <- div(
             p("comparado con los parametros de simulación aleatoria: "),
             p(a("para recordar",href="https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network")),
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
             p(" Un punto de vista mas estricto dice de la red llevarla a 'smallworldness' >= 3 ",a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0002051","(Humphries & Gurney, 2008)")),
             p("Para considerar una red  'mundo pequeño', Es sugerido inspeccionar transitivity substancialmente mas grande que las simulaciones aleatorias, "),
             p("tambien que el el promedio distancia del camino mas corto es similar o mayor (pero no mucho mayor)"),
             br(),
             p("smallworldness: ", smallworldness_grafo["smallworldness"]),
             p("mayor a 1: ", es_mayor_a_1),
             p("mayor a 3: ", es_mayor_a_3),
             br(),
             p("transitivity [red]:", valores_para_red$transitivity, " | ",smallworldness_grafo["trans_target"]),
             p("transitivity [promedio simulaciones]:", simulaciones_resumen_random$mean_transitivity, " | ", smallworldness_grafo["trans_rnd_M"] ),
             br(),
             # FIXME: revisar lo de smallworldness hasta donde debatirlo un poco.
             p("promedio distancia del camino mas corto [red]:", valores_para_red$avg_path, " | ",smallworldness_grafo["averagelength_target"]),
             p("promedio distancia del camino mas corto [promedio simulaciones]:", simulaciones_resumen_random$mean_avg_path_length, " | ",smallworldness_grafo["averagelength_rnd_M"] ),
             
             
             # FIXME: SACAR ESTO:
             p("otro para ver:", "SAND - 5.5.2 Assessing Small World Properties", "de csardi")
         )
         
         resultado
     }) 
# SERVER - SCALE FREE ---------------------------------------------------------------
     output$scale_free <- renderUI({
         
         valores_para_red <- valores_para_red_reactive()
         
         resultado <- div(
             p(" A scale-free network is a network whose degree distribution follows a power law."),
             p(" [2] A.-L. Barabási and R.Albert. Emergence of scaling in random networks. Science, 286:509-512, 1999. "),
             p(" Verificamos por la ley de potencia:"),
             # ley de potencia ( power fit law)
             # resultado_fit_ley_potencia$alpha > 1  # 2.515571             
             p("El parámetro alfa de la función es mayor que 1?"),
             p("Exponente alfa: ",valores_para_red["pow_law_alpha"]),
             p("El exponente es mayor a 1: ", valores_para_red["pow_law_greater_than_1"]),
             
             p("Dada la hipotesis:"),
             p("the original data could have been drawn from the fitted power-law distribution."),
             p("Small p-values (less than 0.05) indicate that the test rejected the hypothesis "),
             br(),
             p("El test de ajuste de Kolmogorov-Smirnov es no significativo?"),
             p("p-value test KS: ",valores_para_red["pow_law_ks_p"]),
             p("significativo para 0.05:",valores_para_red["pow_law_ks_p_is_signif"])

         )
         
         resultado
         
     }) 
}