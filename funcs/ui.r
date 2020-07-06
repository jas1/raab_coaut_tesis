# ui.r > moviendo toda la UI de shiny a UI.r

# SHINY: HEADER -----------------------------------------------------------------------------------------------
# header <- dashboardHeader(
#     title="Análisis de coautoría de Revista Argentina de Antropología Biológica 1996 a 2016",
#     titleWidth = 770#,
#     #dropdownMenu(dropdownMenuOutput("msg_menu"))
# )
header <- dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    title="Análisis de coautoría de Revista Argentina de Antropología Biológica 1996 a 2016",
    titleWidth = 770#,
    #dropdownMenu(dropdownMenuOutput("msg_menu"))
    
)

# SHINY: SIDEBAR  -----------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Consideraciones",
                 tabName = "tab_consideraciones"
        ),
        menuItem("Análisis Estático",
                 tabName = "tab_analisis_estatico"
        ),
        menuItem("Análisis Temporal",
                 tabName = "tab_analisis_temporal"),
        menuItem("Contacto", 
              href="https://www.juliospairani.com" )
    )
)

# SHINY: Right sidebar: para estatico -------------------------------------


rightsidebar <- rightSidebar(
    background = "dark",
    rightSidebarTabContent(
        id = 1,
        title = "Estático - Configuración periódos",
        icon = "desktop",
        active = TRUE,
        # div(id="sidebar_panel_div",
            # sidebarPanel(
                # width = side_lay_width,
                br(),
                # UI : Input Semilla --------------------------------------------------------------
                textInput("semilla_seed", label = p("Semilla"), value = "12345"),
                # UI : Input Periodos --------------------------------------------------------------                        
                pickerInput(
                    inputId = "input_static_periodos", 
                    label = "Períodos",
                    choices = global_periodos_disponibles, 
                    options = list(
                        'actions-box' = TRUE, 
                        size = 10,
                        'selected-text-format' = "count > 3",
                        'deselect-all-text' = "Ninguno",
                        'select-all-text' = "Todos",
                        'none-selected-text' = "Sin Selección",
                        'count-selected-text' = "{0} seleccionados."
                    ), 
                    multiple = TRUE
                ),
                
                conditionalPanel('input.input_static_periodos != null',
                                 # UI : Input Select Layout --------------------------------------------------------------
                                 selectizeInput(inputId = "input_static_layout_select", label = "Disposición", choices = '',
                                                options = list(
                                                    placeholder = 'Seleccionar Disposición',
                                                    onInitialize = I('function() { this.setValue("layout_nicely"); }')),
                                                multiple = FALSE),
                                 # UI : Input Select Autor --------------------------------------------------------------
                                 selectizeInput(inputId = "input_static_network_selected_nodes", label = "Autores", choices = '',
                                                options = list(
                                                    placeholder = 'Seleccionar un Autor',
                                                    onInitialize = I('function() { this.setValue(""); }')),
                                                multiple = FALSE),
                                 bs_accordion(id = "comunidades_accordion") %>%
                                     bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                     bs_append(title = "Comunidades",coll ,content = div(
                                         # UI : Input : Comunidades: Select Algoritmo --------------------------------------------------------------
                                         
                                         selectizeInput(inputId = "comunidades_sel_algo", label = "Algoritmo", choices = '',
                                                        options = list(
                                                            placeholder = 'Seleccionar Algoritmo',
                                                            onInitialize = I('function() { this.setValue(""); }')),
                                                        multiple = FALSE),
                                         # UI : Input : Comunidades: Select Cant. coms --------------------------------------------------------------
                                         
                                         sliderInput("comunidades_n_view",
                                                     "Cantidad:",
                                                     min = 1, 
                                                     max = 7,
                                                     value = 5)
                                     )
                                     )
                )
                
        #     )# fin sidebar panel
        # )#,# fin sidebar div
    )
)

# SHINY: BODY -----------------------------------------------------------------------
body <- dashboardBody(
    # useShinyjs(),
    tabItems(
        # TAB CONSIDERACIONES --------------------------------------------------------------------------
        tabItem(tabName = "tab_consideraciones",
                tabsetPanel(type = "tabs",
                            id="tab_consideraciones_tabs",
# --- UI : consideraciones: inicial  ------------------
                            
                            tabPanel("Generales", div(
                                     br(),
                                     p("El análisis del estudio esta acotado a:"),
                                     tags$ul(
                                         tags$li(" Tipo de publicación: TRABAJOS ORIGINALES"),
                                         tags$li(" Periodos comprendidos entre 1996 y 2016"))
                                     
                                     )), # fin div de tab panel 
# --- UI : consideraciones: EDA  ------------------
                            tabPanel("Análisis Exploratorio datos", 
                                     br(),
                                     checkboxInput("eda_acotado_2016", 
                                                   label = "Análisis Exploratorio acotado a Periodos: 1996 a 2016 y Sección: Trabajos originales",
                                                   value = TRUE),
                                     
                                tabsetPanel(type = "tabs",id="tabs-eda",
                                    tabPanel("Autores", div(
                                        br(),
                                        eda_autores_ui("eda_autores",
                                                       acotar_anios_secciones='input.eda_acotado_2016')
                                        )# fin div de tab panel
                                    ), # fin tab panel
                                    tabPanel("Artículos", div(
                                        br(),
                                        eda_articulos_ui("eda_art")#,
                                                         # acotar_anios_secciones='input.eda_acotado_2016')
                                        )# fin div de tab panel
                                    ), # fin tab panel
                                    tabPanel("Autores-Artículos", div(
                                        br(),
                                        eda_aut_art_ui("eda_aut_art")#,
                                                       #acotar_anios_secciones='input.eda_acotado_2016')
                                    )# fin div de tab panel
                                    ) # fin tab panel
                                ) # fin div de tabset panel
                            )# fin exploratorio datos
            ) # tab_consideraciones_tabs

        ),#fin tab_consideraciones 
        # TAB ESTATICO --------------------------------------------------------------------------
        tabItem(tabName = "tab_analisis_estatico",
                fluidPage(
                    tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                        tags$script(src="https://cdn.plot.ly/plotly-locale-es-latest.js")
                        # tags$script("Plotly.setPlotConfig({locale: 'es'});")
                    ),# fin tags head
                    mainPanel(
                        width = main_lay_width,
                        # actionButton("toggle_sidebar", "Mostrar / Ocultar Barra del costado"),
                        tabsetPanel(type = "tabs",
                                    id="estatico_tabs",
# --- UI : Panel principal : visualizacion  ------------------
                                    
                                    tabPanel("Visualización de coautoría",
                                             conditionalPanel('input.input_static_periodos != null',
                                                              
# --- UI : Panel principal : red  ------------------
                                                              tabsetPanel(type = "tabs",
                                                                          id="estatico_tabs_red", 
                                                                          tabPanel("Red",
                                                                          div(
                                                                             br(),
                                                                             
                                                                             fluidRow(
                                                                                 column(9, 
                                                                                        conditionalPanel('input.input_static_periodos != null',
                                                                                                         fluidRow(
                                                                                                             column(4,
                                                                                                                    sliderInput("input_static_network_filter_edges_threshold", 
                                                                                                                                "Filtro Visualizar aristas",
                                                                                                                                min = 1, 
                                                                                                                                max = network_filter_edges_threshold_max, # maximo segun filtros
                                                                                                                                value = network_filter_edges_threshold_max,
                                                                                                                                step=1,
                                                                                                                                ticks=FALSE) # current filtro
                                                                                                                    # ) 
                                                                                                             ),
                                                                                                             column(6,
                                                                                                                    radioButtons("input_static_network_filter_edges_threshold_tipo", 
                                                                                                                                 "Tipo:",
                                                                                                                                 inline = TRUE,
                                                                                                                                 c("Todos"="todo",
                                                                                                                                   "Mayor igual A" = "mayorigual",
                                                                                                                                   "Menor igual A" = "menorigual",
                                                                                                                                   "Exacto" = "exacto"))
                                                                                                             )
                                                                                                         )
                                                                                                         
                                                                                                         
                                                                                        )# fin conditional panel periodos != null
          
                                                                                 ), # fin col 9
                                                                                 column(3, 
                                                                                        downloadButton (outputId = "output_static_download_network",
                                                                                                        label = "Bajar red como html") )
                                                                             ), # fin fluid row
                                                                             
                                                                             br(),
                                                                             visNetworkOutput("output_static_network")
                                                                             ) # fin del div  de la red
                                                                          ),# fin tab panel red
# --- UI : Panel principal : biofabric  ------------------
                                                                          tabPanel("Biofabric",div(
                                                                              #p("agregar el panel de biofabric " ),
                                                                                bioFabric_htmlwidgetOutput("output_biofabric_net",
                                                                                                           width = "100%", 
                                                                                                           height = "400px") 
                                                                              )  # fin div biofabric
                                                                              ),# fin tab panel biofabric
# --- UI : Panel principal : heatmap  ------------------
                                                                        tabPanel("Heatmap",div(
                                                                            #p("agregar el panel de biofabric " ),
                                                                            plotlyOutput('output_heatmap_net')
                                                                        )  # fin div Heatmap net
                                                                        )# fin tab Heatmap
                                             )# fin tab panel vis de la red
                                             ),# fin conditional panel pare mostrar la red
                                            conditionalPanel('input.input_static_periodos == null',
                                                             div(
                                                                 p("Debe seleccionar un periodo."),
                                                                 p("Esto se hace en el tab de configuración derecho."),
                                                                 p("(son los engranajes arriba a la derecha de la pantalla)")
                                                             )# fin div condigional panel periodos == null.
                                            )# fin conditional panel periodos == null
                                    ),# fin tab panel network statica
                                    # --- UI : Panel principal : Artículos Asociados  ------------------
                                    
                                    tabPanel("Artículos Asociados", 
                                             
                                             br(),
                                             conditionalPanel('input.input_static_periodos != null',
                                                              conditionalPanel('input.input_static_network_click_vertex != null || input.input_static_network_click_edge != null', 
                                                                               br(),
                                                                               downloadButton (outputId = "download_art_asoc",
                                                                                               label = "Bajar Art. Asoc."),
                                                                               uiOutput("output_info_seleccion_vertex_degree",inline = TRUE), # class="hidden_degree")
                                                                               fluidRow(
                                                                                   column(6,
                                                                                          conditionalPanel('input.input_static_network_click_vertex != null && input.input_static_network_click_edge == ""',
                                                                                                           # p(" a llenar con datos autor"),
                                                                                                           uiOutput('output_info_seleccion_vertex')
                                                                                          ), # fin condicional vertex
                                                                                          conditionalPanel('input.input_static_network_click_vertex == "" && input.input_static_network_click_edge != null',
                                                                                                           uiOutput('output_info_seleccion_edge')
                                                                                                           # p(" a llenar con datos relación")
                                                                                          ) # fin condicional edge
                                                                                   ), # fin col 6 parte 1
                                                                                   column(6, 
                                                                                          conditionalPanel('input.input_static_network_click_vertex != null && input.input_static_network_click_edge == ""',
                                                                                                           
                                                                                                           # conditionalPanel('grado_0 != "0" ',
                                                                                                                                tabsetPanel(type = "tabs",
                                                                                                                                        id="coaut_tabs",
                                                                                                                                        tabPanel("Subred de coautoría",
                                                                                                                                                 visNetworkOutput("output_subgrafo_coautoria_autor")),
                                                                                                                                        tabPanel("Heatmap de coautoría",
                                                                                                                                                 plotlyOutput('output_heatmap_coautoria_autor'))
                                                                                                                                )# fin tabs panel coaut
                                                                                                                            # )# fin conditional panel degree
                                                                                                           
                                                                                                           
                             
                                                                                          ), # fin condicional vertex
                                                                                          conditionalPanel('input.input_static_network_click_vertex == null && input.input_static_network_click_edge != null',
                                                                                                           br()
                                                                                                           #p(" a llenar con datos relación")
                                                                                          ) # fin condicional edge
                                                                                   ) # fin col 6 parte 2
                                                                               ),# fin fluid row
                                                                               br(),
                                                                               conditionalPanel('input.input_static_network_click_vertex != null && input.input_static_network_click_edge == ""',
                                                                                                # p(" a llenar con datos autor"),
                                                                                                DT::dataTableOutput("output_doc_asociados_vertex")
                                                                               ), # fin condicional vertex
                                                                               conditionalPanel('input.input_static_network_click_vertex == "" && input.input_static_network_click_edge != null',
                                                                                                DT::dataTableOutput("output_doc_asociados_edge")
                                                                                                # p(" a llenar con datos relación")
                                                                               )
                                                              ),# fin condicion vertex o edge
                                                              
                                                              conditionalPanel('input.input_static_network_click_vertex == null && input.input_static_network_click_edge == null', 
                                                                               p("Para ver los artículos asociados debe seleccionar un Nodo o Arista de la Red."))                                            
                                             ),
                                             conditionalPanel('input.input_static_periodos == null',
                                                              "Para ver los artículos asociados, debe seleccionar al menos un Período.")
                                             
                                    ),
# --- UI : Panel principal : estructura de red  ------------------
                                    tabPanel("Estructura Red", 
                                             
                                             conditionalPanel('input.input_static_periodos != null',
                                                              bs_accordion(id = "estructura_red_accordion") %>%
                                                                  bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                                                                  
                                                                  # metricas RED ------------------------------------------------------------
                                                              
                                                              bs_append(title = "Métricas de Red", content = div(
                                                                  
                                                                  br(),
                                                                  downloadButton (outputId = "output_static_download_estructura_red",
                                                                                  label = "Bajar est. red."),
                                                                  br(),
                                                                  br(),
                                                                  DT::dataTableOutput('output_static_estructura_red_DT')
                                                              )) %>%
                                                                  
                                                                  
# metricas nodos ----------------------------------------------------------
                                                              bs_set_opts(panel_type = "info") %>%
                                                              bs_append(title = "Métricas de Nodos", content = div(
                                                                  estructura_nodos_ui("est_nodos")
                                                      
                                                                  # br(),
                                                                  # downloadButton (outputId = "output_static_download_est_nodos",
                                                                  #                 label = "Bajar est. nodos"),
                                                                  # br(),
                                                                  # br(),
                                                                  # DT::dataTableOutput('output_static_estructura_nodos_table')
                                                              )# fin div
                                                              ) %>%

# detalles componentes ----------------------------------------------------

bs_set_opts(panel_type = "info") %>%
    bs_append(title = "Detalle de Componentes", content = div(
        # similar a lode comunidades, pero por componentes
        # listado de componetes y su estructura
        # listado de autores por componente
        # autores + estructura componente + articulos asociados componente
        # visualizacion del componente seleccionado.
        subgrafos_ui("estr_componentes")
    )) #%>% 

                                             ),
                                             conditionalPanel('input.input_static_periodos == null',
                                                              "Para ver la estructura, debe seleccionar al menos un Período.")
                                             
                                    ),
# --- UI : Panel principal : Comparacion de modelos  ------------------
tabPanel("Comparación con modelos", 
         br(),
# simulacion similares ----------------------------------------------------

         estructura_modelos_ui("modelos")

         ),

tabPanel("Comunidades", 
         br(),
         conditionalPanel('input.comunidades_sel_algo != null && input.comunidades_sel_algo!= "" ',
            bs_accordion(id = "estructura_comunidades_accordion") %>%
            bs_set_opts(panel_type = "info") %>%
            bs_append(title = "Detalle de Comunidades", content = div(
                # similar a lode comunidades, pero por componentes
                # listado de componetes y su estructura
                # listado de autores por componente
                # autores + estructura componente + articulos asociados componente
                # visualizacion del componente seleccionado.
                comunidades_ui("comunidades_panel")
            )) #%>% 
         ),
         conditionalPanel('input.comunidades_sel_algo == null || input.comunidades_sel_algo== "" ',
                          div(
                              br(),
                              p("Debe seleccionar un Algoritmo de comunidades en el panel lateral")
                          )
         )# fin condicion falta seleccion
    )#,# fin tab panel comunidades
# --- UI : Panel principal : Comunidades  ------------------
# tabPanel("Comunidades", 
#          br(),
#          conditionalPanel('input.comunidades_sel_algo != null && input.comunidades_sel_algo!= "" ',
#                           bs_accordion(id = "estructura_comunidades_accordion") %>%
#                               bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
#                               
#                               # comunidades - resultado algo --------------------------------------------
#                           bs_append(title = "Resultado Algoritmo", 
#                                     content = div(
#                                         htmlOutput('comunidades_result'),
#                                         br(),
#                                         downloadButton (outputId = "download_com_metricas",
#                                                         label = "Bajar metricas Comunidades."),
#                                         br(),
#                                         br(),
#                                         
#                                         DT::dataTableOutput('comunidades_result_metricas_listado')
#                                     )) %>%
#                               bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
#                               
#                               # comunidades - autores ---------------------------------------------------
#                           bs_append(title = "Autores por Comunidad", content = div(
#                               br(),
#                               downloadButton (outputId = "download_com_metricas_aut",
#                                               label = "Bajar metricas Comunidades."),
#                               br(),
#                               br(),
#                               DT::dataTableOutput('cantidad_comunidades_metricas'))
#                           ) %>%
#                               bs_set_opts(panel_type = "info") %>%
#                               
#                               # comunidades - detalle ---------------------------------------------------
#                           bs_append(title = "Detalle Comunidad Seleccionada", content = 
#                                         
#                                         div(
#                                             conditionalPanel('!(input.cantidad_comunidades_metricas_rows_selected != null && input.cantidad_comunidades_metricas_rows_selected != "")', 
#                                                              br(),
#                                                              div(p("Debe seleccionar una comunidad en 'Autores por Comunidad'"))
#                                             ), # fin debe seleccionar comunidad
#                                             conditionalPanel('input.cantidad_comunidades_metricas_rows_selected != null && input.cantidad_comunidades_metricas_rows_selected != ""', 
#                                                              div(htmlOutput('detalle_comunidad_seleccionada'),
#                                                                  br(),
#                                                                  h2('Estructura Comunidad'),
#                                                                  DT::dataTableOutput('detalle_comunidad_seleccionada_metricas_subgrafo_table'),
#                                                                  br(),
#                                                                  h2('Artículos Comunidad'),
#                                                                  br(),
#                                                                  downloadButton (outputId = "download_com_sel_arts",
#                                                                                  label = "Bajar articulos Comunidad"),
#                                                                  br(),
#                                                                  br(),
#                                                                  DT::dataTableOutput('detalle_comunidad_seleccionada_articulos')
#                                                              ) # fin div detalle comunidad  
#                                             )# fin conditional comunidad seleccionada
#                                         )
#                           )# fin bs_append detalle comunidad seleccionada. 
#          ),# fin comunidades ok.
#          conditionalPanel('input.comunidades_sel_algo == null || input.comunidades_sel_algo== "" ',
#                           div(
#                               br(),
#                               p("Debe seleccionar un Algoritmo de comunidades en el panel lateral")
#                           )
#                           
#          )# fin condicion falta seleccion
# )# fin tab comunidades
                        )# fin tabs panel
                    )# fin main panel
                )# fin fluid page
        ),# fin tab estatico
        
# TAB TEMPORAL --------------------------------------------------------------------------
        tabItem(tabName = "tab_analisis_temporal",
                textInput("temporal_semilla_seed", label = p("Semilla"), value = "12345"),
                # p("Las operacionres realizadas en cada seccion temporal pueden demorar bastante tiempo. ( hasta 5 mins aprox. )"),
                # br(),
                
                # p("Esta pestaña estará disponible proximamente."),
                # p("Esta pestaña tiene la info de análisis temporal, esto incluye todos los periodos desde t1 hasta t20."),
                # p("Esta pestaña es independiente de lo realizado en el análisis estático")
                
                bs_accordion(id = "accordeon_temporal") %>%
                    bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                    
                    # temporal - dinamico - acumulado  ----------------------------------------
                
                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                    bs_append(title = "Visualización dinámica - Acumulado", 
                              content = div(
                                  fluidRow(
                                      column(6,div(
                                          ndtv:::ndtvAnimationWidgetOutput("temporal_dinamico_acumulado"))
                                      ),# fin column 6
                                      column(6,uiOutput("out_temporal_descripcion_dinamico_ui")) # fin column 6
                                  )# fin fluid row
                              )# fin content div
                    ) %>% # fin bs append Visualización dinámica - Acumulado
                
                
                    
                    # temporal - basico -------------------------------------------------
                # download_temporal_acumulado_estruct
                # download_temporal_basico_estruct
                
                bs_append(title = "Métricas de Red en el tiempo por año", 
                          content = div(
                              pickerInput(
                                  inputId = "temporal_sel_vars", 
                                  label = "Variables",
                                  choices = estructura_red_vars_compara_simu_list,
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
                                  multiple = TRUE
                              ),
                              downloadButton (outputId = "download_temporal_basico_estruct",
                                              label = "Bajar est. red. temporal"),
                              
                              # actionButton('temporal_estructura_refrescar_boton', 'Ver'),
                              
                              
                              conditionalPanel('(input.temporal_sel_vars != null && input.temporal_sel_vars!= "" )',
                                               plotlyOutput('temporal_grafos_plot_1'))
                              
                          )# fin div - tab basico
                ) %>% # fin bs append - tab basico
                    
                    # temporal - acumulado ----------------------------------------------------
                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%                
                    bs_append(title = "Métricas de Red en el tiempo, valores acumulados", content = div(
                        
                        pickerInput(
                            inputId = "temporal_sel_vars_2", 
                            label = "Variables",
                            choices = estructura_red_vars_compara_simu_list,
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
                            multiple = TRUE
                        ),
                        downloadButton (outputId = "download_temporal_acumulado_estruct",
                                        label = "Bajar est. red. temporal - acumulado"),
                        # actionButton('temporal_estructura_refrescar_boton', 'Ver'),
                        
                        
                        conditionalPanel('(input.temporal_sel_vars_2 != null && input.temporal_sel_vars_2!= "" )',
                                         plotlyOutput('temporal_grafos_plot_2'))
                        
                    ) # fin div - temporal - acumulado
                    ) %>% # fin bs append - temporal - acumulado
                    
                    
                    # temporal - top n - anual ------------------------------------------------
                
                
                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                    bs_append(title = "Métricas de Nodos, principales N en el tiempo", 
                              content = div(
                                  fluidRow(
                                      column(3,
                                             pickerInput(
                                                 inputId = "temporal_sel_vars_anual", 
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
                                             )),
                                      column(3,
                                             sliderInput("top_n_periodos_anual", label = p("Principales N"), value = 5,min = 1,max = 10)
                                      ),
                                      column(6,
                                             downloadButton (outputId = "download_temporal_grafos_top_n_anual",
                                                             label = "Bajar est. nodos red. temporal - acumulado")
                                             
                                      )
                                      # actionButton('temporal_estructura_refrescar_boton', 'Ver'),
                                  ),
                                  fluidRow(
                                      column(12,
                                             conditionalPanel('(input.temporal_sel_vars_anual != null && input.temporal_sel_vars_anual!= "" )',
                                                              plotlyOutput('temporal_grafos_top_n_anual')
                                                              # timevisOutput('temporal_grafos_heathmap_acum')
                                             )
                                      )
                                  )
                              ) # fin div temporal - top N - acumulado 
                    ) %>%         
                                        
                    # temporal - top N - acumulado ---------------------------------------
                
                bs_set_opts(panel_type = "info", use_heading_link = FALSE) %>%
                    bs_append(title = "Métricas de Nodos, principales N en el tiempo, valores acumulados", 
                              content = div(
                                  fluidRow(
                                      column(3,
                                             pickerInput(
                                                 inputId = "temporal_sel_vars_3", 
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
                                             )),
                                      column(3,
                                             sliderInput("top_n_periodos", label = p("Principales N"), value = 5,min = 1,max = 10)
                                             ),
                                      column(6,
                                             downloadButton (outputId = "download_temporal_grafos_top_n",
                                                             label = "Bajar est. nodos red. temporal - acumulado")

                                      )
                                      # actionButton('temporal_estructura_refrescar_boton', 'Ver'),
                                  ),
                                  fluidRow(
                                      column(12,
                                             conditionalPanel('(input.temporal_sel_vars_3 != null && input.temporal_sel_vars_3!= "" )',
                                                              plotlyOutput('temporal_grafos_top_n_acum')
                                                              # timevisOutput('temporal_grafos_heathmap_acum')
                                             )
                                      )
                                  )
                              ) # fin div temporal - top N - acumulado 
                    ) #%>% # fin append temporal - top N - acumulado 

           
                    
                    
                    

                
        )# fin tab temporal
    )
)
# dashboard 1: no temporal

# dashboard 2: temporal

# ui <- dashboardPage(header, sidebar, body)
ui <- dashboardPagePlus(header = header,sidebar = sidebar,body = body, rightsidebar = rightsidebar)