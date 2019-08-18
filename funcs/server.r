# server.r moviendo todo el backend shiny a server.


server <- function(input, output,session) {
    # generico menu messages -----------------------------------------------------------
    output$msg_menu <- renderMenu({
        
        message_data <- data.frame(from=c("Julio"),message=c("Bienvenido al Análisis de la red de coaturía de la RAAB"))
        
        messages <- apply(message_data, 1, function(row) {
            messageItem(from = row[["from"]],
                        message = row[["message"]])
        })
        dropdownMenu(type = "message", .list = messages)
    })

    # generico - EDA autores --------------------------------------------------

    # va dentdo de osberve event, por el checkbox
    # de esta forma checkeado o no checkeado te muestra las cosas.
    
    observeEvent(input$eda_acotado_2016,{
        tmp <- input$eda_acotado_2016
        # print(paste0("observeEvent_2016:", tmp))
        callModule(eda_autores_server, "eda_autores",
                   stringsAsFactors = FALSE,
                   acotar_anios_secciones=tmp)
        
        callModule(eda_articulos_server, "eda_art",
                   stringsAsFactors = FALSE,
                   acotar_anios_secciones=tmp)
        callModule(eda_aut_art_server, "eda_aut_art",
                   stringsAsFactors = FALSE,
                   acotar_anios_secciones=tmp)
        
    })
    
    
    # estatico -----------------------------------------------------------------------------------------------
    
    # estatico 1: NETWORK -----------------------------------------------------------------------------------------------
    # la base de datos de articulos / autores
    # input_static_periodos
    static_data_base <- reactive({
        req(input$input_static_periodos)
        # globales: db_limpia y cota_seccion
        # segun seleccion: periodos
        db_resultado <- articulos_todos_grafo(db_limpia,input$input_static_periodos,cota_seccion)
        db_resultado
    })
    
    # el grafo resultante de los datos
    static_network_grafo_reactive <- reactive({
        grafo_bipartito <- armado_grafo_bipartito(static_data_base())
        
        g_coaut <- extraccion_grafo_coautoria(grafo_bipartito,static_data_base(),
                                              width_multiplier = static_edge_width_multiplier)
        
        g_coaut
    })
    
    observe({
        req(input$input_static_network_selected_nodes)
        nodes_selection <- input$input_static_network_selected_nodes
        
        # print(nodes_selection)
        grafo <- static_network_grafo_reactive()
        filter_vertex_cond <- str_detect(V(grafo)$label,nodes_selection)
        filter_vertex <- igraph:::V(grafo)[filter_vertex_cond]
        # show_details_vertex(filter_vertex)

        selected_id <- filter_vertex$id
        # selected_id <- filter_vertex$label

        visNetworkProxy("output_static_network") %>%
            visSelectNodes(id = selected_id)
    })
    observe({
        updateSelectizeInput(session, "input_static_network_selected_nodes",
                             choices = static_network_nodes_select_options())
    })
    observe({
        static_network_grafo_reactive()
        updateSelectizeInput(session, "comunidades_sel_algo", 
                             choices = algoritmos_comunidad_options())
    })
    algoritmos_comunidad_options <- reactive({
        comunidades_algos_disponibles_list
    })
    
    static_network_nodes_select_options <- reactive({
        ids <- V(static_network_grafo_reactive())$id
        nombres <- V(static_network_grafo_reactive())$label
        
        nodos_df <- data.frame(name=nombres,value=ids,stringsAsFactors = FALSE) %>% as_tibble()
        
        # cat(paste0(length(ids)," - ",length(nombres)))
        
        resultado <- nodos_df %>% pull(name) # list(ids)
        # names(resultado) <- nombres
        names(resultado) <- nodos_df %>% pull(name)
        # ids
        resultado
    })
    
    # la visualizacion resultante
    # input_static_network_filter_edges_threshold
    # input_static_network_filter_edges_threshold_tipo
    output_static_network_reactive <- reactive({
        set.seed(input$semilla_seed)
        
        tmp_grafo <- static_network_grafo_reactive()
        
        
        # cat(vertex_attr_names(tmp_grafo))
        # name id anio fuerza_colaboracion cant_autores size
        # cat(edge_attr_names(tmp_grafo))
        # weight fuerza_colaboracion color width
        
        # ACTUALIZA EL WIDTH POR LOS FILTROS  
        #
        # color_no_visible_edges <- "#FFFFFF" # blanco
        color_no_visible_edges <- "rgba(0,0,0,0)" #  # transparente: https://github.com/datastorm-open/visNetwork/issues/145#issuecomment-283583419
        filter_edge_resultado <- data.frame(autores=E(tmp_grafo)$autores,
                                            width=E(tmp_grafo)$width,
                                            color=E(tmp_grafo)$color,
                                            weight=E(tmp_grafo)$weight,
                                            fuerza_colaboracion=E(tmp_grafo)$fuerza_colaboracion,
                                            stringsAsFactors = FALSE) %>% as_tibble() %>% 
            mutate(multiplier=if_else(width>1,1,static_edge_width_multiplier)) %>% # si es +1 entonces nada , sino * multip
            mutate(width_multiplier=width*multiplier) %>%  # si es 2 , va a quedar 2,4,6
            mutate(width_mayor = 
                       if_else( width_multiplier >= input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                width,# si es 2 <= 2 -> width , si es 2 <= 4 -> 0
                                0),
                   color_mayor =if_else(width_multiplier >= input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                        color,# si es 2 <= 2 -> color , si es 2 <= 4 -> blanco
                                        color_no_visible_edges),
                   width_menor = 
                       if_else( width_multiplier <= input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                width,# si es 2 <= 2 -> width , si es 2 <= 4 -> 0
                                0),
                   color_menor =if_else(width_multiplier <= input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                        color,# si es 2 <= 2 -> color , si es 2 <= 4 -> blanco
                                        color_no_visible_edges),
                   width_exacto = 
                       if_else( width_multiplier == input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                width,# si es 2 <= 2 -> width , si es 2 <= 4 -> 0
                                0),
                   color_exacto =if_else(width_multiplier == input$input_static_network_filter_edges_threshold * static_edge_width_multiplier,
                                         color,# si es 2 <= 2 -> color , si es 2 <= 4 -> blanco
                                         color_no_visible_edges
                   ),
                   width_todos = width,
                   color_todos = color
            ) 
        
        filter_width_resultado <- filter_edge_resultado %>% pull(width_todos)
        filter_color_resultado <- filter_edge_resultado %>% pull(color_todos) 
        
        switch (input$input_static_network_filter_edges_threshold_tipo,
                "todo" = {
                    filter_width_resultado <- filter_edge_resultado %>% pull(width_todos);
                    filter_color_resultado <- filter_edge_resultado %>% pull(color_todos)},
                "mayorigual" = {
                    filter_width_resultado <- filter_edge_resultado %>% pull(width_mayor);
                    filter_color_resultado <- filter_edge_resultado %>% pull(color_mayor)},
                "menorigual" = {
                    filter_width_resultado <- filter_edge_resultado %>% pull(width_menor);
                    filter_color_resultado <- filter_edge_resultado %>% pull(color_menor)},
                "exacto" = {
                    filter_width_resultado <- filter_edge_resultado %>% pull(width_exacto);
                    filter_color_resultado <- filter_edge_resultado %>% pull(color_exacto)}
                
        )
        title_edges_tooltip <- filter_edge_resultado %>% 
            mutate(title=paste0( "Autores: ",autores,"<br/>",
                                 "Fuerza Colaboración: ",fuerza_colaboracion,"<br/>",
                                 "Cantidad Coautorias: ",weight)) %>% 
            pull(title)
        
        # cat(vertex_attr_names(tmp_grafo))
        # name id anio fuerza_colaboracion cant_autores size
        nodos_df <- data.frame(name=V(tmp_grafo)$label,
                               id=V(tmp_grafo)$id,
                               fuerza_colaboracion=V(tmp_grafo)$fuerza_colaboracion,
                               cant_autores=V(tmp_grafo)$cant_autores,stringsAsFactors = FALSE) %>%
            as_tibble() 
        # nodos_df %>% glimpse()
        
        title_vertex_tooltip <- nodos_df %>% mutate(title=paste0("Autor: ",name,"<br/>",
                                                                 "Fuerza Colaboración: ",fuerza_colaboracion)
        ) %>% 
            pull(title)
        
        label_vertex <- nodos_df %>% pull(name)
        
        tmp_grafo <-  tmp_grafo %>% 
            set_edge_attr(name="width",value = filter_width_resultado) %>% 
            set_edge_attr(name="color",value = filter_color_resultado) %>% 
            set_edge_attr(name="title",value = title_edges_tooltip) %>% 
            set_vertex_attr(name="title",value = title_vertex_tooltip) %>%
            set_vertex_attr(name="label",value = label_vertex)
        
        
        #
        tmp_layout <- if_else(input$input_static_layout_select=='','layout_nicely',input$input_static_layout_select)
        
        visNetwork:::visIgraph(tmp_grafo,
                  idToLabel = FALSE,
                  randomSeed = input$semilla_seed) %>% 
            visNetwork:::visNodes(size = 10) %>%
            visNetwork:::visIgraphLayout(randomSeed = input$semilla_seed,
                                         layout=tmp_layout) %>% 
            visNetwork:::visOptions( # selectedBy= list(variable = "label"), # esto hace aparecer combos en la red.
                highlightNearest = list(enabled = TRUE, hover = TRUE)
                
                #nodesIdSelection = list(useLabels=TRUE) 
                ) %>%
            #NULL
            visNetwork:::visEvents(click = "function(clickEvent){
                      nodesVar = clickEvent.nodes[0];
                      edgesVar = clickEvent.edges[0];
                      if (nodesVar == null & edgesVar == null){
                      //Shiny.onInputChange('input_static_network_click_vertex', '');
                      //Shiny.onInputChange('input_static_network_click_edge', '');
                      }
                      
                      if (nodesVar != null){
                      Shiny.onInputChange('input_static_network_click_vertex', nodesVar);
                      Shiny.onInputChange('input_static_network_click_edge', '');
                      }
                      
                      if (nodesVar == null & edgesVar != null){
                      Shiny.onInputChange('input_static_network_click_edge', edgesVar);
                      Shiny.onInputChange('input_static_network_click_vertex', '');
                      }
                      //alert(nodesVar,edgesVar);
                      ;}")
        # ) %>%
        # visLegend(addEdges = edge_labels_reactive(), useGroups = FALSE)
})
    
    # output_static_download_network
    output$output_static_download_network <- downloadHandler(filename = function() {
        paste('red-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
        
        withProgress(message = 'Exportando la red ... ', value = 0, {
            incProgress(1/4, detail = paste("Exportando visualización ... ", 1))
            grafo_reactive_tmp <- static_network_grafo_reactive()
            output_static_network_reactive() %>% visSave(con,selfcontained = TRUE)
        })
        
    }
    )
    # output_static_network: render de la visualizacion
    output$output_static_network <- renderVisNetwork({
        withProgress(message = 'Generando la red ... ', value = 0, {
            incProgress(1/2, detail = paste("Generando red ... ", 1))
            grafo_reactive_tmp <- static_network_grafo_reactive()
            incProgress(2/2, detail = paste("Generando visualización ... ", 2))
            output_static_network_reactive()
        })
    })
    
    # opciones de layouts
    layouts_disponibles_options <- reactive({
        layouts_disponibles_list
    })
    # si cambia el grafo, se cambia las opciones de dibujarlo
    # en realidad deberia ser si se selecciona periodo, 
    observe({
        req(input$input_static_periodos)
        updateSelectizeInput(session, "input_static_layout_select", choices = layouts_disponibles_options())
    })
    
    observe({
        static_network_grafo_reactive()
        updateSliderInput(session, "input_static_network_filter_edges_threshold", 
                          min = edge_filter_range_valor()[1],
                          max = edge_filter_range_valor()[2],
                          value=edge_filter_range_valor()[2],
                          step =1
        )
    })  
    
    edge_filter_range_valor <- reactive({
        resultado <- range(ceiling(E(static_network_grafo_reactive())$width/static_edge_width_multiplier))
        resultado
    })
# estatico 1: NETWORK: biofabric -----------------------------------------------------------------------------------------------
    output$output_biofabric_net <- renderBioFabric_htmlwidget({
        
        withProgress(message = 'Generando biofabric ... ', value = 0, {
            incProgress(1/2, detail = paste("Generando red ... ", 1))
            grafo_reactive_tmp <- static_network_grafo_reactive()
            incProgress(2/2, detail = paste("Generando visualización ... ", 2))
            
            grafo_names <- grafo_reactive_tmp
            
            V(grafo_names)$name <- V(grafo_names)$label
            
            bioFabric_htmlwidget(bioFabric(grafo_names),height = 500)
        })
        
    })
    
# estatico 1: NETWORK: heatmap -----------------------------------------------------------------------------------------------
    output$output_heatmap_net <- renderPlotly({
        withProgress(message = 'Generando heatmap ... ', value = 0, {
            incProgress(1/2, detail = paste("Generando red ... ", 1))
            grafo_reactive_tmp <- static_network_grafo_reactive()
            incProgress(2/2, detail = paste("Generando visualización ... ", 2))
            
            grafo_names <- grafo_reactive_tmp
            
            plot_out <- armar_heatmap_ggplot_from_grafo(grafo_names)
            
            # V(grafo_names)$name <- V(grafo_names)$label
            
            # bioFabric_htmlwidget(bioFabric(grafo_names))
            ggplotly(plot_out,height=500) 
        })
        
        
        
    })
    
    
    # estatico 2: ARTICULOS -----------------------------------------------------------------------------------------------
    # WISHLIST MATRIZ: ggplot(melt(m), aes(Var1,Var2, fill=value)) + geom_raster()
    # mostrar resumen autor
    # mostrar resumen relacion
    # mostrar articulos asociados autor
    # mostrar articulos asociados relacion
    # si es autor, mostrar network pequeña de relaciones de ese autor y fuerza de colaboracion como paper newman.
    
    filtered_articulos_anios_edge_reactive <- reactive({
        req(input$input_static_network_click_edge)
        # input.input_static_network_click_vertex
        # input.input_static_network_click_edge
        
        input_edge <- input$input_static_network_click_edge
        current_db <- static_data_base()
        
        ret <- get_art_asoc_from_click_edge(input_edge,current_db)
        ret
    })
    
    filtered_articulos_anios_autor_reactive <- reactive({
        
        
        # req(input$input_static_network_click_vertex)
        # input.input_static_network_click_vertex
        # input.input_static_network_click_edge
        g <- static_network_grafo_reactive()
        db <-  static_data_base()
        vertice <- get_vertex_from_click_vertex(g,input$input_static_network_click_vertex)
        autor <-  as.character(vertice$label)

        ret <- obtener_listado_articulos_vertice(db,autor)

        ret
    })
    
    subgrafo_coautoria_autor <- reactive({
        g <- static_network_grafo_reactive()

        vertice <- get_vertex_from_click_vertex(g,input$input_static_network_click_vertex)

        subgrafo_autor <- generar_subgrafo_vecinos (g,vertice,input$semilla_seed)

        subgrafo_autor
    })
    
    
    # output output_subgrafo_coautoria_autor: render de la visualizacion acotado a autor seleccionado
    output$output_subgrafo_coautoria_autor <- renderVisNetwork({

        ret <- generar_visualizacion_subgrafo_vecinos_from_subgrafo(subgrafo_coautoria_autor(),input$semilla_seed)
        
        ret
    })
    
    
    output$output_heatmap_coautoria_autor <- renderPlotly({ 
        
        g <- subgrafo_coautoria_autor()
        
        plot_out <- armar_heatmap_ggplot_from_grafo(g)
        
        ggplotly(plot_out) 
    })
    
    info_seleccion_vertex_degree_reactive <- reactive({
        g <- static_network_grafo_reactive()
        
        vertice <- get_vertex_from_click_vertex(g,input$input_static_network_click_vertex)
        
        grado <- igraph:::degree(g,vertice)
        #glimpse(grado)
        as.numeric(grado)
    })
    
    output$output_info_seleccion_vertex_degree <- renderUI({

        span (info_seleccion_vertex_degree_reactive(),id="grado_0") # me esta devolviendo id 0 :S
    })
    
    
    output$output_info_seleccion_vertex <- renderUI({
        g <- static_network_grafo_reactive()

        vertice <- get_vertex_from_click_vertex(g,input$input_static_network_click_vertex)
        
        current_select <- filtered_articulos_anios_autor_reactive()
        
        # autores <- current_select %>% select(autores) %>%filter()
        anio <- current_select %>% data.frame() %>% arrange(anio) %>% pull(anio) %>% unique() %>% paste(collapse = ";") #%>% unique()
        # cant_autores <- current_select %>% pull(anio) %>% unique()
        articulo <- nrow(current_select)
        
        div(
            br(),
            p(strong("Autor: "),vertice$label),
            p(strong("Periodos participación: "),anio),
            p(strong("Cantidad Artículos Relacionados: "),articulo),
            p(strong("Cantidad Autores Relacionados: "),igraph:::degree(g,vertice)),
            p(strong("Fuerza colaboración total: "),vertice$fuerza_colaboracion)#,
            # p(paste0("Fuerza colaboración total: ",vertice$fuerza_colaboracion)),
        )
        
        # hacer output ui de todas las priopiedades
        
        
    })
    
    output$output_info_seleccion_edge <- renderUI({
        g <- static_network_grafo_reactive()
        # "Alicia Liliana Caratini--Francisco Raúl Carnese"
        filter_cond <- str_detect(igraph:::E(g)$id,pattern = input$input_static_network_click_edge)
        edge <- igraph:::E(g)[filter_cond]
        
        #"weight" "id" "fuerza_colaboracion" "autores" "color" "width"
        current_select <- filtered_articulos_anios_edge_reactive()
        anio <- current_select %>% data.frame() %>% arrange(anio) %>% pull(anio) %>% unique() %>% paste(collapse = ";") #%>% unique()
        
        div(
            br(),
            p(strong("Autores: "),edge$autores),
            p(strong("Periodos participación: "),anio),
            p(strong("Cantidad Artículos Relacionados: "),edge$weight),
            p(strong("Fuerza colaboración: "),edge$fuerza_colaboracion)#,
        )
        
        # current_select <- filtered_articulos_anios_autor_reactive()
        
        # autores <- current_select %>% select(autores) %>%filter()
        # anio <- current_select %>% data.frame() %>% arrange(anio) %>% pull(anio) %>% unique() %>% paste(collapse = ";") #%>% unique()
        # cant_autores <- current_select %>% pull(anio) %>% unique()
        # articulo <- nrow(current_select)
        
        # div(
        #     br(),
        #     p(strong("Autor: "),vertice$label),
        #     p(strong("Periodos participación: "),anio),
        #     p(strong("Cantidad Artículos Relacionados: "),articulo),
        #     p(strong("Cantidad Autores Relacionados: "),degree(g,vertice)),
        #     p(strong("Fuerza colaboración total: "),vertice$fuerza_colaboracion)#,
        #     # p(paste0("Fuerza colaboración total: ",vertice$fuerza_colaboracion)),
        # )
        # 
        # hacer output ui de todas las priopiedades
        
        
    })
    
    output$output_doc_asociados_edge <- DT::renderDataTable({
        
        tmp <- filtered_articulos_anios_edge_reactive() %>% 
            rename(Autores=autores,
                   Periodo=anio,
                   'Artículo'=articulo,
                   '#Autores'=cant_autores,
                   'Fuerza Colaboración'=fuerza_colaboracion)
        
        dt_result <- DT::datatable(options = list(language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            tmp,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none') %>% 
            formatRound('Fuerza Colaboración',2) 
        
        dt_result
    })
    
    output$output_doc_asociados_vertex <- DT::renderDataTable({
        
        
        tmp <- filtered_articulos_anios_autor_reactive() %>% 
            rename(Autores=autores,
                   Periodo=anio,
                   'Artículo'=articulo,
                   '#Autores'=cant_autores,
                   'Fuerza Colaboración'=fuerza_colaboracion)
        
        dt_result <- DT::datatable(options = list(language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            tmp,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none') %>% 
            formatRound('Fuerza Colaboración',2) 
        
        dt_result
    })
    
    doc_asociados_reactive <- reactive({
        
        tmp <- ""
        if (input$input_static_network_click_edge!="") {
            tmp <- filtered_articulos_anios_edge_reactive() %>% 
                rename(Autores=autores,
                       Periodo=anio,
                       'Artículo'=articulo,
                       '#Autores'=cant_autores,
                       'Fuerza Colaboración'=fuerza_colaboracion) 
        }else if(input$input_static_network_click_vertex!=""){
            tmp <- filtered_articulos_anios_autor_reactive() %>% 
                rename(Autores=autores,
                       Periodo=anio,
                       'Artículo'=articulo,
                       '#Autores'=cant_autores,
                       'Fuerza Colaboración'=fuerza_colaboracion)
        }
        
        tmp
    })
    
    output$download_art_asoc <- downloadHandler( 
        filename = paste('art_asoc-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(doc_asociados_reactive(), file,row.names = FALSE)
        })
    
    
    # estatico 3: ESTRUCTURA -----------------------------------------------------------------------------------------------
    
    # calcula la estructura de la red, lo pone en un data frame
    estructura_red_reactive <- reactive({
        grafo_reactive_tmp <- static_network_grafo_reactive()
        base_autores_tmp <- static_data_base()
        est_vars <- calcular_estructura_grafo(grafo_reactive_tmp,base_autores_tmp)
        est_vars
    })
    
    # para mostrar la estructura de red
    output$output_static_estructura_red_DT <- DT::renderDataTable({
        estructura_con_nuevos_header_DT <- estructura_grafo_para_DT(estructura_red_reactive(),dt_option_dom='t')
        estructura_con_nuevos_header_DT
    })
    
    # para bajar la estructura de red actual
    output$output_static_download_estructura_red <- downloadHandler( 
        filename = paste('estrucutra_red-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(estructura_red_reactive(), file,row.names = FALSE)
        })
    
# estructura de nodos  -----------------------------------------------------------------------------------------------
    
    # dentro del observe event para que si cambia la seleccion se actualice
    observeEvent(input$input_static_periodos,{
        
        callModule(estructura_nodos_server, "est_nodos",
                   static_network_grafo_reactive()# parametros del componente: grafo
        )
    })
    
    # estructura_nodos_grafo_reactive <- reactive({
    #     grafo_reactive_tmp <- static_network_grafo_reactive()
    #     
    #     estructura_red_df <- metricas_nodos_grafo(grafo_reactive_tmp) %>%
    #         
    #         rename('Autor' = autor,
    #                'Grado' = degree,
    #                'Betweeness' = betweeness,
    #                'Eigen Centrality' = eigen_centrality,
    #                'Closeness' = closeness,
    #                'Page Rank' = page_rank,
    #                '# Triangulos' = count_triangles,
    #                'Fuerza Colaboración' = fuerza_colaboracion)
    #     
    #     estructura_red_df
    # })
    # 
    # estructura_red_grafo_dt <- reactive({
    #     
    #     dt_return <- DT::datatable(options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
    #                                estructura_nodos_grafo_reactive(),
    #                                escape = FALSE,
    #                                rownames = FALSE,
    #                                selection = 'none') %>%
    #         formatRound('Betweeness',4) %>%
    #         formatRound('Eigen Centrality',4) %>%
    #         formatRound('Closeness',7) %>%
    #         formatRound('Page Rank',4) %>%
    #         formatRound('Fuerza Colaboración',4) 
    #     
    #     dt_return
    # })
    # 
    # output$output_static_estructura_nodos_table <- DT::renderDataTable({ 
    #     # ver inconexos, ver, top 10 , ver last 10 o last N , sin contar inconexos. 
    #     # todo eso sale de la tabla. 
    #     # hay que agregar tooltips de inteprretacion.
    #     estructura_red_grafo_dt()
    # })
    # 
    # # exportar nodos
    # output$output_static_download_est_nodos <- downloadHandler( 
    #     filename = paste('est_nodos-', Sys.Date(), '.csv', sep=''), content = function(file) {
    #         write.csv(estructura_nodos_grafo_reactive(), file,row.names = FALSE)
    #     })

# estructura - COMPONENTES ------------------------------------------------
    observeEvent(input$input_static_periodos,{

    callModule(subgrafos_server, "estr_componentes",
               static_network_grafo_reactive(), # parametros del componente: grafo
               static_data_base() # parametros del componente: base articulos
               )
    })

    
# estructura - SIMULACION -------------------------------------------------

    # si cambia el grafo se vuelve a renderizar el modulo.
    observeEvent(input$input_static_periodos,{
        
        callModule(estructura_modelos_server, "modelos",
                   static_network_grafo_reactive(), # parametros del componente: grafo # parametros del componente: base articulos
                   modelado_df_vars_list # listado de variables para la seleccion en histogramas.
        )
    })
    
    # # var comparar simulacion
    # estructura_red_vars_compara_simu_reactive <- reactive({
    #     estructura_red_vars_compara_simu_list
    # })
    # por tema de simulacion
    observe({
        estructura_red_reactive()
        updateSelectizeInput(session, "input_static_simulacion_sel_variable", 
                             choices = estructura_red_vars_compara_simu_list)# sacado de globales
    })
    
    grafos_simulados_reactive <- reactive({
        # input$compara_estructura_boton
        grafo_reactive_tmp <- static_network_grafo_reactive()
        generar_grafos_similares(grafo_reactive_tmp,1000)
    })
   
    
    

 
    output$output_static_estructura_red_simulacion_comparativa <- renderPlotly({ 
        # para comparar la red con 1000 redes con similares caracteristicas.
        # para ver que tan en la media cae la nuestra.
        
        # para que se refresque si se aprieta el boton.
        # input$compara_estructura_boton
        req(input$input_static_simulacion_sel_variable)
        # para agregar la parte de la simulacion progreso
        # https://shiny.rstudio.com/articles/progress.html
        withProgress(message = 'Simulando ... ', value = 0, {
            
            grafo_reactive_tmp <- static_network_grafo_reactive()
            
            incProgress(1/4, detail = paste("Generando grafos ... ", 1))
            
            lista_generados <- grafos_simulados_reactive()
            
            incProgress(2/4, detail = paste("Calculando valores ... ", 2))
            
            estructuras_grafos <- map(lista_generados,calcular_estructura_sobre_grafo) %>% reduce(union_all)
            
            estructura_grafo_activo <- estructura_red_reactive()
            
            incProgress(3/4, detail = paste("Generando plot ... ", 3))
            
            delta_mas_menos <- 5
            
            min_x <- 0
            max_x <- 100
            num_bins <- if_else(max_x<2,20,100)
            
            var_comparar <- input$input_static_simulacion_sel_variable
            
            plot_out <- ggplot(data=estructuras_grafos, 
                               aes(x= estructuras_grafos[[var_comparar]])) + 
                theme_light()+
                geom_histogram(bins = num_bins) + 
                xlab(var_comparar) + ylab('Frecuencia') +
                geom_vline(xintercept = estructura_grafo_activo[[var_comparar]], colour='red')
                
            
            
            incProgress(4/4, detail = paste("Fin simulación ... ", 4))
        })
        
        ggplotly(plot_out)
    })
    
    
    # estatico # 4: COMUNIDADES  #-----------------------------------------------------------------------------------------------
    observe({
        req(input$input_static_periodos)
        req(input$comunidades_sel_algo)
        req(input$comunidades_n_view)
        
        comunidad_sel_detalles <- community_reactive()
        cantidad_coms <- input$comunidades_n_view
        update_static_network_comunities_df <- armar_df_comunidades(cantidad_coms,comunidad_sel_detalles)
        
        visNetworkProxy("output_static_network") %>%
            visUpdateNodes(nodes=update_static_network_comunities_df)
    })
    community_reactive <- reactive({
        req(input$input_static_periodos)
        req(input$comunidades_sel_algo)
        comunidad_sel_detalles <- arma_comunidad(input$semilla_seed,static_network_grafo_reactive(),input$comunidades_sel_algo)
    })

    # comunidades - modulo ----------------------------------------------------

    
    observeEvent(input$comunidades_sel_algo,{
        
        callModule(comunidades_server, "comunidades_panel",
                   static_network_grafo_reactive(), # parametros del componente: grafo
                   static_data_base(), # parametros del componente: base articulos
                   community_reactive()
        )
    })
    
    
    # comunidades - resultado info ----------------------------------------------------------
    output$comunidades_result <- renderUI ({
        
        current_comunidad <- community_reactive()
        
        mensaje_algo <- paste0('Para la búsqueda de comunidades se usó el Algoritmo: ', algorithm(current_comunidad))
        mensaje_cantidad <- paste0('<p>Se generaron: ', length(current_comunidad), ' comunidades.</p>')
        
        # infomap muestra diferente.
        if(str_detect(algorithm(current_comunidad),'Info')){
            mensaje_cantidad <- paste0('<p>Se generaron: ', code_len(current_comunidad), ' comunidades.</p>')
        }
        # recordar hay un comparador de comunidades, por si quiero comparar resultados de algoritmos.
        # se puede comparar quienes estan en las comunidaes.
        mensaje_final <- paste0(mensaje_algo,mensaje_cantidad)
        
        HTML(mensaje_final)
    }) 
    
    # comunidades - resultado tabla ---------------------------------------------------------
    listado_comunidades_estructura_reactive <- reactive({
        current_comunidad <- community_reactive() 
        current_grafo <- static_network_grafo_reactive()  
        current_base_autores <- static_data_base()
        
        resul_acum <- estructura_comunidades_df(current_grafo,current_base_autores,current_comunidad)
        
        resul_acum
    })
    
    output$comunidades_result_metricas_listado <- DT::renderDataTable({ 
        str_comunidades <- listado_comunidades_estructura_reactive()
        resultado_dt <- estructura_grafo_para_DT(str_comunidades)
        resultado_dt
    })
    
    
    # comunidades - tabla - download --------------------------------------------------------
    
    output$download_com_metricas <- downloadHandler( 
        filename = paste('com_metricas-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
            write.csv(listado_comunidades_estructura_reactive(), file,row.names = FALSE)
        })  
    
    # comunidades - autores - tabla ---------------------------------------------------------
    
    listado_comunidades_autores_reactive <- reactive({
        autores_db <- static_data_base()
        current_comunidades <- community_reactive()
        result <- listado_comunidades_autores (current_comunidades,autores_db)
        result
    })
    
    output$cantidad_comunidades_metricas <- DT::renderDataTable({ 
        DT::datatable(
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            listado_comunidades_autores_reactive(),
            escape = FALSE,
            selection = 'single')
    })
    
    # comunidades - autores - download ---------------------------------------------------------
    
    output$download_com_metricas_aut <- downloadHandler( 
        filename = paste('com_metricas_aut-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
            write.csv(listado_comunidades_autores_reactive(), file,row.names = FALSE)
        })  
    
    # comunidades - detalle - info ----------------------------------------------------
    
    output$detalle_comunidad_seleccionada <- renderUI({
        
        current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        mensaje <- paste0('Se selecciono la comunidad: ' ,current_com_selected[1])
        mensaje2 <- paste0('Con una cantidad de ' ,current_com_selected[2], ' integrantes. ')
        mensaje3 <- paste0('Siendo: ' ,current_com_selected[3] )
        mensaje_final <- paste('<p>',mensaje,mensaje2,'</p><p>',mensaje3,'</p>')
        HTML(mensaje_final)
    })
    
    detalle_comunidad_seleccionada_metricas_subgrafo_reactive <- reactive({
        req(input$cantidad_comunidades_metricas_rows_selected)
        current_comunidades <- community_reactive()
        
        current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        
        current_id <- current_com_selected[,1] %>% as.character()
        
        current_group <- igraph::groups(current_comunidades)[[current_id]]
        
        tmp_grafo <- static_network_grafo_reactive()
        
        current_subgraph <- induced_subgraph(tmp_grafo,current_group) 
        
        # obtener base dado un grafo en un periodo particular, filtrado por los autores participantes
        current_autores <- V(current_subgraph)$label
        base_autores_tmp <- static_data_base() %>% filter(autor %in% current_autores)
        
        estructura_grafo_df <- calcular_estructura_grafo(current_subgraph,base_autores_tmp)
        
        estructura_grafo_df
    })
    
    output$detalle_comunidad_seleccionada_metricas_subgrafo_table <- DT::renderDataTable({
        temp_df <- detalle_comunidad_seleccionada_metricas_subgrafo_reactive() 
        resultado_dt <- estructura_grafo_para_DT(temp_df)
        resultado_dt
    })
    
    # comunidades - detalle - tabla -------------------------------------------
    
    detalle_comunidad_seleccionada_articulos_reactive <- reactive({
        
        current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        autores_comunidad <- str_split(current_com_selected[3],pattern = '; ') %>% unlist()
        autores_db <- static_data_base()
        input_periodos <-  input$input_static_periodos
        
        filtro_coautores <- autores_db %>% 
            filter(anio %in% input_periodos) %>% 
            filter(autor %in% autores_comunidad) %>%
            select(autores,anio,titulo,url,cant_autores,fuerza_colaboracion) %>%
            mutate(articulo=paste0("<p><a target='_blank' href='",url,"'>",titulo,"</a></p>")) %>% 
            group_by(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% tally() %>% 
            select(autores,anio,articulo,cant_autores,fuerza_colaboracion)
        
        filtro_coautores
    })
    
    output$detalle_comunidad_seleccionada_articulos <- DT::renderDataTable({
        # temp_df <- detalle_comunidad_seleccionada_articulos_reactive()
        # DT::datatable(
        #     options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
        #     temp_df,
        #     escape = FALSE,
        #     selection = 'none')
        
        
        tmp <- detalle_comunidad_seleccionada_articulos_reactive() %>% 
            rename(Autores=autores,
                   Periodo=anio,
                   'Artículo'=articulo,
                   '#Autores'=cant_autores,
                   'Fuerza Colaboración'=fuerza_colaboracion)
        
        dt_result <- DT::datatable(options = list(language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            tmp,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none') %>% 
            formatRound('Fuerza Colaboración',2) 
        
        dt_result
    })
    
    # comunidades - detalle - download ----------------------------------------
    output$download_com_sel_arts <- downloadHandler( 
        filename = paste('com_sel_arts-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
            com_reactive_to_csv <- detalle_comunidad_seleccionada_articulos_reactive() %>%
                mutate(titulos=str_replace_all(articulo,"<p><a target='_blank' href='",';')) %>%
                mutate(titulos=str_replace_all(titulos,"'>",'\\|')) %>%
                mutate(titulos=str_replace_all(titulos,"</a></p>",'')) %>%
                separate_rows(titulos,sep = ";") %>%
                filter(titulos != "") %>% separate(titulos,c("URL","Título"),sep="\\|") %>%
                rename(Autores=autores,
                       Periodo=anio,
                       'Artículo'=articulo,
                       'Cantidad_Autores'=cant_autores,
                       'Fuerza_Colaboración'=fuerza_colaboracion) %>% data.frame(stringsAsFactors = FALSE) %>% 
                select(Periodo,Autores,"Título",'Cantidad_Autores','Fuerza_Colaboración',URL)
            
            write.csv(com_reactive_to_csv, file,row.names = FALSE)
        })
    
    # TEMPORAL #-----------------------------------------------------------------------------------------------
    
    temporal_estructuras_reactive_vars <- reactive({
        ret <- estructura_red_vars_compara_simu_list
        
        ret 
    })
    temporal_nodos_reactive_vars <- reactive({
        ret <- estructura_red_nodos_vars_compara_simu_list
        ret 
    })
    
    temporal_estructuras_reactive_vars_tibble <- reactive({
        current_metricas <- temporal_estructuras_reactive_vars() %>% as_tibble()
        current_metricas$nombre <- names(temporal_estructuras_reactive_vars())
        current_metricas
    })
    
    temporal_nodos_reactive_vars_tibble <- reactive({
        current_metricas <- temporal_nodos_reactive_vars() %>% as_tibble
        current_metricas$nombre <- names(temporal_nodos_reactive_vars())
        current_metricas
    })
    
    # temporal - basico -------------------------------------------------------
    
    # para inicializar los valores de estructura disponibles 
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "temporal_sel_vars", choices = temporal_estructuras_reactive_vars())
    })

    # anios sin acumular
    temporal_basico_grafos_reactive <- reactive({
        listado_grafos <- temporal_basico_grafos
        
        # semilla=input$temporal_semilla_seed
        listado_grafos
    })
    
    # temporal - basico - output ----------------------------------------------
    
    # output temporal plot metricas anio sin acumular
    output$temporal_grafos_plot_1 <- renderPlotly({
        # input$temporal_estructura_refrescar_boton
        # temporal_grafos_estr_plot
        
        withProgress(message = 'Armando visualización ...', value = 0, {
            # Number of times we'll go through the loop
            n <- 4
            indice <- 1
            incProgress(indice/n, detail = paste("Armando grafos ...", indice)) #1 
            indice <- indice+1
            grafos <- temporal_basico_grafos_reactive()
            incProgress(indice/n, detail = paste("Calculando medidas ...", indice)) # 2
            indice <- indice+1

            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            plot_out <- temporal_basico_grafo_estr_as_stack %>% 
                filter(metrica %in% input$temporal_sel_vars) %>%
                rename(Periodo = periodo, 
                       'Métrica' = metrica , 
                       Valor = valor ) %>% 
                ggplot(aes(x=Periodo,
                           y=Valor,
                           color=Métrica,
                           group=Métrica,
                           text=plot_text)) +
                geom_point() + 
                geom_line() +
                theme_light()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                labs(x="")
        })
        
        # config(ggplotly(plot_out,tooltip="text"), locale="es")
        ggplotly(plot_out,tooltip="text")
    })
    
    output$download_temporal_basico_estruct <- downloadHandler( 
        filename = paste('temp_simple_-', Sys.Date(),'-', '.csv', sep=''), content = function(file) {
            #temporal_acumulado_estr_grafos
            #temporal_basico_estr_grafos
                    write.csv(temporal_basico_estr_grafos, file,row.names = FALSE)
    })

    # temporal - acumulado -------------------------------------------------------
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        input$anio
        updateSelectizeInput(session, "temporal_sel_vars_2", choices = temporal_estructuras_reactive_vars())
    })
    
    temporal_grafos_acum_reactive <- reactive({
# 
#         tmp_anios_acum_2 <- temporal_generar_grafos_acumulados(global_periodos_disponibles,
#                                                                cota_seccion,
#                                                                db_limpia,
#                                                                static_edge_width_multiplier)
        
        tmp_anios_acum_2 <- temporal_acumulado_grafos
        
        tmp_anios_acum_2
    })
    
    output$download_temporal_acumulado_estruct <- downloadHandler( 
        filename = paste('temp_acumulado_-', Sys.Date(),'-', '.csv', sep=''), content = function(file) {
            #temporal_acumulado_estr_grafos
            #temporal_basico_estr_grafos
            write.csv(temporal_acumulado_estr_grafos, file,row.names = FALSE)
    })
    
# temporal - acumulado - output ----------------------------------------------
    output$temporal_grafos_plot_2 <- renderPlotly({
        # input$temporal_estructura_refrescar_boton
        # temporal_grafos_estr_plot
        
        withProgress(message = 'Armando visualización ...', value = 0, {
            # Number of times we'll go through the loop
            n <- 4
            indice <- 1
            incProgress(indice/n, detail = paste("Armando grafos ...", indice)) #1 
            indice <- indice+1

            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            plot_out <- temporal_acumulado_grafo_estr_as_stack %>% 
                filter(metrica %in% input$temporal_sel_vars_2) %>%
                rename(Periodo = periodo, 'Métrica' = metrica , Valor = valor ) %>% 
                ggplot(aes(x=Periodo,y=Valor,color=Métrica,group=Métrica,text=plot_text)) +
                geom_point() + 
                geom_line() + 
                theme_light()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                labs(x="")
        })
        plotly_out <- ggplotly(plot_out,tooltip="text")
        
        # config(plotly_out, locale="es")
        ggplotly(plot_out,tooltip="text")
    })
    
# temporal - top N - acumulados --------------------------------------------

    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "temporal_sel_vars_3", choices = temporal_nodos_reactive_vars())
        updateSelectizeInput(session, "temporal_sel_vars_anual", choices = temporal_nodos_reactive_vars())
        
    })
    
# temporal - top N - acumulados - output -----------------------------------
    
    temporal_grafos_top_n_acum_metricas_reactive <- reactive({
        
        # grafos <- temporal_grafos_acum_reactive()
        # calculo_grafos <- map(grafos,metricas_nodos_grafo)
        # names(calculo_grafos) <- global_periodos_disponibles
        # calculo_grafos
        
        temporal_acumulado_nodos_grafos
        
    })

# temporal - top n anual - download  ----------------------------------------------------

    
    output$download_temporal_grafos_top_n_anual <- downloadHandler( 
        filename = paste('temp_grafos_top_n_anual-', Sys.Date(),'-', '.csv', sep=''), content = function(file) {
            req(input$top_n_periodos_anual)
            req(input$temporal_sel_vars_anual)
            
            resu <- temporal_grafos_top_n_anual_data_reactive()
            #temporal_acumulado_estr_grafos
            #temporal_basico_estr_grafos
            write.csv(resu, file,row.names = FALSE)
        })
 
# temporal - top n total - download  ----------------------------------------------------
    
    output$download_temporal_grafos_top_n <- downloadHandler( 
        filename = paste('temp_grafos_top_n_-', Sys.Date(),'-', '.csv', sep=''), content = function(file) {
            #temporal_acumulado_estr_grafos
            #temporal_basico_estr_grafos
            write.csv(temporal_grafos_top_n_acum_metricas_reactive(), file,row.names = FALSE)
    })
    
    temporal_grafos_top_n_anual_data_reactive <- reactive({
        req(input$top_n_periodos_anual)
        req(input$temporal_sel_vars_anual)
        
        cant_n <- input$top_n_periodos_anual;
        metrica_s <- input$temporal_sel_vars_anual;
        metrica_sym <- rlang::sym(metrica_s) 
        
        resu <- temporal_acumulado_nodos_grafos %>% 
            nest(-periodo) %>% 
            mutate(top_n_periodo=purrr::map(data,
                                            cant=cant_n,
                                            metrica=metrica_s,
                                            .f=function(d,metrica,cant){
                                                metrica_sym <-rlang::sym(metrica_s)
                                                # ranking <- seq(from=1,to = cant,by = 1)
                                                # ret <- d %>% arrange(desc(!!metrica_sym)) %>% head(cant) %>% 
                                                #     mutate(posicion=ranking) %>% 
                                                #     mutate(valor_seleccion= !!metrica_sym)
                                                # ret
                                                nested_arranged <- d %>% 
                                                    nest(-!!metrica_sym) %>% 
                                                    arrange(desc(!!metrica_sym))
                                                
                                                ranking <- seq(from=1,to = nrow(nested_arranged),by = 1)
                                                
                                                ret <- nested_arranged %>% 
                                                    mutate(posicion=ranking) %>% 
                                                    mutate(valor_seleccion= !!metrica_sym) %>% 
                                                    head(cant)
                                                
                                                ret <- ret %>% unnest() 
                                                
                                                ret
                                                
                                            }))
        
        resu
    })
# temporal - top n anual - plot -------------------------------------------
    
    output$temporal_grafos_top_n_anual <- renderPlotly({
        req(input$top_n_periodos_anual)
        req(input$temporal_sel_vars_anual)
        
        cant_n <- input$top_n_periodos_anual;
        metrica_s <- input$temporal_sel_vars_anual;
        metrica_sym <- rlang::sym(metrica_s) 
        
        
        withProgress(message = 'Armando visualización ...', value = 0, {
            # Number of times we'll go through the loop
            n <- 4
            indice <- 1
            incProgress(indice/n, detail = paste("Armando grafos ...", indice)) #1
            indice <- indice+1
            grafos <- temporal_grafos_acum_reactive()
            incProgress(indice/n, detail = paste("Calculando medidas ...", indice)) # 2
            indice <- indice+1
            # calculo_grafos <- temporal_grafos_top_n_acum_metricas_reactive()
            incProgress(indice/n, detail = paste("Armando estructura ...", indice)) # 3
            indice <- indice+1
            # estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names)
            # glimpse(estr_grafos)
            
            
            resu <- temporal_grafos_top_n_anual_data_reactive()
            
 
            incProgress(indice/n, detail = paste("Armando Plot ...", indice)) # 3
            indice <- indice+1
            # req(input$top_n_periodos_anual)
            # req(input$temporal_sel_vars_anual)
            metrica_nombre <- estructura_red_nodos_vars_compara_simu_DF %>%  
                filter( value == input$temporal_sel_vars_anual  ) %>% 
                pull(name)
            plot_out <- resu %>% 
                unnest(top_n_periodo) %>%
                mutate(autor=fct_reorder(autor,posicion,.desc = TRUE)) %>% 
                mutate(posicion=as.factor(posicion)) %>% 
                ggplot(aes(x=periodo,y=autor,color=posicion,group=posicion,label=valor_seleccion))+
                geom_point()+
                geom_line(show.legend = FALSE)+
                theme_light()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1),
                      axis.text.y = element_text(size = 6))+
                labs(x="",y="",
                     color="Posición",
                     title=paste0("Ranking de ",input$top_n_periodos_anual," en la métrica ",metrica_nombre))

        })
        
        
        plotly::ggplotly(plot_out)
    })
    

# temporal - top n total - plot -------------------------------------------

 
    output$temporal_grafos_top_n_acum <- renderPlotly({
        # input$temporal_estructura_refrescar_boton
        # temporal_grafos_estr_plot
        
        withProgress(message = 'Armando visualización ...', value = 0, {
            # Number of times we'll go through the loop
            n <- 4
            indice <- 1
            incProgress(indice/n, detail = paste("Armando grafos ...", indice)) #1
            indice <- indice+1
            grafos <- temporal_grafos_acum_reactive()
            incProgress(indice/n, detail = paste("Calculando medidas ...", indice)) # 2
            indice <- indice+1
            # calculo_grafos <- temporal_grafos_top_n_acum_metricas_reactive()
            incProgress(indice/n, detail = paste("Armando estructura ...", indice)) # 3
            indice <- indice+1

            var_selected <- input$temporal_sel_vars_3
            # glimpse(var_selected)
            
            estr_grafos_2 <- temporal_grafos_top_n_acum_metricas_reactive() %>% 
                select(autor,periodo,var_selected) %>%
                gather(Métrica,Valor,-autor,-periodo)
            # glimpse(estr_grafos_2)
            
            top_5 <- estr_grafos_2 %>% 
                group_by(periodo) %>%
                top_n(n = input$top_n_periodos,wt=Valor ) %>%
                # arrange(periodo,desc(Valor))
                ungroup() %>% 
                mutate(autor=fct_reorder(autor,Valor))
            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            
            plot_out <- top_5 %>% ggplot(aes(x=periodo, 
                                             # y=reorder(autor,Valor),
                                             y=autor,
                                             color=Valor)) +
                geom_point(size=1,color='black',alpha=0.95) + 
                geom_point(size=0.75) +
                # xlab("Periodo") + ylab("Autor")+
                # scale_colour_gradient(low = "#a6bddb", high = "#034e7b") +
                scale_colour_gradient(low = "#d9edf7", high = "#2c7fb8") + 
                theme_light()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1),
                      axis.text.y = element_text(size = 6))+
                labs(x="",y="")
            
        })
        
        
        plotly::ggplotly(plot_out)
    })
    
    # temporal - dinamico - acumulado -----------------------------------------
    # agarrar el dinamico calculado , y sobre eso hacer las transformaciones de networkDynamic
    
    temporal_acumulado_network_pkg_reactive<- reactive({
        # nets <- generar_nets_from_igraph(temporal_grafos_acum_reactive())
        temporal_acumulado_network_pkg
    })
    
    temporal_dinamico_acumulado_reactive <- reactive({ 
        # tnet_time <- generar_dyn_net_from_nets(temporal_acumulado_network_pkg_reactive())
        # tnet_time
        temporal_dinamico_acumulado
    })
    
    output$temporal_dinamico_acumulado <- ndtv:::renderNdtvAnimationWidget({
        
        # current_test_tnet <- temporal_dinamico_acumulado_reactive()
        
        # ret <- armar_render_ndtvd3_animacion(current_test_tnet)
        
        # ret
        withProgress(message = 'Armando visualización ...', value = 0, {
            temporal_dinamico_acumulado_anim
        })
    })
    # temporal - dinamico - individual -----------------------------------------
    # agarrar el dinamico calculado , y sobre eso hacer las transformaciones de networkDynamic
    
    options_temporal_basico_periodos_reactive <- reactive({
        # como son 18 periodos , solo acepto divisores de 18
        # 1,3,4,9
        cantidad <- 18
        divisores <- c(1,2,3,6,9,18)
        opciones <- rev(c(cantidad/divisores)) 
        
        opciones
    })
    
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        updateSelectizeInput(session, "input_temporal_basico_periodos", choices =options_temporal_basico_periodos_reactive() )
    })
    
    temporal_basico_network_pkg_reactive<- reactive({
        nets <- generar_nets_from_igraph(temporal_basico_grafos_reactive())
    })
    
    temporal_dinamico_basico_reactive <- reactive({ 
        tnet_time <- generar_dyn_net_from_nets(temporal_basico_network_pkg_reactive())
        tnet_time
    })
    
    output$temporal_dinamico_basico <- ndtv:::renderNdtvAnimationWidget({
        
        req(input$input_temporal_basico_periodos)
        
        input_aggregation_value <- as.numeric(input$input_temporal_basico_periodos)
        
        current_test_tnet <- temporal_dinamico_basico_reactive()
        
        # glimpse(current_test_tnet)
        # .. ..$ na                        : logi FALSE
        # .. ..$ vertex.names              : chr "a0058"
        # .. ..$ active                    : num [1, 1:2] 1 18
        # .. ..$ anio.active               :List of 2
        # .. ..$ cant_autores.active       :List of 2
        # .. ..$ fuerza_colaboracion.active:List of 2
        # .. ..$ id.active                 :List of 2
        # .. ..$ id_old.active             :List of 2
        # .. ..$ label.active              :List of 2
        # .. ..$ size.active               :List of 2
        
        rend0 <- Sys.time()
        print(rend0)
        main_title_agg <- paste('Coautorías, ',
                                input_aggregation_value,
                                '-años agregados')
        slice_params<-list(start=0,end=18,
                           interval=input_aggregation_value, 
                        aggregate.dur=input_aggregation_value,
                        rule="earliest")#latest # earliest
        
        current_test_tnet<-compute.animation(current_test_tnet,
                                             slice.par=slice_params,
                                       default.dist=3,
                                       animation.mode='kamadakawai',
                                       # animation.mode='MDSJ',
                                       verbose=TRUE)# FALSE
        
        
        n_bins <- 3 # 5 : descartado x arcoiris
        color_palette <- "Blues"#"YlGnBu" #"YlOrBr" # Spectral 
        # render.d3movie(net, output.mode = 'htmlWidget')
        ret_render <- render.d3movie(current_test_tnet, 
                                     usearrows = FALSE, 
                                     displaylabels = FALSE, 
                                     # label=function(slice){slice%v%'vertex.names'},
                                     label=function(slice){slice%v%'label'},
                                     
                                     bg="#ffffff", 
                                     #vertex.border="#FAFAFA",
                                     vertex.border="lightgrey",
                                     vertex.cex = 0.5,
                                     vertex.col = function(slice){
                                         ret  <-  '#BABABA' # default color
                                         current_slice_var <- (slice %v% "fuerza_colaboracion")
                                         if( !is.null(current_slice_var)){
                                             if( length(current_slice_var) < n_bins){
                                                 # zVar <- (current_slice_var - mean(current_slice_var)) / sd(current_slice_var)
                                                 arma_bins <- cut_number(current_slice_var, n = n_bins)
                                                 levels(arma_bins ) <- brewer.pal(n_bins, color_palette)
                                                 
                                                 ret <- as.character(arma_bins)                                                 
                                             }
                                         }
                                         ret
                                     },
                                     edge.lwd = function(slice){ (slice %e% "fuerza_colaboracion") * 2 },
                                     edge.col = function(slice){slice %e% "color"},
                                     vertex.tooltip =  function(slice){
                                         # paste("<b>Autor:</b>", (slice %v% "vertex.names") , 
                                         paste("<b>Autor:</b>", (slice %v% "label") , "<br>",
                                               "<b>Fuerza Colaboración:</b>",(slice %v% "fuerza_colaboracion"),"<br>",
                                               "<b>Periodo:</b>",(slice %v% "anio")
                                         )},
                                     edge.tooltip = function(slice){paste("<b>Autores:</b>",(slice %e% "autores"), "<br>",
                                                                          "<b>Fuerza Colaboración:</b>", (slice %e% "fuerza_colaboracion" ),"<br>",
                                                                          "<b>Periodo:</b>",(slice %v% "anio")
                                     )},
                                     render.par=list(tween.frames = 10, show.time = F),
                                     plot.par=list(mar=c(0,0,0,0)),
                                     output.mode='htmlWidget',
                                     main=main_title_agg)
        
        rend1 <- Sys.time()
        print(rend1)
        print(paste0("elapsed render:",rend1-rend0))
        
        ret_render
    })

}