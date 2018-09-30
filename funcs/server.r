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
        
        coautores <- str_split(input$input_static_network_click_edge,"--")
        # glimpse(coautores)
        # glimpse(coautores[[1]][1])
        # glimpse(coautores[[1]][2])
        # autor,articulo_id,autor_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores
        ret <- static_data_base() %>% 
            filter(str_detect(autores,coautores[[1]][1])) %>%
            filter(str_detect(autores,coautores[[1]][2])) %>%
            select(autores,anio,titulo,url,cant_autores,fuerza_colaboracion) %>%
            mutate(articulo=paste0("<p><a target='_blank' href='",url,"'>",titulo,"</a></p>")) %>% 
            group_by(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% tally() %>% 
            select(autores,anio,articulo,cant_autores,fuerza_colaboracion)
        
        # autores,articulo_id,autor_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores
        # glimpse(ret)
        # descripciones_autores_df %>% 
        #   filter(autor %in% input$click ) %>%
        #   filter(periodo %in% input$anio )
        # %>%
        #   select(articulos,periodo)
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
    
    # estructura de nodos
    estructura_nodos_grafo_reactive <- reactive({
        grafo_reactive_tmp <- static_network_grafo_reactive()
        
        estructura_red_df <- metricas_nodos_grafo(grafo_reactive_tmp) %>%
            
            rename('Autor' = autor,
                   'Grado' = degree,
                   'Betweeness' = betweeness,
                   'Eigen Centrality' = eigen_centrality,
                   'Closeness' = closeness,
                   'Page Rank' = page_rank,
                   '# Triangulos' = count_triangles)
        
        estructura_red_df
    })
    
    estructura_red_grafo_dt <- reactive({
        
        dt_return <- DT::datatable(options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                                   estructura_nodos_grafo_reactive(),
                                   escape = FALSE,
                                   rownames = FALSE,
                                   selection = 'none') %>%
            formatRound('Betweeness',4) %>%
            formatRound('Eigen Centrality',4) %>%
            formatRound('Closeness',7) %>%
            formatRound('Page Rank',4)
        
        dt_return
    })
    
    output$output_static_estructura_nodos_table <- DT::renderDataTable({ 
        # ver inconexos, ver, top 10 , ver last 10 o last N , sin contar inconexos. 
        # todo eso sale de la tabla. 
        # hay que agregar tooltips de inteprretacion.
        estructura_red_grafo_dt()
    })
    
    # exportar nodos
    output$output_static_download_est_nodos <- downloadHandler( 
        filename = paste('est_nodos-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(estructura_nodos_grafo_reactive(), file,row.names = FALSE)
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
                geom_histogram(bins = num_bins) + 
                xlab(var_comparar) + ylab('Frecuencia') +
                geom_vline(xintercept = estructura_grafo_activo[[var_comparar]], colour='red')
            
            
            incProgress(4/4, detail = paste("Fin simulación ... ", 4))
        })
        
        ggplotly(plot_out)
    })
    
    
    # estetico # 4: COMUNIDADES  #-----------------------------------------------------------------------------------------------
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
        resultado_dt <- estructura_grafo_para_DT(listado_comunidades_estructura_reactive())
        resultado_dt
    })
    
    
    # comunidades - tabla - download --------------------------------------------------------
    
    output$download_com_metricas <- downloadHandler( 
        filename = paste('com_metricas-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
            write.csv(listado_comunidades_estructura_reactive(), file,row.names = FALSE)
        })  
    
    # comunidades - autores - tabla ---------------------------------------------------------
    
    listado_comunidades_reactive <- reactive({
        
        current_comunidad <- community_reactive()
        
        nodo_comunidad <- armar_df_membership(current_comunidad)
        
        listado_comunidades <- nodo_comunidad %>% arrange(member) %>% 
            group_by(member) %>% summarize(n=n(),autores=paste(collapse='; ',nombre)) %>% 
            arrange(desc(n)) %>% rename(comunidad=member,autores=n)
        listado_comunidades
        
    })
    
    output$cantidad_comunidades_metricas <- DT::renderDataTable({ 
        DT::datatable(
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            listado_comunidades_reactive(),
            escape = FALSE,
            selection = 'single')
    })
    
    # comunidades - autores - download ---------------------------------------------------------
    
    output$download_com_metricas_aut <- downloadHandler( 
        filename = paste('com_metricas_aut-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
            write.csv(listado_comunidades_reactive(), file,row.names = FALSE)
        })  
    
    # comunidades - detalle - info ----------------------------------------------------
    
    output$detalle_comunidad_seleccionada <- renderUI({
        
        current_com_selected <- listado_comunidades_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        mensaje <- paste0('Se selecciono la comunidad: ' ,current_com_selected[1])
        mensaje2 <- paste0('Con una cantidad de ' ,current_com_selected[2], ' integrantes. ')
        mensaje3 <- paste0('Siendo: ' ,current_com_selected[3] )
        mensaje_final <- paste('<p>',mensaje,mensaje2,'</p><p>',mensaje3,'</p>')
        HTML(mensaje_final)
    })
    
    detalle_comunidad_seleccionada_metricas_subgrafo_reactive <- reactive({
        req(input$cantidad_comunidades_metricas_rows_selected)
        current_comunidades <- community_reactive()
        
        current_com_selected <- listado_comunidades_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        
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
        
        current_com_selected <- listado_comunidades_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
        autores_comunidad <- str_split(current_com_selected[3],pattern = '; ') %>% unlist()
        
        filtro_coautores <- static_data_base() %>% 
            filter(anio %in% input$input_static_periodos) %>% 
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
    
    # temporal #-----------------------------------------------------------------------------------------------
    
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
        # temporal_grafos_reactive_acum()
        # antes: input$anio, ahora: anios disponibles dataset
        global_periodos_disponibles
        listado_grafos <- map(global_periodos_disponibles, ~ grafo_para_periodo_x(.x,
                                                                                  cota_seccion,
                                                                                  db_limpia,
                                                                                  static_edge_width_multiplier)) 
        
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
            calculo_grafos <- map(grafos,calcular_estructura_sobre_grafo)
            names(calculo_grafos) <- global_periodos_disponibles
            incProgress(indice/n, detail = paste("Armando estructura ...", indice)) # 3
            indice <- indice+1
            estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names)  
            
            grafo_estr_as_stack <- estr_grafos %>% 
                select(-str_largest_cliques,
                       -diametro_participantes,
                       -str_dist_lejanos_1,
                       -str_dist_lejanos_2) %>%
                select(periodo,input$temporal_sel_vars) %>%
                gather(key=metrica,value='valor',-periodo) %>%
                left_join(temporal_estructuras_reactive_vars_tibble(),by=c("metrica"="value")) %>%
                mutate(plot_text=paste0('Período: ',periodo,'<br />',
                                        'Métrica: ',nombre,'<br />',
                                        'Valor: ',valor)) %>%
                rename(Periodo = periodo, 'Métrica' = nombre , Valor = valor )
            # glimpse(grafo_estr_as_stack)
            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            plot_out <- grafo_estr_as_stack %>% 
                ggplot(aes(x=Periodo,y=Valor,color=Métrica,group=Métrica,text=plot_text)) +
                geom_point() + 
                geom_line() + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
        
        # config(ggplotly(plot_out,tooltip="text"), locale="es")
        ggplotly(plot_out,tooltip="text")
    })
    
    # temporal - acumulado -------------------------------------------------------
    observe({
        # req(input$anio)
        # input$temporal_estructura_refrescar_boton
        input$anio
        updateSelectizeInput(session, "temporal_sel_vars_2", choices = temporal_estructuras_reactive_vars())
    })
    
    temporal_grafos_acum_reactive <- reactive({

        tmp_anios_acum_2 <- temporal_generar_grafos_acumulados(global_periodos_disponibles,
                                                               cota_seccion,
                                                               db_limpia,
                                                               static_edge_width_multiplier)
        tmp_anios_acum_2
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
            grafos <- temporal_grafos_acum_reactive()
            incProgress(indice/n, detail = paste("Calculando medidas ...", indice)) # 2
            indice <- indice+1
            calculo_grafos <- map(grafos,calcular_estructura_sobre_grafo)
            names(calculo_grafos) <- global_periodos_disponibles
            incProgress(indice/n, detail = paste("Armando estructura ...", indice)) # 3
            indice <- indice+1
            estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names)  
            # glimpse(estr_grafos)
            
            grafo_estr_as_stack <- estr_grafos %>% 
                select(-str_largest_cliques,
                       -diametro_participantes,
                       -str_dist_lejanos_1,
                       -str_dist_lejanos_2) %>%
                select(periodo,input$temporal_sel_vars_2) %>%
                gather(key=metrica,value='valor',-periodo) %>%
                left_join(temporal_estructuras_reactive_vars_tibble(),by=c("metrica"="value")) %>%
                mutate(plot_text=paste0('Período: ',periodo,'<br />',
                                        'Métrica: ',nombre,'<br />',
                                        'Valor: ',valor)) %>%
                rename(Periodo = periodo, 'Métrica' = nombre , Valor = valor )
            # glimpse(grafo_estr_as_stack)
            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            plot_out <- grafo_estr_as_stack %>% 
                ggplot(aes(x=Periodo,y=Valor,color=Métrica,group=Métrica,text=plot_text)) +
                geom_point() + 
                geom_line() + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
    })
    
    # temporal - top N - acumulados - output -----------------------------------
    
    temporal_grafos_top_n_acum_metricas_reactive <- reactive({
        
        grafos <- temporal_grafos_acum_reactive()
        calculo_grafos <- map(grafos,metricas_nodos_grafo)
        names(calculo_grafos) <- global_periodos_disponibles
        
        calculo_grafos
    })
    
    
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
            calculo_grafos <- temporal_grafos_top_n_acum_metricas_reactive()
            incProgress(indice/n, detail = paste("Armando estructura ...", indice)) # 3
            indice <- indice+1
            estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names)
            # glimpse(estr_grafos)
            
            # para_top_5 <- estr_grafos %>% gather(metrica,valor,-periodo,-autor)
            
            # glimpse(para_top_5)
            
            var_selected <- input$temporal_sel_vars_3
            # glimpse(var_selected)
            
            estr_grafos_2 <- estr_grafos %>% select(autor,periodo,var_selected) %>%
                gather(Métrica,Valor,-autor,-periodo)
            # glimpse(estr_grafos_2)
            
            top_5 <- estr_grafos_2 %>% 
                group_by(periodo) %>%
                top_n(n = input$top_n_periodos,wt=Valor ) %>%
                arrange(periodo,desc(Valor))
            
            incProgress(indice/n, detail = paste("Procesando visualización ...", indice)) # 4
            indice <- indice+1
            
            
            plot_out <- top_5 %>% ggplot(aes(x=periodo, 
                                             y=reorder(autor,Valor),
                                             color=Valor)) +
                geom_point() + 
                xlab("Periodo") + ylab("Autor")+
                scale_colour_gradient(low = "#a6bddb", high = "#034e7b") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1),
                      axis.text.y = element_text(size = 6))
            
        })
        
        
        ggplotly(plot_out)
    })
    
    # temporal - dinamico - acumulado -----------------------------------------
    # agarrar el dinamico calculado , y sobre eso hacer las transformaciones de networkDynamic
    
    temporal_acumulado_network_pkg_reactive<- reactive({
        tmp_anios_acum_2 <- temporal_grafos_acum_reactive()
        nets <- map(tmp_anios_acum_2,intergraph::asNetwork)
        nets
    })
    
    temporal_dinamico_acumulado_reactive <- reactive({ 
        start_time_net<- Sys.time()
        print(start_time_net)
        tmp_nets <- temporal_acumulado_network_pkg_reactive()
        fin_time_net <- Sys.time()
        print(fin_time_net)
        print(paste0("elapsed net:",fin_time_net-start_time_net))
        tnet_time <- networkDynamic::networkDynamic(network.list=tmp_nets,
                                                    vertex.pid='vertex.names',
                                                    create.TEAs = TRUE)
        fin_time_netdyn_teas <- Sys.time()
        print(fin_time_netdyn_teas)
        print(paste0("elapsed teas:",fin_time_netdyn_teas-fin_time_net))
        tnet_time
    })
    
    output$temporal_dinamico_acumulado <- ndtv:::renderNdtvAnimationWidget({
        
        current_test_tnet <- temporal_dinamico_acumulado_reactive()
        
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
        
        n_bins <- 3 # 5 : descartado x arcoiris
        color_palette <- "Blues"#"YlGnBu" #"YlOrBr" # Spectral 
        # render.d3movie(net, output.mode = 'htmlWidget')
        ret_render <- render.d3movie(current_test_tnet, 
                       usearrows = F, 
                       displaylabels = F, 
                       label=function(slice){slice%v%'vertex.names'},
                       bg="#ffffff", 
                       #vertex.border="#FAFAFA",
                       vertex.border="lightgrey",
                       vertex.cex = 0.5,
                       vertex.col = function(slice){
                           ret  <-  '#BABABA' # default color
                           current_slice_var <- (slice %v% "fuerza_colaboracion")
                           if( !is.null(current_slice_var)){
                               
                               # zVar <- (current_slice_var - mean(current_slice_var)) / sd(current_slice_var)
                               arma_bins <- cut_number(current_slice_var, n = n_bins)
                               levels(arma_bins ) <- brewer.pal(n_bins, color_palette)
                               
                               ret <- as.character(arma_bins)
                           }
                           ret
                       },
                       edge.lwd = function(slice){ (slice %e% "fuerza_colaboracion") * 2 },
                       edge.col = function(slice){slice %e% "color"},
                       vertex.tooltip =  function(slice){
                           paste("<b>Autor:</b>", (slice %v% "vertex.names") , 
                                 "<br>",
                                 "<b>Fuerza Colaboración:</b>",
                                 (slice %v% "fuerza_colaboracion"))},
                       edge.tooltip = function(slice){paste("<b>Autores:</b>", 
                                                            (slice %e% "autores"), 
                                                            "<br>",
                                                            "<b>Fuerza Colaboración:</b>",
                                                            (slice %e% "fuerza_colaboracion" ))},
                       render.par=list(tween.frames = 10, show.time = F),
                       plot.par=list(mar=c(0,0,0,0)),
                       output.mode='htmlWidget' )
        
        rend1 <- Sys.time()
        print(rend1)
        print(paste0("elapsed render:",rend1-rend0))
        
        ret_render
    })

}