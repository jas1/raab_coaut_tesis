# functions : funciones necesarias para que corra la app.

# FUNCIONES ----------------------------------------------------------------------------

# FUNCIONES: GENERICAS -----------------------------------------------------------------
# obtener_periodos_disponibles(db_limpia,min(cota_anio), max(cota_anio),cota_seccion)

get_db_connection <- function(db_name){

    data_file <- here:::here("data",db_name )

    # pre logic    
    assertive.files::assert_all_are_existing_files(data_file)
    
    # logic
    raab_db_conn <- dbConnect(RSQLite::SQLite(),data_file)
    
    tmp_tables_check <- dbListTables(raab_db_conn)
    # not empty
    
    # expected
    stopifnot(length(tmp_tables_check) > 0 ) 
    # got the tables
    stopifnot(c('autores_articulos', 'articulos') %in% dbListTables(raab_db_conn) ) 
    
    raab_db_conn
}

obtener_periodos_disponibles <- function(db_name,periodo_min,periodo_max,secciones){
    
    raab_db_conn <- get_db_connection(db_name)
    condicion1 <- paste0(" WHERE anio BETWEEN ",periodo_min," AND ",periodo_max)
    condicion <- paste0(condicion1," AND seccion IN ( '",paste(secciones,collapse = "','"),"')")
    consulta1 <- paste0(" SELECT DISTINCT anio FROM articulos ", condicion , " ORDER BY anio  ")
    
    periodos <- dbGetQuery(raab_db_conn, consulta1 ) %>% as_tibble()
    dbDisconnect(raab_db_conn)
    periodos %>% rename(periodo = anio) %>% pull(periodo)
}

# para agregar un id de nodos que no importa sean autores o articulos.
agregar_id_nodos_univoco <- function(resultado_db){
    
    # pre logic
    assertive::assert_is_data.frame(resultado_db)
    
    # logic
    
    edges_autor <- resultado_db %>% 
        group_by(autor) %>% tally() %>%
        tibble::rowid_to_column("id") %>% 
        mutate(aut_id = paste0("a",str_pad(id, 4, pad = "0")) ) %>% # fix: bug:ids
        select (aut_id,autor)
    
    edges_articulo <- resultado_db %>% 
        group_by(articulo_id) %>% tally() %>% 
        tibble::rowid_to_column("id")  %>% 
        mutate(art_id = paste0("b",str_pad(id, 4, pad = "0")) ) %>%  # fix: bug:ids
        select (art_id,articulo_id)
    
    para_edgelist <- resultado_db %>% 
        left_join(edges_autor,by = c("autor"="autor")) %>% #agrega el campo aut_id
        left_join(edges_articulo,by = c("articulo_id"="articulo_id")) %>%  #agrega el campo art_id
        # TODO: 0000 - Nombre> esto era para que se muestre bien el grafo en la visualizacion.
        # mutate(aut_id_orig = aut_id) %>%
        # mutate(aut_id = paste0(aut_id," - ", autor )) %>%  # fix: bug:ids
        select(aut_id,art_id,everything())

    # expected

    para_edgelist
}

# FUNCIONES: DB  ---------------------------------------------------------------
articulos_todos_grafo <- function(db_name,anios,secciones){
    raab_db_conn <- get_db_connection(db_name)
    # collab str = 1/n_autores x articulo >  
    condicion1 <- paste0(" WHERE aa.anio IN ( '",paste(anios,collapse = "','"),"')")
    condicion <- paste0(condicion1," AND aa.seccion IN ( '",paste(secciones,collapse = "','"),"')")
    # collab str = 1/n_autores x articulo >  
    consulta1 <- paste0( "SELECT aa.*, art.autores_norm as autores FROM autores_articulos aa left join articulos art ON aa.articulo_id=art.articulo_id ",condicion )
    resultado <- dbGetQuery(raab_db_conn, consulta1 ) %>% as_tibble()
    dbDisconnect(raab_db_conn)
    
    if (nrow(resultado) == 0) {
        warning("No hay resultados en la DB para el periodo y secciones",
                paste0('\nperiodo: ',anios),
                paste0('\nsecciones: ',secciones)) 
    }
    
    res_DB <- resultado %>% select(autor,articulo_id,autor_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores)
    
    # agrega las columnsa aut_id y art_id adelante asi lo prepara para armar la edgelist.
    res_final <- agregar_id_nodos_univoco(res_DB)
    res_final
}

# FUNCIONES: ARMADO GRAFO -----------------------------------------------------------------

lista_vertices_autores <- function(data_acotado){

    # pre logic
    assertive::assert_is_data.frame(data_acotado)
    
    # logic
    vertex_list_autores <- data_acotado %>% 
        group_by(aut_id,autor,autor_id) %>% 
        summarise(anios=paste0(anio,collapse = ";"),
                  secciones=paste0(seccion,collapse = ";"),
                  fuerza_colaboracion_total=sum(fuerza_colaboracion),# valor de "collaboration strength" acumulados todos los articulos en colaboracion
                  cant_autores_coautoria=sum(cant_autores)) %>%
        arrange(autor)
    
    # expected
    if (nrow(vertex_list_autores) == 0 ) {
        warning("la lista de vertices autores esta vacia")
    }
    
    vertex_list_autores
}

lista_vertices_articulos <- function(data_acotado){
    
    # pre logic
    assertive::assert_is_data.frame(data_acotado)
    
    # logic
    vertex_list_articulos <- data_acotado %>% 
        group_by(art_id,articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores) %>% 
        tally() %>% select(-n)
    
    # expected result
    if (nrow(vertex_list_articulos) == 0 ) {
        warning("la lista de vertices articulos esta vacia")
    }
    
    vertex_list_articulos
}

# transforma a bipartito segun pa propiedad autor
transformar_en_bipartito <- function(g_aut_art,data_acotado){
    
    # pre logic
    stopifnot(igraph:::is.igraph(g_aut_art))
    assertive::assert_is_data.frame(data_acotado)

    # logic
    igraph:::V(g_aut_art)$type <- igraph:::V(g_aut_art)$name %in% (data_acotado %>% pull(aut_id) )
    
    # expected result
    stopifnot(igraph:::is.bipartite(g_aut_art))
    
    g_aut_art
}

# agrega: 
# id, tanto articulo como autor, 
# anio: anio / anios para art y aut
# fuerza_colaboracion: para art y aut
# cant_autores: para art y aut
# 
agregar_propiedades_a_bipartito <- function(g_aut_art,data_acotado){
    
    # pre logic
    stopifnot(igraph:::is.igraph(g_aut_art))
    assertive::assert_is_data.frame(data_acotado)
    
    # logic
    
    vertex_list_autores <- lista_vertices_autores(data_acotado)
    
    vertex_list_articulos <- lista_vertices_articulos(data_acotado)

    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$id <- vertex_list_autores$aut_id
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$id <- vertex_list_articulos$art_id
        
    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$id_old <- vertex_list_autores$autor_id
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$id_old <- vertex_list_articulos$articulo_id
    
    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$label <- vertex_list_autores$autor
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$label <- vertex_list_articulos$articulo_id
    
    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$anio <- vertex_list_autores$anios
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$anio <- vertex_list_articulos$anio
    
    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$fuerza_colaboracion <- vertex_list_autores$fuerza_colaboracion_total
    igraph:::V(g_aut_art)[igraph:::V(g_aut_art)$type]$cant_autores <- vertex_list_autores$cant_autores_coautoria
    
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$fuerza_colaboracion <- vertex_list_articulos$fuerza_colaboracion
    igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$cant_autores <- vertex_list_articulos$cant_autores
    
    # length(vertex_list_autores$anios)
    # length(vertex_list_articulos$anio)
    
    #expected
    expected_attrs <- c('name','id','anio','fuerza_colaboracion','cant_autores','id_old','label')
    
    stop_condition <-  all(igraph:::vertex_attr_names(g_aut_art) %in% expected_attrs) 
    if(stop_condition){
        warning( "No estan todos los atributos esperados",
                 paste0("\nexpected:" , paste0(collapse=";",expected_attrs)),
                 paste0("\nexistentes:" , paste0(collapse=";",igraph:::vertex_attr_names(g_aut_art)))
           )
        stopifnot(stop_condition)     
    }
    
    
    g_aut_art
}

# validacion de FCAutor
fuerza_colaboracion_autorut_expected_validator <- function(current_autor, data_acotado,grafo_bipartito,con_mensaje=FALSE){
    total <- art_full %>% filter(str_detect(autor,current_autor)) %>% 
        group_by(autor,articulo_id,anio,fuerza_colaboracion) %>% 
        tally() %>% group_by(autor) %>% 
        summarise(total_fca=sum(fuerza_colaboracion))
    
    FCAut_expected <- total %>% pull(total_fca) %>% as.numeric()
    
    vertex_current <- igraph:::V(grafo_bipartito)[str_detect(igraph:::V(grafo_bipartito)$label,current_autor) ]
    FCAut_current <- vertex_current$fuerza_colaboracion

    if (con_mensaje) {
        mensaje <- paste0("\nautor: ",current_autor,
                          "\nexpected: ",FCAut_expected,
                          "\nactual: ",FCAut_current) 
        message(mensaje)
    }
    
    
    
    if(FCAut_expected!=FCAut_current){
        warning( "No dan bien los Fuerzca colaboracion para Autor.",
                 paste0("\nautor: ",current_autor,
                        "\nexpected: ",FCAut_expected,
                        "\nactual: ",FCAut_current))
        
        stopifnot(FCAut_expected!=FCAut_current)
    }
}


# armado_grafo_bipartito(articulos_todos_grafo(db_limpia,cota_anio,cota_seccion))
# para armar grafo igraph
armado_grafo_bipartito <- function(edgelist_para_grafo){
    # edgelist_para_grafo <- articulos_todos_grafo(db_limpia,cota_anio2,cota_seccion)

    # pre logic
    data_acotado <- edgelist_para_grafo
    assertive::assert_is_data.frame(data_acotado)
    

    # logic
    
    g_aut_art <- igraph:::graph.data.frame(data_acotado,directed = FALSE)
    
    g_aut_art <- transformar_en_bipartito(g_aut_art,data_acotado)
    
    g_aut_art <- agregar_propiedades_a_bipartito(g_aut_art,data_acotado)
    
    # expected

    g_aut_art
}


# para generacion del grafo de coautoria a partir del bi partito + la edgelsit filtrada
# width_multiplier: sirve para que tan gruesa va a ser la diferencia enter tipo 1, tipo 2 y tipo 3.
# tipo 1: sin multiplicador, tipo 2 * valor multiplicador, tipo 3 * valor multiplicador.
# ej: width_multiplier= 2 => 2,4,6; width_multiplier= 3 => 3,6,9
extraccion_grafo_coautoria <- function(grafo_bipartito,edgelist_para_grafo,width_multiplier = 2,color_brew = 'Dark2'){
    
    g_projections <- igraph:::bipartite_projection(grafo_bipartito,multiplicity = TRUE)
    
    g_aut <- g_projections$proj2
    # g_art <- g_projections$proj1
    
    # measure of collaboration strength illustrated in Fig. 5. Newman 2004 ; entonces el tamaño del nodo esta con que tan colaborativo fue
    igraph:::V(g_aut)$size <- 10 + (igraph:::V(g_aut)$fuerza_colaboracion*10) # *10 para que aumente el tamaño , dejando de ser decimal
    
    # esto esta para ponerle los colores a los edges segun lo calculado   
    
    color_bins_cant <- 3
    bins_cant <- if_else(color_bins_cant < 3, 3 , color_bins_cant)
    color_palette <- brewer.pal(bins_cant,name=color_brew)
    
    elist <- igraph:::as_edgelist(g_aut)
    elist_df <- data.frame(elist,stringsAsFactors = FALSE) %>% as_tibble() %>% 
        rename(autor1=X1,autor2=X2) %>% 
        mutate(id=paste0(autor1,"--",autor2))
    # glimpse(elist)
    # glimpse(data.frame(elist))
    
    
    fuerza_colaboracion_output <- vector(mode='double',length=nrow(elist))
    autores_output <- vector(mode='character',length=nrow(elist))
    for (idx in seq_along(1:nrow(elist))) {
        fuerza_colaboracion_output[[idx]] <- (fuerza_colaboracion_relacion(elist[idx,],edgelist_para_grafo))
        autores_output[[idx]] <- paste0(elist[idx,1],' - ',elist[idx,2])
    }
    
    g_aut <- g_aut %>% 
        igraph:::set_edge_attr(igraph:::E(g_aut),name = "id",elist_df %>% pull(id)) %>% 
        igraph:::set_edge_attr(igraph:::E(g_aut),name = "fuerza_colaboracion",fuerza_colaboracion_output) %>% 
        igraph:::set_edge_attr(igraph:::E(g_aut),name = "autores",autores_output)
    
    #criterio_visualizacion <- E(g_aut)$weight # viejo, por peso, que el peso = cant articulos relacion
    criterio_visualizacion <- igraph:::E(g_aut)$fuerza_colaboracion
    threshold <- max(criterio_visualizacion) /3  # par ahacer 3 bins de colores
    edge_color <- colores_edges_en_n_bins(criterio_visualizacion,
                                          bins = 3,
                                          color_palette = color_palette)
    
    
    edge_width <-  width_edges_en_n_bins(criterio_visualizacion,
                                         bins = 3,
                                         width_multiplier = width_multiplier)
    
    if_else(criterio_visualizacion < threshold,
            1,
            if_else(criterio_visualizacion <  threshold*2,
                    2*width_multiplier,
                    3*width_multiplier)
    )
    g_aut <- g_aut %>%
        igraph:::set_edge_attr(igraph:::E(g_aut),name = "color",edge_color) %>% 
        igraph:::set_edge_attr(igraph:::E(g_aut),name = "width",edge_width)
    
    g_aut
}

width_edges_en_n_bins <- function(listado_valores_edges_criterio,bins=3,width_multiplier=2){
    criterio_visualizacion <- listado_valores_edges_criterio
    threshold <- max(criterio_visualizacion) / bins  # par ahacer 3 bins de colores
    edge_width <- if_else(criterio_visualizacion < threshold,
                          1,
                          if_else(criterio_visualizacion <  threshold*2,
                                  2*width_multiplier,
                                  3*width_multiplier)
    )
    edge_width
}


colores_edges_en_n_bins <- function(listado_valores_edges_criterio,bins=3,color_palette=c("#1B9E77","#D95F02","#7570B3")){
    criterio_visualizacion <- listado_valores_edges_criterio
    threshold <- max(criterio_visualizacion) / bins  # par ahacer 3 bins de colores
    edge_color <- if_else(criterio_visualizacion < threshold,
                          color_palette[1],
                          if_else(criterio_visualizacion <  threshold*2,
                                  color_palette[2],
                                  color_palette[3])
    )
    edge_color
}

# esto es una funcion que dado un edge, extraigo los nombres y busco los valores en los edgelist
# se da por implicito que este edgelist es el mismo que se usa para armar el grafo
# de esta forma ya tiene los filtros de fechasy secciones
fuerza_colaboracion_relacion <- function(edge_analizar_ends,edgelist_para_grafo){
    # edge_analizar_ends <- ends(g_aut,edge_ejemplo)
    # edgelist_para_grafo <- articulos_todos_grafo(db_limpia,cota_anio,cota_seccion)
    # dame los ends
    # de los ends, dame los articulos en los que figuren los 2.
    resultado <- edgelist_para_grafo %>% 
        filter(str_detect(autores,edge_analizar_ends[1]) & str_detect(autores,edge_analizar_ends[2])) %>% 
        count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>%
        count(fuerza_colaboracion) %>% 
        summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()
    resultado
}

# FUNCIONES: ART ASOC -----------------------------------------------------------------
generar_visualizacion_subgrafo_vecinos <- function(g,vertice,random_seed=12345){
    
    subgrafo_autor <- igraph:::make_ego_graph(graph = g, # para el grafo de la red
                                              order=1, # 1 nivel de vecinos
                                              nodes = vertice, # donde el vertice tenga de nombre el selected
                                              mode = "all" )
    
    igraph:::edge_attr_names(subgrafo_autor[[1]])
    
    
    sg2 <- subgrafo_autor[[1]] %>% 
        igraph:::set_edge_attr(name="width",value = igraph:::E(subgrafo_autor[[1]])$weight) %>%
        igraph:::set_edge_attr(name="color",value = colores_edges_en_n_bins(igraph:::E(subgrafo_autor[[1]])$weight)) %>% 
        igraph:::set_vertex_attr(name = "title",value = igraph:::V(subgrafo_autor[[1]])$name)
    tmp2 <- as_data_frame(sg2) %>% as_tibble() %>% mutate(nombre = paste0(from," - ",to,'<br />',"fuerza colaboración:",fuerza_colaboracion)) %>% pull(nombre)
    
    
    sg2 <- sg2 %>% 
        igraph:::set_edge_attr(name="title",value = tmp2)
    
    
    resultado_visnet <-  visNetwork:::visIgraph(sg2) %>% 
        visNetwork:::visNodes(size = 10) %>%
        visNetwork:::visIgraphLayout(randomSeed = random_seed,
                                     layout="layout_in_circle"
        ) %>%
        visNetwork:::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                                nodesIdSelection =FALSE)
    resultado_visnet
    
}


# FUNCIONES: ESTRUCTURA GRAFO ---------------------------------------------------------


# Este calcula la estructura sobre el grafo , las cosas que pueden ser extraidas desde el grafo
calcular_estructura_sobre_grafo <- function(grafo_reactive_tmp){
    resultado <- data.frame(tmp =c(''))
    resultado$cant_autores <- igraph:::gorder(grafo_reactive_tmp)
    
    # cantidad relaciones
    resultado$cant_relaciones <- length(igraph:::E(grafo_reactive_tmp))
    
    #densidad de la red
    resultado$densidad_red <- igraph:::edge_density(grafo_reactive_tmp)
    
    resultado$distancia_media <- igraph:::mean_distance(grafo_reactive_tmp)
    
    mas_lejanos <- igraph:::farthest_vertices(grafo_reactive_tmp)
    resultado$num_dist_lejanos <-  mas_lejanos$distance
    resultado$str_dist_lejanos_1 <- names(mas_lejanos$vertice[1])
    resultado$str_dist_lejanos_2 <- names(mas_lejanos$vertice[2])
    
    resultado$diametro_participantes <- igraph:::get_diameter(grafo_reactive_tmp) %>% names() %>%paste0(collapse='; ')
    
    largest_clique_str <- igraph:::largest_cliques(grafo_reactive_tmp) %>% unlist() %>% names()
    
    resultado$num_largest_cliques <- length(largest_clique_str )
    resultado$str_largest_cliques <- largest_clique_str  %>%paste0(collapse='; ')
    resultado$porc_largest_cliques <- resultado$num_largest_cliques / resultado$cant_autores * 100 
    
    resultado$num_cliques <- igraph:::clique_num(grafo_reactive_tmp)
    resultado$num_transitivity <- igraph:::transitivity(grafo_reactive_tmp)
    resultado$num_assort_degree <- igraph:::assortativity_degree(grafo_reactive_tmp)
    
    #largest component, no es lo mismo que largest clique.
    # esta la funcion http://igraph.org/r/doc/components.html
    
    componentes <- igraph:::components(grafo_reactive_tmp)
    
    resultado$componentes_largest <- max(componentes$csize)
    resultado$componentes_largest_porc <- resultado$componentes_largest / resultado$cant_autores * 100
    resultado$componentes_cantidad <- length(componentes$csize)
    # average distance
    resultado$distancia_media <- igraph:::mean_distance(grafo_reactive_tmp)
    
    resultado[,2:ncol(resultado)]
}

# Este calcula la estructura sobre el grafo Y ademas otras metricas que salen de la base de articulos.
calcular_estructura_grafo <- function(grafo_reactive_tmp,base_articulos){
    
    # resultado <- calcu
    resultado <- calcular_estructura_sobre_grafo(grafo_reactive_tmp)
    # glimpse(resultado)
    # glimpse(base_articulos)
    # RESULTADOS SEGUN BASE AUTORES
    resultado$cantidad_autores <- base_articulos %>% group_by(autor_id) %>% tally() %>% count() %>% as.integer()
    # cantidad papers: sale de base articulos
    resultado$cantidad_papers <- base_articulos %>% group_by(articulo_id) %>% tally() %>% count() %>% as.integer()
    # no newman <- cantidad autorias ( o sea que 1 autor es parte de 1 paper, seria la suma de los autores de los papers.)
    resultado$cantidad_contribuciones <- base_articulos %>% group_by(articulo_id) %>% tally() %>% 
        summarize(total=sum(n)) %>% as.integer()
    # cantidad papers por autor AVG: sale de la base articulos
    resultado$cantidad_paper_por_autor_avg <- base_articulos %>% group_by(autor_id) %>% tally() %>% summarize(media=mean(n)) %>% as.double()
    # cantidad autores por paper AVG: sale de bae articulos
    resultado$cantidad_autores_por_paper_avg <- base_articulos %>% group_by(articulo_id) %>% tally() %>% 
        summarize(media=mean(n)) %>% as.double() # idea que tan en la media esta el autor respecto de la estructura.
    resultado
}

# Agarra un DF de estructura grafo y lo transforma en un datatables
estructura_grafo_para_DT <- function(estructura_grafo_df,dt_option_dom='ft'){
    resultado_df <- estructura_grafo_df %>%
        select(
            cantidad_autores,
            cantidad_papers,
            cantidad_contribuciones,
            cant_relaciones,
            cantidad_paper_por_autor_avg,
            cantidad_autores_por_paper_avg,
            componentes_cantidad,
            componentes_largest,
            componentes_largest_porc,
            densidad_red,
            distancia_media,
            num_dist_lejanos,
            # num_cliques,
            # num_largest_cliques,
            porc_largest_cliques,
            num_transitivity,
            num_assort_degree) %>%
        rename('# Aut.' = cantidad_autores,
               '# Art.' = cantidad_papers,
               '# Contrib.' = cantidad_contribuciones, # Cant. Aut. por Art. sumados.
               '# Rel.' = cant_relaciones, # relacion 1 a 1 entre Aut.
               '# Media de Art. por Autor' = cantidad_paper_por_autor_avg, 
               '# Media de Aut. por Art.' = cantidad_autores_por_paper_avg,
               '# de Comp.' = componentes_cantidad, # Cant. de subgrafos conectados
               '# Aut. en Comp. Grande' = componentes_largest, # mayor subgrafo conectado
               'Comp. más Grande (%)' = componentes_largest_porc, # ratio en porcentaje de Aut. del componente mas grande / Aut. total del grafo
               'Densidad' = densidad_red,
               'Dist. Media' = distancia_media,
               'Diámetro' = num_dist_lejanos,
               # 'Cant. de Cliques' = num_cliques,
               # 'Cant. Aut. Clique mas grande' = num_largest_cliques,
               'Cliqué más grande (%)' = porc_largest_cliques, # ratio en porcentaje del clique mas grande / Aut. total del grafo
               'Tran.' = num_transitivity,
               'Asort.' = num_assort_degree)
    resultado_DT <- DT::datatable(options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        dom = dt_option_dom),
        resultado_df,
        escape = FALSE,
        rownames = FALSE,
        selection="none"
    ) %>% 
        formatRound('# Media de Art. por Autor',2) %>%
        formatRound('# Media de Aut. por Art.',2) %>%
        formatRound('Comp. más Grande (%)',2) %>%
        formatRound('Densidad',2) %>%
        formatRound('Dist. Media',2) %>%
        formatRound('Cliqué más grande (%)',2) %>%
        formatRound('Tran.',4) %>%
        formatRound('Asort.',4)
    
    resultado_DT
}

# metricas par anodos dado un grafo
metricas_nodos_grafo <- function(grafo_reactive_tmp){
    grado_valor <- igraph:::degree(grafo_reactive_tmp) %>% data.frame()
    grado_valor$autor <- rownames(grado_valor)
    colnames(grado_valor) <- c('degree','autor')
    grado_valor <- grado_valor %>% select(autor,degree)
    
    # betweeness
    betweeness_valores <- igraph:::betweenness(grafo_reactive_tmp) %>% data.frame()
    betweeness_valores$autor <- rownames(betweeness_valores)
    colnames(betweeness_valores) <- c('betweeness','autor')
    
    # eigenvector centrality
    eigen_valor <- igraph:::eigen_centrality(grafo_reactive_tmp)$vector %>% data.frame()
    eigen_valor$autor <- rownames(eigen_valor)
    colnames(eigen_valor) <- c('eigen_centrality','autor')
    
    # closeness centrality
    closeness_valor <- igraph:::closeness(grafo_reactive_tmp) %>% data.frame()
    closeness_valor$autor <- rownames(closeness_valor)
    colnames(closeness_valor) <- c('closeness','autor')
    
    # pagerank centrality
    page_rank_valor <- igraph:::page_rank(grafo_reactive_tmp)$vector %>% data.frame()
    page_rank_valor$autor <- rownames(page_rank_valor)
    colnames(page_rank_valor) <- c('page_rank','autor')
    
    #triangulos
    triangulos_count <- igraph:::count_triangles(grafo_reactive_tmp) %>% data.frame()
    triangulos_count$autor <-  igraph:::V(grafo_reactive_tmp)$name
    colnames(triangulos_count) <- c('count_triangles','autor')
    
    estructura_red_df <- grado_valor %>% 
        inner_join(betweeness_valores) %>%
        inner_join(eigen_valor) %>%
        inner_join(closeness_valor) %>%
        inner_join(page_rank_valor) %>%
        inner_join(triangulos_count) 
    
    estructura_red_df
}

# para generacion grafos similares
generar_grafos_similares <- function(grafo_reactive_tmp,cantidad=1000) {
    cant_autores <- igraph:::gorder(grafo_reactive_tmp)
    densidad_red <- igraph:::edge_density(grafo_reactive_tmp)
    
    lista_generados <- vector('list',cantidad)
    
    # 1000
    for (i in 1:cantidad){
        # ver ?erdos.renyi.game: gnp = probabilidad, gnm = M cantidad de edges
        # lista_generados[[i]] <- erdos.renyi.game(
        #     n=cant_autores,
        #     p.or.m = densidad_red,
        #     type = 'gnp',
        #     directed=FALSE)
        
        lista_generados[[i]] <- igraph:::barabasi.game(n = cant_autores,directed = FALSE )
    }
    
    lista_generados
}

# FUNCIONES: COMUNIDADES-----------------------------------------------------------------

arma_comunidad <- function(semilla_seed,tmp_grafo,comunidades_sel_algo){
    set.seed(semilla_seed)
    comunidad_sel_detalles <- ''
    
    comunidad_sel_detalles <-  switch(comunidades_sel_algo,
                                      cluster_edge_betweenness = igraph:::cluster_edge_betweenness(tmp_grafo,weights = NULL),
                                      cluster_label_prop = igraph:::cluster_label_prop(tmp_grafo),
                                      cluster_leading_eigen = igraph:::cluster_leading_eigen(tmp_grafo),
                                      cluster_louvain = igraph:::cluster_louvain(tmp_grafo),
                                      cluster_walktrap = igraph:::cluster_walktrap(tmp_grafo),
                                      cluster_infomap = igraph:::cluster_infomap(tmp_grafo),
                                      stop(paste0('NO DISPONIBLE',comunidades_sel_algo))
    )
    
    comunidad_sel_detalles
}
armar_df_membership <- function(comunidad_sel_detalles){
    memb <- igraph:::membership(comunidad_sel_detalles)
    values_memb <- as.vector(memb)
    names_memb <- names(memb) 
    membership_df <- data.frame(nombre=names_memb,member=values_memb)
    membership_df
}
armar_df_comunidades <- function(cantidad_coms,comunidad_sel_detalles,
                                 color_palette='Set1',
                                 color_default='#D3D3D3'){
    
    membership_df <- armar_df_membership(comunidad_sel_detalles)
    
    comunity_palete <- brewer.pal(if_else(cantidad_coms<3,
                                          as.integer(3),
                                          cantidad_coms),
                                  color_palette)
    
    # para ordenar segun TOP N
    cuales_top <- membership_df %>% 
        group_by(member) %>% tally() %>% arrange(desc(n)) %>% head(n=cantidad_coms) %>%
        mutate(color_code = 1:cantidad_coms) %>% 
        mutate(color=comunity_palete[color_code]) 
    
    refactor_n_comunidades <- membership_df %>% 
        mutate(member_ref=if_else(member %in% cuales_top$member, member, 0)) %>%
        left_join(cuales_top %>% select(member,color), by = c('member'='member')) %>%
        mutate(color=if_else(is.na(color),color_default,color))
    # glimpse(refactor_n_comunidades)
    
    resultado <- refactor_n_comunidades %>% 
        mutate(label=nombre) %>%
        rename(id=nombre,group=member_ref) %>%
        select(id,label,group,color)
    
    resultado
}

# para buscar la estructura de todas las comunidades
estructura_comunidades_df <- function(current_grafo,current_base_autores,current_comunidad){
    resul_acum <- calcular_estructura_grafo(current_grafo,current_base_autores)
    resul_acum <- resul_acum[-1,]
    
    # glimpse(resul_acum)
    
    for(i in 1:length(current_comunidad)){
        current_group <- igraph::groups(current_comunidad)[[i]]
        current_subgraph <- igraph:::induced_subgraph(current_grafo,current_group) 
        current_autores <- igraph:::V(current_subgraph)$name
        base_autores_tmp <- current_base_autores %>% filter(autor %in% current_autores)
        
        estructura_grafo_df <- calcular_estructura_grafo(current_subgraph,base_autores_tmp)
        resul_acum <- resul_acum %>% bind_rows(estructura_grafo_df)
    }
    resul_acum$comunidad_id <- 1:length(current_comunidad)
    # glimpse(resul_acum)
    resul_acum
}


# FUNCIONES: TEMPORAL -----------------------------------------------------

grafo_para_periodo_x <- function(periodos,cota_seccion,db_limpia,static_edge_width_multiplier){
    
    db_resultado <- articulos_todos_grafo(db_limpia,periodos,cota_seccion)
    grafo_bipartito <- armado_grafo_bipartito(db_resultado)
    g_coaut <- extraccion_grafo_coautoria(grafo_bipartito,db_resultado,
                                          width_multiplier = static_edge_width_multiplier)
    
    g_coaut
}

