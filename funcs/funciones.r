# functions : funciones necesarias para que corra la app.

# FUNCIONES ----------------------------------------------------------------------------

# FUNCIONES: GENERICAS -----------------------------------------------------------------
# obtener_periodos_disponibles(db_limpia,min(cota_anio), max(cota_anio),cota_seccion)

get_db_connection <- function(db_name){

    data_file <- here::here("data",db_name )

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
        arrange(autores) %>% # esto para ordenar por autores los articulos entonces me quedan bien los edges
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
        select(aut_id,art_id,everything()) %>%
        arrange(aut_id)

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
        arrange(aut_id)
    
    vertex_list_autores2 <- vertex_list_autores %>% 
        mutate(anios2=str_split(anios,pattern = ";")) %>% 
        unnest(cols = c(anios2)) %>% 
        count(aut_id,autor,autor_id,anios,secciones,
              fuerza_colaboracion_total,cant_autores_coautoria,anios2) %>% 
        mutate(anios3=paste0(anios2,"[",n,"]")) %>% 
        select(-anios2,-n) %>% 
        group_by(aut_id,autor,autor_id,anios,secciones,
                 fuerza_colaboracion_total,cant_autores_coautoria) %>% 
        summarise(anios2=paste0(anios3,collapse = ";")) %>% 
        ungroup() %>% #%>% select(anios2)
        select( aut_id,autor,autor_id,anios2,secciones,
                   fuerza_colaboracion_total,cant_autores_coautoria) %>% 
        rename(anios=anios2)
    
    # expected
    if (nrow(vertex_list_autores2) == 0 ) {
        warning("la lista de vertices autores esta vacia")
    }
    
    vertex_list_autores2
}

lista_vertices_articulos <- function(data_acotado){
    
    # pre logic
    assertive::assert_is_data.frame(data_acotado)
    
    # logic
    vertex_list_articulos <- data_acotado %>% 
        group_by(art_id,articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores) %>% 
        tally() %>% select(-n) %>% 
        arrange(autores) # esto para ordenar por autores los articulos entonces me quedan bien los edges
    
    # expected result
    if (nrow(vertex_list_articulos) == 0 ) {
        warning("la lista de vertices articulos esta vacia")
    }
    
    vertex_list_articulos
}

# transforma a bipartito segun pa propiedad autor
transformar_en_bipartito <- function(g_aut_art,data_acotado){
    
    # pre logic
    stopifnot(igraph::is.igraph(g_aut_art))
    assertive::assert_is_data.frame(data_acotado)

    # logic
    igraph::V(g_aut_art)$type <- igraph::V(g_aut_art)$name %in% (data_acotado %>% pull(aut_id) )
    
    # expected result
    stopifnot(igraph::is.bipartite(g_aut_art))
    
    g_aut_art
}


# este tiene que estar acotado a el tipo articulos
# art_name_list: vertex art $ NAME
# vertex_list_articulos_acotado: listado de articulos
ordenear_lista_articulos_by_vertex_attr <- function(art_name_list,vertex_list_articulos_acotado){
    orden_original <- data.frame(stringsAsFactors = FALSE, 
                                 art_id=art_name_list)

    resultado <-  orden_original %>% 
        left_join(vertex_list_articulos_acotado,by=c('art_id'='art_id')) %>%
        select(art_id,articulo_id,anio,cant_autores,fuerza_colaboracion)

    resultado
}


# agrega: 
# id, tanto articulo como autor, 
# anio: anio / anios para art y aut
# fuerza_colaboracion: para art y aut
# cant_autores: para art y aut
# 
agregar_propiedades_a_bipartito <- function(g_aut_art,data_acotado){
    
    # pre logic
    stopifnot(igraph::is.igraph(g_aut_art))
    assertive::assert_is_data.frame(data_acotado)
    
    # logic
    
    vertex_list_autores <- lista_vertices_autores(data_acotado)
    vertex_list_articulos_old <- lista_vertices_articulos(data_acotado)
    vertex_articulos_names <- igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$name
    vertex_list_articulos <- ordenear_lista_articulos_by_vertex_attr(vertex_articulos_names,vertex_list_articulos_old)

    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$id <- vertex_list_autores$aut_id
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$id <- vertex_list_articulos$art_id
        
    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$id_old <- vertex_list_autores$autor_id
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$id_old <- vertex_list_articulos$articulo_id
    
    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$label <- vertex_list_autores$autor
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$label <- vertex_list_articulos$articulo_id
    
    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$anio <- vertex_list_autores$anios
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$anio <- vertex_list_articulos$anio
    
    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$fuerza_colaboracion <- vertex_list_autores$fuerza_colaboracion_total
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$fuerza_colaboracion <- vertex_list_articulos$fuerza_colaboracion
    
    igraph::V(g_aut_art)[igraph::V(g_aut_art)$type]$cant_autores <- vertex_list_autores$cant_autores_coautoria
    igraph::V(g_aut_art)[!igraph::V(g_aut_art)$type]$cant_autores <- vertex_list_articulos$cant_autores
    
    # length(vertex_list_autores$anios)
    # length(vertex_list_articulos$anio)
    
    #expected
    expected_attrs <- c('name','id','anio','fuerza_colaboracion','cant_autores','id_old','label')
    
    stop_condition <-  all(igraph::vertex_attr_names(g_aut_art) %in% expected_attrs) 
    if(stop_condition){
        warning( "No estan todos los atributos esperados",
                 paste0("\nexpected:" , paste0(collapse=";",expected_attrs)),
                 paste0("\nexistentes:" , paste0(collapse=";",igraph::vertex_attr_names(g_aut_art)))
           )
        stopifnot(stop_condition)     
    }
    
    
    g_aut_art
}

# armado_grafo_bipartito(articulos_todos_grafo(db_limpia,cota_anio,cota_seccion))
# para armar grafo igraph
armado_grafo_bipartito <- function(edgelist_para_grafo){
    # edgelist_para_grafo <- articulos_todos_grafo(db_limpia,cota_anio2,cota_seccion)

    # pre logic
    data_acotado <- edgelist_para_grafo
    assertive::assert_is_data.frame(data_acotado)
    

    # logic
    
    g_aut_art <- igraph::graph.data.frame(data_acotado,directed = FALSE)
    
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
    # grafo_bipartito <- gb_ok
    # edgelist_para_grafo <- art_full 
    g_projections <- igraph::bipartite_projection(grafo_bipartito,multiplicity = TRUE)
    
    g_aut <- g_projections$proj2
    # g_art <- g_projections$proj1
    
    # measure of collaboration strength illustrated in Fig. 5. Newman 2004 ; entonces el tamaño del nodo esta con que tan colaborativo fue
    igraph::V(g_aut)$size <- 10 + (igraph::V(g_aut)$fuerza_colaboracion*10) # *10 para que aumente el tamaño , dejando de ser decimal
    
    # elist <- igraph::as_edgelist(g_aut)
    # igraph::as_data_frame(g_aut,what="edges")
    # glimpse(elist)
    
    lista_autores <- lista_vertices_autores(edgelist_para_grafo) %>% select(aut_id,autor)
    
    #data.frame(elist,stringsAsFactors = FALSE)
    elist_df <- igraph::as_data_frame(g_aut,what="edges") %>% as_tibble() %>% 
        rename(autor1=from,autor2=to) %>% 
        mutate(id=paste0(autor1,"--",autor2)) %>% 
        left_join(lista_autores,by=c('autor1'='aut_id')) %>% 
        rename(autor1_label=autor) %>% 
        left_join(lista_autores,by=c('autor2'='aut_id')) %>% 
        rename(autor2_label=autor) %>% 
        mutate(autores=paste0(autor1_label,' - ',autor2_label))

    
    anios_output <- vector(mode='character',length=nrow(elist_df))
    fuerza_colaboracion_output <- vector(mode='double',length=nrow(elist_df))
    autores_output <- vector(mode='character',length=nrow(elist_df))
    for (idx in seq_along(1:nrow(elist_df))) {
        # fuerza_colaboracion_output[[idx]] <- (fuerza_colaboracion_relacion(elist_df[idx,],edgelist_para_grafo))
        calculo_tmp <- fuerza_colaboracion_y_anios_relacion(elist_df[idx,],edgelist_para_grafo)
        fuerza_colaboracion_output[[idx]] <- calculo_tmp$fuerza_colab
        anios_output[[idx]] <- calculo_tmp$anios
        # autores_output[[idx]] <- paste0(elist[idx,1],' - ',elist[idx,2])
    }
    
    g_aut <- g_aut %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "id",elist_df %>% pull(id)) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "fuerza_colaboracion",fuerza_colaboracion_output) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "anios",anios_output) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "autores",elist_df %>% pull(autores)) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "autor1_label",elist_df %>% pull(autor1_label)) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "autor2_label",elist_df %>% pull(autor2_label))
        # igraph::set_edge_attr(igraph::E(g_aut),name = "autores",autores_output)
    
    
    # esto esta para ponerle los colores a los edges segun lo calculado   
    
    color_bins_cant <- 3
    bins_cant <- if_else(color_bins_cant < 3, 3 , color_bins_cant)
    color_palette <- brewer.pal(bins_cant,name=color_brew)
    
    #criterio_visualizacion <- E(g_aut)$weight # viejo, por peso, que el peso = cant articulos relacion
    criterio_visualizacion <- igraph::E(g_aut)$fuerza_colaboracion

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
        igraph::set_edge_attr(igraph::E(g_aut),name = "color",edge_color) %>% 
        igraph::set_edge_attr(igraph::E(g_aut),name = "width",edge_width)
    
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
    # cant valores diferentes
    cant_val_diferentes <- length(unique(listado_valores_edges_criterio))
    threshold <- max(criterio_visualizacion) / bins  # par ahacer 3 bins de colores
    
    # default al color 1 de la paleta
    edge_color <- rep(color_palette[1],times=length(criterio_visualizacion)) 
    
    if(cant_val_diferentes < bins){
         
    }else{
        # si es mayor iguala bins, aplica la logica
        edge_color <- if_else(criterio_visualizacion < threshold,
                              color_palette[1],
                              if_else(criterio_visualizacion <  threshold*2,
                                      color_palette[2],
                                      color_palette[3])
        )    
    }
    
    
    edge_color
}

# esto es una funcion que dado un edge, extraigo los nombres y busco los valores en los edgelist
# se da por implicito que este edgelist es el mismo que se usa para armar el grafo
# de esta forma ya tiene los filtros de fechasy secciones
fuerza_colaboracion_y_anios_relacion <- function(edge_analizar_ends,edgelist_para_grafo){

    autor1_end <- edge_analizar_ends %>% pull(autor1_label)
    autor2_end <- edge_analizar_ends %>% pull(autor2_label)

    filtro_1 <- edgelist_para_grafo %>% 
        filter(str_detect(autores,autor1_end) & str_detect(autores,autor2_end))
    
    fuerza_colab <- filtro_1 %>% 
        count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>%
        # count(fuerza_colaboracion) %>% 
        summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()

    anios_edge <- filtro_1 %>% 
        count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>% 
        select(anio) %>% count(anio) %>% mutate(anio2=paste0(anio,"[",n,"]")) %>% 
        summarise(anios_total = paste(collapse=";",anio2)) %>% as.character()

    ret <- list()
    ret$fuerza_colab <- fuerza_colab
    ret$anios <- anios_edge
    
    ret
    # anio_relacion <- filtro_1 %>% count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>%
}

# FUNCIONES: ART ASOC -----------------------------------------------------------------

get_vertex_from_click_vertex <- function(grafo,input_click){

    filter_cond <- str_detect(igraph::V(grafo)$id,pattern = input_click)
    vertice <- igraph::V(grafo)[filter_cond]
    
    vertice
}


get_art_asoc_from_click_edge <- function(input_edge,current_db){
    coautores <- str_split(input_edge,"--")
    # glimpse(coautores)
    # glimpse(coautores[[1]][1])
    # glimpse(coautores[[1]][2])
    # autor,articulo_id,autor_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores
    
    listado_autores <- lista_vertices_autores(current_db)
    current_autor_1 <- listado_autores %>% filter(aut_id==coautores[[1]][1]) %>% pull(autor)
    current_autor_2 <- listado_autores %>% filter(aut_id==coautores[[1]][2]) %>% pull(autor)
    
    ret <- current_db %>% 
        filter(str_detect(autores,current_autor_1)) %>%
        filter(str_detect(autores,current_autor_2)) %>%
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
}

# dada la DB y un nombre de autor, devolver el lsitado
obtener_listado_articulos_vertice <- function(db,autor_nombre){
    ret <- db %>% 
        filter(str_detect(autores,autor_nombre)) %>%
        select(autores,anio,titulo,url,cant_autores,fuerza_colaboracion) %>%
        mutate(articulo=paste0("<p><a target='_blank' href='",url,"'>",titulo,"</a></p>")) %>% 
        group_by(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% tally() %>% 
        select(autores,anio,articulo,cant_autores,fuerza_colaboracion)
    
    ret
}


generar_subgrafo_vecinos <- function(g,vertice,random_seed=12345){
    if(!igraph::is_igraph(g)){
        warning("el parametro G debe ser un grafo !")
        stopifnot(igraph::is_igraph(g))
    }

    subgrafo_autor_ego <- igraph::make_ego_graph(graph = g, # para el grafo de la red
                                              order=1, # 1 nivel de vecinos
                                              nodes = vertice, # donde el vertice tenga de nombre el selected
                                              mode = "all" )

    #https://stackoverflow.com/questions/44712041/subset-igraph-object-to-just-2nd-order-ego-graph-of-certain-vertices
    # ego_list <- make_ego_graph(graph, order=2, nodes=V(graph)$condition=="something")
    
    subgrafo_autor <- NULL
    for (i in seq_along(subgrafo_autor_ego)){
        x <- subgrafo_autor_ego[[i]]
        subgrafo_autor <- igraph::graph.union(subgrafo_autor, x)
    }
    
    
    
    sg2 <- subgrafo_autor %>% 
        igraph::set_edge_attr(name="width",value = igraph::E(subgrafo_autor)$weight) %>%
        igraph::set_edge_attr(name="color",value = colores_edges_en_n_bins(igraph::E(subgrafo_autor)$weight)) %>% 
        igraph::set_vertex_attr(name = "title",value = igraph::V(subgrafo_autor)$name)
    tmp2 <- igraph::as_data_frame(sg2) %>% as_tibble() %>% mutate(nombre = paste0(from," - ",to,'<br />',"fuerza colaboración:",fuerza_colaboracion)) %>% pull(nombre)
    

    sg2 <- sg2 %>% 
        igraph::set_edge_attr(name="title",value = tmp2)
    
    sg2
}

generar_visualizacion_subgrafo_vecinos_from_subgrafo <- function(subgrafo,random_seed=12345){
    
    if(!igraph::is_igraph(subgrafo)){
        warning("el parametro G debe ser un grafo !")
        stopifnot(igraph::is_igraph(subgrafo))
    }
    
    tmp_grafo <- subgrafo
    
    filter_edge_resultado <- data.frame(autores=E(tmp_grafo)$autores,
                                        width=E(tmp_grafo)$width,
                                        color=E(tmp_grafo)$color,
                                        weight=E(tmp_grafo)$weight,
                                        fuerza_colaboracion=E(tmp_grafo)$fuerza_colaboracion,
                                        stringsAsFactors = FALSE)
    
    title_edges_tooltip <- filter_edge_resultado %>% 
        mutate(title=paste0( "Autores: ",autores,"<br/>",
                             "Fuerza Colaboración: ",fuerza_colaboracion,"<br/>",
                             "Cantidad Coautorias: ",weight)) %>% 
        pull(title)
    
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
        # set_edge_attr(name="width",value = filter_width_resultado) %>% 
        # set_edge_attr(name="color",value = filter_color_resultado) %>% 
        set_edge_attr(name="title",value = title_edges_tooltip) %>% 
        set_vertex_attr(name="title",value = title_vertex_tooltip) %>%
        set_vertex_attr(name="label",value = label_vertex)
    
    
    resultado_visnet <-  visNetwork::visIgraph(tmp_grafo,
                                                idToLabel = FALSE,
                                                randomSeed = random_seed ) %>% 
        visNetwork::visNodes(size = 10) %>%
        visNetwork::visIgraphLayout(randomSeed = random_seed,
                                     layout="layout_in_circle"
        ) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                                nodesIdSelection =FALSE)
    resultado_visnet
}
    

generar_visualizacion_subgrafo_vecinos <- function(g,vertice,random_seed=12345){

    if(!igraph::is_igraph(g)){
        warning("el parametro G debe ser un grafo !")
        stopifnot(igraph::is_igraph(g))
    }
    
    sg2 <- generar_subgrafo_vecinos (g,vertice,random_seed)
    
    resultado_visnet <- generar_visualizacion_subgrafo_vecinos_from_subgrafo(sg2,random_seed)
    resultado_visnet
    
}

armar_heatmap_ggplot_from_grafo <- function(g, color_palette="Dark2"){
    edge_list <- igraph::as_data_frame(g,what="edges") %>% as_tibble()
    
    # var1 <- edge_list %>% pull(from)
    # var2 <- edge_list %>% pull(to)
    # value <- edge_list %>% pull(fuerza_colaboracion)
    
    plot_out <- ggplot(edge_list, aes(autor1_label,
                                      autor2_label, 
                                      fill=fuerza_colaboracion)) + 
        scale_colour_brewer(palette = color_palette) +
        geom_raster() + 
        theme_light()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        labs(x = "", 
             y = "",
             fill="fuerza colabroación")
        
    plot_out
}


# FUNCIONES: ESTRUCTURA GRAFO ---------------------------------------------------------


# Este calcula la estructura sobre el grafo , las cosas que pueden ser extraidas desde el grafo
calcular_estructura_sobre_grafo <- function(grafo_reactive_tmp){
    resultado <- data.frame(tmp =c(''))
    resultado$cant_autores <- igraph::gorder(grafo_reactive_tmp)
    
    # cantidad relaciones
    resultado$cant_relaciones <- length(igraph::E(grafo_reactive_tmp))
    
    #densidad de la red
    resultado$densidad_red <- igraph::edge_density(grafo_reactive_tmp)
    
    resultado$distancia_media <- igraph::mean_distance(grafo_reactive_tmp)
    
    mas_lejanos <- igraph::farthest_vertices(grafo_reactive_tmp)
    resultado$num_dist_lejanos <-  mas_lejanos$distance
    #resultado$str_dist_lejanos_1 <- names(mas_lejanos$vertice[1])
    #resultado$str_dist_lejanos_2 <- names(mas_lejanos$vertice[2])
    
    #resultado$diametro_participantes <- igraph::get_diameter(grafo_reactive_tmp) %>% names() %>%paste0(collapse='; ')
    # te devuelve los nombres de los participantes del clique mas grande.
    largest_clique_str <- igraph::largest_cliques(grafo_reactive_tmp) %>% unlist() %>% names()
    
    # cuantos autores hay en el clique mas grandes
    resultado$num_largest_cliques <- length(largest_clique_str )
    
    # quienes son los que estan en el clique mas grande.
    #resultado$str_largest_cliques <- largest_clique_str  %>%paste0(collapse='; ')
    
    # porcentaje de la red que representa el clique mas grande.
    resultado$porc_largest_cliques <- resultado$num_largest_cliques / resultado$cant_autores * 100 
    
    resultado$num_cliques <- igraph::clique_num(grafo_reactive_tmp)
    resultado$num_transitivity <- igraph::transitivity(grafo_reactive_tmp)
    resultado$num_assort_degree <- igraph::assortativity_degree(grafo_reactive_tmp)
    
    #largest component, no es lo mismo que largest clique.
    # esta la funcion http://igraph.org/r/doc/components.html
    
    componentes <- igraph::components(grafo_reactive_tmp)
    
    resultado$componentes_largest <- max(componentes$csize)
    resultado$componentes_largest_porc <- resultado$componentes_largest / resultado$cant_autores * 100
    resultado$componentes_cantidad <- length(componentes$csize)
    # average distance
    resultado$distancia_media <- igraph::mean_distance(grafo_reactive_tmp)
    
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

# metricas para nodos dado un grafo
metricas_nodos_grafo <- function(grafo_reactive_tmp){
    grado_valor <- igraph::degree(grafo_reactive_tmp) %>% data.frame()
    grado_valor$autor <- rownames(grado_valor)
    colnames(grado_valor) <- c('degree','autor')
    grado_valor <- grado_valor %>% select(autor,degree)
    
    # betweeness
    betweeness_valores <- igraph::betweenness(grafo_reactive_tmp) %>% data.frame()
    betweeness_valores$autor <- rownames(betweeness_valores)
    colnames(betweeness_valores) <- c('betweeness','autor')
    
    # eigenvector centrality
    eigen_valor <- igraph::eigen_centrality(grafo_reactive_tmp)$vector %>% data.frame()
    eigen_valor$autor <- rownames(eigen_valor)
    colnames(eigen_valor) <- c('eigen_centrality','autor')
    
    # closeness centrality
    closeness_valor <- igraph::closeness(grafo_reactive_tmp) %>% data.frame()
    closeness_valor$autor <- rownames(closeness_valor)
    colnames(closeness_valor) <- c('closeness','autor')
    
    # pagerank centrality
    page_rank_valor <- igraph::page_rank(grafo_reactive_tmp)$vector %>% data.frame()
    page_rank_valor$autor <- rownames(page_rank_valor)
    colnames(page_rank_valor) <- c('page_rank','autor')
    
    #triangulos
    triangulos_count <- igraph::count_triangles(grafo_reactive_tmp) %>% data.frame()
    triangulos_count$autor <-  igraph::V(grafo_reactive_tmp)$name
    colnames(triangulos_count) <- c('count_triangles','autor')
    
    
    # nombre nodo
    nombres_nodos <- data.frame(nombre_autor=igraph::V(grafo_reactive_tmp)$label,
                                autor=igraph::V(grafo_reactive_tmp)$name,
                                fuerza_colaboracion=igraph::V(grafo_reactive_tmp)$fuerza_colaboracion,
                                stringsAsFactors = FALSE)

    estructura_red_df <- nombres_nodos %>% 
        inner_join(grado_valor) %>% 
        inner_join(betweeness_valores) %>%
        inner_join(eigen_valor) %>%
        inner_join(closeness_valor) %>%
        inner_join(page_rank_valor) %>%
        inner_join(triangulos_count) %>% 
        rename(id_autor=autor) %>% 
        rename(autor=nombre_autor) %>% 
        select(-id_autor)
    
    # igraph::vertex_attr_names(grafo_reactive_tmp)
    # V(grafo_reactive_tmp)$fuerza_colaboracion
    
    estructura_red_df
}


# tmp <- generar_grafos_similares(grafo_reactive_tmp)
# para generacion grafos similares
generar_grafos_similares <- function(grafo_reactive_tmp,cantidad=1000) {
    cant_autores <- igraph::gorder(grafo_reactive_tmp)
    densidad_red <- igraph::edge_density(grafo_reactive_tmp)
    
    # lista_generados <- vector('list',cantidad)
    
    lista_generados <- purrr::map(1:cantidad,~ igraph::barabasi.game(n = cant_autores,directed = FALSE ))
    
    # 1000
    # for (i in 1:cantidad){
    #     # ver ?erdos.renyi.game: gnp = probabilidad, gnm = M cantidad de edges
    #     # lista_generados[[i]] <- erdos.renyi.game(
    #     #     n=cant_autores,
    #     #     p.or.m = densidad_red,
    #     #     type = 'gnp',
    #     #     directed=FALSE)
    #     
    #     lista_generados[[i]] <- igraph::barabasi.game(n = cant_autores,directed = FALSE )
    # }
    
    lista_generados
}


# FUNCIONES: ESTRUCTURA GRAFO: SIMULACIONES  ------------------------------

# entra listado simulaciones + parametros
# lo extraigo a funcion porque lo voy a tener que hacer en los 3 modelos
# si eventualmente hay otros modelos se toca en 1 solo lugar
# lo mismo si se agregan metricas, se agregan en 1 solo lugar
# sale metricas resumidas + parametros
simulaciones_to_resumen_ejecucion <- function(simulaciones_metricas_list,params_list){
    # armar el DF de la lista
    simulaciones_metricas_df <- simulaciones_metricas_list %>% 
        dplyr::bind_rows()
    
    # armar las metricas resumen de interes: OJO DEBEN EXISTIR EN EL LISTADO !
    simulaciones_metricas_df_acum <- simulaciones_metricas_df %>% 
        summarise(
            mean_transitivity=mean(transitivity),
            mean_diameter=mean(diameter),
            mean_avg_path_length=mean(avg_path),
            mean_pow_law_alpha=mean(pow_law_alpha),
            mean_pow_law_greater_than_2=mean(pow_law_greater_than_2),
            mean_pow_law_ks_p=mean(pow_law_ks_p),
            mean_pow_law_ks_p_is_signif=mean(pow_law_ks_p_is_signif))
    
    # junto las metricas con los parametros y ordeno parametros adelante, metricas al final
    resultado <- simulaciones_metricas_df_acum %>% 
        bind_cols(params_list) %>% 
        select(starts_with("param_"),starts_with("mean_"))
    
    # devuelvo el resultado ordenado parametros adelante, metricas al final
    resultado
}

# guardo todos los resultados, les agrego los parametros.
simulaciones_to_resultados_con_params <- function(simulaciones_metricas_list,params_list){
    # armar el DF de la lista
    
    # simulaciones_metricas_list <- simulaciones_metricas
    simulaciones_metricas_df <- simulaciones_metricas_list %>% 
        map(~bind_cols(.x,params_list)) %>% 
        dplyr::bind_rows() %>% as_tibble()
    
    
    
    # junto las metricas con los parametros y ordeno parametros adelante, metricas al final
    resultado <- simulaciones_metricas_df %>% 
        # bind_cols(params_list) %>% 
        select(dplyr::starts_with("param_"),colnames(simulaciones_metricas_list[[1]]))
    
    # devuelvo el resultado ordenado parametros adelante, metricas al final
    resultado
}

# dado un grafo
# para que aca se cambie toda la parte de metricas 
# y se apliquen los mismos calculos en cada simulacion
calcular_metricas <- function(grafo,ks_p_signif=0.05){
    current_transitivity <- transitivity(grafo)
    current_diameter <- diameter(grafo) # distance between vertices is non-trivial. diameter 10 . avg path = 5.45
    current_avg_path <- average.path.length(grafo) 
    
    # este esta sacado de el apunte de soria de redes.
    
    # minimo_pow_law <- determinar_x_min_pow_law(igraph::degree(grafo))
    # glimpse(minimo_pow_law)
    # power law fit
    
    # resultado_fit_ley_potencia_basico <- power.law.fit(x = igraph::degree(grafo))
    # glimpse(resultado_fit_ley_potencia_basico)
    # resultado_fit_ley_potencia_xmin_manual <- power.law.fit(x = igraph::degree(grafo),xmin = minimo_pow_law)
    # glimpse(resultado_fit_ley_potencia_xmin_manual)
    resultado_fit_ley_potencia <- power.law.fit(x = igraph::degree(grafo),implementation = "plfit")
    # glimpse(resultado_fit_ley_potencia)
    
    # ¿El parámetro alfa de la función es mayor que 1? ¿Si? OK
    # 2.515571
    threshold_alpha <- 2
    pow_law_alpha <- resultado_fit_ley_potencia$alpha
    pow_law_greater_than_2 <- resultado_fit_ley_potencia$alpha > threshold_alpha 
    
    # ¿El test de ajuste de Kolmogorov-Smirnov es no significativo? OK.
    # Numeric scalar, the p-value of the Kolmogorov-Smirnov test. 
    # Small p-values (less than 0.05) indicate that the test rejected the hypothesis 
    # that the original data could have been drawn from the fitted power-law distribution.
    pow_law_ks_p <- resultado_fit_ley_potencia$KS.p
    pow_law_ks_p_is_signif <- resultado_fit_ley_potencia$KS.p < ks_p_signif
    
    #return for each
    ret <- data.frame(transitivity=current_transitivity,
               diameter=current_diameter,
               avg_path=current_avg_path,
               pow_law_alpha=pow_law_alpha,
               pow_law_greater_than_2=pow_law_greater_than_2,
               pow_law_ks_p=pow_law_ks_p,
               pow_law_ks_p_is_signif=pow_law_ks_p_is_signif)
    ret
}
# 
# determinar_x_min_pow_law <- function(data){
#     # https://www.stat.berkeley.edu/~aldous/Research/Ugrad/Willy_Lai.pdf
#     xmins <- unique(data) # search over all unique values of data
#     dat <- numeric(length(xmins))
#     z  <- sort(data)
#     for (i in 1:length(xmins)){
#         xmin <- xmins[i]    # choose next xmin candidate
#         z1 <- z[z>=xmin]    # truncate data below this xmin value
#         n <- length(z1)
#         a <- 1+ n*(sum(log(z1/xmin)))^-1    # estimate alpha using direct MLE
#         cx<- (n:1)/n # construct the empirical CDF
#         cf <- (z1/xmin)^(-a+1)# construct the fitted theoreticalDF
#         dat[i]  <-  max(abs(cf-cx))# compute the KS statistic
#     }
#     D = min(dat[dat>0],na.rm=TRUE)
#     # find smallest D
#     valuexmin = xmins[which(dat==D)] 
#     valuexmin
# }
    

# porque contra random ?
# porque la verificacion de small wolrd se corre contra modelos 
# aleatorios y la hipotesis era decir " che aca hay algo "
# Generate random graphs according to the G(n,m) Erdos-Renyi model
simular_random <- function(iter=100,vertex_count=100, edge_count=100,semilla=12345){
    #  iter=100; vertex_count=100; edge_count=100; semilla=12345
    set.seed(semilla)
    
    # maximo de edges en un grafo = n * (n-1) /2
    max_posible_edges <- vertex_count * (vertex_count-1) /2
    valida_max <-  edge_count <= max_posible_edges
    
    stopifnot(edge_count <= max_posible_edges)
    
    # if(!valida_max){
    #     mensaje <- paste0("Excede nro maximo de edges. maximo: ",max_edges," actual: ",edge_count)
    #     warning(mensaje)
    #     stopifnot(valida_max)
    # }
    
    

    # para cada 
    # obtener las simulaciones
    simulaciones_metricas <- purrr::map(seq_along(1:iter),function(x){
        # print(x)
        g_gnm <- igraph::sample_gnm(vertex_count, edge_count)
        ret <- calcular_metricas(g_gnm)
        ret
    })
    
    params_list <- data.frame(param_iter=iter,
                              param_vertex_count=vertex_count,
                              param_edge_count=edge_count,
                              stringsAsFactors = FALSE)
    #  iter=100; vertex_count=100; edge_count=100; semilla=12345
    
    
    
    simulaciones_metricas_df_resumen <- simulaciones_to_resultados_con_params(simulaciones_metricas,params_list)
    
    simulaciones_metricas_df_resumen
}

# porque contra small world ? porque interesa ver si se parece
# mas alla de las metricas de la red para comparar con las otras.
# correr 100 veces y sacar un promedio
# devuelve: parametros + media: avg_path_length
simular_small_world <- function(iter=100,vertex_count=100,vecindad=5,prob_reescritura=0.05,semilla=12345){
    #  semilla <- 12345 ; iter <- 100;  vertex_count <- 100; vecindad <- 5 ; prob_reescritura <- 0.05
    set.seed(semilla)
    
    # lista de parametros
    params_list <- data.frame(param_iter=iter,
                              param_vertex_count=vertex_count,
                              param_vecindad=vecindad,
                              param_prob_reescritura=prob_reescritura,stringsAsFactors = FALSE)
    
    # para cada 
    # obtener las simulaciones
    simulaciones_sw_metricas <- purrr::map(seq_along(1:iter),function(x){
        # print(x)
        g_small_world <- igraph::sample_smallworld(1, vertex_count, vecindad, prob_reescritura)
        ret <- calcular_metricas(g_small_world)
        ret
    })
    
    simulaciones_sw_met_df_acum <- simulaciones_to_resultados_con_params(simulaciones_sw_metricas,params_list)
    simulaciones_sw_met_df_acum
}

simular_scale_free <- function(iter=100,vertex_count=100,poder_pref_attach=3,m_edges_per_step=8,semilla=12345){
    #  iter<-100;vertex_count<-100;poder_pref_attach<-5;m_edges_per_step<-0.05;semilla<-12345
    set.seed(semilla)
    
    # lista de parametros
    params_list <- data.frame(param_iter=iter,
                              param_vertex_count=vertex_count,
                              param_poder_pref_attach=poder_pref_attach,
                              param_m_edges_per_step=m_edges_per_step,stringsAsFactors = FALSE)
    
    # para cada 
    # obtener las simulaciones
    simulaciones_metricas <- purrr::map(seq_along(1:iter),function(x){
        # print(x)
        grafo <- igraph::sample_pa(n = vertex_count,power = poder_pref_attach ,m= m_edges_per_step)
        ret <- calcular_metricas(grafo)
        ret
    })
    
    simulacione_df_acum <- simulaciones_to_resultados_con_params(simulaciones_metricas,params_list)
    simulacione_df_acum
}


# FUNCIONES: COMUNIDADES-----------------------------------------------------------------

arma_comunidad <- function(semilla_seed,tmp_grafo,comunidades_sel_algo){
    set.seed(semilla_seed)
    comunidad_sel_detalles <- ''
    
    comunidad_sel_detalles <-  switch(comunidades_sel_algo,
                                      cluster_edge_betweenness = igraph::cluster_edge_betweenness(tmp_grafo,weights = NULL),
                                      cluster_label_prop = igraph::cluster_label_prop(tmp_grafo),
                                      cluster_leading_eigen = igraph::cluster_leading_eigen(tmp_grafo),
                                      cluster_louvain = igraph::cluster_louvain(tmp_grafo),
                                      cluster_walktrap = igraph::cluster_walktrap(tmp_grafo),
                                      cluster_infomap = igraph::cluster_infomap(tmp_grafo),
                                      stop(paste0('NO DISPONIBLE',comunidades_sel_algo))
    )
    
    comunidad_sel_detalles
}
# nombre=names_memb, > nombres de los vertices
# member=values_memb > comunidad de membresia
armar_df_membership <- function(comunidad_sel_detalles){
    memb <- igraph::membership(comunidad_sel_detalles)
    values_memb <- as.vector(memb)
    names_memb <- names(memb) 
    membership_df <- data.frame(nombre=names_memb,member=values_memb,stringsAsFactors = FALSE) %>% as_tibble()
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
        current_subgraph <- igraph::induced_subgraph(current_grafo,current_group) 
        current_autores <- igraph::V(current_subgraph)$label
        base_autores_tmp <- current_base_autores %>% filter(autor %in% current_autores)
        
        estructura_grafo_df <- calcular_estructura_grafo(current_subgraph,base_autores_tmp)
        resul_acum <- resul_acum %>% bind_rows(estructura_grafo_df)
    }
    resul_acum$comunidad_id <- 1:length(current_comunidad)
    # glimpse(resul_acum)
    resul_acum
}

listado_comunidades_autores <- function(current_comunidad,autores_db){
    
    # nombre=names_memb, > nombres de los vertices
    # member=values_memb > comunidad de membresia
    nodo_comunidad <- armar_df_membership(current_comunidad)
    tmp_autores <- lista_vertices_autores(autores_db)
    
    # glimpse(nodo_comunidad)
    # 
    # glimpse(tmp_autores)
    
    nodo_comunidad_2 <- nodo_comunidad %>% 
        left_join(autores_db,by=c("nombre"="aut_id")) %>%
        select(nombre,member,autor) %>% 
        group_by(nombre,member,autor) %>% 
        tally() %>% 
        select(-n)
    
    listado_comunidades <- nodo_comunidad_2 %>% 
        arrange(member) %>% 
        group_by(member) %>% 
        summarize(n=n(),autores=paste(collapse='; ',autor)) %>% 
        arrange(desc(n)) %>% 
        rename(comunidad=member,cant_autores=n)
    listado_comunidades
}


# FUNCIONES: TEMPORAL -----------------------------------------------------

grafo_para_periodo_x <- function(periodos,cota_seccion,db_limpia,static_edge_width_multiplier){
    
    db_resultado <- articulos_todos_grafo(db_limpia,periodos,cota_seccion)
    grafo_bipartito <- armado_grafo_bipartito(db_resultado)
    g_coaut <- extraccion_grafo_coautoria(grafo_bipartito,db_resultado,
                                          width_multiplier = static_edge_width_multiplier)
    
    g_coaut
}

# devuelve un DF: periodo | lista periodos que incluye ese periodo
# ej: disponibles = 1996, 1999, 2000 => 
#    periodo= 1996  ;lista_periodos = 1996
#    periodo= 1999  ;lista_periodos = 1996,1999
#    periodo= 2000  ;lista_periodos = 1996,1999,2000
armar_periodos_acumulados_segun_disponibles <- function(periodos_disponibles){
    tmp_anios_acum <- global_periodos_disponibles %>% as_tibble() 
    
    tmp_anios_acum <- tmp_anios_acum %>% rename(periodo = value)
    tmp_anios_acum$periodo_agrup <- Reduce(paste, as.character(tmp_anios_acum$periodo), accumulate = TRUE)
    tmp_anios_acum <- tmp_anios_acum %>% 
        mutate(periodos_lista = str_split(periodo_agrup,pattern = " ")) %>% 
        select(periodo,periodos_lista)
    
    tmp_anios_acum
}


temporal_generar_grafos_acumulados <- function(global_periodos_disponibles,
                                               cota_seccion,
                                               db_limpia,
                                               static_edge_width_multiplier){
    
    tmp_anios_acum <- armar_periodos_acumulados_segun_disponibles(global_periodos_disponibles)
    
    tmp_anios_acum_2 <- map(tmp_anios_acum$periodos_lista, ~ grafo_para_periodo_x(.x,
                                                                                  cota_seccion,
                                                                                  db_limpia,
                                                                                  static_edge_width_multiplier)) 
    tmp_anios_acum_2
}

generar_nets_from_igraph <- function(listado_grafos_igraph){
    
    nets <- map(listado_grafos_igraph,intergraph::asNetwork)
    nets <- map(nets,delete.vertex.attribute,"id_old")
    nets <- map(nets,delete.vertex.attribute,"vertex.names")
    nets <- map(nets,delete.edge.attribute,"autor1_label")
    nets <- map(nets,delete.edge.attribute,"autor2_label")
    
    # delete.vertex.attribute(net,"id_old")
    # delete.vertex.attribute(net,"vertex.names")
    # delete.edge.attribute(net,"autor1_label")
    # delete.edge.attribute(net,"autor2_label")
    
    nets
    
}


generar_dyn_net_from_nets <- function(listado_nets){
    tnet_time <- networkDynamic::networkDynamic(network.list=listado_nets,
                                                vertex.pid='label',
                                                create.TEAs = TRUE)
    tnet_time
}

armar_render_ndtvd3_animacion <- function(current_test_tnet,
                                          n_bins = 3,
                                          color_palette = "Blues",
                                          default_v_border="lightgrey",
                                          default_v_color='#BABABA',
                                          param_out_mode = 'htmlWidget'){
    # 5 : descartado x arcoiris
    #n_bins <-  3
    #color_palette <- "Blues"#"YlGnBu" #"YlOrBr" # Spectral 
    
    print(Sys.time())
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
    # render.d3movie(net, output.mode = 'htmlWidget')
    ret_render <- render.d3movie(current_test_tnet, 
                                 usearrows = FALSE, 
                                 displaylabels = FALSE, 
                                 # label=function(slice){slice%v%'vertex.names'},
                                 label=function(slice){slice%v%'label'},
                                 
                                 bg="#ffffff", 
                                 #vertex.border="#FAFAFA",
                                 vertex.border=default_v_border,
                                 vertex.cex = 0.5,
                                 vertex.col = function(slice){
                                     ret  <-  default_v_color # default color
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
                                     curr_aut <- slice %v% "label"
                                     curr_fc <- slice %v% "fuerza_colaboracion"
                                     curr_anio <- slice %v% "anio"
                                     # curr_anio <- str_split(curr_anio,pattern = ";" ) %>% unlist() %>% unique() %>% paste(collapse = ";")
                                     
                                     paste("<b>Autor:</b>", curr_aut , "<br>",
                                           "<b>Fuerza Colaboración:</b>",curr_fc,"<br>",
                                           "<b>Periodo:</b>",curr_anio
                                     )},
                                 edge.tooltip = function(slice){
                                     
                                     curr_aut <- slice %e% "autores"
                                     curr_fc <- slice %e% "fuerza_colaboracion"
                                     curr_anio <- slice %e% "anios"
                                     # curr_anio <- str_split(curr_anio,pattern = ";" ) %>% unlist() %>% unique() %>% paste(collapse = ";")
                                     
                                     paste("<b>Autores:</b>",curr_aut, "<br>",
                                           "<b>Fuerza Colaboración:</b>",curr_fc ,"<br>",
                                           "<b>Periodo:</b>",curr_anio
                                     )},
                                 render.par=list(tween.frames = 10, show.time = F),
                                 plot.par=list(mar=c(0,0,0,0)),
                                 output.mode=param_out_mode )
    
    rend1 <- Sys.time()
    print(rend1)
    print(paste0("elapsed render:",rend1-rend0))
    
    ret_render
}


# FUNCIONES: debugging ---------------------------------------------------------------

# validacion de FCAutor
fuerza_colaboracion_autorut_expected_validator <- function(current_autor, data_acotado,grafo_bipartito,con_mensaje=FALSE){
    total <- art_full %>% filter(str_detect(autor,current_autor)) %>% 
        group_by(autor,articulo_id,anio,fuerza_colaboracion) %>% 
        tally() %>% group_by(autor) %>% 
        summarise(total_fca=sum(fuerza_colaboracion))
    
    FCAut_expected <- total %>% pull(total_fca) %>% as.numeric()
    
    vertex_current <- igraph::V(grafo_bipartito)[str_detect(igraph::V(grafo_bipartito)$label,current_autor) ]
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

# VER DETALLE VERTEX
show_details_vertex <- function(filter_vertex){

    cat(paste0("\nid:",filter_vertex$id,
               "\nname:",filter_vertex$name,
               "\nlabel:",filter_vertex$label,
               "\nfuerza_colaboracion:",filter_vertex$fuerza_colaboracion,"\n"))     
    
}


show_bipart_vertex_id_diff <- function(grafo,data_acotado){
    vertex_list_autores <- lista_vertices_autores(data_acotado)
    tmp_df <- data.frame(id=igraph::V(grafo)$id,
                         name=igraph::V(grafo)$name,
                         label=igraph::V(grafo)$label,
                         fuerza_colaboracion=igraph::V(grafo)$fuerza_colaboracion,
                         stringsAsFactors = FALSE)

    # id se agrega a mano ,
    # name se lo otorga el grafo.
    
    #aut_id,autor,autor_id,anios,secciones,fuerza_colaboracion_total,cant_autores_coautoria
    #arrange(autor)

    tmp_df %>% left_join(vertex_list_autores, by=c('label'='autor')) %>%
        filter(id != name) %>% select(id,name,label,fuerza_colaboracion)
    
}
