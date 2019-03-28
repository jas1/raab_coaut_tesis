#;install.packages("assertive")

# test generacion grafo


# check db existente ------------------------------------------------------

#check KO
db_name <- 'watuzi'
conn <- get_db_connection(db_name)

# check OK
db_name <- 'db_raab_grafos.sqlite'
conn <- get_db_connection(db_name)

# df se genera bien -------------------------------------------------------
# db_name,anios,secciones
# cota seccion segun tesis
cota_seccion <- c("Trabajos Originales")
# db recreada
db_limpia <- "db_raab_grafos.sqlite"

# resultado vacio # warning, sigue adelante.
art_empty <- articulos_todos_grafo(db_limpia,anios = c(1991),secciones = cota_seccion)

# resultado total: 
cota_anio <-  c(1996:2016)
art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)


# pre condciones bipartito ----------------------------------------------

# manda cualquier param
lista_vertices_autores("fruta")
# manda algo vacio
lista_vertices_autores(art_empty)
# manda algo OK
lista_vertices_autores(art_full)
    
# manda cualquier param
lista_vertices_articulos("fruta")
# manda algo vacio
lista_vertices_articulos(art_empty)
# manda algo OK
lista_vertices_articulos(art_full)

# generacion grafo bipartito ----------------------------------------------

# manda cualquier param
armado_grafo_bipartito("fruta")

# manda vacio
armado_grafo_bipartito(art_empty) 
# revienta por no bipartito, lo cual estaria bien ? ( no estoy seguro)

# manda ok
gb_ok <- armado_grafo_bipartito(art_full) # OK

# verificaciones extra: 

# fuerza colaboracion autor
# 1
glimpse(art_full)


current_aut <- "Russo"
# fuerza_colaboracion_autorut_expected_validator("Carne",art_full,gb_ok)
fuerza_colaboracion_autorut_expected_validator(current_aut,art_full,gb_ok,con_mensaje = TRUE)

# vertex_attr_names(gb_ok)
filter_vertex_cond <- str_detect(V(gb_ok)$label,current_aut)
filter_vertex <- igraph:::V(gb_ok)[filter_vertex_cond]
show_details_vertex(filter_vertex)
show_bipart_vertex_id_diff(gb_ok,art_full) # ahora se ve que el problema esta en los articulos.

art_full %>% filter(str_detect(art_id,'b001') )

art_full %>% select()

VLART <- art_full %>% 
    group_by(art_id,articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion,autores) %>% 
    tally() %>% select(-n) %>% arrange(autores,articulo_id)


test_that(
    "grafo: orden de aristas OK",
    {
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        data_acotado <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        
        g_aut_art <- igraph:::graph.data.frame(data_acotado,directed = FALSE)
        
        g_aut_art <- transformar_en_bipartito(g_aut_art,data_acotado)
        
        vertex_list_autores <- lista_vertices_autores(data_acotado)
        vertex_list_articulos_old <- lista_vertices_articulos(data_acotado)
        vertex_articulos_names <- igraph:::V(g_aut_art)[!igraph:::V(g_aut_art)$type]$name
        vertex_list_articulos <- ordenear_lista_articulos_by_vertex_attr(vertex_articulos_names,vertex_list_articulos_old)
        
        # 
        # orden_original <- data.frame(stringsAsFactors = FALSE, 
        #                              art_id=vertex_articulos_names)
        # # print("----")
        # # glimpse(art_name_list)
        # # print("----")
        # # glimpse(orden_original)
        # # print("----")
        # # glimpse(vertex_list_articulos_old)
        # # print("----")
        # resultado <-  orden_original %>% 
        #     left_join(vertex_list_articulos_old,by=c('art_id'='art_id')) %>%
        #     select(art_id,articulo_id,anio,cant_autores,fuerza_colaboracion)
        
        
        
        expected <- vertex_articulos_names
        actual <- vertex_list_articulos %>% pull(art_id)
        expect_equal(actual, expected)
    }
)


test_that(
    "grafo: generacion aristas en coautorias ok ",
    {
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        # igraph:::E(g_aut)
        # 
        # igraph:::edge_attr_names(g_aut)
        # 
        # head(igraph:::E(g_aut)$id)
        # head(igraph:::E(g_aut)$autores)
            
        
            
        g_aut
        
    }
)


testthat::test_that(
    "grafo: generacion edges",{
        
        fuerza_colaboracion_relacion
    
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        
        
        grafo_bipartito <- gb_ok
        edgelist_para_grafo <- art_full 
        g_projections <- igraph:::bipartite_projection(grafo_bipartito,multiplicity = TRUE)
        
        g_aut <- g_projections$proj2
        # g_art <- g_projections$proj1
        
        # measure of collaboration strength illustrated in Fig. 5. Newman 2004 ; entonces el tamaño del nodo esta con que tan colaborativo fue
        igraph:::V(g_aut)$size <- 10 + (igraph:::V(g_aut)$fuerza_colaboracion*10) # *10 para que aumente el tamaño , dejando de ser decimal
        
        # elist <- igraph:::as_edgelist(g_aut)
        # igraph:::as_data_frame(g_aut,what="edges")
        # glimpse(elist)
        
        lista_autores <- lista_vertices_autores(edgelist_para_grafo) %>% select(aut_id,autor)
        
        #data.frame(elist,stringsAsFactors = FALSE)
        elist_df <- igraph:::as_data_frame(g_aut,what="edges") %>% as_tibble() %>% 
            rename(autor1=from,autor2=to) %>% 
            mutate(id=paste0(autor1,"--",autor2)) %>% 
            left_join(lista_autores,by=c('autor1'='aut_id')) %>% 
            rename(autor1_label=autor) %>% 
            left_join(lista_autores,by=c('autor2'='aut_id')) %>% 
            rename(autor2_label=autor) %>% 
            mutate(autores=paste0(autor1_label,' - ',autor2_label))
        
        
        # fuerza_colaboracion_relacion(elist_df %>% filter(str_detect(autor1_label,"Oyhenart") ,
        #                                                  str_detect(autor2_label,"Pucciarelli")),
        #                              edgelist_para_grafo)
        
        fuerza_colaboracion_y_anios_relacion(elist_df %>% filter(str_detect(autor1_label,"Oyhenart") ,
                                                         str_detect(autor2_label,"Pucciarelli")),
                                     edgelist_para_grafo)
        
        
        # edge_analizar_ends <- elist_df %>% filter(str_detect(autor1_label,"Oyhenart") ,
        #                                           str_detect(autor2_label,"Pucciarelli"))
        # autor1_end <- edge_analizar_ends %>% pull(autor1_label)
        # autor2_end <- edge_analizar_ends %>% pull(autor2_label)
        # 
        # filtro_1 <- edgelist_para_grafo %>% 
        #     filter(str_detect(autores,autor1_end) & str_detect(autores,autor2_end))
        # 
        # fuerza_colab <- filtro_1 %>% 
        #     count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>%
        #     count(fuerza_colaboracion) %>% 
        #     summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()
        # 
        # 
        # filtro_1 %>% 
        #     count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>% 
        #     
        #     count(fuerza_colaboracion)
        # 
        # filtro_1 %>% 
        #     count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>% 
        #     summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()
        # 
        # filtro_1 %>% 
        #     count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>% 
        #     select(titulo,anio,fuerza_colaboracion) %>% 
        #     summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()
        # 
        # filtro_1 %>% 
        #     count(articulo_id,url,titulo,anio,seccion,cant_autores,fuerza_colaboracion, autores) %>% 
        #     select(titulo,anio,fuerza_colaboracion) %>% 
        #     count(anio,fuerza_colaboracion) %>% 
        #     summarise(fuerza_colaboracion_total = sum(fuerza_colaboracion)) %>% as.double()
        # 
        # fuerza_colab

})


test_that(
    "grafo: orden de vertices OK",
    {

        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        
        # execute 
        func_test <- show_bipart_vertex_id_diff(gb_ok,art_full)
        
        # Test
        expected <- 0
        actual <- nrow(func_test)
        
        if (actual!=expected) {
            print(func_test)
        }
        
        expect_equal(actual, expected)
    }
)
# 
# write.table(art_full,file = here("tmp","tmp_todos.txt"),sep = "\t")
# 
# # art_full %>% filter(str_length(articulo_id) > 4)
# # la joda es lograr una indexacion que sirva para ambos ...
# 
# # esto me resolvio el problema de autores
# art_full %>% filter(str_detect(aut_id,"a002")) %>% arrange(aut_id)
# ahora tengo el problema de edges xD ! 
test_that(
    "click nodo: que devuelva el vertice",
    {
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        vertice_label_expected <- "María Gabriela Russo"
        vertice_id_expected <- "a0185"
        vertice_name_expected <- "a0185"
        
        glimpse(art_full)
        art_full %>% filter(aut_id==vertice_id_expected)
        art_full %>% filter(autor==vertice_label_expected)
        
        # execute 
        vertice <- get_vertex_from_click_vertex(g_aut,vertice_id_expected)
        
        expect_equal(vertice$label, vertice_label_expected)
        expect_equal(vertice$id, vertice_id_expected)
        expect_equal(vertice$name, vertice_name_expected)
    }
)

test_that(
    "art.asoc: subgrafo para autor",
    {
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        vertice_id_expected <- "a0183"
        
        vertice <- get_vertex_from_click_vertex(g_aut,vertice_id_expected)
        
        subgrafo <- generar_subgrafo_vecinos(g_aut,vertice)
        
        amount_vertex <- length(igraph:::V(subgrafo)$name)
        
        expected_amount <- 7
        
        expect_equal(amount_vertex, expected_amount)
        
        generar_visualizacion_subgrafo_vecinos_from_subgrafo(subgrafo)
        
    }
)


test_that::test_that(
    "grafo: bipartito show",
    {
        library(here)
        source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        data_acotado <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        
        g_aut_art <- igraph:::graph.data.frame(data_acotado,directed = FALSE)
        
        g_aut_art <- transformar_en_bipartito(g_aut_art,data_acotado)
        
        # voy a agarrar un componente mediano: 
        #
        autor_en_componente <- "Noemí Estela Acreche"
        autor_id <- data_acotado %>% 
            count(aut_id,autor) %>% 
            filter(autor == autor_en_componente) %>% 
            pull(aut_id)
        
        # agrego componente a los vertices
        set.seed(12345)
        V(g_aut_art)$componente <- igraph::components(g_aut_art)$membership
        

        
          
          class(vertice_autor)
          attributes(vertice_autor)
        
        subgrafos_componentes <- igraph::decompose.graph(g_aut_art)
        
        compos_filtered <- map(seq(length(subgrafos_componentes)),function(x){
            tmp_g_0 <- subgrafos_componentes[[x]]
            tmp_g <- tmp_g_0
            vertice_autor <- V(tmp_g)[V(tmp_g)$name == autor_id]
            
            # print(vertice_autor)

            if (length(attr(vertice_autor,"names"))!=0 ) {
                tmp_g
            }else{
                tmp_g <- NULL
                # message("no se encuentra")
            }
            tmp_g
        })
        
        any(compos_filtered)
         
        subrafo_componente_autor <- compos_filtered[!is.null(unlist(compos_filtered))]
        
        subrafo_componente_autor %>% subgraph()
            add_layout_(as_bipartite()) %>%
            plot()
        
})




# fuerza_colaboracion_autorut_expected_validator("Hayes",art_full,gb_ok)

# g_aut_art <- igraph:::graph.data.frame(art_full,directed = FALSE)
# g_aut_art <- transformar_en_bipartito(g_aut_art,art_full)
# 
# filtro_solo_autores <- igraph:::V(g_aut_art)$type
# idx_name_vertex <- as_tibble(igraph:::V(g_aut_art)[filtro_solo_autores]$name) %>% tibble::rowid_to_column() %>% rename(autor=value)
# idx_name_lista <- lista_vertices_autores(art_full) %>% tibble::rowid_to_column()
# 
# nrow(idx_name_vertex) == nrow(idx_name_lista)
# 
# join_lists <- idx_name_vertex %>% left_join(idx_name_lista,by = c("autor"="autor"))
# join_lists %>% filter(str_detect(autor,current_aut)) %>% select(rowid.x,rowid.y,autor,fuerza_colaboracion_total)
# 


# varios

# fuerza colaboracion articulo



# generacion grafo coautoria ----------------------------------------------


