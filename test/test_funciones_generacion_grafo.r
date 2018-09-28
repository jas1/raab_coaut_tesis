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
        data_acotado <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        vertice_label_expected <- "María Gabriela Russo"
        vertice_id_expected <- "a0185"
        vertice_name_expected <- "a0185"
        
        # execute 
        vertice <- get_vertex_from_click_vertex(g_aut,vertice_id_expected)
        
        expect_equal(vertice$label, vertice_label_expected)
        expect_equal(vertice$id, vertice_id_expected)
        expect_equal(vertice$name, vertice_name_expected)
    }
)




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


