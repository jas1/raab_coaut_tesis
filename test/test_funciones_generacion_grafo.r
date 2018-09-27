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


