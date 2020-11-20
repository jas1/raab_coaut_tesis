# toca revisar bug: 
# analisis estatico > periodo 1996 > ver estructura > ver detalle componentes > seleccionar componente de 5
# 
# 82: <reactive:palabras_analizadas_reactive> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#168]
# 66: palabras_analizadas_reactive
# 62: <reactive:palabras_analizadas_count_reactive> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#194]
# 46: palabras_analizadas_count_reactive
# 44: <observer> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#86]

# forma correcta par air viendo: 
# https://mastering-shiny.org/scaling-testing.html#testing-functions
# https://mastering-shiny.org/scaling-modules.html

# la app quedo outdated y mil errores en el medio toca ir revisando y mejorando.
# library("shiny")
# library("testthat")
# library("shinytest")
# pacman::p_load("shiny")
# pacman::p_load("shinytest")
# pacman::p_load("testthat")

#usethis::use_test()

testthat::test_that("datos tabla reactive",{
    
    semilla <- 12345
    
    # cota temporal segun tesis
    cota_anio <-  c(1996:2016)
    
    # cota seccion segun tesis
    cota_seccion <- c("Trabajos Originales")
    
    # db recreada
    db_limpia <- paste0("db_raab_grafos.sqlite")
    
    # articulos todo
    art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
    
    # grafo todo
    gb_ok <- armado_grafo_bipartito(art_full)
    
    # algoritmo seleccionado
    algoritmo_seleccionado <- 'cluster_edge_betweenness'
    
    comunidad_sel_detalles <- arma_comunidad(
        semilla,
        gb_ok,
        algoritmo_seleccionado)
    
    
    autores_db <- art_full
    current_comunidades <- comunidad_sel_detalles
    resultado <- listado_comunidades_autores (current_comunidades,autores_db)
    
    head(resultado)
    
    nodo_comunidad <- armar_df_membership(current_comunidades)
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
    
    
    stopifnot(resultado$cant_autores == listado_comunidades$cant_autores)
    
})
