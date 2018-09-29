# test funciones Web


testthat::test_that(
    "mostrar grado: devuelva grado",{
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        in_value <- "a0185"
        g <- g_aut
        
        vertice <- get_vertex_from_click_vertex(g,in_value)
        
        grado <- igraph:::degree(g,vertice)
        #glimpse(grado)
        actual <- as.numeric(grado)
        expected <- 0
        
        expect_equal(actual, expected)
        
})
