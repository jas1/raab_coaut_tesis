


# globales: db_limpia y cota_seccion
# segun seleccion: periodos


testthat::test_that("grafo: orden de aristas OK",{

    source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
    
    input_static_periodos <- c(1996)
    
    cota_seccion <- c("Trabajos Originales")
    # db recreada
    db_limpia <- "db_raab_grafos.sqlite"
    # resultado vacio # warning, sigue adelante.
    #art_empty <- articulos_todos_grafo(db_limpia,anios = input_static_periodos,secciones = cota_seccion)
    
    p1996 <- articulos_todos_grafo(db_limpia,input_static_periodos,cota_seccion)    
    
    testthat::expect_true( tibble::is_tibble(p1996)  )
    testthat::expect_true( nrow(p1996) > 0  )
    testthat::expect_true( unique(p1996$anio) == input_static_periodos  )
    testthat::expect_true( unique(p1996$seccion) == cota_seccion  )

})

