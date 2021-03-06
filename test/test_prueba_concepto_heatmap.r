# prueba de concepto heatmap
# probar la funcion de heatmap para toda la red


# el objetivo de este archivo es probar que biofabric funciona ok con el grafo

library(here)#install.packages("here")
source(here:::here("funcs","imports.r"),encoding = "UTF-8")

testthat::test_that(
    "heatmap:ok",{
        source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
        
        semilla <- 12345
        
        # cota temporal segun tesis
        cota_anio <-  c(1996)
        
        # cota seccion segun tesis
        cota_seccion <- c("Trabajos Originales")
        
        # db recreada
        db_limpia <- paste0("db_raab_grafos.sqlite")
        
        # articulos todo
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        
        # grafo todo
        gb_ok <- armado_grafo_bipartito(art_full)
        # grafo coautoria
        grafo <- extraccion_grafo_coautoria (gb_ok,art_full)
        
        
        
        grafo_names <- grafo
        plot_out <- armar_heatmap_ggplot_from_grafo(grafo)
        
        ggplotly(plot_out)
        
                
        # V(grafo_names)$name <- V(grafo_names)$label
        # 
        # # biofabric
        # bioFabric_htmlwidget(bioFabric( grafo_names))
    })

testthat::test_that(
    "heatmap:ok:todos",{
        source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
        
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
        # grafo coautoria
        grafo <- extraccion_grafo_coautoria (gb_ok,art_full)
        
        plot_out <- armar_heatmap_ggplot_from_grafo(grafo)
        
        plot_out
        ggplotly(plot_out)
        # grafo_names <- grafo
        # 
        # V(grafo_names)$name <- V(grafo_names)$label
        # 
        # # biofabric
        # bioFabric_htmlwidget(bioFabric( grafo_names))
    })