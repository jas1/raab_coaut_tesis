# test_prueba_concepto_cant_nodos_metrica
# la idea es como tener la metrica por tiempo, buen pero en vez de eso seria
# cada metrica vs los nodos que la tienene. 

library(here)
source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version

testthat::test_that("probar metricas en vs nodos",{
    
    source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
    source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
    
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
    
    # extraccion metricas nodos:
    metricas_nodos <- metricas_nodos_grafo(grafo_reactive_tmp = grafo)
    currnet <- colnames(metricas_nodos)
    metricas_nodos %>% 
        ggplot(aes(x=currnet[[2]])) +
        geom_histogram(stat="count")+
        labs(x=paste("métrica:",currnet[2]), y = "cantidad nodos")
    
    metricas_nodos %>% 
        ggplot(aes(x=betweeness)) +
        geom_histogram()+
        labs(x="métrica", y = "cantidad nodos")  
    
    metricas_nodos %>% 
        ggplot(aes(x=eigen_centrality)) +
        geom_histogram()+
        labs(x="métrica", y = "cantidad nodos")
    metricas_nodos %>% 
        ggplot(aes(x=closeness)) +
        geom_histogram()+
        labs(x="métrica", y = "cantidad nodos")
    metricas_nodos %>% 
        ggplot(aes(x=page_rank)) +
        geom_histogram()+
        labs(x="métrica", y = "cantidad nodos")
    metricas_nodos %>% 
        ggplot(aes(x=count_triangles)) +
        geom_histogram()+
        labs(x="métrica", y = "cantidad nodos")
})