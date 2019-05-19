# prueba de concepto distribucion grado y otras metricas.
# dada una estructura de nodos.
# 

library(here)
source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
testthat::test_that(
    "prueba concepto componentes R",{
        
        
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        # cota_anio <- c("1996")
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        degree_distrib <- degree_distribution(g_aut) %>% 
            as_tibble() %>% 
            rename(degree_distribution=value) %>% 
            tibble::rownames_to_column("degree") %>% 
            mutate(degree=as.integer(degree)) %>% 
            arrange(degree)
            
        plot_dist_grado <- degree_distrib %>% 
            ggplot(aes(x=degree,y=degree_distribution)) + 
            geom_col() +
            labs(title="Distribución de Grado",
                 x="grado",
                 y="probabilidad de grado")+
            theme_light()
        ggplotly(plot_dist_grado)
        
        degree_distrib %>%  filter(degree_distribution == 0 )
        
        plot_dist_grado_log_log <- degree_distrib %>% 
            ggplot(aes(x=degree,y=degree_distribution+1)) + 
            geom_point() +
            scale_x_log10()+
            scale_y_log10()+
            labs(title="Distribución de Grado ( log / log )",
                 subtitle="distribución+1 para arreglo de infinito",
                 x="grado",
                 y="probabilidad de grado")+
            theme_light()
        ggplotly(plot_dist_grado_log_log)
        
        
        
        # y que tal distribucion de otras metricas ?
        
        metricas_nodos <- metricas_nodos_grafo(g_aut)
        # colnames(metricas_nodos)
        metricas_nodos_long <- metricas_nodos %>%  tidyr::gather(metrica,valor,-autor) %>%  as_tibble() 
        
        metrica_seleccionada <- "degree"
        metrica_seleccionada <- "fuerza_colaboracion"
        metrica_seleccionada <- "closeness"
        metrica_seleccionada <- "count_triangles"
        
        metricas_nodos_long %>% 
            filter(metrica==metrica_seleccionada) %>% 
            ggplot(aes(valor)) +
            geom_histogram() +
            labs(title=paste0("Histograma de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "cantidad nodos")+
            theme_light()
        
        metricas_nodos_long %>% 
            filter(metrica==metrica_seleccionada) %>% 
            ggplot(aes(valor)) +
            geom_density_line() +
            labs(title=paste0("Densidad de ",metrica_seleccionada),
                 x= metrica_seleccionada,
                 y= "densidad")+
            theme_light()
        
        # para terminar ver todas las metricas tipo el ggridges
        # problema: no manejan misma escala => no se pueden mostrat todos a la vez.
        # descartado
        metricas_nodos %>% skimr::skim()
        metricas_nodos_long %>% 
            
            # filter(metrica==metrica_seleccionada) %>% 
            ggplot(aes(y=metrica,x=valor,color=metrica)) +
            ggridges::geom_density_ridges(alpha = 0.2)+
            labs(title=paste0("Densidades de metricas"),
                 x= "valores",
                 y= "cantidad nodos") 
        
        
        metricas_plots <- metricas_nodos_long %>% 
            nest(-metrica) %>% 
            mutate(plot_densidad=map2(data,metrica,function(x,metrica){
                x %>% ggplot(aes(x=valor)) +
                    geom_density_line(alpha = 0.2)+
                    labs(title=metrica,
                         x="valor",
                         y="densidad")
                })) %>% 
            mutate(plot_histograma=map2(data,metrica,function(x,metrica){
                x %>% ggplot(aes(x=valor)) +
                    geom_histogram(alpha = 0.2)+
                    labs(title=metrica,
                         x="valor",
                         y="cantidad nodos")
            }))

        metricas_plots$plot_densidad[[1]] + labs(title="Fuerza de Colaboración")
        metricas_plots$plot_histograma
            # filter(metrica==metrica_seleccionada) %>% 
            
        
})