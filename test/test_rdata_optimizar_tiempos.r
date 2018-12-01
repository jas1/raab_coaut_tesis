# exportar un rdata para optimizar tiempos

library(here)
source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # por variables globales
source(here:::here("funcs","globals.r"),encoding = "UTF-8") # por variables globales

testthat::test_that("armar rdata para no acumulado",{

    #- listado de grafos: temporal_basico_grafos_reactive
    
    temporal_basico_grafos <- map(global_periodos_disponibles, 
        ~ grafo_para_periodo_x(.x,
                               cota_seccion,
                               db_limpia,
                               static_edge_width_multiplier)) 
    
    tmp_db_filtrada <- map(global_periodos_disponibles,function(x){
        articulos_todos_grafo(db_limpia,x,cota_seccion)  
    })

    # ver como aplico MAP X, Y contra funcion , 
    # pasandole X1 > Y1 a XN > YN 
    # de esta forma van a dar bien los numeros dado que la DB esta filtrada por año
    
    # mean(V(temporal_basico_grafos[[1]])$fuerza_colaboracion)
    # mean(E(temporal_basico_grafos[[1]])$fuerza_colaboracion)
    # 
    # mean(V(temporal_basico_grafos[[18]])$fuerza_colaboracion)
    # mean(V(temporal_basico_grafos[[18]])$fuerza_colaboracion)
    
    calculo_grafos <- purrr::map2(temporal_basico_grafos,tmp_db_filtrada,calcular_estructura_grafo)
    
    #- listado de metricas: temporal_basico_grafos_reactive
    # calculo_grafos <- map(temporal_basico_grafos,
    #                       ~ calcular_estructura_grafo(
    #                           .x,tmp_db_filtrada
    #                       ))
    # glimpse(calculo_grafos)
    names(calculo_grafos) <- global_periodos_disponibles
    
   # - transformar la lista a un DF de todos los periodos		
    temporal_basico_estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names)  
    
   # - armar una estructura tidy del wide , via gather, par apoder usar en ggplot facil
    temporal_basico_grafo_estr_as_stack <- temporal_basico_estr_grafos %>% as_tibble() %>% 
        gather(key=metrica,value='valor',-periodo) %>%
        mutate(plot_text=paste0('Período: ',periodo,'<br />',
                                'Métrica: ',metrica,'<br />',
                                'Valor: ',valor)) 
    
    # temporal_basico_grafo_estr_as_stack %>% count(metrica)
    
    temporal_basico_data_name <- here::here("data","temporal_basico_data.Rdata")
    variables_a_guardar <- c("temporal_basico_grafos",
                             "temporal_basico_estr_grafos",
                             "temporal_basico_grafo_estr_as_stack")

    # grabo las variables de interes
    base::save(list =variables_a_guardar, 
               file = temporal_basico_data_name)
    
    # limpiar variables
    base::remove(list =variables_a_guardar)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})

    # levantarlas desde el nuevo archivo
    base::load(temporal_basico_data_name)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})
    
    #luego de probarlo live, en velocidad no hace falta chachear todo, solo el inicial
})


testthat::test_that("armar rdata para acumulado",{
    
    #- listado de grafos: temporal_basico_grafos_reactive
    
    temporal_acumulado_grafos <- temporal_generar_grafos_acumulados(global_periodos_disponibles,
                                                           cota_seccion,
                                                           db_limpia,
                                                           static_edge_width_multiplier)
    
    
    
    tmp_anios_acum <- armar_periodos_acumulados_segun_disponibles(global_periodos_disponibles)
    
    tmp_db_filtrada <- map(tmp_anios_acum$periodos_lista,function(x){
        articulos_todos_grafo(db_limpia,x,cota_seccion)  
    })
    
    calculo_grafos <- purrr::map2(temporal_acumulado_grafos,tmp_db_filtrada,calcular_estructura_grafo)
    names(calculo_grafos) <- global_periodos_disponibles
    temporal_acumulado_estr_grafos <- dplyr::bind_rows(calculo_grafos, .id = 'names') %>% rename(periodo=names) 
# V( temporal_acumulado_grafos[[18]])$anio
    
    temporal_acumulado_grafo_estr_as_stack <- temporal_acumulado_estr_grafos %>% as_tibble() %>% 
        gather(key=metrica,value='valor',-periodo) %>%
        mutate(plot_text=paste0('Período: ',periodo,'<br />',
                                'Métrica: ',metrica,'<br />',
                                'Valor: ',valor)) 
    
    # metricas_nodos_grafo
    calculo_nodos_grafos <- map(temporal_acumulado_grafos,metricas_nodos_grafo)
    names(calculo_nodos_grafos) <- global_periodos_disponibles
    # glimpse(calculo_nodos_grafos)
    temporal_acumulado_nodos_grafos <- dplyr::bind_rows(calculo_nodos_grafos, .id = 'names') %>% rename(periodo=names)  
    glimpse(temporal_acumulado_nodos_grafos)

    temporal_acumulado_data_name <- here::here("data","temporal_acumulado_data.Rdata")
    variables_a_guardar <- c("temporal_acumulado_grafos","temporal_acumulado_nodos_grafos","temporal_acumulado_estr_grafos","temporal_acumulado_grafo_estr_as_stack")
    
    base::save(list =variables_a_guardar, 
               file = temporal_acumulado_data_name)
    
    # limpiar variables
    base::remove(list =variables_a_guardar)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})
    
    # levantarlas desde el nuevo archivo
    base::load(temporal_acumulado_data_name)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})
    
    #luego de probarlo live, en velocidad no hace falta chachear todo, solo el inicial
})



testthat::test_that("armar rdata para animacion acumulada",{
    
    # grafo_reactive_tmp <- temporal_basico_grafos[[18]]
    # data_acotado <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
# 
#     head(data_acotado)
#     g_aut_art <- igraph:::graph.data.frame(data_acotado,directed = FALSE)
#     V(g_aut_art)$anios
    # igraph::vertex_attr_names(g_aut_art)
    # igraph::vertex_attr(temporal_acumulado_grafos[[18]],"anio") %>% 
    #     as_tibble() %>% 
    #     map(function(x){
    #         x$
    # #     })
    #     
    #     
    #     mutate(anio_chr=as.data.frame(table(str_split(value,";")))) %>% 
    #     mutate(anio_df=as.data.frame(table(anio_chr))) %>% 
    
    temporal_acumulado_network_pkg <- generar_nets_from_igraph(temporal_acumulado_grafos)
    temporal_dinamico_acumulado <- generar_dyn_net_from_nets(temporal_acumulado_network_pkg)
    temporal_dinamico_acumulado_anim <- armar_render_ndtvd3_animacion(temporal_dinamico_acumulado)

    temporal_acumulado_animacion_data_name <- here::here("data","temporal_acumulado_dyn_data.Rdata")
    variables_a_guardar <- c("temporal_acumulado_network_pkg","temporal_dinamico_acumulado","temporal_dinamico_acumulado_anim")
    
    base::save(list =variables_a_guardar, 
               file = temporal_acumulado_animacion_data_name)
    
    # limpiar variables
    base::remove(list =variables_a_guardar)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})
    
    # levantarlas desde el nuevo archivo
    base::load(temporal_acumulado_animacion_data_name)
    purrr::map(variables_a_guardar,function(x){base::exists(x)})
    
    #luego de probarlo live, en velocidad no hace falta chachear todo, solo el inicial
})