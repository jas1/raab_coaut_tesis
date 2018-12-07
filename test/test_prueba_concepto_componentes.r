
library(here)
testthat::test_that(
    "prueba concepto componentes R",{
        
        source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        # cota_anio <-  c(1996:2016)
        cota_anio <- c("1996")
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        # 
        # componentes <- igraph::components(g_aut)
        # compo_df <- data.frame(nombres=componentes$membership %>% names(),
        #            membership=componentes$membership,row.names = NULL,stringsAsFactors = FALSE ) %>% as_tibble()
        
        #PARTE I: LISTADO
        subgrafos_componentes <- igraph::decompose.graph(g_aut)
        names(subgrafos_componentes) <- 1:length(subgrafos_componentes)
        listados_componentes <- purrr::map(subgrafos_componentes,calcular_estructura_sobre_grafo)
        names(listados_componentes) <- 1:length(listados_componentes)
        estr_componentes_list <- dplyr::bind_rows(listados_componentes, .id = 'names') %>% rename(componente_id=names)  
        
        # compo_df %>% 
        #     mutate(vertex_name=V(g_aut)$name) %>% 
        #     mutate(igual=nombres==vertex_name) %>% filter(igual)
        #PARTE II: AUTORES
        igraph::vertex_attr_names(subgrafos_componentes[[1]])
        V(subgrafos_componentes[[1]])$label
        listado_autores <- purrr::map(subgrafos_componentes,function(x){ paste0(V(x)$label,collapse = ";") })
        names(listado_autores) <- 1:length(listado_autores) 
        
        autores_componentes_listado <- listado_autores%>% unlist() %>% as_tibble() %>% 
            tibble::rowid_to_column("componente_id") %>% 
            rename(autores=value) %>% 
            mutate(cantidad_autores=str_count(autores,";")+1) %>%
            select(componente_id,cantidad_autores,autores)

})
