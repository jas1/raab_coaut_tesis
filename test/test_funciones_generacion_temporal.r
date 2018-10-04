# test de generacion de la parte temporal

# aca hay que reducir la cantidad de TEAS a lo que se muestra unicamente


testthat::test_that("generacion Network lists",{
    
    source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
    
    # cota temporal segun tesis
    cota_anio <-  c(1996:2016)
    
    # cota seccion segun tesis
    cota_seccion <- c("Trabajos Originales")
    
    # db recreada
    db_limpia <- paste0("db_raab_grafos.sqlite")
    
    # esta variable es glboal dado q ue no puede cambiarse porque es cota de la tesis
    # se analiza entre 1996 y 2016 y solo la seccion trabajos originales.
    global_periodos_disponibles <- obtener_periodos_disponibles(db_limpia,min(cota_anio), max(cota_anio),cota_seccion)
    
    static_edge_width_multiplier <- 3
    
    tmp_anios_acum_2 <- temporal_generar_grafos_acumulados(global_periodos_disponibles,
                                                           cota_seccion,
                                                           db_limpia,
                                                           static_edge_width_multiplier)
    
    
    nets <- generar_nets_from_igraph(tmp_anios_acum_2)
    
    
    # length(nets)
    # nets[1]

    # ahora a sacar atributos que no se usan en el network dynamic
    # https://cran.r-project.org/web/packages/network/vignettes/networkVignette.pdf
    
    # > delete.vertex.attribute(net,"boo")
    # Vertex attribute names: 
    #     anio cant_autores fuerza_colaboracion id id_old label size vertex.names 
    # 2018-10-03T02:40:42.545594+00:00 shinyapps[453923]: 	
    # Vertices:  anio cant_autores fuerza_colaboracion id size 
    
    # delete.vertex.attribute(net,"id_old")
    # delete.vertex.attribute(net,"vertex.names")
    
    
    # > delete.edge.attribute(net,"boo")
    # Edge attribute names: 
    #     autor1_label autor2_label autores color fuerza_colaboracion id weight width 
    # 2018-10-03T02:40:42.545620+00:00 shinyapps[453923]: 	
    # Edges:  autores color fuerza_colaboracion id weight width 
    
    # delete.edge.attribute(net,"autor1_label")
    # delete.edge.attribute(net,"autor2_label")

    # 2018-10-03T02:40:42.528902+00:00 shinyapps[453923]: Neither start or onsets specified, assuming start=0
    # 2018-10-03T02:40:42.528983+00:00 shinyapps[453923]: Onsets and termini not specified, assuming each network in network.list should have a discrete spell of length 1
    # 2018-10-03T02:40:42.545594+00:00 shinyapps[453923]: 	Vertices:  anio cant_autores fuerza_colaboracion id size 
    # 2018-10-03T02:40:42.545590+00:00 shinyapps[453923]: Dynamic attributes (TEAs) will be created for the following attributes of the networks on network.list:
    # 2018-10-03T02:40:42.558335+00:00 shinyapps[453923]: Argument base.net not specified, using first element of network.list instead
    # 2018-10-03T02:40:42.545620+00:00 shinyapps[453923]: 	Edges:  autores color fuerza_colaboracion id weight width 

})    