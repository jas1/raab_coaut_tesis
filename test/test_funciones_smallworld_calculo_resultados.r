# toca revisar bug: 
# analisis estatico > periodo 1996 > ver estructura > ver detalle componentes > seleccionar componente de 5
# 
# 82: <reactive:palabras_analizadas_reactive> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#168]
# 66: palabras_analizadas_reactive
# 62: <reactive:palabras_analizadas_count_reactive> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#194]
# 46: palabras_analizadas_count_reactive
# 44: <observer> [/home/julio/Documents/git/raab-coaut-t/funcs/shiny_module_articulos_text_mining.r#86]



test_that("funciones.r: palabras_analizadas", {
    library("here")#install.packages("here")
    library("futile.logger")
    library("data.table")

    log_file_name <- here::here("logs",format(x=Sys.time(),format="%Y%m%d_%H%M%S_%Z_-_log.log"))
    logger_name <- 'raab_coaut_tesis'
    flog.appender(appender.file(log_file_name), name=logger_name)
    flog.threshold(TRACE,name=logger_name)
    flog.info("LOGGER INICIADO")
    #install.packages("futile.logger")
    source(here:::here("funcs","imports.r"),encoding = "UTF-8")
    source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
    
    
    tmp_smallworld_test_data <- here::here("test","test_data_small_world_calculo.Rdata")
    # variables_a_guardar <- c("valores_para_red","params","simulaciones_resumen_random","grafo_actual")
    # 
    # # grabo las variables de interes
    # base::save(list = variables_a_guardar, 
    #            file = tmp_smallworld_test_data)
    base::load(tmp_smallworld_test_data)
    ret_list <- smallworld_data_calculo(valores_para_red,params,simulaciones_resumen_random,grafo_actual)
    
    # logre replicar el error de la shiny app.
    #ret_list$smallworldness_grafo$smallworldness > 1
    
    # fix
    ret_list$smallworldness_grafo["smallworldness"] > 1
    # ret_list$smallworldness_grafo["smallworldness"] > 1
    
    testthat::expect_true( tibble::is_tibble(resultado_palabras)  )
    testthat::expect_true( nrow(resultado_palabras) > 0  )
    testthat::expect_true( unique(resultado_palabras$anio) == input_static_periodos  )

})

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

# https://mastering-shiny.org/scaling-testing.html?#testing-reactivity
# test_that("reactives and output updates", {
#     testServer(server, {
#         session$setInputs(x = 1, y = 1, z = 1)
#         expect_equal(xy(), 0)
#         expect_equal(yz(), 2)
#         expect_equal(output$out, "Result: 0")
#     })
# })


# testthat::test_that("datos tabla reactive",{
#     
#     semilla <- 12345
#     
#     # cota temporal segun tesis
#     cota_anio <-  c(1996:2016)
#     
#     # cota seccion segun tesis
#     cota_seccion <- c("Trabajos Originales")
#     
#     # db recreada
#     db_limpia <- paste0("db_raab_grafos.sqlite")
#     
#     # articulos todo
#     art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
#     
#     # grafo todo
#     gb_ok <- armado_grafo_bipartito(art_full)
#     
#     # algoritmo seleccionado
#     algoritmo_seleccionado <- 'cluster_edge_betweenness'
#     
#     comunidad_sel_detalles <- arma_comunidad(
#         semilla,
#         gb_ok,
#         algoritmo_seleccionado)
#     
#     
#     autores_db <- art_full
#     current_comunidades <- comunidad_sel_detalles
#     resultado <- listado_comunidades_autores (current_comunidades,autores_db)
#     
#     head(resultado)
#     
#     nodo_comunidad <- armar_df_membership(current_comunidades)
#     tmp_autores <- lista_vertices_autores(autores_db)
#     
#     # glimpse(nodo_comunidad)
#     # 
#     # glimpse(tmp_autores)
#     
#     nodo_comunidad_2 <- nodo_comunidad %>% 
#         left_join(autores_db,by=c("nombre"="aut_id")) %>%
#         select(nombre,member,autor) %>% 
#         group_by(nombre,member,autor) %>% 
#         tally() %>% 
#         select(-n)
#     
#     listado_comunidades <- nodo_comunidad_2 %>% 
#         arrange(member) %>% 
#         group_by(member) %>% 
#         summarize(n=n(),autores=paste(collapse='; ',autor)) %>% 
#         arrange(desc(n)) %>% 
#         rename(comunidad=member,cant_autores=n)
#     listado_comunidades
#     
#     
#     stopifnot(resultado$cant_autores == listado_comunidades$cant_autores)
#     
# })
