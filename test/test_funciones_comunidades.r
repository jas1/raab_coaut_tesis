# test_funciones_comunidades
# para probar: de comunidades

# FUNCIONES
# arma_comunidad <- function(semilla_seed,tmp_grafo,comunidades_sel_algo)
# armar_df_membership <- function(comunidad_sel_detalles)
# armar_df_comunidades <- function(cantidad_coms,comunidad_sel_detalles, color_palette='Set1',color_default='#D3D3D3')
# para buscar la estructura de todas las comunidades
# estructura_comunidades_df <- function(current_grafo,current_base_autores,current_comunidad)
# listado_comunidades_autores <- function(current_comunidad,autores_db)


# COSAS WEB


# armar comunidades -------------------------------------------------------

# community_reactive <- reactive({
#     req(input$input_static_periodos)
#     req(input$comunidades_sel_algo)
#     comunidad_sel_detalles <- arma_comunidad(input$semilla_seed,static_network_grafo_reactive(),input$comunidades_sel_algo)
# })

# comunidades - resultado info ----------------------------------------------------------
# output$comunidades_result <- renderUI ({
#     
#     current_comunidad <- community_reactive()
#     
#     mensaje_algo <- paste0('Para la búsqueda de comunidades se usó el Algoritmo: ', algorithm(current_comunidad))
#     mensaje_cantidad <- paste0('<p>Se generaron: ', length(current_comunidad), ' comunidades.</p>')
#     
#     # infomap muestra diferente.
#     if(str_detect(algorithm(current_comunidad),'Info')){
#         mensaje_cantidad <- paste0('<p>Se generaron: ', code_len(current_comunidad), ' comunidades.</p>')
#     }
#     # recordar hay un comparador de comunidades, por si quiero comparar resultados de algoritmos.
#     # se puede comparar quienes estan en las comunidaes.
#     mensaje_final <- paste0(mensaje_algo,mensaje_cantidad)
#     
#     HTML(mensaje_final)
# })

# visualizar el grafo con comunidades -------------------------------------
# observe({
#     req(input$input_static_periodos)
#     req(input$comunidades_sel_algo)
#     req(input$comunidades_n_view)
#     
#     comunidad_sel_detalles <- community_reactive()
#     cantidad_coms <- input$comunidades_n_view
#     update_static_network_comunities_df <- armar_df_comunidades(cantidad_coms,comunidad_sel_detalles)
#     
#     visNetworkProxy("output_static_network") %>%
#         visUpdateNodes(nodes=update_static_network_comunities_df)
# })

# comunidades - resultado tabla ---------------------------------------------------------
# listado_comunidades_estructura_reactive <- reactive({
#     current_comunidad <- community_reactive() 
#     current_grafo <- static_network_grafo_reactive()  
#     current_base_autores <- static_data_base()
#     
#     resul_acum <- estructura_comunidades_df(current_grafo,current_base_autores,current_comunidad)
#     
#     resul_acum
# })
# 
# output$comunidades_result_metricas_listado <- DT::renderDataTable({ 
#     str_comunidades <- listado_comunidades_estructura_reactive()
#     resultado_dt <- estructura_grafo_para_DT(str_comunidades)
#     resultado_dt
# })
# 
# 
# # comunidades - tabla - download --------------------------------------------------------
# 
# output$download_com_metricas <- downloadHandler( 
#     filename = paste('com_metricas-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
#         write.csv(listado_comunidades_estructura_reactive(), file,row.names = FALSE)
#     })  
# 
# # comunidades - autores - tabla ---------------------------------------------------------
# 
# listado_comunidades_autores_reactive <- reactive({
#     autores_db <- static_data_base()
#     current_comunidades <- community_reactive()
#     result <- listado_comunidades_autores (current_comunidades,autores_db)
#     result
# })
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


# 
# output$cantidad_comunidades_metricas <- DT::renderDataTable({ 
#     DT::datatable(
#         options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
#         listado_comunidades_autores_reactive(),
#         escape = FALSE,
#         selection = 'single')
# })
# 
# # comunidades - autores - download ---------------------------------------------------------
# 
# output$download_com_metricas_aut <- downloadHandler( 
#     filename = paste('com_metricas_aut-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
#         write.csv(listado_comunidades_autores_reactive(), file,row.names = FALSE)
#     })  
# 
# # comunidades - detalle - info ----------------------------------------------------
# 
# output$detalle_comunidad_seleccionada <- renderUI({
#     
#     current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
#     mensaje <- paste0('Se selecciono la comunidad: ' ,current_com_selected[1])
#     mensaje2 <- paste0('Con una cantidad de ' ,current_com_selected[2], ' integrantes. ')
#     mensaje3 <- paste0('Siendo: ' ,current_com_selected[3] )
#     mensaje_final <- paste('<p>',mensaje,mensaje2,'</p><p>',mensaje3,'</p>')
#     HTML(mensaje_final)
# })
# 
# detalle_comunidad_seleccionada_metricas_subgrafo_reactive <- reactive({
#     req(input$cantidad_comunidades_metricas_rows_selected)
#     current_comunidades <- community_reactive()
#     
#     current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
#     
#     current_id <- current_com_selected[,1] %>% as.character()
#     
#     current_group <- igraph::groups(current_comunidades)[[current_id]]
#     
#     tmp_grafo <- static_network_grafo_reactive()
#     
#     current_subgraph <- induced_subgraph(tmp_grafo,current_group) 
#     
#     # obtener base dado un grafo en un periodo particular, filtrado por los autores participantes
#     current_autores <- V(current_subgraph)$label
#     base_autores_tmp <- static_data_base() %>% filter(autor %in% current_autores)
#     
#     estructura_grafo_df <- calcular_estructura_grafo(current_subgraph,base_autores_tmp)
#     
#     estructura_grafo_df
# })
# 
# output$detalle_comunidad_seleccionada_metricas_subgrafo_table <- DT::renderDataTable({
#     temp_df <- detalle_comunidad_seleccionada_metricas_subgrafo_reactive() 
#     resultado_dt <- estructura_grafo_para_DT(temp_df)
#     resultado_dt
# })
# 
# # comunidades - detalle - tabla -------------------------------------------
# 
# detalle_comunidad_seleccionada_articulos_reactive <- reactive({
#     
#     current_com_selected <- listado_comunidades_autores_reactive()[input$cantidad_comunidades_metricas_rows_selected,]
#     autores_comunidad <- str_split(current_com_selected[3],pattern = '; ') %>% unlist()
#     autores_db <- static_data_base()
#     input_periodos <-  input$input_static_periodos
#     
#     filtro_coautores <- autores_db %>% 
#         filter(anio %in% input_periodos) %>% 
#         filter(autor %in% autores_comunidad) %>%
#         select(autores,anio,titulo,url,cant_autores,fuerza_colaboracion) %>%
#         mutate(articulo=paste0("<p><a target='_blank' href='",url,"'>",titulo,"</a></p>")) %>% 
#         group_by(autores,anio,articulo,cant_autores,fuerza_colaboracion) %>% tally() %>% 
#         select(autores,anio,articulo,cant_autores,fuerza_colaboracion)
#     
#     filtro_coautores
# })
# 
# output$detalle_comunidad_seleccionada_articulos <- DT::renderDataTable({
#     # temp_df <- detalle_comunidad_seleccionada_articulos_reactive()
#     # DT::datatable(
#     #     options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
#     #     temp_df,
#     #     escape = FALSE,
#     #     selection = 'none')
#     
#     
#     tmp <- detalle_comunidad_seleccionada_articulos_reactive() %>% 
#         rename(Autores=autores,
#                Periodo=anio,
#                'Artículo'=articulo,
#                '#Autores'=cant_autores,
#                'Fuerza Colaboración'=fuerza_colaboracion)
#     
#     dt_result <- DT::datatable(options = list(language = list(
#         url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
#         tmp,
#         escape = FALSE,
#         rownames = FALSE,
#         selection = 'none') %>% 
#         formatRound('Fuerza Colaboración',2) 
#     
#     dt_result
# })
# 
# # comunidades - detalle - download ----------------------------------------
# output$download_com_sel_arts <- downloadHandler( 
#     filename = paste('com_sel_arts-', Sys.Date(),'-',input$comunidades_sel_algo, '.csv', sep=''), content = function(file) {
#         com_reactive_to_csv <- detalle_comunidad_seleccionada_articulos_reactive() %>%
#             mutate(titulos=str_replace_all(articulo,"<p><a target='_blank' href='",';')) %>%
#             mutate(titulos=str_replace_all(titulos,"'>",'\\|')) %>%
#             mutate(titulos=str_replace_all(titulos,"</a></p>",'')) %>%
#             separate_rows(titulos,sep = ";") %>%
#             filter(titulos != "") %>% separate(titulos,c("URL","Título"),sep="\\|") %>%
#             rename(Autores=autores,
#                    Periodo=anio,
#                    'Artículo'=articulo,
#                    'Cantidad_Autores'=cant_autores,
#                    'Fuerza_Colaboración'=fuerza_colaboracion) %>% data.frame(stringsAsFactors = FALSE) %>% 
#             select(Periodo,Autores,"Título",'Cantidad_Autores','Fuerza_Colaboración',URL)
#         
#         write.csv(com_reactive_to_csv, file,row.names = FALSE)
#     })