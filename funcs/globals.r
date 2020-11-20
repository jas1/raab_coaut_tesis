#globals
log_prefix <- "globals.r"

# GLOBALS ---------------------------------------------------------------------
# theme_light()

# cota temporal segun tesis
cota_anio <-  c(1996:2016)

# cota seccion segun tesis
cota_seccion <- c("Trabajos Originales")

# db recreada
db_limpia <- paste0("db_raab_grafos.sqlite")

# esta variable es glboal dado q ue no puede cambiarse porque es cota de la tesis
# se analiza entre 1996 y 2016 y solo la seccion trabajos originales.
global_periodos_disponibles <- obtener_periodos_disponibles(db_limpia,min(cota_anio), max(cota_anio),cota_seccion)

# cacheadas para optimizar funcionamiento
# temporal basico: 
# temporal_basico_grafos: grafos para cada periodo sin acumular
# temporal_basico_estr_grafos: calculos de estructura de temporal_basico_grafos para cada periodo sin acumular
# temporal_basico_grafo_estr_as_stack: punto de vista stacked de temporal_basico_estr_grafos 
base::load(here::here("data","temporal_basico_data.Rdata"))
base::load(here::here("data","temporal_acumulado_data.Rdata"))
base::load(here::here("data","temporal_acumulado_dyn_data.Rdata"))

#

# DEFINICION LISTADO ALGORITMOS DISPOSICION RED ----------------------------------------
layouts_disponibles_df <- data.frame(stringsAsFactors = FALSE,
                                     igraph_name = c('layout_nicely',
                                                     'layout_in_circle',
                                                     'layout_as_tree',
                                                     'layout_on_grid',
                                                     'layout_with_lgl',
                                                     'layout_with_mds',
                                                     'layout_with_fr',
                                                     'layout_with_graphopt',
                                                     'layout_with_kk',
                                                     'layout_with_sugiyama'),
                                     ES = c('Automático',
                                            'Circular',
                                            'Árbol',
                                            'Grilla',
                                            'Gran disposición',
                                            'Escalamiento Multidim.',
                                            'Fruchterman Reingold',
                                            'Graphopt',
                                            'Kamada Kawai',
                                            'Sugiyama'),
                                     EN = c('Default',
                                            'Circle',
                                            'Tree',
                                            'Grid',
                                            '(LGL)Large Graph Layout',
                                            '(MDS)Multidim Scaling',
                                            'Fruchterman Reingold',
                                            'Graphopt',
                                            'Kamada Kawai',
                                            'Sugiyama')) %>%
    mutate(name = paste0(ES,"(",EN,")"),
           value = igraph_name) %>% as_tibble()


#layouts disponibles:
layouts_disponibles_list <- layouts_disponibles_df %>% pull(value)
names(layouts_disponibles_list) <- layouts_disponibles_df %>% pull(name)

# DEFINICION LISTADO ALGORITMOS BUSQUEDA COMUNIDADES ----------------------------------------
comunidades_algos_disponibles_df <- data.frame(stringsAsFactors = FALSE,
                                               igraph_name=c("cluster_edge_betweenness",
                                                             # "cluster_fast_greedy", # At fast_community.c:639 : fast-greedy community finding works only on graphs without multiple edges, Invalid value
                                                             "cluster_label_prop",
                                                             "cluster_leading_eigen",
                                                             "cluster_louvain",
                                                             # "cluster_optimal", # Graphs with up to fifty vertices should be fine, graphs with a couple of hundred vertices might be possible. (http://igraph.org/r/doc/cluster_optimal.html)
                                                             # "cluster_spinglass",  #  cluster_spinglass(grafo) : At clustertool.cpp:286 : Cannot work with unconnected graph, Invalid value
                                                             "cluster_walktrap",
                                                             'cluster_infomap'),
                                               EN=c("Edge Betweenness",
                                                    # "Greedy Optimization of Modularity",
                                                    "Propagating Labels",
                                                    "Leading Eigenvector",
                                                    "Multi-level Optimization of Modularity",
                                                    # "Optimal Community Structure",
                                                    # "Spin-glass Model",
                                                    "Short Random Walks",
                                                    'Infomap community finding'),
                                               ES=c("Intermediación Aristas",
                                                    # "Greedy Optimization of Modularity",
                                                    "Etiquetas Propagantes",
                                                    "Autovector Líder",
                                                    "Optimización de Modularidad Multi-nivel",
                                                    # "Optimal Community Structure",
                                                    # "Spin-glass Model",
                                                    "Caminatas Aleatorias Cortas",
                                                    "Búsqueda de Comunidaes Infomap")) %>% 
    mutate(name = paste0(ES,"(",EN,")"),
           value = igraph_name) %>% as_tibble()



comunidades_algos_disponibles_list <- comunidades_algos_disponibles_df %>% pull(value)
names(comunidades_algos_disponibles_list) <- comunidades_algos_disponibles_df %>% pull(name)

# DEFINICION VARS ESTRUCTURA DE RED ----------------------------------------

estructura_red_vars_compara_simu_DF <- data.frame(stringsAsFactors = FALSE,
                                                  var_name=c(
                                                      'cantidad_autores',# 'Cant. Aut.', = cantidad_autores, # no tiene sentido comparar por igual estructura
                                                      'cantidad_papers',#'Cant. Art.' = cantidad_papers, # no tiene sentido comparar por igual estructura
                                                      'cantidad_contribuciones',#'Cant. Contrib.' = cantidad_contribuciones, # Cant. Aut. por Art. sumados.
                                                      'cant_relaciones',#'Cant. Rel.' = cant_relaciones,#'cant_relaciones', # para ver cuantas relaciones se generaron
                                                      'cantidad_paper_por_autor_avg',#'Cant. Media de Art. por Autor' = cantidad_paper_por_autor_avg,
                                                      'cantidad_autores_por_paper_avg',#'Cant. Media de Aut. por Art.' = cantidad_autores_por_paper_avg,
                                                      'componentes_cantidad', # Cant. de subgrafos conectados
                                                      'componentes_largest_porc', # ratio en porcentaje de Aut. del componente mas grande / Aut. total del grafo
                                                      'componentes_largest', # mayor subgrafo conectado
                                                      'densidad_red',
                                                      'distancia_media',
                                                      'num_dist_lejanos',
                                                      'num_cliques',#'Cant. de Cliques' = num_cliques,
                                                      'num_largest_cliques',#'Cant. Aut. Clique mas grande' = num_largest_cliques,
                                                      'porc_largest_cliques', # ratio en porcentaje del clique mas grande / Aut. total del grafo
                                                      'num_transitivity',
                                                      'num_assort_degree'),
                                                  EN=c( '# Aut.' ,#= cantidad_autores, # no tiene sentido comparar por igual estructura
                                                        '# Art.' ,#= cantidad_papers, # no tiene sentido comparar por igual estructura
                                                        '# Contrib.',# = cantidad_contribuciones, # Cant. Aut. por Art. sumados.
                                                        '# Rel.', # para ver cuantas relaciones se generaron
                                                        '# Avg. Art. / Aut.',# 'Cant. Media de Art. por Autor' = cantidad_paper_por_autor_avg, 
                                                        '# Avg. Aut. / Art.',# 'Cant. Media de Aut. por Art.' = cantidad_autores_por_paper_avg,
                                                      '# Components', # Cant. de subgrafos conectados
                                                      'Larger Component (%)', # ratio en porcentaje de Aut. del componente mas grande / Aut. total del grafo
                                                      '# Aut. in Larger Component', # mayor subgrafo conectado
                                                      'Density',
                                                      'Avg. Distance',
                                                      'Diameter',
                                                      '# Cliques',# 'Cant. de Cliques' = num_cliques,
                                                      '# Aut. in larger Cliqué',# 'Cant. Aut. Clique mas grande' = num_largest_cliques,
                                                      'Larger Cliqué(%)', # ratio en porcentaje del clique mas grande / Aut. total del grafo
                                                      'Tran.',
                                                      'Asort.'),
                                                  ES=c('# Aut.',# = cantidad_autores, # no tiene sentido comparar por igual estructura
                                                       '# Art.',# = cantidad_papers, # no tiene sentido comparar por igual estructura
                                                       '# Contrib.',# = cantidad_contribuciones, # Cant. Aut. por Art. sumados.
                                                       '# Rel.', # para ver cuantas relaciones se generaron
                                                       '# Media Art. / Aut.',# 'Cant. Media de Art. por Autor' = cantidad_paper_por_autor_avg, 
                                                       '# Media Aut. / Art.',# 'Cant. Media de Aut. por Art.' = cantidad_autores_por_paper_avg,
                                                      '# de Componentes', # Cant. de subgrafos conectados
                                                      'Componente más Grande (%)', # ratio en porcentaje de Aut. del componente mas grande / Aut. total del grafo
                                                      '# Aut. en Componente mas Grande', # mayor subgrafo conectado
                                                      'Densidad',
                                                      'Distancia Media',
                                                      'Diametro',
                                                      '# Cliques',# 'Cant. de Cliques' = num_cliques,
                                                      '# Aut. en Cliqué más grande',# 'Cant. Aut. Clique mas grande' = num_largest_cliques,
                                                      'Cliqué más grande (%)', # ratio en porcentaje del clique mas grande / Aut. total del grafo
                                                      'Tran.',
                                                      'Asort.')) %>% 
    mutate(name = paste0(ES,"(",EN,")"),
           value = var_name) %>% as_tibble()

estructura_red_vars_compara_simu_list <- estructura_red_vars_compara_simu_DF %>% pull(value)
names(estructura_red_vars_compara_simu_list) <- estructura_red_vars_compara_simu_DF %>% pull(ES)

# DEFINICION VARS ESTRUCTURA DE RED - NODOS ----------------------------------------

estructura_red_nodos_vars_compara_simu_DF <- data.frame(stringsAsFactors = FALSE,
                                                        var_name=c('fuerza_colaboracion',
                                                                   'degree',
                                                                   'betweeness',
                                                                   'eigen_centrality',
                                                                   'closeness',
                                                                   'page_rank',
                                                                   'count_triangles'),
                                                        EN=c('Collaboration Strenght',
                                                             'Degree',
                                                             'Betweeness',
                                                             'Eigen Centrality',
                                                             'Closeness',
                                                             'Page Rank',
                                                             'Count Triangles'),
                                                        ES=c('Fuerza Colaboración',
                                                            'Grado',
                                                             'Intermediación',
                                                             'Autovector',
                                                             'Cercanía',
                                                             'Page Rank',
                                                             'Cant. Triángulos')
) %>% 
    mutate(name = paste0(ES,"(",EN,")"),
           value = var_name) %>% as_tibble()

estructura_red_nodos_vars_compara_simu_list <- estructura_red_nodos_vars_compara_simu_DF %>% pull(value)
names(estructura_red_nodos_vars_compara_simu_list) <- estructura_red_nodos_vars_compara_simu_DF %>% pull(ES)


# DEFINICION VARS MODELADO ------------------------------------------------


# DEFINICION VARS ESTRUCTURA DE RED - NODOS ----------------------------------------

modelado_df_vars <- data.frame(stringsAsFactors = FALSE,
                                                        var_name=c('transitivity',
                                                                   'diameter',
                                                                   'avg_path',
                                                                   'pow_law_alpha',
                                                                   'pow_law_ks_p'),
                                                        ES=c('Transitividad',
                                                             'Diámetro',
                                                             'Dist. Camino medio',
                                                             'Ley Pot. Alfa',
                                                             'Ley Pot. Signif. Test KS'),
                                                        EN=c('Transitivity',
                                                             'Diameter',
                                                             'Average path lenght',
                                                             'Power Law Alpha',
                                                             'Power Law KS test Significance')
) %>% 
    mutate(name = paste0(ES,"(",EN,")"),
           value = var_name) %>% as_tibble()

modelado_df_vars_list <- modelado_df_vars %>% pull(value)
names(modelado_df_vars_list) <- modelado_df_vars %>% pull(ES)



# GLOBALS: PARA SHINY ---------------------------------------------------------------
# side_lay_width <- 3
# main_lay_width <- 12 - side_lay_width
main_lay_width <- 12
network_filter_edges_threshold_max <- 3 # para el inicial, esto luego se va a pisar cuando se recalcule la red.
static_edge_width_multiplier <- 3

flog.info(paste0(log_prefix,"  - LOADED"))