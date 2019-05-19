# prueba de c oncepto , small world y free scale
library(here)
testthat::test_that(
    "prueba concepto small world y free scale R",{
        
        source(here:::here("funcs","imports.r"),encoding = "UTF-8") # asi toma la ultima version
        source(here:::here("funcs","funciones.r"),encoding = "UTF-8") # asi toma la ultima version
        
        # setup        
        cota_seccion <- c("Trabajos Originales")
        db_limpia <- "db_raab_grafos.sqlite"
        cota_anio <-  c(1996:2016)
        # cota_anio <- c("1996")
        art_full <- articulos_todos_grafo(db_limpia,anios = cota_anio,secciones = cota_seccion)
        gb_ok <- armado_grafo_bipartito(art_full)
        g_aut <- extraccion_grafo_coautoria(gb_ok,art_full,width_multiplier = 3)
        
        # esto fue par ala escritura de tesis para ver los tributos y hacer ejemplos
        # igraph::vertex_attr_names(g_aut)
        # igraph::vertex_attr(g_aut,index = 10)
        # igraph::edge_attr_names(g_aut)
        # igraph::edge_attr(g_aut,index = "a0187--a0208" )
        # igraph::edge_attr(g_aut,name = "id" )
        # igraph::edge_attr(g_aut,)
        # E(g_aut)[1]$weight
        # E(g_aut)[1]$id
        # E(g_aut)[10]$fuerza_colaboracion
        # E(g_aut)$anios %>% as_tibble() %>% filter(str_length(value)>7)
        # E(g_aut)[1]$autores
        # E(g_aut)[1]$autor1_label
        # E(g_aut)[1]$autor2_label
        # E(g_aut)[1]$color
        # E(g_aut)[1]$width
        # V(g_aut)[str_detect(V(g_aut)$label,"Carne")]$size
        
        
        # small world
        
        # extraido de: SAND csardi 2016
        # Work on modeling of this type received a good deal of its impetus from the seminal
        # paper of Watts and Strogatz [146] and the ‘small-world’ network model introduced
        # therein. These authors were intrigued by the fact that many networks in the real
        # world display high levels of clustering, but small distances between most nodes.
        
        
        
        # 1996
        transitivity(g_aut) #  0.6483516 high clustering
        diameter(g_aut) # 3
        average.path.length(g_aut) # 1.472
        
        # para comparar smallworld
        set.seed(12345)
        g.ws <- sample_smallworld(1, 25, 5, 0.05)
        plot(g.ws, layout=layout.circle, vertex.label=NA)
        # clustering coeficient / transitivity ; va entre 0 y 1: 0.6666667, substantial amount
        transitivity(g.ws)
        # distance between vertices is non-trivial. diameter 10 . avg path = 5.45
        diameter(g.lat100)
        average.path.length(g.ws)

        # 1996 - 2016
        transitivity(g_aut) #  0.5429888 high clustering
        diameter(g_aut) # 11
        average.path.length(g_aut) # 3.410348
        
        valores_para_red <- calcular_metricas(g_aut)
        
        simulaciones_to_resumen_ejecucion
        simular_random(iter=100,vertex_count=10, edge_count=40,semilla=12345)
        
        # FIT POWERLAW ------------------------------------------------------------
        
        # LEY DE POTENCIA sobre los grados del grafo.
        
        # ley de potencia ( power fit law)
        resultado_fit_ley_potencia <- power.law.fit(igraph::degree(g_aut))
        # ¿El parámetro alfa de la función es mayor que 1? ¿Si? OK
        resultado_fit_ley_potencia$alpha > 1  # 2.515571
        # ¿El test de ajuste de Kolmogorov-Smirnov es no significativo? OK.
        # Numeric scalar, the p-value of the Kolmogorov-Smirnov test. 
        # Small p-values (less than 0.05) indicate that the test rejected the hypothesis 
        # that the original data could have been drawn from the fitted power-law distribution.
        resultado_fit_ley_potencia$KS.p < 0.05 #( tiene que dar false ) # 0.5081879 ( no significativo )
        
        
        
        # otro que se vee con el tema de avg path v
        
        
        # para comparar smallworld 1996-2016

        # mi objetivo para comparar modelos sería: 
        #   comparar contra N modelos random / small world / barabasi albert


# SIMULAR RANDOM ----------------------------------------------------------

        tmp_n <- 10 ; tmp_n*(tmp_n-1)/2
        simular_random(iter=100,vertex_count=10, edge_count=40,semilla=12345)
        simular_random(iter=100,vertex_count=100, edge_count=100,semilla=12345)
        simular_random(iter=100,vertex_count=1000, edge_count=100,semilla=12345)

        

# SIMULAR SMALLWORD -------------------------------------------------------

        simular_small_world(iter = 100,vertex_count = 50,vecindad = 5,prob_reescritura = 0.05,semilla = 12345)
        simular_small_world(iter = 100,vertex_count = 100,vecindad = 5,prob_reescritura = 0.05,semilla = 12345)
        simular_small_world(iter = 100,vertex_count = 1000,vecindad = 5,prob_reescritura = 0.05,semilla = 12345)

# SIMULAR SCALE FREE ------------------------------------------------------

        simular_scale_free(iter =100 ,vertex_count = 100,poder_pref_attach = 1,m_edges_per_step = 8,semilla = 12345)
        simular_scale_free(iter =100 ,vertex_count = 100,poder_pref_attach = 2,m_edges_per_step = 8,semilla = 12345)
        simular_scale_free(iter =100 ,vertex_count = 100,poder_pref_attach = 3,m_edges_per_step = 8,semilla = 12345)
        
        simular_scale_free(iter =100 ,vertex_count = 100,poder_pref_attach = 1,m_edges_per_step = 5,semilla = 12345)
        simular_scale_free(iter =100 ,vertex_count = 100,poder_pref_attach = 2,m_edges_per_step = 5,semilla = 12345)
        simular_scale_free(iter =1000 ,vertex_count = 100,poder_pref_attach = 3,m_edges_per_step = 5,semilla = 12345)
        
        
        # TENGO 
        # - LOS VALORES DE REFERENCIA
        # - la simulacion random
        # - la simulacion SW
        # - la simulacion scale freen
        
        # como validar smallworld
        
#        https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network
        

        
        valores_para_red <- calcular_metricas(g_aut)
        param_list <- data.frame(param_iter=1000,
                                 param_vertex_count=vcount(g_aut),
                                 param_edge_count=ecount(g_aut),
                                 param_semilla=12345)
        simulacion_random <- simular_random(iter=param_list$param_iter,
                                            vertex_count=param_list$param_vertex_count,
                                            edge_count=param_list$param_edge_count,
                                            semilla=param_list$param_semilla)
        
        simulaciones_resumen <- simulaciones_to_resumen_ejecucion(simulacion_random,param_list)
        
        validar_small_world <- function(valores_para_red,
                                        param_list,
                                        simulacion_random,
                                        simulaciones_resumen){

            #λ:=L/Lr. and γ:=C/Cr.
            delta_net <- valores_para_red$avg_path / simulaciones_resumen$mean_avg_path_length
            gamma_net <- valores_para_red$transitivity / simulaciones_resumen$mean_transitivity
            
            es_small_world <- round(delta_net) == 1  &  gamma_net > 1            
            
            qgraph::smallworldness(g_aut)
        }
        

        # como validar scale free
        
        
        
        
    })