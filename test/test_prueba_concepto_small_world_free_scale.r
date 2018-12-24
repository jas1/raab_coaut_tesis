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
        # FALTA 
        # - comparacion dentro de aplicacion
            # - tal vez un shiny module para la parte de simulaciones
            # - un tab por cada tipo de simulacion
            # - arriba los valores de referencia de " current network "
            # - que le puedas configurar los parametros a cada simulacion
            # - que los valores default sean los de la red
            # - recordatorio de porque esos valores para que el usuario entienda
            # - y yo me acuerde ! xD 

    })