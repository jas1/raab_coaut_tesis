# ejemplo replicable del problema de smallworldness

library(igraph)
library(igraphdata)
# library(sand)
library(qgraph)
library(dplyr)

# dado un grafo no conectado , me caga el " smallworldness", 
# porque por default le pone conectedness FALSE. lo que hace que pondere muy pesadamente los desconectados.

# paso 1: levantar grafo no conectado

data(package="igraphdata")

# carga data
data(kite)
# verificar conectado
igraph::is_connected(kite)
# ver data
plot(kite)

# ver los edges
E(kite)
# seleccinar el que voy a borrar
edge_delete <- E(kite)[17]

# borrar el deseado
kite_unconected <- kite %>% delete_edges(edge_delete)
# verificar desconectado
igraph::is_connected(kite_unconected)
# ver data
plot(kite_unconected)

# paso 2: verificar smallworldness


# paso 2-a: verificar smallworldness grafo conectado
smallworldness_res_kite <- qgraph::smallworldness(kite)
smallworldness_res_kite_df <- data.frame(nombres=names(smallworldness_res_kite),
                                         valores=smallworldness_res_kite, 
                                         row.names = 1:length(smallworldness_res_kite))

igraph::transitivity(kite)
igraph::mean_distance(kite) # average path lenght
igraph::mean_distance(kite,directed = FALSE,unconnected = FALSE)


# paso 2-b: verificar smallworldness grafo no conectado
smallworldness_res_unc_kite <- qgraph::smallworldness(kite_unconected)
smallworldness_res_unc_kite_df <- data.frame(nombres=names(smallworldness_res_unc_kite),
                                         valores=smallworldness_res_unc_kite, 
                                         row.names = 1:length(smallworldness_res_unc_kite))

igraph::transitivity(kite_unconected)
igraph::mean_distance(kite_unconected) # average path lenght
igraph::mean_distance(kite_unconected,directed = FALSE,unconnected = FALSE)

# paso 3: observar diferencias