
# armar plot ranking anual

#levantar_datos 
library("tidyverse")
base::load(here::here("data","temporal_acumulado_data.Rdata"))

get_tabla_valores_metrica_seleccionada <- function(data_df,cant_n,metrica_s,fundadores_df){
    # fundadores_df=fundadores
    # data_df=temporal_acumulado_nodos_grafos
    # cant_n=5;
    # metrica_s='degree';
    metrica_sym <-rlang::sym(metrica_s) 
    # armar para cada periodo segun parametros en top N
    resu_final <- data_df %>% select(-es_fundador) %>% 
        nest(data = c(autor, fuerza_colaboracion, degree, betweeness, eigen_centrality, 
                      closeness, page_rank, count_triangles)) %>% 
        mutate(top_n_periodo=purrr::map(data,
                                        cant=cant_n,
                                        metrica=metrica_s,
                                        .f=function(d,metrica,cant){
                                            metrica_sym <-rlang::sym(metrica_s)
                                            ranking <- seq(from=1,to = cant,by = 1)
                                            
                                            ret <-  d %>% arrange(desc(!!metrica_sym)) %>% 
                                                nest(-!!metrica_sym) %>% # fix empates
                                                arrange(desc(!!metrica_sym)) %>% head(cant) %>% 
                                                mutate(posicion=ranking) %>% 
                                                mutate(valor_seleccion= !!metrica_sym) %>%
                                                unnest()%>% 
                                                select(autor,valor_seleccion,posicion)
                                            
                                            ret
                                        })) %>% 
        unnest(top_n_periodo) %>% 
        select(-data) %>% 
        left_join(fundadores_df,by="autor") %>% 
        group_by(periodo,valor_seleccion,posicion,es_fundador) %>% 
        summarise(autores_posicion=paste(autor,collapse = ";"),cant_autores=n()) %>% 
        nest(data = c(periodo, valor_seleccion, es_fundador, autores_posicion, cant_autores) ) %>%  #nest(-posicion)
        mutate(mantiene_data=purrr::map(data,.f=function(dat){
            resu = dat %>% 
                mutate(mantiene = purrr::map2(.x=autores_posicion,.y=lag(autores_posicion,1),.f=function(auts,anterior){
                    # que pasa si vienen varios, o si van varios. es un muchos v muchos.
                    autores_originales  <- str_split(auts,";",simplify = TRUE) 
                    autores_lag <-  str_split(anterior,";",simplify = TRUE) # ojo si es el 1ro viene vacio.
                    
                    #si alguno de los originales estaba en lag, entonces true.
                    resultado <- any(autores_originales %in% autores_lag )
                    resultado
                })) %>% unnest(mantiene)
            resu
            
        })) %>% 
        select(-data) %>% 
        unnest(mantiene_data) %>% select(-cant_autores,-autores_posicion) %>% 
        rename(valor=valor_seleccion) %>% 
        mutate(metrica=metrica_s) %>% 
        mutate(es_fundador=if_else(is.na(es_fundador),FALSE,es_fundador)) %>% 
        pivot_longer(cols = c("mantiene","es_fundador","valor"),names_to="variable",values_to="valores") %>% 
        #ungroup() %>% 
        select(periodo,metrica,posicion,variable,valores) %>% 
        pivot_wider(names_from = periodo,values_from=valores) %>% 
        arrange(posicion)
    resu_final
}


temporal_acumulado_nodos_grafos <- temporal_acumulado_nodos_grafos %>% 
    mutate(es_fundador=(periodo==1996)) %>%  as_tibble()

fundadores <- temporal_acumulado_nodos_grafos %>%  filter(es_fundador) %>% distinct(autor,es_fundador)

# definicion parametros

#metrica_s='betweeness';
#metrica_s='betweeness';

# tomar caracter dinamico
cant_n=5;

metrica_s='betweeness';
resumen_btw <- get_tabla_valores_metrica_seleccionada(temporal_acumulado_nodos_grafos,cant_n=5,metrica_s=metrica_s,fundadores_df=fundadores)
readr::write_tsv(x = resumen_btw,here::here("tmp","resumen_betweeness.csv"))


# aca hay algo que se repiten filas, sin embargo los valores repetidos son iguales. es probable que haya mas de 1 autor y por eso repite.
# toca validar para que quede limpito de una.
metrica_s='degree';
resumen_degree <- get_tabla_valores_metrica_seleccionada(temporal_acumulado_nodos_grafos,cant_n=5,metrica_s=metrica_s,fundadores_df=fundadores) %>% 
    unnest(cols = c(`1996`, `1999`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, 
                    `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, 
                    `2015`, `2016`))
readr::write_tsv(x = resumen_degree,here::here("tmp","resumen_degree.csv"))

metrica_s='fuerza_colaboracion';
resumen_fuerza_colab <- get_tabla_valores_metrica_seleccionada(temporal_acumulado_nodos_grafos,cant_n=5,metrica_s=metrica_s,fundadores_df=fundadores)
readr::write_tsv(x = resumen_fuerza_colab,here::here("tmp","resumen_fuerza_colaboracion.csv"))

