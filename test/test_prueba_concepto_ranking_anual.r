
# armar plot ranking anual

# definicion parametros
cant_n=5;
metrica_s='betweeness';

# tomar caracter dinamico
metrica_sym <-rlang::sym(metrica_s) 

# pryueba concepto top N
temporal_acumulado_nodos_grafos %>% 
    as_tibble() %>% 
    arrange(.data = .,desc(!!metrica_sym)) %>% 
    # nest(-periodo,-!!metrica_sym) %>% 
    head(cant_n)# %>%
    # unnest()


# armar para cada periodo segun parametros en top N
resu <- temporal_acumulado_nodos_grafos %>% 
nest(-periodo) %>% 
    mutate(top_n_periodo=purrr::map(data,
                                    cant=cant_n,
                                    metrica=metrica_s,
                                    .f=function(d,metrica,cant){
        metrica_sym <-rlang::sym(metrica_s)
        ranking <- seq(from=1,to = cant,by = 1)
        ret <- d %>% arrange(desc(!!metrica_sym)) %>% 
            # nest(-!!metrica_sym) %>% 
            head(cant) %>% 
            # unnest() %>% 
            mutate(posicion=ranking) %>% 
            mutate(valor_seleccion= !!metrica_sym)
        ret
    }))
metrica_sym <-rlang::sym(metrica_s)
# plot con ranking
plot_out <- resu %>% 
    # filter(periodo==1996) %>% 
    unnest(top_n_periodo) %>%
    mutate(autor=fct_reorder(autor,posicion,.desc = TRUE)) %>% 
    mutate(posicion=as.factor(posicion)) %>% 
    ggplot(aes(x=periodo,y=autor,color=posicion,group=posicion,label=valor_seleccion))+
    geom_point()+
    geom_line(show.legend = FALSE)+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 6))+
    labs(x="",y="")
    
plotly::ggplotly(plot_out)



# V2, con repeticion anual. -----------------------------------------------
# que pasa cuando empatan en un año quien queda ?


tmp_2 <- temporal_acumulado_nodos_grafos %>%
    as_tibble() %>% 
    nest(-periodo)

metrica_sym2 <- rlang::sym("betweeness") 
tmp_2[1,]$data %>% purrr::map(.,.f=function(x){
    
    x %>% nest(-!!metrica_sym2) %>% 
        arrange(desc(!!metrica_sym2)) %>% 
        head(5) %>% unnest()
})
    

resu_3 <- temporal_acumulado_nodos_grafos %>% 
    nest(-periodo) %>% 
    mutate(top_n_periodo=purrr::map(data,
                                    cant=cant_n,
                                    metrica=metrica_s,
                                    .f=function(d,metrica,cant){
                                        metrica_sym <-rlang::sym(metrica_s)

                                        nested_arranged <- d %>% 
                                            nest(-!!metrica_sym) %>% 
                                            arrange(desc(!!metrica_sym))
                                        
                                        ranking <- seq(from=1,to = nrow(nested_arranged),by = 1)
                                        
                                        ret <- nested_arranged %>% 
                                            mutate(posicion=ranking) %>% 
                                            mutate(valor_seleccion= !!metrica_sym) %>% 
                                            head(cant)
                                            
                                        # ret %>% glimpse()
                                            
                                        ret <- ret %>% unnest() #%>% 
                                            #filter(posicion %in% seq(from=1,to=cant,by=1) ) #%>% 
                                            # as_tibble()
                                            
                                        ret
                                    }))

# resu_3 %>% as_tibble()
# resu_3[1,]$top_n_periodo[[1]]

plot_out_3 <- resu_3 %>% 
    # filter(periodo==1996) %>% 
    unnest(top_n_periodo) %>%
    mutate(autor=fct_reorder(autor,posicion,.desc = TRUE)) %>% 
    mutate(posicion=as.factor(posicion)) %>% 
    ggplot(aes(x=periodo,y=autor,color=posicion,group=posicion,label=valor_seleccion))+
    geom_point()+
    geom_line(show.legend = FALSE)+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 6))+
    labs(x="",y="")

plotly::ggplotly(plot_out_3)

# estructura_red_nodos_vars_compara_simu_list == "degree" 
# estructura_red_nodos_vars_compara_simu_DF %>%  
#     filter( value == "degree"  ) %>% 
#     pull(name)
# names(estructura_red_nodos_vars_compara_simu_list == "degree" )
# 
# estructura_red_nodos_vars_compara_simu_list <- estructura_red_nodos_vars_compara_simu_DF %>% pull(value)
# names(estructura_red_nodos_vars_compara_simu_list) <- estructura_red_nodos_vars_compara_simu_DF %>% pull(ES)
