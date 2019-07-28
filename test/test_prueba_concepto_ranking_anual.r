
# armar plot ranking anual

# definicion parametros
cant_n=5;
metrica_s='betweeness';

# tomar caracter dinamico
metrica_sym <-rlang::sym(metrica_s) 

# pryueba concepto top N
temporal_acumulado_nodos_grafos %>% 
    arrange(.data = .,desc(!!metrica_sym)) %>% 
    head(cant_n)

# armar para cada periodo segun parametros en top N
resu <- temporal_acumulado_nodos_grafos %>% 
nest(-periodo) %>% 
    mutate(top_n_periodo=purrr::map(data,
                                    cant=cant_n,
                                    metrica=metrica_s,
                                    .f=function(d,metrica,cant){
        metrica_sym <-rlang::sym(metrica_s)
        ranking <- seq(from=1,to = cant,by = 1)
        ret <- d %>% arrange(desc(!!metrica_sym)) %>% head(cant) %>% 
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
