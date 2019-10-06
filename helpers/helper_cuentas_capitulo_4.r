# IMPORT LIBS ----------------------------------------------------------------------------
library(here)#install.packages("here")
source(here:::here("funcs","imports.r"),encoding = "UTF-8")
source(here:::here("funcs","funciones.r"),encoding = "UTF-8")
source(here:::here("funcs","globals.r"),encoding = "UTF-8")

# cuentas_cantidades_graficos_eda -----------------------------------------

db_name <- "db_raab_grafos.sqlite"
raab_db_conn <- get_db_connection(db_name)
acotar_selecciones <- FALSE
articulos <-  dbGetQuery(raab_db_conn, paste0("select * from ", "articulos" )) %>% as_tibble()

# print(paste0("acotar value: ",acotar_anios_secciones))

    autores_articulos <- dbGetQuery(raab_db_conn, paste0("select * from ", "autores_articulos" )) %>% as_tibble()
    if (acotar_selecciones) {
    autores_articulos <- autores_articulos %>% 
        filter(anio %in% cota_anio) %>% 
        filter(seccion %in% cota_seccion) 
    }
    # autores <- autores_articulos %>% select(autor) %>% unique() %>% left_join(autores)
    # print(paste0("autores filter: ",nrow(autores)))
    articulos <- autores_articulos %>% select(articulo_id) %>% unique() %>% left_join(articulos)
    articulos_aut <- autores_articulos %>% select(articulo_id) %>% left_join(articulos)
    
dbDisconnect(raab_db_conn)
# glimpse(autores)
articulos

articulos_totales_anio <- articulos %>% 
    count(anio) %>% 
    rename(total=n)


articulos_seccion_anio <- articulos %>% 
    count(anio,seccion) %>% 
    spread(key = seccion,value = n,fill = 0) 

articulos_seccion_anio %>% View()

articulos_edicion_anio <- articulos %>% 
    # mutate(anio = factor(anio)) %>% 
    mutate(seccion = factor(seccion)) %>% 
    separate(codigo_edicion, into = c('ed_anio','ed_total','ed_issue'), sep = '_') %>% 
    # group_by(anio,ed_issue) %>% 
    count(anio,ed_issue) %>% 
    spread(key = ed_issue,value = n,fill = 0) 
articulos_edicion_anio %>% View()
# ?eval_tidy
    View()
    
articulos_totales <- articulos_totales_anio %>% 
        left_join(articulos_seccion_anio,by = 'anio') %>% 
        left_join(articulos_edicion_anio,by = 'anio')

# readr::write_tsv(x = articulos_totales,
#                  path = here::here('tmp','articulos_totales.txt'))

# cantidad de articulos con N autores por anio. ------------------------------

formas_autoria <- articulos_aut %>% 
    count(articulo_id,anio,name = "cant_autores") %>% 
    count(anio,cant_autores ,name="cant_articulos") %>% 
    arrange(anio,desc(cant_articulos)) %>% 
    mutate(cant_autores_bins= case_when(
        cant_autores==1 ~ '1',
        (cant_autores >= 2 & cant_autores <= 4) ~ '2 a 4',
        (cant_autores >= 5 & cant_autores <= 7) ~ '5 a 7',
        cant_autores >= 8 ~ '8 o +'
    ))
formas_autoria

cant_autores_articulo_anio <- formas_autoria %>% 
    group_by(anio,cant_autores_bins) %>% 
    summarise(total_cant_articulos=sum(cant_articulos)) %>% 
    spread(key = cant_autores_bins,value = total_cant_articulos,fill = 0) 
# 
# readr::write_tsv(x = cant_autores_articulo_anio,
#                  path = here::here('tmp','cant_autores_articulo_anio.txt'))

# ranking autores. --------------------------------------------------------



