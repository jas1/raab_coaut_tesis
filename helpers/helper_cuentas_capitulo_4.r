# IMPORT LIBS ----------------------------------------------------------------------------
library(here)#install.packages("here")
source(here:::here("funcs","imports.r"),encoding = "UTF-8")
source(here:::here("funcs","funciones.r"),encoding = "UTF-8")
source(here:::here("funcs","globals.r"),encoding = "UTF-8")

# cuentas_cantidades_graficos_eda -----------------------------------------

cuentas_cantidades_graficos_eda <- function(){
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
}


# cantidad de articulos con N autores por anio. ------------------------------

cantidad_articulos_N_autores <- function(){
    
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
    
}

# ranking autores. --------------------------------------------------------


# cap4: hipotesis componentes ---------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(purrr)
pacman::p_load(char=c("fuzzyjoin","stringdist")) 

path_compos <- here::here("tmp","20200328_cap_4_datos_componentes")
files_autores <- list.files(path=path_compos,full.names = TRUE,pattern = "autores")
files_compos <- list.files(path=path_compos,full.names = TRUE,pattern = "componentes")
files_estruct <- list.files(path=path_compos,full.names = TRUE,pattern = "estructura")

levantar_archivos<- function(lista_files,path_read){
    archivos<- lista_files %>% 
        tibble::enframe(value="archivo") %>% 
        mutate(archivo_sin_path=stringr::str_replace(archivo,path_read,"")) %>% 
        tidyr::separate(remove = FALSE,
                        col = "archivo_sin_path",
                        into = c('path_fecha','tipo','periodo'),sep="_" ) %>% 
        mutate(periodo=stringr::str_replace(periodo,".csv",""))
    
    df_full <- archivos %>% 
        mutate(datos=purrr::map(.x=archivo, .f=readr::read_csv)) %>% 
        tidyr::unnest(datos)
    df_full
}

#' cruza_compos; cruza llas listas de nombres de nodos de los componentes
#' 
#' explota las listas, se fija que todo T0, este incluido en T1.
#' de ser asi es mismo componente con crecimiento o absorbido por otro.
#'
#' @param aut_t1 
#' @param aut_t0 
#'
#' @return
#' @export
#'
#' @examples
mismo_crecido_o_absorbido <- function(aut_t1,aut_t0){
    
    t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
    t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()
    
    mismo_con_crecimiento_o_absorbido <- all(t0_list %in% t1_list) 
    
    mismo_con_crecimiento_o_absorbido
}

cruza_compos_absorcion <- function(aut_t1,aut_t0){
    t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
    t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()
    
    # si t1 tiene todos t0, entonces: mismo componente | fue absorbido.
    mismo_con_crecimiento_o_absorbido <- all(t0_list %in% t1_list) 
    # si tiene todos igual y los que faltan son nuevos: mismo compo > necesito el parametro que me diga si son nuevos o no.
    
    # si tiene todos igual, y los que faltan son mas que los que habia absorbido
    # - que pasa si se incorporan un monton de personas ? > si no son nuevos > absorbido por el que tiene mas.
    # - 
    
}

cantidades_absorcion <- function(aut_t1,aut_t0,nuevos){
    t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
    t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()
    t1_nuevos_list <- nuevos %>% stringr::str_split(pattern = ";") %>% unlist()

    cant_t0 <- length(t0_list)
    cant_t1 <- length(t1_list)
    cantidad_nuevos_en_t1 <- length(t1_nuevos_list)

    # agregados
    cond_agregados_en_t1 <- !(t1_list %in% t0_list)
    cant_agregados_t1 <- length(t1_list[cond_agregados_en_t1])
    
    # nuevos
    # cond_nuevos_en_t1 <- (nuevos %in% t1_list)
    
    
    
    # cantidad_nuevos_en_t1
    # de los viejos, lenght es mayor ? > absorbido
    # cond_agregados_existentes_en_t0 <- !(cond_nuevos_en_t1)
    cantidad_agregados_existentes_en_t0 <- cant_t1 - cantidad_nuevos_en_t1
    
    # de los viejos length es = ? > empate ?, merge mas que absorcion
    
    nuevos_t1 <- nuevos
    
    ret <- data.frame("cantidad_agregados_t1"=cant_t1-cant_t0,
                "cantidad_existentes_en_t0"=cantidad_agregados_existentes_en_t0,
                "cantidad_nuevos_vienen_t0"=cantidad_agregados_existentes_en_t0-cant_t0,
                "cantidad_nuevos_en_t1"=cantidad_nuevos_en_t1,
                "cantidad_en_t0"=cant_t0,
                "cantidad_en_t1"=cant_t1)

    ret 
}

fue_absorbido <- function(aut_t1,aut_t0,nuevos){
    t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
    t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()
    
    mismo_con_crecimiento_o_absorbido <- all(t0_list %in% t1_list) 
    
    mismo_con_crecimiento_o_absorbido
    
    absorbido <- FALSE
    if(mismo_con_crecimiento_o_absorbido){
        # agregados
        cond_agregados_en_t1 <- !(t1_list %in% t0_list)
        cant_agregados_t1 <- length(t1_list[cond_agregados_en_t1])
        
        # nuevos
        cond_nuevos_en_t1 <- (nuevos %in% t1_list)
        cantidad_nuevos_en_t1 <- length(t1_list[cond_nuevos_en_t1])
        
        # de los viejos, lenght es mayor ? > absorbido
        cond_agregados_existentes_en_t0 <- !(cond_nuevos_en_t1)
        cantidad_agregados_existentes_en_t0 <- length(t1_list[cond_agregados_existentes_en_t0])
        
        # de los viejos length es = ? > empate ?, merge mas que absorcion
        
        nuevos_t1 <- nuevos
        
        absorbido <- case_when(cantidad_agregados_existentes_en_t0 == length(t0_list)~NA, # empate , no es absorbido es mas un merge/union
                  cantidad_agregados_existentes_en_t0 > length(t0_list)~TRUE, # cant viejos mayor a este > entonces absorcion
                  cantidad_agregados_existentes_en_t0 < length(t0_list)~FALSE) # cant viejos menor a este > este se comio al otro.
        
        # Y que pasa si en vez de 1 solo componte el gigante se comio a 5 componentes ?
        # cuando compare voy a tener mas existentes, porque es la suma de todos esos 5 .
        # asi que todavia quedaria comparar, de donde venian esos viejos u_u
            
        
        #absorbido <- cantidad_agregados_existentes_en_t0 > length(t0_list)
        absorbido
    }
    absorbido     
}

df_full_autores <- levantar_archivos(files_autores,path_read = path_compos) %>% mutate(compo_key=paste0(periodo,"_",componente_id))
df_full_compos <- levantar_archivos(files_compos,path_read = path_compos) %>% mutate(compo_key=paste0(periodo,"_",componente_id))
df_full_estruct <- levantar_archivos(files_estruct,path_read = path_compos)

df_compos_autores <- df_full_compos %>% left_join(df_full_autores,by = "compo_key") 
df_full_estruct %>% select(periodo,cant_autores,componentes_largest,componentes_largest_porc)

# aca tengo que filtrar pro periodo 1 y periodo 2, y luego tirarle fuzzy join para que se matcheen.
# esto deberia dar bien , para saber que componete id matchea con que componente id. 
# porque voy a hacer join con la columna de autores.
# fuzzyjoin::distance_left_join()

df_compo_autores_filtrado <- df_compos_autores %>% 
    rename(tipo=tipo.x, periodo=periodo.x,componente_id=componente_id.x) %>% 
    select(tipo,periodo,componente_id,cant_autores,autores)


df_autores <- df_full_autores %>% select(periodo,componente_id ,compo_key,cantidad_autores,autores)

a_96_99_names <- df_autores %>% filter(periodo %in% c('1996','1996-1999')) %>% select(compo_key,periodo,autores)

a_96 <- df_autores %>% filter(periodo=='1996') %>% tidyr::separate_rows(autores,sep=";") %>% rename(autor=autores) %>% left_join(a_96_99_names, by=c("compo_key"="compo_key"))
a_99 <- df_autores %>% filter(periodo=='1996-1999') %>% tidyr::separate_rows(autores,sep=";",) %>% rename(autor=autores) %>% left_join(a_96_99_names, by=c("compo_key"="compo_key"))

comparar_99_96 <- a_99 %>% 
    left_join(a_96,by=c("autor"="autor")) %>% 
    select(compo_key.x,compo_key.y,autor,autor,cantidad_autores.x,cantidad_autores.y,componente_id.x,componente_id.y,autores.x,autores.y) %>% 
    mutate(flag_nuevo=is.na(compo_key.y)) %>% 
    mutate(flag_igual=if_else(flag_nuevo,FALSE,stringdist::stringdist(autores.x,autores.y,method="cosine")==0)) %>% 
    mutate(flag_crecido_absorbido=if_else(flag_nuevo|flag_igual,FALSE,unlist(purrr::map2(.x=autores.x,
                                                                          .y=autores.y,
                                                                          .f=mismo_crecido_o_absorbido))))

# 
# periodos_naems <- df_autores %>% filter(periodo %in% c("1996","1996-1999")) %>% 
#     select(compo_key,periodo,autores) %>% 
#     mutate(cant_autores=purrr::map(.x=autores,.f=function(a){
#         sp <- stringr::str_split(a,pattern = ";") %>% unlist()
#         length(sp)})) %>% 
#     tidyr::unnest(cant_autores)

obtener_numeros_autores_componentes <- function(df_autores,periodo_t0,periodo_t1){
    periodos_naems <- df_autores %>% filter(periodo %in% c(periodo_t0,periodo_t1)) %>% 
        select(compo_key,periodo,autores) %>% 
        mutate(cant_autores=purrr::map(.x=autores,.f=function(a){
            sp <- stringr::str_split(a,pattern = ";") %>% unlist()
            length(sp)})) %>% 
        tidyr::unnest(cant_autores)
                
    
    a_t0 <- df_autores %>% filter(periodo==periodo_t0) %>% tidyr::separate_rows(autores,sep=";") %>% rename(autor=autores) %>% left_join(periodos_naems, by=c("compo_key"="compo_key"))
    a_t1 <- df_autores %>% filter(periodo==periodo_t1) %>% tidyr::separate_rows(autores,sep=";",) %>% rename(autor=autores) %>% left_join(periodos_naems, by=c("compo_key"="compo_key"))
    
    comparar_t1_t0 <- a_t1 %>% 
        left_join(a_t0,by=c("autor"="autor")) %>% 
        select(compo_key.x,compo_key.y,autor,autor,cantidad_autores.x,cantidad_autores.y,componente_id.x,componente_id.y,autores.x,autores.y,cant_autores.x,cant_autores.y) %>% 
        mutate(flag_nuevo=is.na(compo_key.y)) %>% 
        mutate(flag_igual=if_else(flag_nuevo,FALSE,stringdist::stringdist(autores.x,autores.y,method="cosine")==0)) %>% 
        mutate(flag_crecido_absorbido=if_else(flag_nuevo|flag_igual,FALSE,unlist(purrr::map2(.x=autores.x,
                                                                                             .y=autores.y,
                                                                                             .f=mismo_crecido_o_absorbido)))) 
    autores_nuevos <- comparar_t1_t0 %>% filter(flag_nuevo) %>% group_by(compo_key.x) %>% 
        summarise(autores_nuevos=paste0(autor,collapse = ";"),cantidad_nuevos=n())
    
    # cant_nuevos <- nrow(autores_nuevos)
    
    comparar_t1_t0 <- comparar_t1_t0 %>% 
        left_join(autores_nuevos,by=c("compo_key.x"="compo_key.x")) %>% 
        mutate(cantidades=purrr::pmap(.l=list("aut_t1"=autores.x,
                                              "aut_t0"=autores.y,
                                              "nuevos"=autores_nuevos),
                                      .f=cantidades_absorcion)) %>% 
        tidyr::unnest(cantidades)
    
    ret <- list("comparar"=comparar_t1_t0,
         "autores_nuevos"=autores_nuevos)
    ret
}

resu <- obtener_numeros_autores_componentes(df_autores = df_autores,periodo_t0 = "1996",periodo_t1 = "1996-1999")
resu <- obtener_numeros_autores_componentes(df_autores = df_autores,periodo_t0 = "1996-2012",periodo_t1 = "1996-2013")
resu <- obtener_numeros_autores_componentes(df_autores = df_autores,periodo_t0 = "1996-2013",periodo_t1 = "1996-2014")

resu$comparar %>% 
    separate(col=compo_key.x,into=c("periodo","componente"),sep="_") %>% 
    group_by(periodo) %>% filter(cantidad_nuevos_en_t1==max(cantidad_nuevos_en_t1)) %>%
    select(cantidad_en_t0 , cantidad_en_t1,cantidad_agregados_t1,cantidad_existentes_en_t0,cantidad_nuevos_vienen_t0,cantidad_nuevos_en_t1) %>% distinct()


resu$comparar %>% select(autor,autores.x,autores.y,autores_nuevos)

df_autores %>% group_by(periodo) %>% summarise(componente_mayor=max(cantidad_autores))

df_autores %>% group_by(periodo) %>% filter(cantidad_autores==max(cantidad_autores))




comparar_99_96 %>% filter(flag_nuevo) %>% 
    tidyr::nest(data = c(compo_key.y, autor, cantidad_autores.x, cantidad_autores.y, 
                         componente_id.x, componente_id.y, autores.x, autores.y, flag_nuevo, 
                         flag_igual, flag_crecido_absorbido)) %>% 
    mutate(autores_nuevos = purrr::map(data,.f = function(d){d %>% pull(autor) %>% paste0(sep=";")})) %>% 
    tidyr::unnest(autores_nuevos)
    
nuevos_de_compos <- comparar_99_96 %>% filter(flag_nuevo) %>% count(compo_key.x) group_by(compo_key.x) %>% summarise(autores_nuevos=paste0(autor,sep=";"))
    mutate(flag_absorbido=if_else(!flag_crecido_absorbido,NA,unlist(purrr::pmap(.l=list("aut_t1"=autores.x,
                                                                                           "aut_t0"=autores.y,
                                                                                           "nuevos"= (. %>% filter(flag_nuevo) %>% group_by(compo_key.x) %>% summarise(autores_nuevos=paste0(autor,collapse = ";"))),
                                                                                   .f=fue_absorbido)))))



comparar_99_96 %>% filter(flag_absorbido) %>% View("absorbido")
comparar_99_96 %>% filter(flag_crecido_absorbido) %>% View("crecido_abs")
comparar_99_96 %>% filter(flag_absorbido)
comparar_99_96 %>% filter(flag_igual)
comparar_99_96 %>% filter(flag_nuevo)

# codigo a borrar ---------------------------------------------------------



# ejemplo crecido: Viviana Broglia: es nuevo., pero no lo se.

tmp_t1 <- "Graciela Caruso;María Virginia Albeza;Noemí Estela Acreche;Viviana Broglia"
tmp_t0 <- "Graciela Caruso;María Virginia Albeza;Noemí Estela Acreche"

t1_list <- tmp_t1 %>% str_split(pattern = ";") %>% unlist()
t0_list <- tmp_t0 %>% str_split(pattern = ";") %>% unlist()

cond_agregados_en_t1 <- !(t1_list %in% t0_list)
cant_agregados_t1 <- length(t1_list[cond_agregados_en_t1])

absorbido <- length(cant_agregados_t1) > length(cond_agregados_en_t1)
absorbido
t0_list %in% t1_list

tmp_func <- function(aut_t1,aut_t0){
    t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
    t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()
    
    mismo_con_crecimiento_o_absorbido <- all(t0_list %in% t1_list) 
    
    mismo_con_crecimiento_o_absorbido
    
    absorbido <- FALSE
    if(mismo_con_crecimiento_o_absorbido){
        cond_agregados_en_t1 <- !(t1_list %in% t0_list)
        cant_agregados_t1 <- length(t1_list[cond_agregados_en_t1])
        
        absorbido <- length(cant_agregados_t1) > length(cond_agregados_en_t1)
        absorbido
    }
    absorbido   
}

tmp_func(tmp_t1,tmp_t0)


t0_tmp_2 <- "Andrés G. Bolzán;Luis Manuel Guimarey"
t1_tmp_2 <- "Abraham I. Kohan;Alicia B. Orden;Alicia Liliana Caratini;Alicia M. Arrayago;Alicia Susana Goicochea;Ana Haedo;Ana L. Lustig;Andre Prous;Andrés G. Bolzán;Cristina Beatriz Dejean;Edith I. Milat;Eduardo R. Pons;Elvira Inés Baffi;Emile Crognier;Evelia Edith Oyhenart;Francisco Raúl Carnese;Francisco Rothhammer;Héctor Hugo Varela;Héctor Mario Pucciarelli;Jorge A. Rey;Jorge R. Zavatti;José Alberto Cocilovo;José M. Ranieri;Juan C. Agosti;Leandro L. Finger;Leonardo Oliva Ioanidis;Liliana Ingold;Luis Manuel Guimarey;Marcelo Soria;Margarita Ioanidis;María A. Rodrigo;María Antonia Luis;María C. Zanini;María Cristina Muñe;María Fernanda Torres;María Florencia Cesani;María Fucini;María T. Salaberry;Marina Laura Sardi;Miriam E. Villanueva;Nilda Zubieta;Paola V. Ponce;Raúl R. Rogríguez;Ricardo C. Niborski;Rolando González-José;Sergio Alejandro Avena;Silvia Cornero;Silvia Elizondo;Silvia G. Valdano;Silvia L. V. Dahinten;Vicente Dressino;Walter A. Neves"
nuevos <- 
fue_absorbido(t1_tmp_2,t0_tmp_2)
aut_t0 <- t0_tmp_2
aut_t1 <- t1_tmp_2
t1_list <- aut_t1 %>% stringr::str_split(pattern = ";") %>% unlist()
t0_list <- aut_t0 %>% stringr::str_split(pattern = ";") %>% unlist()

mismo_con_crecimiento_o_absorbido <- all(t0_list %in% t1_list) 

mismo_con_crecimiento_o_absorbido

absorbido <- FALSE
if(mismo_con_crecimiento_o_absorbido){
    cond_agregados_en_t1 <- !(t1_list %in% t0_list)
    cant_agregados_t1 <- length(t1_list[cond_agregados_en_t1])
    
    absorbido <- cant_agregados_t1 > length(t0_list)
    absorbido
}
absorbido
