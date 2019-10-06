# helper_componentes_artiuclos.r

# selecciono el top 5 de componentes para analizar. 
# para la red total. 

library(here)
library(tidyverse)
library(xml2)
library(tidytext) #; install.packages('tidytext')
library(tm)#; install.packages('tm')
path_search <- here::here('tmp','compo_top_5')
obtener_top_5_path <- list.files(path_search,
                                 pattern = '.csv',
                                 full.names = FALSE) %>% 
    as_tibble() %>% mutate(full_path=paste0(path_search,'/',value)) %>% 
    rename(archivo=value)

top_5_compo_articulos <- obtener_top_5_path %>% 
    mutate(datos=purrr::map(.x=full_path,.f=readr::read_csv) ) %>% 
    unnest(datos) 
# tmp_url <- top_5_compo_articulos %>% pull(articulo)
# purrr::map(tmp_url,.f = xml2::read_html) %>% 
#     xml2::read_html() %>% rvest::html_nodes(xpath = '//a') %>% 
#     rvest::html_attr("href")
# 
#     mutate(titulo=xml2::read_html(articulo) %>% rvest::html_nodes(xpath = '//a') %>% rvest::html_text())
df_top_5_compo_articulos <- top_5_compo_articulos %>% 
    mutate(parsed_data=purrr::map(articulo,.f = xml2::read_html)) %>% 
    mutate(df_parsed=purrr::map(parsed_data,.f = function(x){
        url <- x %>% 
            rvest::html_nodes(xpath = '//a') %>% 
            rvest::html_attr("href")
        titulos <- x %>% 
            rvest::html_nodes(xpath = '//a') %>% 
            rvest::html_text()
        data.frame(url,titulos,stringsAsFactors = FALSE)
        
    })) %>% 
    unnest(df_parsed) %>% 
    select(-parsed_data,-articulo)
# 
#     mutate(parsed_data=purrr::map(articulo,.f = function(x){
#         document <- x %>% purrr::map(.,xml2::read_html) %>% unlist()
#         url <- document %>% 
#             rvest::html_nodes(xpath = '//a') %>% 
#             rvest::html_attr("href")
#         titulos <- document %>% 
#             rvest::html_nodes(xpath = '//a') %>% 
#             rvest::html_text()
#         data.frame(url,titulos)
#     })) 

# readr::read_csv(here())
# readr::write_tsv(df_top_5_compo_articulos,path = here::here('tmp','compo_top_5','top5_juntos.txt'))
# text mining breve -------------------------------------------------------

# en realidad lo que pasa aca es ... para determinar si comparten temas entre varios tengo que fijarme aunque sea los titulos o abstracts.
# aca solo tengo los titulos

# del componente seleccionado , hacer un palabras frecuentes. para ver las mas representativas


es_stopwords <- data.frame(stringsAsFactors = FALSE,palabra=tm::stopwords("spanish"))
en_stopwords <- stop_words %>%  rename(palabra=word)
unnested_titulos <- df_top_5_compo_articulos  %>%
    select(archivo,autores,url,titulos) %>% 
    unnest_tokens(palabra, titulos,drop = FALSE) %>% 
    anti_join(en_stopwords) %>% # porque hay en ingles
    anti_join(es_stopwords) # porque tambien hay en espaniol

# count(word, sort = TRUE)

plot_palabras_data <- unnested_titulos %>%
    count(archivo,palabra, sort = TRUE) %>% # por subgrafo y palabras
    # filter(n > 600) %>%
    mutate(palabra = fct_reorder(palabra, n)) 

plots_varios <- plot_palabras_data %>%
    nest(-archivo) %>% 
    mutate(plot_data=purrr::map2(.x=data,.y=archivo,.f=function(d,lab){
        plot_out <- d %>% 
            mutate(palabra = fct_reorder(palabra, n)) %>% 
            head(10) %>% 
            ggplot(aes(palabra, n,fill=lab)) +
            geom_col() +
            xlab(NULL) +
            coord_flip()+
            theme(legend.position = 'none')
        ggsave(filename = here::here('tmp','compo_top_5',paste0(lab,'.png')),
               plot = plot_out,width = 5,height = 7)
        plot_out
    }))
# plots_varios$plot_data %>% 

plotly::ggplotly(plots_varios$plot_data[[1]])
plotly::ggplotly(plots_varios$plot_data[[2]])
plotly::ggplotly(plots_varios$plot_data[[3]])
