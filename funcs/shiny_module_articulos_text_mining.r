
textmining_ui <- function(id, # escencial para poder armar el componente
                         label = "Análisis de articulos") {
    
    ns <- NS(id) # namespace para como se va a llamar 
    

tabsetPanel(type = "tabs",
            id=ns("text_mining_tabs"),

# ARTICULOS ANALIZADOS ----------------------------------------------------

                tabPanel("Articulos Analizados",div(
                    br(),
                    downloadButton (outputId = ns("articulos_analizados_download"),
                                    label = "Bajar articulos"),
                    br(),
                    br(),
                    DT::dataTableOutput(ns('dt_articulos_analizados'))
                )),

# STOPWORDS ES ------------------------------------------------------------

            tabPanel("Stopwords ES",div(
                br(),
                downloadButton (outputId = ns("stopwords_es_download"),
                                label = "Bajar Stopwords ES"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_stopwords_es'))
            )),

# STOPWORDS EN ------------------------------------------------------------

            tabPanel("Stopwords EN",div(
                br(),
                downloadButton (outputId = ns("stopwords_en_download"),
                                label = "Bajar Stopwords EN"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_stopwords_en'))
            )),

# palabras_analizadas -----------------------------------------------------

            tabPanel("Palabras analizadas",div(
                br(),
                downloadButton (outputId = ns("palabras_analizadas_download"),
                                label = "Bajar palabras analizadas"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_palabras_analizadas'))
            )),

# palabras_analizadas_count --------------------------------------

            tabPanel("Palabras analizadas cantidad",div(
                br(),
                downloadButton (outputId = ns("palabras_analizadas_count_download"),
                                label = "Bajar palabras analizadas Cantidad"),
                br(),
                br(),
                DT::dataTableOutput(ns('dt_palabras_analizadas_count'))
            )),

# plot_freq_palabras --------------------------------------
            tabPanel("Gráfico palabras analizadas cantidad",div(
                br(),
                plotlyOutput(ns('plot_freq_palabras'))
            ))

            ) # fin taglist del componente
}

textmining_server <- function(input, output, session, # parametros de shiny
                             current_articulos, # parametros del componente: listado de articulos
                             current_datos_seleccion # datos de seleccion (componte id, cant autores, autores)
) {

# datos -------------------------------------------------------------------

    palabras_stopwords_es_reactive <- reactive({
        es_stopwords <- data.frame(stringsAsFactors = FALSE,
                                   palabra=tm::stopwords("spanish"))
        es_stopwords
    })
    
    palabras_stopwords_en_reactive <- reactive({
        en_stopwords <- stop_words %>% 
            rename(palabra=word)
        en_stopwords
    })
    
    articulos_analizados_reactive <- reactive({
        current_articulos 
    })

# DTs ---------------------------------------------------------------------

    
    output$dt_stopwords_es <- DT::renderDataTable({ 
        dt_data <- palabras_stopwords_es_reactive()
        
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
        
    })
    output$dt_stopwords_en <- DT::renderDataTable({ 
        dt_data <- palabras_stopwords_en_reactive()
        
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
        
    })
    output$dt_articulos_analizados <- DT::renderDataTable({ 
        dt_data <- articulos_analizados_reactive() %>% select(-titulos,-url)
        
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })

# downloads ---------------------------------------------------------------

    output$stopwords_es_download <- downloadHandler( 
        filename = paste('text_stopwords_es-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(palabras_stopwords_es_reactive(), file,row.names = FALSE)
        })
    output$stopwords_en_download <- downloadHandler( 
        filename = paste('text_stopwords_en-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(palabras_stopwords_en_reactive(), file,row.names = FALSE)
        })
    output$articulos_analizados_download <- downloadHandler( 
        filename = paste('text_articulos_analizados-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(palabras_stopwords_en_reactive(), file,row.names = FALSE)
        })

# texto - palabras_analizadas_reactive --------------------------------------------------
    palabras_analizadas_reactive <- reactive({
        unnested_titulos <- articulos_analizados_reactive()  %>%
            select(autores,url,titulos) %>% 
            unnest_tokens(palabra, titulos,drop = FALSE) %>% 
            anti_join(en_stopwords) %>% # porque hay en ingles
            anti_join(es_stopwords)
        
        unnested_titulos
    })
    output$palabras_analizadas_download <- downloadHandler( 
        filename = paste('text_palabras_analizadas-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(palabras_analizadas_reactive(), file,row.names = FALSE)
        })
    output$dt_palabras_analizadas <- DT::renderDataTable({ 
        dt_data <- palabras_analizadas_reactive() %>% select(-titulos,-url)
        
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })

# texto - palabras_analizadas_count_reactive --------------------------------------------------
    palabras_analizadas_count_reactive <- reactive({
        count_palabras <- palabras_analizadas_reactive() %>%
            ungroup() %>%
            count(palabra, sort = TRUE) %>% # por subgrafo y palabras
            # filter(n > 600) %>%
            mutate(palabra = fct_reorder(palabra, n))
        count_palabras
    })
    output$palabras_analizadas_count_download <- downloadHandler( 
        filename = paste('text_palabras_analizadas_count-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(palabras_analizadas_count_reactive(), file,row.names = FALSE)
        })
    output$dt_palabras_analizadas_count <- DT::renderDataTable({ 

        dt_data <- palabras_analizadas_count_reactive() #%>% select(-articulo)
        
        dt_out <- DT::datatable(
            dt_data,
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
    
    # head(10) %>% 
# texto - plot_freq_palabras_reactive --------------------------------------------------
    plot_freq_palabras_reactive <- reactive({
        plot_out <- palabras_analizadas_count_reactive() %>% 
            # mutate(palabra = fct_reorder(palabra, n)) %>% 
            # head(10) %>% 
            ggplot(aes(palabra, n,fill='componente')) + # el fill es solo para que le ponga color
            geom_col() +
            xlab(NULL) +
            coord_flip()+
            theme_light()+
            theme(legend.position = 'none') 
        # ggsave(filename = here::here('tmp','compo_top_5',paste0(lab,'.png')),
        #        plot = plot_out,width = 5,height = 7)
        plot_out
    })
    # est_nodos_densidad 
    output$plot_freq_palabras <- renderPlotly({
        pl <- plot_freq_palabras_reactive()
        # pl
        ggplotly(pl)
    })

}