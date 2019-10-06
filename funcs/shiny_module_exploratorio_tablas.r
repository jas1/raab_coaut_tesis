# modulo: analisis exploratorio datos de las tablas: 
# Autores
# - data cruda ( data tables )
# - composicion ( vis_dat )
# - resumen ( skimr )
# - preguntas ( ggplot + informe )
# 
# Articulos
# - data cruda
# - composicion
# - resumen
# - preguntas
# 
# Autores Articulos
# - data cruda
# - composicion
# - resumen
# - preguntas


# ui modulo autores ---------------------------------------------------------------
eda_autores_ui <- function(id,acotar_anios_secciones=FALSE, label = "Análisis Exploratorio de Datos de Autores") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        tabsetPanel(type = "tabs",
                    id=ns("eda_autores_tabs"),
                    tabPanel("Datos Crudos",div(
                        br(),
                        downloadButton (outputId = ns("eda_download_autores"),
                                        label = "Bajar datos autores"),
                        br(),
                        DT::dataTableOutput(ns('eda_autores_DT'))
                    )),
                    tabPanel("Composición",div(
                        plotOutput(ns('eda_autores_compo_plot'))
                    )),
                    tabPanel("Resumen",div(
                        verbatimTextOutput(ns('eda_autores_resumen'))
                    )),
                    tabPanel("Preguntas",div(
                        tags$h6("Cuantos autores hay en total de la base ?"),
                        br(),
                        uiOutput(ns('eda_autores_total_autores'))
                    ))
        )# fin tabset
    )# fin taglist
}
# server modulo autores ---------------------------------------------------------------
eda_autores_server <- function(input, output, session, stringsAsFactors,acotar_anios_secciones=FALSE) {
    
    autores_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        autores <-  dbGetQuery(raab_db_conn, paste0("select * from ", "autores" )) %>% as_tibble()
        # print(paste0("autores raw: ",nrow(autores)))
        
        # print(paste0("acotar value: ",acotar_anios_secciones))
        if (acotar_anios_secciones==TRUE) {
            autores_articulos_raw <- dbGetQuery(raab_db_conn, paste0("select * from ", "autores_articulos" )) %>% as_tibble()
            autores_articulos <- autores_articulos_raw %>% 
                filter(anio %in% cota_anio) %>% 
                filter(seccion %in% cota_seccion) 
            
            autores <- autores_articulos %>% select(autor) %>% unique() %>% left_join(autores)
            # print(paste0("autores filter: ",nrow(autores)))
            # articulos <- autores_articulos %>% select(articulo_id) %>% unique() %>% left_join(articulos_raw)
        }

        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        autores %>% select(id,autor)
    })
    
    output$eda_download_autores <- downloadHandler( 
        filename = paste('eda_autores-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(autores_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$eda_autores_DT <- DT::renderDataTable({
        dt_out <- DT::datatable(
            autores_reactive(),
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
    
    output$eda_autores_compo_plot <- renderPlot({
        withProgress(message = 'Armando visualización ...', value = 0, {
            plot_vis_compo <- visdat::vis_dat(autores_reactive())
            incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) #
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) #
            # plty_out
        })
        plot_vis_compo
        # plty_out
    })
    
    output$eda_autores_resumen <- renderPrint({ 
        skimr::skim_with(numeric = list(hist = NULL))
        skimr::skim(autores_reactive())
    })
    
    output$eda_autores_total_autores <- renderUI({ 
        p(nrow(autores_reactive()))
    })

}
# ui modulo articulos ---------------------------------------------------------------
eda_articulos_ui <- function(id, label = "Análisis Exploratorio de Datos de Artículos") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        tabsetPanel(type = "tabs",
                    id=ns("eda_tabs"),
                    tabPanel("Datos Crudos",div(
                        br(),
                        downloadButton (outputId = ns("eda_download"),
                                        label = "Bajar datos"),
                        br(),
                        DT::dataTableOutput(ns('eda_DT'))
                    )),
                    tabPanel("Composición",div(
                        plotOutput(ns('eda_compo_plot'))
                    )),
                    tabPanel("Resumen",div(
                        verbatimTextOutput(ns('eda_resumen'))
                    )),
                    tabPanel("Preguntas",div(
                        tags$h6("Cuantos artículos hay en total de la base ?"),
                        br(),
                        uiOutput(ns('eda_total')),
                        br(),
                        ### cuantos articulos por años hay ?
                        plotlyOutput(ns('eda_articulos_anio')),
                        br(),
                        ### cuantas secciones hay ? no tiene sentido si no incluyo las otras secciones.
                        plotlyOutput(ns('eda_articulos_anio_seccion')),
                        br(),
                        ### cuantas ediciones hay ?
                        plotlyOutput(ns('eda_articulos_anio_edicion'))
                    ))
        )# fin tabset
    )# fin taglist
}
# server modulo articulos ---------------------------------------------------------------
eda_articulos_server <- function(input, output, session, stringsAsFactors,acotar_anios_secciones=FALSE) {
    
    data_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        articulos <-  dbGetQuery(raab_db_conn, paste0("select * from ", "articulos" )) %>% as_tibble()
        
        # print(paste0("acotar value: ",acotar_anios_secciones))
        if (acotar_anios_secciones==TRUE) {
            autores_articulos_raw <- dbGetQuery(raab_db_conn, paste0("select * from ", "autores_articulos" )) %>% as_tibble()
            autores_articulos <- autores_articulos_raw %>% 
                filter(anio %in% cota_anio) %>% 
                filter(seccion %in% cota_seccion) 
            
            # autores <- autores_articulos %>% select(autor) %>% unique() %>% left_join(autores)
            # print(paste0("autores filter: ",nrow(autores)))
            articulos <- autores_articulos %>% select(articulo_id) %>% unique() %>% left_join(articulos)
        }
        
        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        articulos
    })
    
    output$eda_download <- downloadHandler( 
        filename = paste('eda_art-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(data_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$eda_DT <- DT::renderDataTable({
        dt_out <- DT::datatable(
            data_reactive(),
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
    
    output$eda_compo_plot <- renderPlot({
        withProgress(message = 'Armando visualización ...', value = 0, {
            plot_vis_compo <- visdat::vis_dat(data_reactive())
            incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) #
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) #
            # plty_out
        })
        plot_vis_compo
        # plty_out
    })
    
    output$eda_resumen <- renderPrint({ 
        skimr::skim_with(numeric = list(hist = NULL))
        skimr::skim(data_reactive())
    })
    
    output$eda_total <- renderUI({ 
        p(nrow(data_reactive()))
    })

# articulos anio ----------------------------------------------------------
    output$eda_articulos_anio <- renderPlotly({
       pl <-  data_reactive() %>% 
            mutate(anio = factor(anio)) %>% 
            group_by(anio) %>% 
            tally() %>% 
            ggplot(aes(anio,n,fill=anio)) +
            geom_col() +
            labs(title="Cantidad de artículos por año ",
                 subtitle="Artículos RAAB, obtenidos de la web",
                 x="",
                 y="# cantidad de articulos")+
            theme_light()+
            theme(legend.position = "none",
                  axis.text.x = element_text(angle=45))
       # pl
       ggplotly(pl)
    })

# Articulos anio seccion --------------------------------------------------
    output$eda_articulos_anio_seccion <- renderPlotly({
       pl <-  data_reactive() %>% 
            mutate(anio = factor(anio)) %>% 
            mutate(seccion = factor(seccion)) %>% 
            group_by(anio,seccion) %>% 
            tally() %>% 
            ggplot(aes(anio,n,fill=seccion)) +
            geom_col() +
            labs(title="Cantidad de artículos por año por sección",
                 subtitle="Artículos RAAB, obtenidos de la web",
                 x="",
                 y="# cantidad por año",
                 fill="sección")+
            theme_light()+
            theme(axis.text.x = element_text(angle=45))
       # pl
       ggplotly(pl)
    })

# articulos anio edicion --------------------------------------------------
    output$eda_articulos_anio_edicion <- renderPlotly({
        pl <- data_reactive() %>% 
            mutate(anio = factor(anio)) %>% 
            mutate(seccion = factor(seccion)) %>% 
            separate(codigo_edicion, into = c('ed_anio','ed_total','ed_issue'), sep = '_') %>% 
            group_by(anio,ed_issue) %>% 
            tally() %>% 
            ggplot(aes(anio,n,fill=ed_issue)) +
            geom_col() +
            labs(title="Cantidad de artículos por año por edición",
                 subtitle="Artículos RAAB, obtenidos de la web",
                 x="",
                 y="# cantidad por año",
                 fill="edición")+
            # coord_flip() +  # rotate the boxplot as there is not clear 
            theme_light()+
            theme(axis.text.x = element_text(angle=45))
        # pl
        ggplotly(pl)
    })
}
# ui modulo aut-art ---------------------------------------------------------------
eda_aut_art_ui <- function(id, label = "Análisis Exploratorio de Datos de Autores-Artículos") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        tabsetPanel(type = "tabs",
                    id=ns("eda_tabs"),
                    tabPanel("Datos Crudos",div(
                        br(),
                        downloadButton (outputId = ns("eda_download"),
                                        label = "Bajar datos"),
                        br(),
                        DT::dataTableOutput(ns('eda_DT'))
                    )),
                    tabPanel("Composición",div(
                        plotOutput(ns('eda_compo_plot'))
                    )),
                    tabPanel("Resumen",div(
                        verbatimTextOutput(ns('eda_resumen'))
                    )),
                    tabPanel("Preguntas",div(
                        tags$h6("Cuantas autorías hay en total de la base ?"),
                        p("autoría es cada vez que un autor participa de un paper."),
                        br(),
                        uiOutput(ns('eda_total')),
                        br(),
                        # UI: Que autores produjeron mas Artículos ? ------------------    
                        plotlyOutput(ns('eda_produccion_articulos')),
                        br(),
                        # UI: como es la distribucion de Artículos por autor ------------------    
                        plotlyOutput(ns('eda_articulos_por_autor')),
                        br(),
                        # UI:  Que Artículos tienen mas autores asociados ? ------------------    
                        plotlyOutput(ns('eda_articulos_con_mas_autores')),
                        br(),
                        # UI: cual es la frecuencia de autores por articulo ? ------------------    
                        plotlyOutput(ns('eda_autores_por_articulo')),
                        br(),
                        # UI: Cantidad de autores por articulo por periodo en bins ------------------    
                        plotlyOutput(ns('eda_autores_por_articulo_bins'))
                        
                    ))
        )# fin tabset
    )# fin taglist
}
# server modulo aut-art ---------------------------------------------------------------
eda_aut_art_server <- function(input, output, session, stringsAsFactors,acotar_anios_secciones=FALSE) {
    
    data_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        autores_articulos <-  dbGetQuery(raab_db_conn, paste0("select * from ", "autores_articulos" )) %>% as_tibble()
        
        # print(paste0("acotar value: ",acotar_anios_secciones))
        if (acotar_anios_secciones==TRUE) {
            autores_articulos <- autores_articulos %>% 
                filter(anio %in% cota_anio) %>% 
                filter(seccion %in% cota_seccion) 
        }
        
        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        autores_articulos
    })
    
    output$eda_download <- downloadHandler( 
        filename = paste('eda_art-', Sys.Date(), '.csv', sep=''), content = function(file) {
            write.csv(data_reactive(), file,row.names = FALSE,fileEncoding = "UTF-8")
        })
    
    output$eda_DT <- DT::renderDataTable({
        dt_out <- DT::datatable(
            data_reactive(),
            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
            escape = FALSE,
            rownames = FALSE,
            selection="none")
        dt_out
    })
    
    output$eda_compo_plot <- renderPlot({
        withProgress(message = 'Armando visualización ...', value = 0, {
            plot_vis_compo <- visdat::vis_dat(data_reactive())
            incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) #
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) #
            # plty_out
        })
        plot_vis_compo
        # plty_out
    })
    
    output$eda_resumen <- renderPrint({ 
        skimr::skim_with(numeric = list(hist = NULL))
        skimr::skim(data_reactive())
    })
    
    output$eda_total <- renderUI({ 
        p(nrow(data_reactive()))
    })
    
# SVR: Que autores produjeron mas Artículos ? eda_produccion_articulos  ------------------
    output$eda_produccion_articulos <- renderPlotly({
        pl <- data_reactive() %>% group_by(autor) %>% tally() %>% 
            arrange(desc(n)) %>% 
            mutate(autor=fct_reorder(autor,n)) %>% 
            head(20) %>% 
            ggplot(aes(autor,n,fill=autor)) + 
            geom_col() +
            labs(subtitle="Artículos RAAB, obtenidos de la web ",
                 title="Top 20 autores con mas artículos", # Cantidad Artículos por autor, top 20
                 x="",
                 y="# cantidad Artículos")+
            coord_flip() +  # rotate the boxplot as there is not clear 
            theme_light() +
            theme(legend.position = "none")
        # pl
        ggplotly(pl)
    })
# SVR: Como es la distribucion de Artículos por autor eda_articulos_por_autor  ------------------
    output$eda_articulos_por_autor <- renderPlotly({
        pl <- data_reactive() %>% group_by(autor) %>% tally() %>%
            ggplot(aes(x=n)) +
            geom_histogram(bins=25) +
            theme_light() +
            labs(subtitle="Artículos RAAB, obtenidos de la web ",
                 title="Frecuencia Artículos por autor",
                 x="cantidad Artículos",
                 y="autores con N Artículos")
        # pl
        ggplotly(pl)
    })
# SVR: Que Artículos tienen mas autores asociados ? eda_articulos_con_mas_autores ------------------
    output$eda_articulos_con_mas_autores <- renderPlotly({
       pl <-  data_reactive() %>% group_by(articulo_id) %>% tally() %>% 
            arrange(desc(n)) %>% 
            mutate(articulo_id=fct_reorder(articulo_id,n)) %>% 
            head(20) %>% 
            ggplot(aes(articulo_id,n,fill=articulo_id)) + 
            geom_col() +
            labs(subtitle="Artículos RAAB, obtenidos de la web ",
                 title="Top 20 artículos con mas autores.",# Cantidad de autores por artículo, top 20.
                 x="",
                 y="# cantidad autores")+
            coord_flip() +  # rotate the boxplot as there is not clear 
            theme_light() +
            theme(legend.position = "none")
       # pl
       ggplotly(pl)
    })
# SVR: Cual es la frecuencia de autores por articulo ? eda_autores_por_articulo ------------------

    output$eda_autores_por_articulo <- renderPlotly({
        pl <- data_reactive() %>% group_by(articulo_id) %>% tally() %>%
            ggplot(aes(x=n)) +
            geom_histogram(bins = 25)+
            theme_light() +
            labs(subtitle="Artículos RAAB, obtenidos de la web ",
                 title="Frecuencia de autores por artículo",
                 x="# autores",
                 y="Artículos con N autores")
        ggplotly(pl)
        #pl
    })

# SVR: Cantidad de autores por articulo por periodo en bins ------------------    
    data_autoria_reactive <- reactive({
        formas_autoria <- data_reactive() %>% 
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
    })
    
    output$eda_autores_por_articulo_bins <- renderPlotly({
        pl <- data_autoria_reactive() %>% 
            mutate(anio=as.factor(anio)) %>% 
            ggplot(aes(x=anio,y=cant_articulos,
                       fill=cant_autores_bins,
                       label=cant_autores))+ # para plotly
            geom_col()+
            theme_light()+
            scale_y_continuous(breaks = c(1:25))+
            theme(axis.text.x = element_text(angle = 45,hjust = 1))+
            labs(title='Cantidad de autores por artículo por año',
                 subtitle='solo trabajos originales',
                 x='',y='# articulos',fill='# autores' )
        ggplotly(pl)
    })
    
    
    
}