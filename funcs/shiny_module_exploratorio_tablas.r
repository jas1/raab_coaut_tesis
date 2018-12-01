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
eda_autores_ui <- function(id, label = "Análisis Exploratorio de Datos de Autores") {
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
eda_autores_server <- function(input, output, session, stringsAsFactors) {
    
    autores_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        autores <-  dbGetQuery(raab_db_conn, paste0("select * from ", "autores" )) %>% as_tibble()
        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        autores
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
            # incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) # 
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            # incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) # 
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
                        tags$h6("Cuantos autores hay en total de la base ?"),
                        br(),
                        uiOutput(ns('eda_total'))
                    ))
        )# fin tabset
    )# fin taglist
}
# server modulo articulos ---------------------------------------------------------------
eda_articulos_server <- function(input, output, session, stringsAsFactors) {
    
    data_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        autores <-  dbGetQuery(raab_db_conn, paste0("select * from ", "articulos" )) %>% as_tibble()
        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        autores
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
            # incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) # 
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            # incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) # 
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
                        tags$h6("Cuantos autores hay en total de la base ?"),
                        br(),
                        uiOutput(ns('eda_total'))
                    ))
        )# fin tabset
    )# fin taglist
}
# server modulo aut-art ---------------------------------------------------------------
eda_aut_art_server <- function(input, output, session, stringsAsFactors) {
    
    data_reactive <- reactive({
        db_name <- "db_raab_grafos.sqlite"
        raab_db_conn <- get_db_connection(db_name)
        # nombres_tablas <- dbListTables(raab_db_conn)
        autores <-  dbGetQuery(raab_db_conn, paste0("select * from ", "autores_articulos" )) %>% as_tibble()
        dbDisconnect(raab_db_conn)
        # glimpse(autores)
        autores
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
            # incProgress(1/2, detail = paste("Procesando visualización interactiva ...", 1)) # 
            # plty_out <- plotly::ggplotly(plot_vis_compo)
            # incProgress(2/2, detail = paste("Finalizando visualización interactiva ...", 2)) # 
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
    
}