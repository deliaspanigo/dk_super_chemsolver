
module_page05_balanceo_paso_a_paso_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  div(
    fluidRow(
      column(3, selectInput(inputId = ns("family_chem"), 
                            label = "Familia Química", 
                            choices = vector_familia_quimica)),
      
      column(3, selectInput(inputId = ns("selected_symbol"), 
                            label = "Elemento", 
                            choices = vector_elemento)),
      
      column(3, radioButtons(inputId = ns("selected_valence"), 
                             label = "Valencia", 
                             choices = "")),
      
      column(3, actionButton(ns("action_load"),
                             label = "LOAD IA"))
    ),br(), br(),br(),
    
    
    
    h3(textOutput(ns("new_title"))),
    DTOutput(ns("df_tabla_02")),
    br(), br(),br(),
    uiOutput(ns("salida01")),
    br(), br(),br() 
  )
}



module_page05_balanceo_paso_a_paso_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      # # # 01 - User selection
      # -01-01) Family Chem
      # -01-02) Element
      selected_value_FC <- reactive({
        selected_value <- as.character(input$family_chem)
        selected_value
        
      })
      
      # El dataframe de la resolución general para familia quimica seleccionada
      selected_data_FC <- reactive({ 
        selected_value <- selected_value_FC()
        #if(selected_value == 2) selected_value <- 1 
        df_reso <- data02_names_fq[[selected_value]] 
        df_reso
      })
      
      ##############################################################################
      
      # # # 02 - Seleccion del usuario - Simbolo Quimico y Basics
      selected_symbol <- reactive({ input$selected_symbol })
      
      dt_rows_PT <- reactive({ data00_tabla_periodica$symbol == selected_symbol() })
      
      only_row_PT <- reactive({
        
        selected_row <- data00_tabla_periodica[dt_rows_PT(), , drop = FALSE]
        selected_row
      })
      
      selected_state_PT <- reactive({ only_row_PT()$state[1]})
      
      selected_name_PT <- reactive({ only_row_PT()$name[1] })
      
      check_gas_PT <- reactive({ selected_state_PT() == "Gas" })
      
      vector_valences_PT <- reactive({
        valences <- only_row_PT()$valence
        valences <- strsplit(valences, ";")[[1]]
        valences <- as.character(as.numeric(valences))
        valences
      })
      
      
      # # # User - Valence Selection
      external_choices <- reactive(paste0(selected_name_PT(), " - ", selected_symbol(), " - Valencia ", vector_valences_PT()))
      internal_choices <- reactive(vector_valences_PT())
      
      the_opt <- reactive({
        the_opt <- internal_choices()
        names(the_opt) <- external_choices()
        the_opt
      })
      
      observe({
       
        
        updateRadioButtons(inputId = "selected_valence", 
                           label = "Valencia", 
                           choices = the_opt())
      })
      
      selected_valence <- reactive({ input$selected_valence})
      
      ##############################################################################
      
      
      
      # # # 03 - Subseleccion de las filas con la resolucion
      
      selected_rows <- reactive({ selected_data_FC()$symbol == selected_symbol() })
      
    
      #######################################
      load_button_status  <- reactiveVal()
      load_button_counter <- reactiveVal()
      
      
      control_01 <- reactive({
        
        ns <- session$ns
        load_button_status(FALSE)
        load_button_counter(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        
        
        # Validar que selected_rows() no sea nulo
        validate(
          need(!is.null(selected_rows()), "Error 01: df_nomenclature() no debe ser nulo.")
        )
        
        
        validate(
          need(sum(selected_rows()) > 0, "No se generan estequiometrías con Gases Nobles.")
        )
        
        
        
        return(TRUE)
      })
      
      
      observeEvent(input$action_load, {
        
        # Todo lo anterior tiene que estar OK.
        #req(control_01())
        
        load_button_counter(load_button_counter() + 1)
      })
      
      
      observeEvent(load_button_counter(), {
        
        # Todo lo anterior tiene que estar OK.
        req(control_01())
        
        ns <- session$ns
        
        #load_button_counter(load_button_counter() + 1)
        if(load_button_counter() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
          load_button_status(TRUE)
          
        }
        
        if(load_button_counter() == 0){
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
          load_button_status(FALSE)
        }
        
        
      })
      
      shiny::observeEvent(input$family_chem,{
        load_button_counter(0)
      })
      
      shiny::observeEvent(input$selected_symbol,{
        load_button_counter(0)
      })
      
      shiny::observeEvent(input$selected_valence,{
        load_button_counter(0)
      })
      
      #######################################
      
      
      
      df_nomenclature <- reactive({
        req(control_01())
        
        algunas_columnas <- colnames(selected_data_FC())
        # 1: Oxidos - gas
        # 2: Oxidos - no gas
        if (selected_value_FC() == "oxidos") {
          algunas_columnas <- c("order_oxido", "atomic_number", "symbol", "name", "type", "type_mod", "selected_valence", "selected_roman", "chem_form01","chem_form02", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          algunas_columnas <- c("symbol", "selected_valence", "selected_roman", "chem_form01", "chem_form02", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          algunas_columnas <- colnames(selected_data_FC())
          algunas_columnas <- c("name", "symbol", "selected_valence", "selected_roman", "balance_final02", "p_parte03", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          #algunas_columnas <- colnames(selected_data_FM())
          
          new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
                        "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
          
          #new_cols <- algunas_columnas
        }
        
        mi_tabla <- selected_data_FC()[selected_rows(), algunas_columnas]
        mi_tabla$p_parte03 <- paste0("$$", mi_tabla$p_parte03, "$$")
        colnames(mi_tabla) <- new_cols
        mi_tabla
      })
      
      dt_the_row <- reactive({df_nomenclature()[,3] == selected_valence()})
      
      df_selected_nomenclature <- reactive({
        df_nomenclature()[dt_the_row(), ]
      })
      
      output$df_tabla_01 <- DT::renderDT({
        req(control_01(), load_button_status())
        
        
        mi_tabla <- df_selected_nomenclature()
        #mi_tabla <- mtcars
        
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        
        datatable(mi_tabla, 
                  rownames = FALSE,
                  options = list(
                    drawCallback = JS("
        function(settings) {
          MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
        }
      "),
                    autoWidth = FALSE,
                    columnDefs = list(
                      list(width = 'auto', targets = '_all'),  # Aplicar ajuste a todas las columnas
                      list(orderable = FALSE, targets = colnames(mi_tabla))  # Desactivar el ordenamiento en la columna 5 (índice 4)
                    ),
                    dom = 't'  # Solo muestra la tabla sin controles adicionales (paginar, buscar, etc.)
                    
                  )) %>%
          formatStyle(
            colnames(mi_tabla),  # Nombre de la columna
            fontSize = '20px'  # Tamaño de fuente deseado
          ) %>%
          formatStyle(
            colnames(mi_tabla)[5],  # Nombre de la columna
            fontSize = '30px'  # Tamaño de fuente deseado
          ) %>%
          formatStyle(
            colnames(mi_tabla)[6],  # Nombre de la columna
            fontSize = '30px'  # Tamaño de fuente deseado
          ) %>%
          formatStyle(
            colnames(mi_tabla),
            backgroundColor = styleRow(vector_pos, vector_color),#,
            target = 'row'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            colnames(mi_tabla)[5],
            backgroundColor = styleRow(vector_pos, "orange"),#,
            target = 'cel'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            colnames(mi_tabla)[6],
            backgroundColor = styleRow(vector_pos, "#39FF14"),#,
            target = 'cel'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            columns = colnames(mi_tabla),  # Afectar todas las columnas
            `text-align` = 'center'  # Centrar el texto
          )
      })
      
      output$salida01 <- renderUI({
        
        ns <- session$ns
        
        
        req(control_01(), load_button_status())
        
        div(
          h3("Ecuación Química, Fórmula Química y Nomenclatura"),
          fluidRow(
            column(12,withMathJax(DTOutput(ns("df_tabla_01"))))
          )
        )
        
      })
      
      #########################################
     
      
      output$new_title <- renderText({
        req(control_01(), load_button_status())
        new_title <- paste0("Resolución paso a paso: ",selected_name_PT(), " - ",
                            selected_symbol(), " - Valencia ", selected_valence())
        
        new_title
      })
      
      
      
      #################################################################
      
      selected_reso_folder <- reactive({ 
        
        armado <- 1 # Oxido para
        if(!check_gas_PT()) armado <- 2
        # selected_value <- vector_reso_folder[selected_value_FC()]
        selected_value <- vector_reso_folder[armado]
        selected_value
        
      })
      
      
      selected_reso_file <- reactive({
        req(selected_valence())
        
        
        if (selected_value_FC() == 1 ) {
          las_partes <- c("oxido_valencia_", paste0(selected_valence(), "_gas.xlsx"))
          paste0(las_partes, collapse = "") } else
            if (selected_value_FC() == 2 ) {
              las_partes <- c("oxido_valencia_", paste0(selected_valence(), "_no_gas.xlsx"))
              paste0(las_partes, collapse = "")
            }
      })
      
      #################################################
      
      ################################################
      df_reso <- reactive({
        vector_filas <- 1:9
        vector_columnas <- c("step", "resumen", "balanceo01", "texto_especifico", "texto_general")
        df_reso <- las_reso[[selected_reso_folder()]][[selected_reso_file()]]
        df_reso <- df_reso[vector_filas, vector_columnas]
        df_reso$texto_especifico <- gsub(pattern = "_elemento_", replacement = selected_symbol(), df_reso$texto_especifico)
        df_reso$balanceo01 <- gsub(pattern = "_elemento_", replacement = selected_symbol(), df_reso$balanceo01)
        df_reso
      })
      
      output$df_tabla_02 <- DT::renderDT({
        
        req(control_01(), load_button_status())
        req(df_reso())
        
        mi_tabla <- df_reso()
        #print(mi_tabla)
        
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        
        datatable(mi_tabla, 
                  rownames = FALSE,
                  options = list(
                    drawCallback = JS("
        function(settings) {
          MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
        }
      "),
                    autoWidth = FALSE,
                    columnDefs = list(
                      list(width = 'auto', targets = '_all')  # Aplicar ajuste a todas las columnas
                    ),
                    dom = 't',  # Solo muestra la tabla sin controles adicionales (paginar, buscar, etc.)
                    ordering = FALSE
                  )) %>%
          formatStyle(
            colnames(mi_tabla)[3],  # Nombre de la columna
            fontSize = '30px'  # Tamaño de fuente deseado
          ) %>%formatStyle(
            colnames(mi_tabla),
            backgroundColor = styleRow(vector_pos, vector_color),#,
            target = 'row'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            columns = colnames(mi_tabla),  # Afectar todas las columnas
            `text-align` = 'center'  # Centrar el texto
          )
      })
      ################################################
      
      
      
    })
}

