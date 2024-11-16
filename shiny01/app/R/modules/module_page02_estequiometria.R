
module_page02_estequiometria_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  div(
    fluidRow(
      column(3, selectInput(inputId = ns("sui_family_chem"), 
                            label = "Familia Química", 
                            choices = vector_familia_quimica)),
      
      column(3, selectInput(inputId = ns("sui_symbol"), 
                            label = "Elemento", 
                            choices = vector_elemento)),
      
      column(3, radioButtons(inputId = ns("sui_valence"), 
                             label = "Valencia", 
                             choices = "")),
      
      column(3, actionButton(ns("action_load"),
                             label = "LOAD IA"))
    ),br(), br(),br(),
    
    
    uiOutput(ns("salida01")),
    br(), br(),br(),
    
    uiOutput(ns("salida02")),
    br(), br(), br()
    
  )
}



module_page02_estequiometria_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      # # # 01 - User selection
      # -01-01) Family Chem
      # -01-02) Element
      sui_family_chem <- reactive({
        selected_value <- as.character(input$sui_family_chem)
        selected_value
      })
      
      # # # 01 - Simbolo Quimico y Basics desde la Tabla Periodica (PT)
      sui_symbol <- reactive({ as.character(input$sui_symbol) })
      
      
      dt_rows_symbol_PT <- reactive({ 
        req(sui_symbol())
        dt_rows <- data00_tabla_periodica$symbol == sui_symbol() 
        dt_rows
      })
      
      
      df_only_row_symbol_PT <- reactive({
        req(dt_rows_symbol_PT())
        selected_row <- data00_tabla_periodica[dt_rows_symbol_PT(), , drop = FALSE]
        selected_row
      })
      
      
      selected_state_PT <- reactive({
        req(df_only_row_symbol_PT())
        the_info <- df_only_row_symbol_PT()$state[1]
        the_info
      })
      
      selected_name_PT <- reactive({ 
        req(df_only_row_symbol_PT())
        the_info <- df_only_row_symbol_PT()$name[1] 
        the_info
      })
      
      selected_type_PT <- reactive({
        req(df_only_row_symbol_PT())
        the_info <- df_only_row_symbol_PT()$type[1]
        the_info
      })
      
      check_gas_PT <- reactive({ 
        req(selected_state_PT())
        the_info <- selected_state_PT() == "Gas" 
        the_info
      })
      
      check_gas_noble_PT <- reactive({ 
        req(selected_state_PT())
        the_info <- selected_type_PT() == "Gas Noble" 
        the_info
      })
      
      vector_valences_PT <- reactive({
        req(df_only_row_symbol_PT())
        
        valences <- df_only_row_symbol_PT()$valence
        valences <- as.character(valences)
        valences <- strsplit(valences, ";")[[1]]
        valences
      })
      
      # # # User - Valence Selection
      external_choices <- reactive(paste0(selected_name_PT(), " - ", sui_symbol(), " - Valencia ", vector_valences_PT()))
      internal_choices <- reactive(vector_valences_PT())
      
      the_opt <- reactive({
        the_opt <- internal_choices()
        names(the_opt) <- external_choices()
        the_opt
      })
      
      observe({
        
        
        updateRadioButtons(inputId = "sui_valence", 
                           label = "Valencia", 
                           choices = the_opt())
      })
      
      sui_valence <- reactive({ input$sui_valence})
      
      ##############################################################################
      
      
      
      # # # 03 - Subseleccion de las filas con la resolucion
      df_selected_data_FC <- reactive({ 
        req(sui_family_chem())
        
        selected_value <- sui_family_chem()
        df_reso <- data02_names_fq[[selected_value]] 
        df_reso
      })
      
      selected_rows <- reactive({ df_selected_data_FC()$symbol == sui_symbol() })
      
      
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
          need(selected_type_PT() != "Gas Noble", "No se generan estequiometrías para gases nobles.")
        )
        
        validate(
          need(sum(selected_rows()) > 0, "No se generan estequiometrías para este elemento en la Familia Química seleccionada.")
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
      
      shiny::observeEvent(input$sui_family_chem,{
        load_button_counter(0)
      })
      
      shiny::observeEvent(input$sui_symbol,{
        load_button_counter(0)
      })
      #######################################
      
      
      
      df_nomenclature <- reactive({
        req(control_01())
        
        algunas_columnas <- colnames(df_selected_data_FC())
        # 1: Oxidos - gas
        # 2: Oxidos - no gas
        if (sui_family_chem() == "oxidos") {
          algunas_columnas <- c("order_oxido", "atomic_number", "symbol", "name", "type", "type_mod", "selected_valence", "selected_roman", "chem_form01","chem_form02", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          algunas_columnas <- c("symbol", "selected_valence", "selected_roman", "chem_form01", "chem_form02", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          algunas_columnas <- colnames(df_selected_data_FC())
          algunas_columnas <- c("name", "symbol", "selected_valence", "selected_roman", "balance_final02", "p_parte03", "oxyde_nomen_01_tradicional", "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
          #algunas_columnas <- colnames(selected_data_FM())
          
          new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
                        "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
          
          #new_cols <- algunas_columnas
        }
        
        mi_tabla <- df_selected_data_FC()[selected_rows(), algunas_columnas]
        mi_tabla$p_parte03 <- paste0("$$", mi_tabla$p_parte03, "$$")
        colnames(mi_tabla) <- new_cols
        mi_tabla
      })
      
      dt_the_subrow <- reactive({df_nomenclature()[,3] == sui_valence()})
      
      df_selected_nomenclature <- reactive({
        df_nomenclature()[dt_the_subrow(), ]
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
       
      selected_row_resolution <- reactive({
        
        #print(selected_valence())
        dt_row01 <- df_selected_data_FC()$symbol == sui_symbol()
        dt_row02 <- df_selected_data_FC()$selected_valence == sui_valence()
        dt_row03 <- (dt_row01 + dt_row02) == 2
        
        vector_pos <- 1:nrow(df_selected_data_FC())
        the_pos <- vector_pos[dt_row03]
        #print(the_pos)
        the_pos
      })
      
      
      df_row_resolution <- reactive({
        
        #print(selected_row_resolution())
        the_row <- df_selected_data_FC()[selected_row_resolution(),]
        the_row
      })
      
      
      list_armado <- reactive({
        
        # Esto es ahora solo para los oxidos...
        # pero hay que ampliarlo despues para las otras sustancias.
        # Arma un df que tiene las moleculas y los coeficientes.
        # Eso lo usa luego hacer la estequiometria y las frases
        
        selected_cols_coef <- c("r_coef_1", "r_coef_2", "p_coef_3")
        selected_cols_sustancias <- c("r_parte01", "r_parte02", "p_parte03")
        vector_coef <- df_row_resolution()[selected_cols_coef]
        vector_sustancias <- df_row_resolution()[selected_cols_sustancias]
        
        output_list <- list(vector_coef, vector_sustancias)
        names(output_list) <- c("vector_coef", "vector_sustancias")
        output_list
      })
      
      list_full_explicado <- reactive({
        req(list_armado())
        
        list_armado <- list_armado()
        
        vector_coef <- list_armado$vector_coef
        vector_coef <- unlist(list_armado$vector_coef)
        
        vector_sustancias <- list_armado$vector_sustancias

        
        # # # INICIACION
        # list_armado <- list()
        # list_armado$vector_coef <- c(4,1,2)
        # list_armado$vector_sustancias <- c("Li", "O_{2}", "Li_{2}O")
        # vector_coef <- c(4,1,2)
        # vector_sustancias <- c("Li", "O_{2}", "Li_{2}O")
        
        outout_list <- op02_estequiometria_oxidos_esp(vector_coef, vector_sustancias)
        outout_list
        
      })
      
      
      output$parte01 <- renderUI({
        aver <- list_full_explicado()[[1]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
        
      })
      
      output$parte02 <- renderUI({
        aver <- list_full_explicado()[[2]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$parte03 <- renderUI({
        aver <- list_full_explicado()[[3]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$parte04 <- renderUI({
        aver <- list_full_explicado()[[4]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$parte05 <- renderUI({
        aver <- list_full_explicado()[[5]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$parte06 <- renderUI({
        aver <- list_full_explicado()[[6]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$parte07 <- renderUI({
        aver <- list_full_explicado()[[7]]
        aver <- unlist(aver)
        aver <- paste0(aver, collapse = "<br><br>")
        aver <- HTML(aver)
        aver
      })
      
      output$df_externo <- DT::renderDT({
        req(control_01(), load_button_status())
        
        
        mi_tabla <- list_full_explicado()[[8]]
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
                    autoWidth = TRUE,
                    columnDefs = list(
                      list(width = 'auto', targets = '_all'),  # Aplicar ajuste a todas las columnas
                      list(orderable = FALSE, targets = colnames(mi_tabla))  # Desactivar el ordenamiento en la columna 5 (índice 4)
                    ),
                    dom = 't'  # Solo muestra la tabla sin controles adicionales (paginar, buscar, etc.)
                    
                  ))%>%
          formatStyle(
            colnames(mi_tabla),
            backgroundColor = styleRow(vector_pos, vector_color),#,
            target = 'row'#,
            #fontSize = "26px"
          )
        
        # %>%
        #   formatStyle(
        #     colnames(mi_tabla),  # Nombre de la columna
        #     fontSize = '20px'  # Tamaño de fuente deseado
        #   ) %>%
        #   formatStyle(
        #     colnames(mi_tabla)[5],  # Nombre de la columna
        #     fontSize = '30px'  # Tamaño de fuente deseado
        #   ) %>%
        #   formatStyle(
        #     colnames(mi_tabla)[6],  # Nombre de la columna
        #     fontSize = '30px'  # Tamaño de fuente deseado
        #   ) %>%
        #   formatStyle(
        #     colnames(mi_tabla),
        #     backgroundColor = styleRow(vector_pos, vector_color),#,
        #     target = 'row'#,
        #     #fontSize = "26px"
        #   )%>%
        #   formatStyle(
        #     colnames(mi_tabla)[5],
        #     backgroundColor = styleRow(vector_pos, "orange"),#,
        #     target = 'cel'#,
        #     #fontSize = "26px"
        #   )%>%
        #   formatStyle(
        #     colnames(mi_tabla)[6],
        #     backgroundColor = styleRow(vector_pos, "#39FF14"),#,
        #     target = 'cel'#,
        #     #fontSize = "26px"
        #   )%>%
        #   formatStyle(
        #     columns = colnames(mi_tabla),  # Afectar todas las columnas
        #     `text-align` = 'center'  # Centrar el texto
        #   )
      })
      
     
      
      output$salida02 <- renderUI({
        
        ns <- session$ns
        
        
        req(control_01(), load_button_status())
      div(
        div(
          h3("Estequimetría - Tabla Resumen"),
          DTOutput(ns("df_externo"))),
        br(),
      div(
        h3("Conceptos a tener en cuenta"),
        "Concepto 1) El número de Avogadro es \\(6.02 \\times 10^{23}\\).",br(),
        "Concepto 2) Un mol de cualquier sustancia son \\(6.02 \\times 10^{23}\\) átomos o moléculas (según corresponda).",br(),
        "Concepto 3) Un mol de cualquier sustancia pesa en gramos lo mismo que su peso en unidades de masa atómica (u.m.a.).",br(),
        "Concepto 4) Un mol de cualquier gas en condiciones normales de presión y temperatura (C.N.P.T) ocupa un volumen de 22.4 litros.",br(),
        "Concepto 5) Ley de Conservación: nada se crea, nada se destruye, todo se transforma.",br(),
        "Concepto 6) Por la 'Ley de Conservación' es que debe ocurrir un balanceo en cualquier ecuación química.",br(),
        "Concepto 7) Por la 'Ley de Conservación' los gramos totales de reactivos deben ser igual a los gramos totales de productos."),br(),
      br(),
      
      h2("Estequimetría paso a paso!"),
      
      div(
        h3("Paso 1 - Elementos químicos"),
        htmlOutput(ns("parte01"))),
      br(),
      
      div(
        h3("Paso 2 - Compuestos Químicos y pesos moleculares"),
        htmlOutput(ns("parte02"))),
      br(),
      
      div(
        h3("Paso 3 - Compuestos Químicos y pesos molares"),
        htmlOutput(ns("parte03"))),
      br(),
      
      div(
        h3("Paso 4 - Moles de reactivos y productos"),
        htmlOutput(ns("parte04"))),
      br(),
      
      div(
        h3("Paso 5 - Gramos de reactivos y productos"),
        htmlOutput(ns("parte05"))),
      br(),
      div(
        h3("Paso 6 - Litros de reactivos y productos"),
        htmlOutput(ns("parte06"))),
      br(),
      div(
        h3("Paso 7 - Número de Avogadro de reactivos y productos"),
        htmlOutput(ns("parte07"))),
      br(),

      # div(
      #   h3("Paso 9 - Tabla Interna"),
      #   DTOutput(ns("df_interno"))),
      br()
      )
      })
    })
}

