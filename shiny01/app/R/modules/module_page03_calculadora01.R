
module_page03_calculadora01_ui <- function(id){
  
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
    
    
    # Ecuacion, Formula y Nomenclatura
    uiOutput(ns("salida01")),
    br(), br(),br(),
    
    # Tablas resumen
    uiOutput(ns("salida02")),
    br(), br(), br(),
    
    uiOutput(ns("menu_calculadora_01")),
    br(),br(),br(),
    
    # Tablas resumen
    uiOutput(ns("salida03")),
    br(), br(), br(),
   
  )
}



module_page03_calculadora01_server <- function(id){
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
      
      dt_the_subrow <- reactive({df_nomenclature()[,3] == selected_valence()})
      
      df_selected_nomenclature <- reactive({
        df_nomenclature()[dt_the_subrow(), ]
      })
      
      
      output$df_tabla_01 <- DT::renderDT({
        req(control_01(), load_button_status(), df_selected_nomenclature())
        
        
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
        dt_row01 <- selected_data_FC()$symbol == selected_symbol()
        dt_row02 <- selected_data_FC()$selected_valence == selected_valence()
        dt_row03 <- (dt_row01 + dt_row02) == 2
        
        vector_pos <- 1:nrow(selected_data_FC())
        the_pos <- vector_pos[dt_row03]
        #print(the_pos)
        the_pos
      })
      
      
      df_row_resolution <- reactive({
        
        #print(selected_row_resolution())
        the_row <- selected_data_FC()[selected_row_resolution(),]
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
        
        vector_coef <- unlist(list_armado$vector_coef)
        vector_sustancias <- list_armado$vector_sustancias
        
        
        # # # INICIACION
        # list_armado <- list()
        # list_armado$vector_coef <- c(4,1,2)
        # list_armado$vector_sustancias <- c("Li", "O_{2}", "Li_{2}O")
        
        
        outout_list <- op02_estequiometria_oxidos_esp(vector_coef, vector_sustancias)
        outout_list
        
      })
      
      ##############################################################
      tabla_externa_estequiometria_01 <- reactive({
        list_full_explicado()[[8]]
      })
      
      
      output$df_externo_01 <- DT::renderDT({
        req(control_01(), load_button_status(), tabla_externa_estequiometria_01())
        
        
        mi_tabla <- tabla_externa_estequiometria_01()
        
        
        #mi_tabla <- mtcars
        
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        
        datatable(mi_tabla, 
                  rownames = TRUE,
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
      
      
      menu_pos_fila_sustancia <- reactive({
        mi_valor <- input$selector_sustancia
        mi_valor <- as.numeric(as.character(mi_valor))
        mi_valor
        
      })
      
      menu_pos_columna_unidad <- reactive({
        mi_valor <- input$selector_unidad
        mi_valor <- as.numeric(as.character(mi_valor))
        mi_valor
      })
      
      menu_cantidad_usuario <- reactive({
        mi_valor <- input$selector_cantidad
        mi_valor <- as.numeric(as.character(mi_valor))
        mi_valor
      })
      
      list_full_calculadora01 <- reactive({
        req(list_full_explicado(), menu_pos_fila_sustancia(), 
            menu_pos_columna_unidad(), menu_cantidad_usuario())
        
        #print(menu_pos_fila_sustancia())
        new_output <- op03_calculadora01_oxidos_esp(input_list = list_full_explicado(), 
                                                    pos_fila_seleccionada = menu_pos_fila_sustancia(),
                                                    pos_columna_seleccionada = menu_pos_columna_unidad(),
                                                    cantidad_usuario = menu_cantidad_usuario() )
        new_output
      })
      
      # # # # # # # # # # # # #               


      tabla_externa_estequiometria_02 <- reactive({
        req(list_full_calculadora01())
        mi_tabla <- list_full_calculadora01()[["tabla_externa_02"]]
        
        #print(mi_tabla)
        mi_tabla
      })
      

      
      output$df_externo_02 <- DT::renderDT({
        req(control_01(), load_button_status(), tabla_externa_estequiometria_02())
        
        mi_tabla <- tabla_externa_estequiometria_02()
        
        #mi_tabla <- mtcars
        
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        
        datatable(mi_tabla, 
                  rownames = TRUE,
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
      
      output$texto_armado01 <- renderUI({ 
        
        mi_texto <- list_full_calculadora01()[["armado"]]
        mi_texto <- HTML(mi_texto)
        mi_texto
        })
      
      output$texto_armado02 <- renderUI({ 
        
        mi_texto <- list_full_calculadora01()[["renglon01"]]
        mi_texto
      })
      
      
      output$texto_armado03 <- renderText({ 
        
        mi_texto <- list_full_calculadora01()[["renglon02"]]
        mi_texto
      })
      
      
      output$texto_armado04 <- renderText({ 
        
        mi_texto <- list_full_calculadora01()[["paso_a_paso"]]
        mi_texto
      })
      
      output$texto_armado05 <- renderUI({ 
        
        mi_texto <- list_full_calculadora01()[["frase_extra"]]
        mi_texto <- HTML(mi_texto)
        mi_texto
      })
      
      #################################################
      
      
      output$menu_calculadora_01 <- renderUI({
        
        ns <- session$ns
        
        
        req(control_01(), load_button_status(), tabla_externa_estequiometria_01())
        
        # Posiciones de fila para la tabla interna
        mis_opciones01 <- 1:nrow(tabla_externa_estequiometria_01())
        names(mis_opciones01) <- tabla_externa_estequiometria_01()[,2]
        
        # Posiciones de columnas para la tabla interna
        mis_opciones02 <- 3:5 #3:6
        names(mis_opciones02) <- c("Moles", "Gramos", "Litros") #, "Avogadro")
        
        div(
          h2("Calculadora01"),
          h4("- Selecciona un compuesto químico de la ecuación química."),
          h4("- Selecciona una unidad de medida (moles, gramos, litros)."),
          h4("- Indica una cantidad (solo agregar números!)'."),
          h4("- Clic en LOAD."),
          h4("- Obten el paso a paso para el cálculo!"),
          fluidRow(
            column(3, 
                   selectInput(inputId = ns("selector_sustancia"), label = "Sustancia",
                               choices = mis_opciones01)),
            column(3, 
                   selectInput(inputId = ns("selector_unidad"), label = "Selector de unidad",
                               choices = mis_opciones02)),
            
            column(3, 
                   textInput(inputId = ns("selector_cantidad"), label = "Cantidad",value = "14")),
            column(3, actionButton(ns("action_load_calculadora01"),
                                   label = "LOAD IA"))
          )
        )
        
        
      })
      
      ##################################################
      
      #######################################
      load_button_calculadora01_status  <- reactiveVal()
      load_button_calculadora01_counter <- reactiveVal()
      
      
      control_calculadora01 <- reactive({
        
        ns <- session$ns
        load_button_calculadora01_status(FALSE)
        load_button_calculadora01_counter(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load_calculadora01")))
        
        
        #Validar que selected_rows() no sea nulo
        validate(
          need(!is.null(menu_pos_fila_sustancia()), "Error 01: df_nomenclature() no debe ser nulo.")
        )
        # 
        # 
        # validate(
        #   need(sum(selected_rows()) > 0, "No se generan estequiometrías con Gases Nobles.")
        # )
        
        
        
        return(TRUE)
      })
      
      observeEvent(input$action_load_calculadora01, {
        
        # Todo lo anterior tiene que estar OK.
        #req(control_01())
        
        load_button_calculadora01_counter(load_button_calculadora01_counter() + 1)
      })
      
      
      observeEvent(load_button_calculadora01_counter(), {
        
        # Todo lo anterior tiene que estar OK.
        req(control_calculadora01())
        
        ns <- session$ns
        
        #load_button_counter(load_button_counter() + 1)
        if(load_button_calculadora01_counter() >= 1){
          runjs(sprintf('$("#%s").css({"background-color": "green",  "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load_calculadora01")))
          load_button_calculadora01_status(TRUE)
          
        }
        
        if(load_button_calculadora01_counter() == 0){
          runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load_calculadora01")))
          load_button_calculadora01_status(FALSE)
        }
        
        
      })
      
      shiny::observeEvent(input$selector_sustancia,{
        load_button_calculadora01_counter(0)
      })
      
      shiny::observeEvent(input$selector_unidad,{
        load_button_calculadora01_counter(0)
      })
      
      shiny::observeEvent(input$selector_cantidad,{
        load_button_calculadora01_counter(0)
      })
      
      ##################################################   
      output$salida02 <- renderUI({
        
        ns <- session$ns
        
        
        req(control_01(), load_button_status())
        
        div(
          div(
            h3("Estequimetría Original - Tabla Resumen"),
            DTOutput(ns("df_externo_01"))),
          br(),
          br()
        )
      })
      
      output$salida03 <- renderUI({
        
        ns <- session$ns
        
        
        req(control_01(), load_button_status(),  load_button_calculadora01_status())
        
        div(
          fluidRow(
            column(4, h3("Idea general"), style = "text-align: center;"),
            column(4, h3("Regla de 3 simple"), style = "text-align: center;"),
            column(4, h3("Explicación"), style = "text-align: center;")
            ),
          fluidRow(
            column(4, htmlOutput(ns("texto_armado01"), inline = TRUE), style = "text-align: left; background-color: lightblue;"),
            column(4, h3(htmlOutput(ns("texto_armado02")), inline = TRUE), br(),
                      h3(htmlOutput(ns("texto_armado03")), inline = TRUE), br(),
                      h3(htmlOutput(ns("texto_armado04")), inline = TRUE), style = "text-align: left;  background-color: lightgreen;"),
            column(4, htmlOutput(ns("texto_armado05"), inline = TRUE), style = "text-align: left;  background-color: lightblue;"),
            
          ),
          div(
            h3("Estequimetría Calculadora 01 - Tabla Resumen"),
            DTOutput(ns("df_externo_02"))
          )
        )
      })
    })
}

