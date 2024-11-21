
module_page01_nomenclatura_00_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  div(
    fluidRow(
      column(3, selectInput(inputId = ns("sui_family_chem"), 
                            label = "Familia Química", 
                            choices = vector_familia_quimica))
   )
  )
}



module_page01_nomenclatura_00_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      ##############################################################################
      
      
      # # # 01) Familia Quimica y Basics desde los archivos de resolucion
      sui_family_chem <- reactive({
        selected_value <- as.character(input$sui_family_chem)
        selected_value
      })

      
      return(sui_family_chem)
    })
}


module_page01_nomenclatura_01_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("la_salida_general"))
  )
}



module_page01_nomenclatura_01_server <- function(id, sui_family_chem){
  moduleServer(
    id,
    function(input, output, session) {
      
      ##############################################################################
      
      
      # # # # 01) Familia Quimica y Basics desde los archivos de resolucion
      # sui_family_chem <- reactive({
      #   selected_value <- as.character(input$sui_family_chem)
      #   selected_value
      # })
     
      
      status_general <- reactive({
        req(sui_family_chem())
        
        
        la_seleccion <- c("oxidos", "hidroxidos", "acidos", "hidruros")
        dt_ok <- sum(la_seleccion == sui_family_chem()) == 1
        dt_ok

      })
      
      vector_opt_elemento <- reactive({
        req(status_general())
        
        mis_cols <- c("symbol", "web_selector")
        mini_seleccion <- LAS_NUEVAS[[sui_family_chem()]][mis_cols]
        mini_seleccion <- na.omit(mini_seleccion)
        mini_seleccion[,1] <- as.character(mini_seleccion[,1])
        mini_seleccion[,2] <- as.character(mini_seleccion[,2])
        
        vector_de_opt <- mini_seleccion[,"symbol"]
        names(vector_de_opt) <- mini_seleccion[,"web_selector"]
        vector_de_opt <- vector_de_opt[!duplicated(vector_de_opt)]
        
        #vector_de_opt <- order(vector_de_opt)
        vector_de_opt
        
      })
      
      # # # 01 - Simbolo Quimico y Basics desde la Tabla Periodica (PT)
      sui_symbol <- reactive({ 
        the_symbol <-as.character(input$sui_symbol) 
        
        print(the_symbol)
        the_symbol
        })

      
      # shiny::observeEvent( sui_family_chem(),{
      #   ns <- session$ns
      #   runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
      #   
      # })
      # 
      # shiny::observeEvent( sui_symbol(),{
      #   ns <- session$ns
      #   runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
      #   
      # })
      
      
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
      
      selected_type_mod_PT <- reactive({
        req(df_only_row_symbol_PT())
        the_info <- df_only_row_symbol_PT()$type_mod[1]
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
      

      ##############################################################################
      
      
      

      
      # El dataframe de la resolución general para familia quimica seleccionada
      df_selected_data_FC <- reactive({ 
        req(sui_family_chem())
        
        selected_value <- sui_family_chem()
        # df_reso <- data02_names_fq[[selected_value]] 
        df_reso <- LAS_NUEVAS[[selected_value]] 
        df_reso
      })
      
      
      
      dt_selected_rows_FC <- reactive({ 
        
        req(df_selected_data_FC(), sui_symbol())
        
        
        
        dt_filas <- df_selected_data_FC()$symbol == sui_symbol() 
        dt_filas <- na.omit(dt_filas)
        dt_filas
        })
      
      
      #######################################
      load_button_status  <- reactiveVal()
      load_button_counter <- reactiveVal()
      
      
      la_aduana <- reactive({
        
        req(selected_name_PT())
        
        lista_A <- list()
        lista_A[[1]] <- TRUE
        lista_A[[2]] <- c("")
        
        lista_B <- list()
        lista_B[[1]] <- FALSE
        lista_B[[2]] <- c("En _fq_ no se realiza nomenclatura para _name_.")

        lista_C <- list()
        lista_C[[1]] <- FALSE
        lista_C[[2]] <- c("En hidróxidos no se realiza estequiometria para No Metales ni para Gases Nobles.")

        lista_D <- list()
        lista_D[[1]] <- FALSE
        lista_D[[2]] <- c("En ácidos no se realiza estequiometria para Metales ni para Gases Nobles.")
        
        
        lista_salida <- lista_A
        if(sui_family_chem() == "oxidos"){
            if(sui_symbol() == "O") lista_salida <- lista_B
        } else 
          
        if(sui_family_chem() == "hidroxidos"){
          if(sui_symbol() == "H") lista_salida <- lista_B else
           if(sui_symbol() == "O") lista_salida <- lista_B #else
             if(selected_type_PT() == "No Metal") lista_salida <- lista_C else
             if(selected_type_PT() == "No Metal") lista_salida <- lista_C
          } else
              
        if(sui_family_chem() == "acidos"){
          if(sui_symbol() == "H") lista_salida <- lista_B else
            if(sui_symbol() == "O") lista_salida <- lista_B else
              if(selected_type_PT() == "Metal") lista_salida <- lista_D else
                if(selected_type_PT() == "Metal") lista_salida <- lista_D
        } else
          
        if(sui_family_chem() == "hidruros"){
          if(sui_symbol() == "H") lista_salida <- lista_B 
        } else
          
        if(sui_family_chem() == "oxosales"){
          if(sui_symbol() == "H") lista_salida <- lista_B else
            if(sui_symbol() == "O") lista_salida <- lista_B
        } else
        
        if(sui_family_chem() == "sales"){
          if(sui_symbol() == "H") lista_salida <- lista_B else
            if(sui_symbol() == "O") lista_salida <- lista_B
        } 
        
        lista_salida[[2]] <- gsub("_fq_",   vector_fq_interno_mod[sui_family_chem()],  lista_salida[[2]])
        lista_salida[[2]] <- gsub("_name_", selected_name_PT(), lista_salida[[2]])
        
        return(lista_salida)
        
      })
      
      control_01 <- reactive({
        
        req(dt_selected_rows_FC())
        ns <- session$ns
        load_button_status(FALSE)
        load_button_counter(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        
        # runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        # 
        # 
        # Validar que dt_selected_rows_FC() no sea nulo
        validate(
          need(!is.null(dt_selected_rows_FC()), "Warning 01: df_nomenclatura() no debe ser nulo.")
        )


        validate(
          need(!check_gas_noble_PT(), "No se generan nomenclaturas para Gases Nobles.")
        )

       
        
        validate(
          need(la_aduana()[[1]], la_aduana()[[2]])
        )
        
        validate(
          need(sum(dt_selected_rows_FC()) > 0, "No se generan nomenclaturas para este elemento en la Familia Química seleccionada.")
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
      
      # shiny::observeEvent(input$sui_family_chem,{
      #   load_button_counter(0)
      # })
      
      shiny::observeEvent(input$sui_symbol,{
        
        load_button_counter(0)
      })
      

      #######################################
      
      
      
      df_nomenclatura <- reactive({
        req(control_01())
        
        algunas_columnas <- colnames(df_selected_data_FC())
        # 1: Oxidos - gas
        # 2: Oxidos - no gas
        if (sui_family_chem() == "oxidos") {
          #algunas_columnas <- colnames(df_selected_data_FC())
          # algunas_columnas <- c("name", "symbol", "selected_valence",
          #                       "selected_roman", "balance_final02", "p_parte03",
          #                       "oxyde_nomen_01_tradicional",
          #                       "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")

          algunas_columnas <- c("name", "symbol", "valence", "roman",
                                "chem_form03", "chem_form02", 
                                "nomen_01_tradicional", "nomen_02_stock", 
                                "nomen_03_sistematica") 
          
          new_colnames <- c("Nombre", "Símbolo", "Valencia", "Romano", 
                            "Ecuación", "Fórmula", 
                            "Nomen. Tradicional", "Nomen. Stock", "Nomen. Sistemática")
          
          # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
          #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
          # 
          mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
          mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
          mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
          colnames(mi_tabla) <- new_colnames
          return(mi_tabla)
          
        } else
          if (sui_family_chem() == "hidroxidos") {
            #algunas_columnas <- colnames(df_selected_data_FC())
            # algunas_columnas <- c("name", "symbol", "selected_valence",
            #                       "selected_roman", "balance_final02", "p_parte03",
            #                       "oxyde_nomen_01_tradicional",
            #                       "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
            
            algunas_columnas <- c("name", "symbol", "valence", "roman",
                                  "chem_form03", "chem_form02", 
                                  "nomen_01_tradicional", "nomen_02_stock", 
                                  "nomen_03_sistematica") 
            
            new_colnames <- c("Nombre", "Símbolo", "Valencia", "Romano", 
                              "Ecuación", "Fórmula", 
                              "Nomen. Tradicional", "Nomen. Stock", "Nomen. Sistemática")
            
            # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
            #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
            # 
            mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
            mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
            mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
            colnames(mi_tabla) <- new_colnames
            return(mi_tabla)
            
          } else
            if (sui_family_chem() == "acidos") {
              #algunas_columnas <- colnames(df_selected_data_FC())
              # algunas_columnas <- c("name", "symbol", "selected_valence",
              #                       "selected_roman", "balance_final02", "p_parte03",
              #                       "oxyde_nomen_01_tradicional",
              #                       "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
              
              algunas_columnas <- c("name", "symbol", "valence", "roman",
                                    "chem_form03", "chem_form02", 
                                    "nomen_01_tradicional", "nomen_02_stock", 
                                    "nomen_03_sistematica") 
              
              new_colnames <- c("Nombre", "Símbolo", "Valencia", "Romano", 
                                "Ecuación", "Fórmula", 
                                "Nomen. Tradicional", "Nomen. Stock", "Nomen. Sistemática")
              
              # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
              #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
              # 
              mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
              mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
              mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
              colnames(mi_tabla) <- new_colnames
              return(mi_tabla)
              
            } else
              if (sui_family_chem() == "hidruros") {
                #algunas_columnas <- colnames(df_selected_data_FC())
                # algunas_columnas <- c("name", "symbol", "selected_valence",
                #                       "selected_roman", "balance_final02", "p_parte03",
                #                       "oxyde_nomen_01_tradicional",
                #                       "oxyde_nomen_02_stock", "oxyde_nomen_03_sistematica")
                
                algunas_columnas <- c("name", "symbol", "valence", "roman",
                                      "chem_form03", "chem_form02", 
                                      "nomen_01_tradicional", "nomen_02_stock", 
                                      "nomen_03_sistematica") 
                
                new_colnames <- c("Nombre", "Símbolo", "Valencia", "Romano", 
                                  "Ecuación", "Fórmula", 
                                  "Nomen. Tradicional", "Nomen. Stock", "Nomen. Sistemática")
                
                # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
                #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
                # 
                mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
                mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
                mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
                colnames(mi_tabla) <- new_colnames
                return(mi_tabla)
                
              } 
              
          
        

        mi_tabla
      })

      
      
      output$df_tabla_01 <- DT::renderDT({
        req(control_01(), load_button_status())
        #req(control_01())
        
        mi_tabla <- df_nomenclatura()
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
      "),scrollX = TRUE,
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
        
        req(status_general())

        
        req(control_01(), load_button_status())
          
        div(
        h3("Ecuación Química, Fórmula Química y Nomenclatura"),
        fluidRow(
          column(12,withMathJax(DTOutput(ns("df_tabla_01"))))
          )
        )
        
      })
      ##########################################################################
      
      
    #   
    #   
    #   control_02 <- reactive({
    #     
    #     ns <- session$ns
    #     
    #     load_button_status(FALSE)
    #     load_button_counter <- reactiveVal(0)
    #     runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
    #     
    #   
    #     
    #     shinyjs::enable("action_load")
    #     load_button_status(TRUE)
    #     return(TRUE)
    # })
    #   
      
    output$la_salida_general <- renderUI({
      ns <- shiny::NS(id)
      req(status_general())
      
      div(
        fluidRow(
          # column(3, selectInput(inputId = ns("sui_family_chem"), 
          #                       label = "Familia Química", 
          #                       choices = vector_familia_quimica)),
          column(3, selectInput(inputId = ns("sui_symbol"), 
                                label = "Elemento", 
                                choices = vector_opt_elemento())),
          column(3, actionButton(ns("action_load"),
                                 label = "LOAD IA"))
        ),br(),
        
        uiOutput(ns("salida01")), br(), br(),
      )
    })
            
    })
}

#################################################################################

module_page01_nomenclatura_02_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("la_salida_general"))
  )
}



module_page01_nomenclatura_02_server <- function(id, sui_family_chem){
  moduleServer(
    id,
    function(input, output, session) {
      
      ##############################################################################
      
      
      # # # # 01) Familia Quimica y Basics desde los archivos de resolucion
      # sui_family_chem <- reactive({
      #   selected_value <- as.character(input$sui_family_chem)
      #   selected_value
      # })
      
      
      status_general <- reactive({
        req(sui_family_chem())
        
        
        la_seleccion <- c("oxosales", "sales")
        dt_ok <- sum(la_seleccion == sui_family_chem()) == 1
        dt_ok
        
      })
      
      vector_opt_elemento_metal <- reactive({
        req(status_general())
        
        mis_cols <- c("metal_symbol", "web_selector_metal")
        mini_seleccion <- LAS_NUEVAS[[sui_family_chem()]][mis_cols]
        mini_seleccion <- na.omit(mini_seleccion)
        mini_seleccion[,1] <- as.character(mini_seleccion[,1])
        mini_seleccion[,2] <- as.character(mini_seleccion[,2])
        
        vector_de_opt <- mini_seleccion[,"metal_symbol"]
        names(vector_de_opt) <- mini_seleccion[,"web_selector_metal"]
        vector_de_opt <- vector_de_opt[!duplicated(vector_de_opt)]
        
        #vector_de_opt <- order(vector_de_opt)
        vector_de_opt
        
      })
      
      vector_opt_elemento_no_metal <- reactive({
        req(status_general())
        
        mis_cols <- c("no_metal_symbol", "web_selector_no_metal")
        mini_seleccion <- LAS_NUEVAS[[sui_family_chem()]][mis_cols]
        mini_seleccion <- na.omit(mini_seleccion)
        mini_seleccion[,1] <- as.character(mini_seleccion[,1])
        mini_seleccion[,2] <- as.character(mini_seleccion[,2])
        
        vector_de_opt <- mini_seleccion[,"no_metal_symbol"]
        names(vector_de_opt) <- mini_seleccion[,"web_selector_no_metal"]
        vector_de_opt <- vector_de_opt[!duplicated(vector_de_opt)]
        
        #vector_de_opt <- order(vector_de_opt)
        vector_de_opt
        
      })
      #observe({

        # updateSelectInput(session, inputId = "sui_symbol_metal", 
        #                   label = "sui_symbol_metal", choices = vector_elemento[2])
        # 
        # updateSelectInput(session, inputId = "sui_symbol_no_metal", 
        #                   label = "sui_symbol_no_metal", choices = vector_elemento)
        
      #})
      
      # # # 01 - Simbolo Quimico y Basics desde la Tabla Periodica (PT)
      sui_symbol_metal <- reactive({ as.character(input$sui_symbol_metal) })
      
      sui_symbol_no_metal <- reactive({ as.character(input$sui_symbol_no_metal) })
      


      
      
      # El dataframe de la resolución general para familia quimica seleccionada
      df_selected_data_FC <- reactive({ 
        req(sui_family_chem())
        
        selected_value <- sui_family_chem()
        # df_reso <- data02_names_fq[[selected_value]] 
        df_reso <- LAS_NUEVAS[[selected_value]] 
        df_reso
      })
      
      
      
      dt_selected_rows_FC <- reactive({ 
        
        req(df_selected_data_FC(), sui_symbol_metal(), sui_symbol_no_metal())
        
        #print(sui_symbol_metal())
        #print(sui_symbol_no_metal())
        #print(df_selected_data_FC())
        
        dt_filas1 <- df_selected_data_FC()$metal_symbol == sui_symbol_metal() 
        dt_filas2 <- df_selected_data_FC()$no_metal_symbol == sui_symbol_no_metal() 
        dt_filas  <- (dt_filas1 + dt_filas2) == 2 
        #print(dt_filas1)
        #print(dt_filas)
        #dt_finana.omit(dt_filas)
        dt_filas
      })
      
      
      #######################################
      load_button_status  <- reactiveVal()
      load_button_counter <- reactiveVal()
      
      
      control_01 <- reactive({
        
        req(dt_selected_rows_FC())
        ns <- session$ns
        load_button_status(FALSE)
        load_button_counter(0)
        runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
        
     
        
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
      
      # shiny::observeEvent(input$sui_family_chem,{
      #   load_button_counter(0)
      # })
      
      shiny::observeEvent(input$sui_symbol_metal,{
        
        load_button_counter(0)
      })
      
      shiny::observeEvent(input$sui_symbol_no_metal,{
        
        load_button_counter(0)
      })
      
      #######################################
      
      
      
      df_nomenclatura <- reactive({
        req(control_01())
        
        algunas_columnas <- colnames(df_selected_data_FC())
        # 1: Oxidos - gas
        # 2: Oxidos - no gas
        if (sui_family_chem() == "oxosales") {
          #algunas_columnas <- colnames(df_selected_data_FC())
          algunas_columnas <- c("metal_name", "metal_symbol",
                                "metal_valence", "metal_roman",
                                "no_metal_name", "no_metal_symbol",
                                "no_metal_valence", "no_metal_roman",
                                "chem_form03", "chem_form02",
                                "nomen_01_tradicional",
                                "nomen_02_stock", "nomen_03_sistematica")
          # 
          # algunas_columnas <- c("name", "symbol", "valence", "roman",
          #                       "chem_form03", "chem_form02", 
          #                       "nomen_01_tradicional", "nomen_02_stock", 
          #                       "nomen_03_sistematica") 
          
          # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
          #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
          # 
          mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
          mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
          mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
          #colnames(mi_tabla) <- new_cols
          return(mi_tabla)
          
        } else
          if (sui_family_chem() == "sales") {
            #algunas_columnas <- colnames(df_selected_data_FC())
            algunas_columnas <- c("metal_name", "metal_symbol",
                                  "metal_valence", "metal_roman",
                                  "no_metal_name", "no_metal_symbol",
                                  "no_metal_valence", "no_metal_roman",
                                  "chem_form03", "chem_form02",
                                  "nomen_01_tradicional",
                                  "nomen_02_stock", "nomen_03_sistematica")
            
            # algunas_columnas <- c("name", "symbol", "valence", "roman",
            #                       "chem_form03", "chem_form02", 
            #                       "nomen_01_tradicional", "nomen_02_stock", 
            #                       "nomen_03_sistematica") 
            
            # new_cols <- c("Nombre", "Símbolo", "Valencia", "Romano", "Ecuación Química", 
            #               "Fórmula Química", "Nomen Tradicional", "Nomen Stock", "Nomen Sistemática (IUPAC)")
            # 
            mi_tabla <- df_selected_data_FC()[dt_selected_rows_FC(), algunas_columnas]
            mi_tabla$chem_form02 <- paste0("$$", mi_tabla$chem_form02, "$$")
            mi_tabla$chem_form03 <- paste0("$$", mi_tabla$chem_form03, "$$")
            #colnames(mi_tabla) <- new_cols
            return(mi_tabla)
            
          } 
        
        
        
        
        mi_tabla
      })
      
      
      
      output$df_tabla_01 <- DT::renderDT({
        req(control_01(), load_button_status())
        #req(control_01())
        
        mi_tabla <- df_nomenclatura()
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
      "),scrollX = TRUE,
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
            colnames(mi_tabla)[9],  # Nombre de la columna
            fontSize = '30px'  # Tamaño de fuente deseado
          ) %>%
          formatStyle(
            colnames(mi_tabla)[10],  # Nombre de la columna
            fontSize = '30px'  # Tamaño de fuente deseado
          ) %>%
          formatStyle(
            colnames(mi_tabla),
            backgroundColor = styleRow(vector_pos, vector_color),#,
            target = 'row'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            colnames(mi_tabla)[9],
            backgroundColor = styleRow(vector_pos, "orange"),#,
            target = 'cel'#,
            #fontSize = "26px"
          )%>%
          formatStyle(
            colnames(mi_tabla)[10],
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
        
        req(status_general())
        
        
        req(control_01(), load_button_status())
        
        div(
          h3("Ecuación Química, Fórmula Química y Nomenclatura"),
          fluidRow(
            column(12,withMathJax(DTOutput(ns("df_tabla_01"))))
          )
        )
        
      })
      ##########################################################################
      
      
      #   
      #   
      #   control_02 <- reactive({
      #     
      #     ns <- session$ns
      #     
      #     load_button_status(FALSE)
      #     load_button_counter <- reactiveVal(0)
      #     runjs(sprintf('$("#%s").css({"background-color": "orange", "color": "white", "border": "none", "padding": "30px 30px", "text-align": "center", "text-decoration": "none", "display": "inline-block", "font-size": "20px", "margin": "4px 4px", "cursor": "pointer", "border-radius": "100%%"});', ns("action_load")))
      #     
      #   
      #     
      #     shinyjs::enable("action_load")
      #     load_button_status(TRUE)
      #     return(TRUE)
      # })
      #   
      
  
      output$la_salida_general <- renderUI({
        ns <- shiny::NS(id)
        req(status_general(), sui_family_chem())
        
        vector_mod1 <- data01_names_general_mod$symbol
        vector_mod1 <- vector_mod1[as.logical(data01_names_general_mod$dt_hidroxido)]
        vector_mod1 <- unique(vector_mod1)
        vector_mod1 <- na.omit(vector_mod1)
        
        vector_mod2 <- data01_names_general_mod$symbol
        vector_mod2 <- vector_mod2[as.logical(data01_names_general_mod$dt_oxacido)]
        vector_mod2 <- unique(vector_mod2)
        vector_mod2 <- na.omit(vector_mod2)
        
        if(sui_family_chem() == "sales"){
          vector_mod2 <- data01_names_general_mod$symbol
          vector_mod2 <- vector_mod2[as.logical(data01_names_general_mod$dt_hidracido)]
          vector_mod2 <- unique(vector_mod2)
          vector_mod2 <- na.omit(vector_mod2)
        }
        
        div(
          fluidRow(
            # column(3, selectInput(inputId = ns("sui_family_chem"), 
            #                       label = "Familia Química", 
            #                       choices = vector_familia_quimica)),
            column(3, selectInput(inputId = ns("sui_symbol_metal"), 
                                  label = "Elemento1 - Metal", 
                                  choices = vector_opt_elemento_metal())),
            column(3, selectInput(inputId = ns("sui_symbol_no_metal"), 
                                  label = "Elemento2 - No Metal", 
                                  choices = vector_opt_elemento_no_metal())),
            column(3, actionButton(ns("action_load"),
                                   label = "LOAD IA"))
          ),br(),
          
          uiOutput(ns("salida01")), br(), br(),
        )
      })
      
    })
}

