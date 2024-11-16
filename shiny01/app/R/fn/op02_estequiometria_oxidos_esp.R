
op02_estequiometria_oxidos_esp <- function(vector_coef, vector_sustancias){
  
  
 
  
  vector_coef_alias <- c("_coef1_", "_coef2_", "_coef3_")
  ###################################################################
  
  # Vector de las sustancias
  vector_sustancias02 <- vector_sustancias
  
  vector_sustancias <- paste0("\\(", vector_sustancias, "\\)")
  vector_sustancias_alias <- c("_sustancia1_", "_sustancia2_", "_sustancia3_")
  
  vector_coef02 <- vector_coef
  vector_coef02[vector_coef02 == 1] <- ""
  vector_sustancias02 <- paste0(vector_coef02, " ", vector_sustancias02)
  vector_sustancias02 <- paste0("\\(", vector_sustancias02, "\\)")
  
  # Roles
  vector_rol <- c("Reactivo", "Reactivo", "Producto")
  # Deteccion de gas
  dt_gas <- length(gsub("[^0-9]", "", vector_sustancias[1])) == 1
  
  # Molecula o atomo - Moleculas o atomos
  vector_rotulo01 <- c(NA, "molécula", "molécula")
  vector_rotulo02 <- c(NA, "moléculas", "moléculas")
  vector_rotulo01[1] <- ifelse(dt_gas, "molécula", "átomo")
  vector_rotulo02[1] <- ifelse(dt_gas, "moléculas", "átomos")
  vector_rotulo01_alias <- c("_rotulo01_1_", "_rotulo01_2_", "_rotulo01_3_")
  vector_rotulo02_alias <- c("_rotulo02_1_", "_rotulo02_2_", "_rotulo02_3_")
  
  # Mol o moles                         
  vector_moles <- ifelse(vector_coef>1, "moles", "mol")
  vector_moles_alias <- c("_mol1_", "_mol2_", "_mol3_")
  
  # Elementos
  vector_elementos <- vector_sustancias[c(1,2)]
  vector_elementos <- sapply(vector_elementos, function(x) gsub("[^a-zA-Z]", "", x))
  #vector_elementos <- sapply(vector_elementos, function(x) {strsplit(x, "")[[1]]})
  vector_elementos <- unlist(vector_elementos)
  vector_elementos <- unique(vector_elementos)
  vector_elementos_alias <- c("_elemento1_", "_elemento2_")
  
  # Posicion de los elementos en la tabla periodica
  vector_pos_elementos <- which(data00_tabla_periodica$symbol%in%vector_elementos)
  
  # Peso u.m.a. de los elementos de la tabla
  vector_uma <- data00_tabla_periodica$uma_mod[vector_pos_elementos]
  vector_uma_alias <- c("_uma1_", "_uma2_")
  names(vector_uma) <- vector_elementos
  
  
  vector_peso <- vector_sustancias
  vector_peso <-   gsub("[^a-zA-Z0-9]", "", vector_peso)
  vector_peso <-   gsub("([A-Z])([A-Z])", "\\11\\2", vector_peso)
  vector_peso <-   gsub("([a-zA-Z])$", "\\11", vector_peso)
  vector_peso <-   gsub("(\\d)([a-zA-Z])", "\\1 + \\2", vector_peso)
  vector_peso <-   gsub("([a-zA-Z])(\\d)", "\\1*\\2", vector_peso)
  vector_peso <- stri_replace_all_fixed(vector_peso, vector_elementos, vector_uma, vectorize_all = FALSE)
  list_peso <- as.list(vector_peso)
  list_peso <- lapply(list_peso, function(x){
    x1 <- x
    x2 <- NA
    
    if(grepl("\\+", x)) {
      x1 <- strsplit(x1, "[+]")[[1]]
      x1 <- gsub(" ", "", x1)
      x1 <- sapply(x1, function(y){eval(parse(text=y))})
      x1 <- paste0(x1, collapse = " + ")
    }
    x2 <- eval(parse(text = x1))
    vector_salida <- c(x, x1, x2)
    vector_salida <- unique(vector_salida)
    return(vector_salida)
  })
  vector_fusion_peso <- unlist(lapply(list_peso, paste0, collapse = " = "))
  vector_fusion_peso_alias <- c("_calculo1_uma_largo_", "_calculo2_uma_largo_", "_calculo3_uma_largo_")
  vector_final_peso <- unlist(lapply(list_peso, function(x){x[length(x)]}))
  vector_final_peso_alias <- c(" _calculo1_uma_final_", " _calculo2_uma_final_", " _calculo3_uma_final_")
  
  vector_final_gramos01 <- paste0(vector_coef, " * ", vector_final_peso)
  vector_final_gramos02 <- sapply(vector_final_gramos01, function(x){eval(parse(text=x))})
  names(vector_final_gramos02) <- NULL
  vector_final_gramos02_alias <- c("_calculo1_p02_",
                                   "_calculo2_p02_",
                                   "_calculo3_p02_")
  
  vector_final_gramos03 <- paste0(vector_final_gramos01, " = ", vector_final_gramos02)
  
  vector_final_gramos_alias <- c("_calculo1_p03_largo_",
                                 "_calculo2_p03_largo_",
                                 "_calculo3_p03_largo_")
  
  vector_peso_total <- c()
  vector_peso_total[1] <- paste0(vector_final_gramos02[1], " + ",
                                 vector_final_gramos02[2], " = ",
                                 sum(as.numeric(vector_final_gramos02[c(1,2)])))
  vector_peso_total[2] <- vector_final_gramos02[3]
  
  vector_peso_total_alias <- c("_peso_total1_", "_peso_total2_")
  
  
  list_volumen <- as.list(vector_coef)
  list_volumen <- lapply(list_volumen, function(x){
    x1 <- paste0(x, "*22.4")
    x2 <- eval(parse(text=x1))
    x3 <- paste0(x1, " = ", x2)
    c(x1, x3, x2)
    
    
  })
  vector_volumen02 <- lapply(list_volumen, function(x){(x[2])})
  vector_volumen02_alias <- c("_volumen1_largo_", "_volumen2_largo_", "_volumen3_largo_")
  
  vector_volumen03 <- unlist(lapply(list_volumen, function(x){(x[3])}))
  vector_volumen03_alias <- c("_volumen1_final_", "_volumen2_final_", "_volumen3_final_")
  
  
  list_avogadro <- as.list(vector_coef)
  list_avogadro <- lapply(list_avogadro, function(x){
    x1 <- paste0(x, "*6.02e23")
    x2 <- eval(parse(text=x1))
    x3 <- paste0(x1, " = ", x2)
    c(x1, x3, x2)
    
    
  })
  vector_avogadro03 <- unlist(lapply(list_avogadro, function(x){(x[3])}))
  
  list_avogadro_mod <- list_avogadro
  list_avogadro_mod <- lapply(list_avogadro_mod, function(x){
    
    #x <- gsub("6.02e23", "6.02 \\times 10^{23}", x)
    x <- gsub("e([0-9]+)", " \\\\times 10^{\\1}", x) 
    x <- gsub("e\\+([0-9]+)", " \\\\times 10^{\\1}", x) 
    x <- paste0("\\(", x, "\\)")
    x
  })
  
  vector_avogadro02_mod <- lapply(list_avogadro_mod, function(x){(x[2])})
  vector_avogadro02_mod_alias <- c("_avogadro1_mod_largo_", "_avogadro2_mod_largo_", "_avogadro3_mod_largo_")
  
  vector_avogadro03_mod <- unlist(lapply(list_avogadro_mod, function(x){(x[3])}))
  vector_avogadro03_mod_alias <- c("_avogadro1_mod_final_", "_avogadro2_mod_final_", "_avogadro3_mod_final_")
  
  
  # #########
  # df_interno <- data.frame()
  # df_interno$"rol" <- vector_rol
  # df_interno$"sustancia" <- vector_sustancias
  
  df_interno <-  data.frame(
    "rol" = vector_rol,
    "en_reaccion" = vector_sustancias02,
    "moles" = vector_coef,
    "gramos" = vector_final_gramos02,
    "litros" = vector_volumen03,
    "avogadro" =  vector_avogadro03,
    stringsAsFactors = FALSE, row.names = NULL)
  rownames(df_interno) <- vector_sustancias
  
  #df_interno <- t(df_interno)
  
  df_externo <- data.frame(
    "rol" = vector_rol,
    "sustancia" = vector_sustancias,
    "u.m.a." =  paste0(vector_final_peso, " u.m.a."),
    "en_reaccion" = vector_sustancias02,
    "moles" = paste0(vector_coef, " ",vector_moles),
    "peso" = paste0(vector_final_gramos02, " gramos"),
    "litros" = paste0(vector_volumen03, " litros"),
    "avogadro" =  paste0(vector_avogadro03_mod, " ", vector_rotulo02),
    stringsAsFactors = FALSE, row.names = NULL)
  rownames(df_externo) <- vector_sustancias
  #df_externo <- t(df_externo)
  
  
  
  ############3
  #######
  parte01 <- list()
  parte01[[1]] <- "
                        Determinamos los elementos químicos que intervienen en toda la ecuación química
                        y buscamos sus pesos atómicos en la tabla periódica. <br>
                        En este caso los elementos _elemento1_ y el _elemento2_.<br>
                        El peso atómico del _elemento1_ es: _uma1_ u.m.a..<br>
                        El peso atómico del _elemento2_ es: _uma2_ u.m.a.."
  parte01[[1]] <- stri_replace_all_fixed(parte01[[1]], vector_elementos_alias, vector_elementos, vectorize_all = FALSE)
  parte01[[1]] <- stri_replace_all_fixed(parte01[[1]], vector_uma_alias, vector_uma, vectorize_all = FALSE)
  
  
  parte02 <- list()
  parte02[[1]] <- "
                        Para cada compuesto químico de la ecuación química determinamos el peso en unidades de masa atómica (u.m.a.).<br>
                        Debemos tener en cuenta los subíndices de cada compuesto. La ausencia de subíndice recordemos debe ser considerada como 1.<br>
                        El peso de 1 _rotulo01_1_ del reactivo _sustancia1_ es: _calculo1_uma_largo_ u.m.a..<br>
                        El peso de 1 _rotulo01_2_ del reacitvo _sustancia2_ es: _calculo2_uma_largo_ u.m.a..<br>
                        El peso de 1 _rotulo01_3_ del producto _sustancia3_ es: _calculo3_uma_largo_ u.m.a.."
  
  
  
  parte02[[1]] <- stri_replace_all_fixed(parte02[[1]], vector_sustancias_alias,  vector_sustancias, vectorize_all = FALSE)
  parte02[[1]] <- stri_replace_all_fixed(parte02[[1]], vector_fusion_peso_alias, vector_fusion_peso, vectorize_all = FALSE)
  parte02[[1]] <- stri_replace_all_fixed(parte02[[1]], vector_rotulo01_alias,    vector_rotulo01, vectorize_all = FALSE)
  
  
  parte03 <- list()
  parte03[[1]] <- "
                        Un mol de cualquier sustancia pesa en gramos lo mismo que su peso en unidades de masa atómica (u.m.a.).<br>
                        Tomamos los valores recién calculados para los pesos en u.m.a. y los expresamos en gramos.<br>
                        El peso de 1 mol del reactivo _sustancia1_ es:  _calculo1_uma_final_ gramos.<br>
                        El peso de 1 mol del reacitvo _sustancia2_ es:  _calculo2_uma_final_ gramos.<br>
                        El peso de 1 mol del producto _sustancia3_ es:  _calculo3_uma_final_ gramos."
  
  parte03[[1]] <- stri_replace_all_fixed(parte03[[1]], vector_sustancias_alias,  vector_sustancias, vectorize_all = FALSE)
  parte03[[1]] <- stri_replace_all_fixed(parte03[[1]], vector_final_peso_alias,   vector_final_peso, vectorize_all = FALSE)
  
  
  
  parte04 <- list()
  parte04[[1]] <- "
                        La cantidad de moles de cada reactivo y producto está representada por los coeficientes de la ecuación química balanceada.<br>
                        Para el reactivo _sustancia1_ son: _coef1_ _mol1_.<br>
                        Para el reactivo _sustancia2_ son: _coef2_ _mol2_.<br>
                        Para el producto _sustancia3_ son: _coef3_ _mol3_.<br>
                        <br>
                        En la ecuación química balanceada reaccionan _coef1_ _mol1_ de _sustancia1_ y 
                        _coef2_ _mol2_ de _sustancia2_ para dar lugar a 
                        _coef3_ _mol3_ de _sustancia3_.
        "
  
  
  parte04[[1]] <- stri_replace_all_fixed(parte04[[1]], vector_sustancias_alias, vector_sustancias, vectorize_all = FALSE)
  parte04[[1]] <- stri_replace_all_fixed(parte04[[1]], vector_moles_alias, vector_moles, vectorize_all = FALSE)
  parte04[[1]] <- stri_replace_all_fixed(parte04[[1]], vector_coef_alias, vector_coef, vectorize_all = FALSE)
  parte04[[1]] <- stri_replace_all_fixed(parte04[[1]], vector_final_gramos_alias, vector_final_gramos03, vectorize_all = FALSE)
  parte04[[1]] <- stri_replace_all_fixed(parte04[[1]], vector_peso_total_alias, vector_peso_total, vectorize_all = FALSE)
  
  
  parte05 <- list()
  parte05[[1]] <- "
                        Los gramos de cada reactivo y producto de la ecuación química balanceada serán calculados como
                        la multiplicación entre los respectivos coeficientes y pesos molares.<br>
                        El peso de _coef1_ _mol1_ del reactivo _sustancia1_ es: _calculo1_p03_largo_ gramos.<br>
                        El peso de _coef2_ _mol2_ del reacitvo _sustancia2_ es: _calculo2_p03_largo_ gramos.<br>
                        El peso de _coef3_ _mol3_ del producto _sustancia3_ es: _calculo3_p03_largo_ gramos.<br>
                        <br>
                        En la ecuación química balanceada reaccionan _calculo1_p02_ gramos de _sustancia1_ y 
                        _calculo2_p02_ gramos de _sustancia2_ para dar lugar a 
                        _calculo3_p02_ gramos de _sustancia3_.
                        <br>
                        El peso total de reactivos es: _peso_total1_ gramos.<br>
                        El peso total de productos es: _peso_total2_ gramos.<br>
                        Cumplimos la 'Ley de Conservación'.
        "
  
  
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_sustancias_alias, vector_sustancias, vectorize_all = FALSE)
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_moles_alias, vector_moles, vectorize_all = FALSE)
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_coef_alias, vector_coef, vectorize_all = FALSE)
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_final_gramos_alias, vector_final_gramos03, vectorize_all = FALSE)
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_peso_total_alias, vector_peso_total, vectorize_all = FALSE)
  parte05[[1]] <- stri_replace_all_fixed(parte05[[1]], vector_final_gramos02_alias, vector_final_gramos02, vectorize_all = FALSE)
  
  
  parte06 <- list()
  parte06[[1]] <- "
                        El volumen en litros de cada reactivo y producto de la ecuación química balanceada serán calculados como
                        la multiplicación entre los respectivos coeficientes y el volumne que ocuparía 1 mol de cada sustancia (22.4 litros).<br>
                        El volumen de _coef1_ _mol1_ del reactivo _sustancia1_ es: _volumen1_largo_ litros.<br>
                        El volumen de _coef2_ _mol2_ del reacitvo _sustancia2_ es: _volumen2_largo_ litros.<br>
                        El volumen de _coef3_ _mol3_ del producto _sustancia3_ es: _volumen3_largo_ litros.<br>
                        <br>
                        En la ecuación química balanceada reaccionan _volumen1_final_ litros de _sustancia1_ y 
                        _volumen2_final_ litros de _sustancia2_ para dar lugar a 
                        _volumen3_final_ litros de _sustancia3_.
        "
  parte06[[1]] <- stri_replace_all_fixed(parte06[[1]], vector_sustancias_alias, vector_sustancias, vectorize_all = FALSE)
  parte06[[1]] <- stri_replace_all_fixed(parte06[[1]], vector_moles_alias, vector_moles, vectorize_all = FALSE)
  parte06[[1]] <- stri_replace_all_fixed(parte06[[1]], vector_coef_alias, vector_coef, vectorize_all = FALSE)
  parte06[[1]] <- stri_replace_all_fixed(parte06[[1]], vector_volumen02_alias, vector_volumen02, vectorize_all = FALSE)
  parte06[[1]] <- stri_replace_all_fixed(parte06[[1]], vector_volumen03_alias, vector_volumen03, vectorize_all = FALSE)
  
  
  parte07 <- list()
  parte07[[1]] <- "
                        El número de átomos o moléculas (según corresponda) de cada reactivo y producto de la ecuación química balanceada serán calculados como
                        la multiplicación entre los respectivos coeficientes y el número de Avogadro (\\(6.02 \\times 10^{23}\\)).<br>
                        El número de _rotulo02_1_ en _coef1_ _mol1_ del reactivo _sustancia1_ es: _avogadro1_mod_largo_ _rotulo02_1_.<br>
                        El número de _rotulo02_2_ en _coef2_ _mol2_ del reacitvo _sustancia2_ es: _avogadro2_mod_largo_ _rotulo02_2_.<br>
                        El número de _rotulo02_3_ en _coef3_ _mol3_ del producto _sustancia3_ es: _avogadro3_mod_largo_ _rotulo02_3_.<br>
                        <br>
                        En la ecuación química balanceada reaccionan _avogadro1_mod_final_ _rotulo02_1_ de _sustancia1_ y 
                        _avogadro2_mod_final_ _rotulo02_2_ de _sustancia2_ para dar lugar a 
                        _avogadro3_mod_final_ _rotulo02_3_ de _sustancia3_.
        "
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_sustancias_alias, vector_sustancias, vectorize_all = FALSE)
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_moles_alias, vector_moles, vectorize_all = FALSE)
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_coef_alias, vector_coef, vectorize_all = FALSE)
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_avogadro02_mod_alias, vector_avogadro02_mod, vectorize_all = FALSE)
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_avogadro03_mod_alias, vector_avogadro03_mod, vectorize_all = FALSE)
  parte07[[1]] <- stri_replace_all_fixed(parte07[[1]], vector_rotulo02_alias, vector_rotulo02, vectorize_all = FALSE)
  
  output_list <- list(parte01, parte02, parte03, parte04, parte05, 
                      parte06, parte07, df_externo, df_interno)
  
  
  return(output_list)
}
