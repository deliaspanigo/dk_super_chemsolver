# # INICIACION
# list_armado <- list()
# list_armado$vector_coef <- c(2,1,2)
# list_armado$vector_sustancias <- c("H_{2}", "O_{2}", "H_{2}O")
# vector_coef <- c(2,1,2)
# vector_sustancias <- c("H_{2}", "O_{2}", "H_{2}O")
# 
# input_list <- op02_estequiometria_oxidos_esp(vector_coef, vector_sustancias)
# 
# # Los Inputssss
# # el input_list
# pos_fila_seleccionada01 <- 1
# pos_columna_seleccionada01 <- 3
# cantidad_usuario01 <- 14
# 
# pos_fila_seleccionada02 <- 2
# pos_columna_seleccionada02 <- 3
# cantidad_usuario02 <- 14
# 

op04_calculadora02_oxidos_esp <- function(input_list, 
                                          pos_fila_seleccionada01, 
                                          pos_columna_seleccionada01,
                                          cantidad_usuario01,
                                          pos_fila_seleccionada02, 
                                          pos_columna_seleccionada02,
                                          cantidad_usuario02) {

  # print(cantidad_usuario01)
  new_output01 <- op03_calculadora01_oxidos_esp(input_list = input_list, 
                                pos_fila_seleccionada = pos_fila_seleccionada01,
                                pos_columna_seleccionada = pos_columna_seleccionada01,
                                cantidad_usuario = cantidad_usuario01)


  new_output02 <- op03_calculadora01_oxidos_esp(input_list = input_list, 
                                                pos_fila_seleccionada = pos_fila_seleccionada02,
                                                pos_columna_seleccionada = pos_columna_seleccionada02,
                                                cantidad_usuario = cantidad_usuario02)


  renglones01 <- list(new_output01$renglon01, new_output01$renglon02)
  renglones02 <- list(new_output02$renglon01, new_output02$renglon02)
    
  la_ecuacion01 <- new_output01$paso_a_paso
  la_ecuacion02 <- new_output02$paso_a_paso
  
  la_prop01 <- new_output01$la_proporcion_redondeada
  la_prop02 <- new_output02$la_proporcion_redondeada
  
  cantidad01 <- cantidad_usuario01
  cantidad02 <- cantidad_usuario02
  
  sustancia01 <- rownames(new_output01$tabla_interna_01)[1]
  sustancia01 <- gsub("\\\\", "\\\\\\\\", sustancia01)
  
  sustancia02 <- rownames(new_output01$tabla_interna_01)[2]
  sustancia02 <- gsub("\\\\", "\\\\\\\\", sustancia02)
  
  unidad01 <- colnames(new_output01$tabla_interna_01)[pos_columna_seleccionada01]
  unidad02 <- colnames(new_output01$tabla_interna_01)[pos_columna_seleccionada02]  
  
  vector_pos01 <- c(pos_fila_seleccionada01, pos_columna_seleccionada01)
  names(vector_pos01) <- c("fila", "columna")
  
  vector_pos02 <- c(pos_fila_seleccionada02, pos_columna_seleccionada02)
  names(vector_pos02) <- names(vector_pos01)

    
  pack_elemento01 <- list(cantidad01, sustancia01, unidad01, la_prop01, vector_pos01)
  names(pack_elemento01) <- c("cantidad", "sustancia", "unidad", "la_prop", "vector_pos")
  pack_elemento02 <- list(cantidad02, sustancia02, unidad02, la_prop02, vector_pos02)
  names(pack_elemento02) <- names(pack_elemento01) 
  
  #######################3
  vector_veces <- c("vez", "veces")
  vector_veces01 <- ifelse(la_prop01 == 1, "vez", "veces")
  vector_veces02 <- ifelse(la_prop02 == 1, "vez", "veces")

  
  ###########################################################################
  caso_reaccion <- c()
  if(la_prop01 < la_prop02) caso_reaccion <- 1 else
    if(la_prop01 > la_prop02) caso_reaccion <- 2 else
      if(la_prop01 == la_prop02) caso_reaccion <- 3
  
  # Caso 1) El reactivo limitante es el 1er reactivo.
  # Caso 2) El reactivo limitante es el 2do reactivo.
  # Caso 3) ambos son limitantes
  ###################################################################

  pack_limitante <- list()
  pack_limitante[[1]] <- c()
  pack_limitante[[2]] <- c()

  pack_exceso <- list()
  pack_exceso[[1]] <- c()
  pack_exceso[[2]] <- c()
  
  
  if(caso_reaccion == 1){
    pack_limitante[[1]] <- pack_elemento01
    pack_exceso[[2]] <- pack_elemento02
  }   else
    
    if(caso_reaccion == 2){
      pack_limitante[[2]] <- pack_elemento02
      pack_exceso[[1]] <- pack_elemento01
    } else
      
      if(caso_reaccion == 3){
        pack_limitante[[1]] <- pack_elemento02
        pack_limitante[[2]] <- pack_elemento01
      }
  ##############################################################################
  pos_limitante <- list()
  if(caso_reaccion == 1){
    pos_limitante[[1]] <- pack_limitante[[1]]$vector_pos
    pos_limitante[[2]] <- c() 
  }   else
  if(caso_reaccion == 2){
    pos_limitante[[1]] <- c() 
    pos_limitante[[2]] <- pack_limitante[[2]]$vector_pos
    } else
      
      if(caso_reaccion == 3){
        pos_limitante[[1]] <- pack_limitante[[1]]$vector_pos
        pos_limitante[[2]] <- pack_limitante[[2]]$vector_pos
      }
  ##############################################################################  
  df_exterior <- data.frame()
  if(caso_reaccion == 1)     df_exterior <- new_output01$tabla_externa_02 else
    if(caso_reaccion == 2)   df_exterior <- new_output02$tabla_externa_02 else
      if(caso_reaccion == 3) df_exterior <- new_output02$tabla_externa_02
  
  ##############################################################################  
  cantidad_original_exceso <- c()
  cantidad_usada_exceso <- c()
  cantidad_excedente <- c()
  prop_usada <- c()
  prop_no_usada <- c()
  porc_usado <- c()
  porc_no_usado <- c()
  
  # Solo para caso 1 y 2
  if(caso_reaccion != 3){
    caso_mod <- c()
    
    # Aca tenemos que dar vuelva el valor de referencia
    # para que vaya a buscar la posicion correcta del rectivo en exceso
    # y no del reactivo limitante
    if(caso_reaccion == 1) caso_mod <- 2 else
      if(caso_reaccion == 2) caso_mod <- 1 

    cantidad_original_exceso <- pack_exceso[[caso_mod]]$cantidad
    cantidad_usada_exceso <-   df_exterior[pack_exceso[[caso_mod]]$vector_pos[1], pack_exceso[[caso_mod]]$vector_pos[2]]
    cantidad_usada_exceso <- strsplit(cantidad_usada_exceso, "=")[[1]]
    cantidad_usada_exceso <- cantidad_usada_exceso[length(cantidad_usada_exceso)]
    cantidad_usada_exceso <- gsub("[^0-9.]", "", cantidad_usada_exceso)
    cantidad_usada_exceso <- as.numeric(cantidad_usada_exceso)
    cantidad_excedente <- cantidad_original_exceso - cantidad_usada_exceso

    armado_dif <- c()
    armado_dif[1] <- paste0(cantidad_original_exceso, " - ", cantidad_usada_exceso)
    armado_dif[2] <- cantidad_excedente
    armado_dif <- paste0(armado_dif[1], " = ", armado_dif[2])
    
    prop_usada_calculada  <- cantidad_usada_exceso/cantidad_original_exceso
    prop_usada_calculada  <- redondear_como_humano(prop_usada_calculada, 8)
    prop_usada_redondeada <- redondear_como_humano(prop_usada_calculada, 4)
    dt_cambio <- prop_usada_calculada != prop_usada_redondeada
    
    porc_usado_redondeado <- prop_usada_redondeada*100
    porc_usado_redondeado <- paste0(porc_usado_redondeado, "\\%")
    
    prop_no_usada <- 1 - prop_usada_redondeada
    porc_no_usada <- prop_no_usada*100
    porc_no_usada <- paste0(porc_no_usada, "%")

    unidad_usada <- pack_exceso[[caso_mod]]$unidad
    sustancia_usada <- pack_exceso[[caso_mod]]$sustancia
    sustancia_usada_mod <- gsub("[\\()]", "", sustancia_usada)
    
    #############
    obj4_01 <- cantidad_usada_exceso*100
    paso_a_paso01 <- c()
    paso_a_paso01[1] <- "x="
    paso_a_paso01[2] <- "\\frac{obj1*obj2}{obj3}="
    paso_a_paso01[3] <- "\\frac{obj4}{obj3}="
    paso_a_paso01[4] <- paste0(porc_usado_redondeado)
    # if(dt_cambio){
    #   paso_a_paso01[5] <- paste0("\\approx",  porc_usado_redondeado)
    # }
    paso_a_paso01 <- paste0(paso_a_paso01, collapse = "")
    paso_a_paso01 <- paste0("\\(", paso_a_paso01, "\\)")
    paso_a_paso01 <- gsub("obj1", cantidad_usada_exceso, paso_a_paso01)
    paso_a_paso01 <- gsub("obj2", "100", paso_a_paso01)
    paso_a_paso01 <- gsub("obj3", cantidad_original_exceso, paso_a_paso01)
    paso_a_paso01 <- gsub("obj4", obj4_01, paso_a_paso01)
    
    
    
    
    paso_a_paso02 <- c()
    paso_a_paso02[1] <- "x="
    paso_a_paso02[2] <- "obj1 \\% - obj2 \\% = obj3 \\%"
    #paso_a_paso02[3] <- porc_no_usada
    # if(dt_cambio){
    #   paso_a_paso01[5] <- paste0("\\approx",  porc_usado_redondeado)
    # }
    aver_mod01 <- "100"
    aver_mod02 <- gsub("[^0-9.]", "", porc_usado_redondeado)
    aver_mod03 <- gsub("[^0-9.]", "", porc_no_usada)
    
    paso_a_paso02 <- paste0(paso_a_paso02, collapse = "")
    paso_a_paso02 <- gsub("obj1", aver_mod01, paso_a_paso02)
    paso_a_paso02 <- gsub("obj2", aver_mod02, paso_a_paso02)
    paso_a_paso02 <- gsub("obj3", aver_mod03, paso_a_paso02)
    
    paso_a_paso02 <- paste0("\\(", paso_a_paso02, "\\)")
    

    paso_a_paso03 <- c("Excedente = Total - Consumido")
    paso_a_paso03 <- paste0("\\(", paso_a_paso03, "\\)")
    
  }
  # 
  ##############################################################################  
  
  fraseA_01 <- "El coeficiente proporcional para el reactivo 1 es _la_prop01_.
                <br>
                <br>
                Con esta cantidad de reactivo 1 podría realizarse _la_prop01_ _veces01_ la reacción completa."
  fraseA_01 <- gsub("_la_prop01_",   la_prop01,  fraseA_01)
  fraseA_01 <- gsub("_veces01_", vector_veces01, fraseA_01)
  
  fraseA_02 <- "El coeficiente proporcional para el reactivo 2 es _la_prop02_.
                <br>
                <br>
                Con esta cantidad de reactivo 2 podría realizarse _la_prop02_ _veces02_ la reacción completa."
  
  fraseA_02 <- gsub("_la_prop02_",   la_prop02,      fraseA_02)
  fraseA_02 <- gsub("_veces02_",     vector_veces02, fraseA_02)
  
  fraseB_opc1 <- "Aquel reactivo que tenga el menor coeficiente proporcional será el reactivo limitante.<br>
                  El coeficiente proporcional para el reactivo 1 es _la_prop01_.<br>
                  El coeficiente proporcional para el reactivo 2 es _la_prop02_.<br>
                  El coeficiente proporcional para el reactivo 1 es menor que
                  el coeficiente proporcional de reactivo 2. <br>
                  El reactivo 1,  _sustancia01_,  es el reactivo limitante.<br>
                  La cantidad de _cantidad01_ _unidad01_ de _sustancia01_ será consumida completamente en la reacción.<br>
                 "
                  
  fraseB_opc2 <- "Aquel reactivo que tenga el menor coeficiente proporcional será el reactivo limitante.<br>
                  El coeficiente proporcional para el reactivo 1 es _la_prop01_.<br>
                  El coeficiente proporcional para el reactivo 2 es _la_prop02_.<br>
                  El coeficiente proporcional para el reactivo 2 es menor que
                  el coeficiente proporcional de reactivo 1. <br>
                  El reactivo 2,  _sustancia02_,  es el reactivo limitante.<br>
                  La cantidad de _cantidad02_ _unidad02_ de _sustancia02_ será consumida completamente en la reacción.<br>
                 "
  
  fraseB_opc3 <- "Los coeficientes proporcional es de ambos reactivos son iguales.<br>
                  Ambos reactivos son reactivos limitantes ya que ambos
                  serán consumidos consumidos completamente.<br>
                  No habrá excedente de ninguno de los dos.
                  "
  fraseB <- c()
  if(caso_reaccion == 1) fraseB <- fraseB_opc1 else
    if(caso_reaccion == 2) fraseB <- fraseB_opc2 else
      if(caso_reaccion == 3) fraseB <- fraseB_opc3 
  
  fraseB <- gsub("_la_prop01_",   la_prop01,   fraseB)
  fraseB <- gsub("_la_prop02_",   la_prop02,   fraseB)
  fraseB <- gsub("_sustancia01_", sustancia01, fraseB)
  fraseB <- gsub("_sustancia02_", sustancia02, fraseB)
  fraseB <- gsub("_cantidad01_", cantidad01, fraseB)
  fraseB <- gsub("_cantidad02_", cantidad02, fraseB)
  fraseB <- gsub("_unidad01_", unidad01, fraseB)
  fraseB <- gsub("_unidad02_", unidad02, fraseB)

  
  #######################################################
  fraseC_opc1 <- "Aquel reactivo que tenga el mayor coeficiente proporcional será el reactivo en exceso.<br>
                  El coeficiente proporcional para el reactivo 1 es _la_prop01_.<br>
                  El coeficiente proporcional para el reactivo 2 es _la_prop02_.<br>
                  El coeficiente proporcional para el reactivo 2 es mayor que
                  el coeficiente proporcional de reactivo 2. <br>
                  El reactivo 2,  _sustancia02_,  es el reactivo en exceso.<br>
                  La cantidad de _cantidad02_ _unidad02_ de _sustancia02_ no será consumida completamente en la reacción.<br>
                  Será necesario calcular cuál es la proporción que reacciona y la que no reacciona del reactivo en exceso.<br>
                 "
  
  fraseC_opc2 <- "Aquel reactivo que tenga el mayor coeficiente proporcional será el reactivo en exceso.<br>
                  El coeficiente proporcional para el reactivo 1 es _la_prop01_.<br>
                  El coeficiente proporcional para el reactivo 2 es _la_prop02_.<br>
                  El coeficiente proporcional para el reactivo 1 es mayor que
                  el coeficiente proporcional de reactivo 2. <br>
                  El reactivo 1,  _sustancia01_,  es el reactivo en exceso.<br>
                  La cantidad de _cantidad01_ _unidad01_ de _sustancia01_ no será consumida completamente en la reacción.<br>
                  Será necesario calcular cuál es la proporción que reacciona y la que no reacciona del reactivo en exceso.<br>
                 "
  
  fraseC_opc3 <- "Los coeficientes proporcionales de ambos reactivos son iguales.<br>
                  Ambos reactivos son reactivos limitantes ya que ambos
                  serán consumidos consumidos completamente.<br>
                  No habrá excedente de ninguno de los dos.<br>
                  Se consumirá completamente la cantidad de _cantidad01_ _unidad01_ de _sustancia01_
                  y la cantidad _cantidad02_ _unidad02_ de _sustancia02_.
                  "
  fraseC <- c()
  if(caso_reaccion == 1) fraseC <- fraseC_opc1 else
    if(caso_reaccion == 2) fraseC <- fraseC_opc2 else
      if(caso_reaccion == 3) fraseC <- fraseC_opc3 
  
  fraseC <- gsub("_la_prop01_",   la_prop01,   fraseC)
  fraseC <- gsub("_la_prop02_",   la_prop02,   fraseC)
  fraseC <- gsub("_sustancia01_", sustancia01, fraseC)
  fraseC <- gsub("_sustancia02_", sustancia02, fraseC)
  fraseC <- gsub("_cantidad01_", cantidad01, fraseC)
  fraseC <- gsub("_cantidad02_", cantidad02, fraseC)
  fraseC <- gsub("_unidad01_", unidad01, fraseC)
  fraseC <- gsub("_unidad02_", unidad02, fraseC)
  #######################################################################
  frase_H <- "Se realizan los cálculos estequimétricos para toda la reacción bajo 
  la condición del valor proporcional del reactivo limitante.<br>
  Multiplicamos cada valor de la estequimetría balanceada por el proporcional del reactivo 
  limitante que es _el_valor_prop_.<br>
  Marcamos en la tabla en color naranja a la información ingresada del reactivo limitante."  
  if(caso_reaccion == 1) frase_H <- gsub("_el_valor_prop_", la_prop01, frase_H) else 
    if(caso_reaccion == 2) frase_H <- gsub("_el_valor_prop_", la_prop02, frase_H) else 
      if(caso_reaccion == 3) frase_H <- gsub("_el_valor_prop_", la_prop01, frase_H)
  

  frase_I <- "
  La cantidad total de reactivo en exceso es _cantidad_a_ _unidad_ de _sustancia_ (100%).<br>
  Según los calculos realizados a partir del reactivo limitante la cantidad consumida
  de reactivo en exceso es _cantidad_b_ _unidad_ de _sustancia_ (_porc_A_).<br>
  La cantidad excedente de reactivo en exceso es entonces: _armado_dif_ _unidad_ de _sustancia_ (_porc_B_)."
  if(caso_reaccion != 3){
    frase_I <- gsub("_cantidad_a_", cantidad_original_exceso, frase_I)
    frase_I <- gsub("_cantidad_b_", cantidad_usada_exceso, frase_I)
    frase_I <- gsub("_armado_dif_", armado_dif, frase_I)
    frase_I <- gsub("_unidad_", unidad_usada, frase_I)
    frase_I <- gsub("_sustancia_", sustancia_usada, frase_I)
    frase_I <- gsub("_porc_A_", porc_usado_redondeado, frase_I)
    frase_I <- gsub("_porc_B_", porc_no_usada, frase_I)
  }  else
    if(caso_reaccion == 3) frase_I <- c("No hay reactivo en exceso.")

  
  #######################################################################

  renglon03_A <- c("\\(", cantidad_original_exceso, "\\;",  unidad_usada, "\\;",
                 "de", "\\;", sustancia_usada_mod, " \\rightarrow ", "\\;", 
                 "100\\%", "\\)")
  
  renglon03_A <- paste0(renglon03_A, collapse = "")
  
  
  renglon03_B <- c("\\(", cantidad_usada_exceso, "\\;",  unidad_usada, "\\;",
                 "de", "\\;", sustancia_usada_mod, " \\rightarrow ", "\\;", "x=", "\\)")
  
  renglon03_B <- paste0(renglon03_B, collapse = "")
  
    
  list_output <- list(la_prop01, la_prop02, fraseA_01, fraseA_02, 
                      fraseB, fraseC, renglones01, renglones02, 
                      la_ecuacion01, la_ecuacion02, df_exterior,
                      caso_reaccion, pos_limitante, frase_H, frase_I,
                      renglon03_A, renglon03_B, paso_a_paso01, 
                      paso_a_paso02, paso_a_paso03)  
  
  
  
  names(list_output) <- c("la_prop01", "la_prop02", "fraseA_01", "fraseA_02",
                          "fraseB","fraseC", "renglones01", "renglones02", 
                          "la_ecuacion01", "la_ecuacion02", "df_exterior",
                          "caso_reaccion", "pos_limitante", "frase_H", "frase_I",
                          "renglon03_A", "renglon03_B", "paso_a_paso01",
                          "paso_a_paso02", "paso_a_paso03")
  
  return(list_output) 
}

