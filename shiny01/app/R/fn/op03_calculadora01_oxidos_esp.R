# # # # INICIACION
# # list_armado <- list()
# # list_armado$vector_coef <- c(4,1,2)
# # list_armado$vector_sustancias <- c("Li", "O_{2}", "Li_{2}O")
# vector_coef <- c(4,1,2)
# vector_sustancias <- c("Li", "O_{2}", "Li_{2}O")
# 
# input_list <- op02_estequiometria_oxidos_esp(vector_coef, vector_sustancias)
# 
# # Los Inputssss
# # el input_list
# pos_fila_seleccionada <- 1
# pos_columna_seleccionada <- 3
# cantidad_usuario <- 5

op03_calculadora01_oxidos_esp <- function(input_list, pos_fila_seleccionada,
                                          pos_columna_seleccionada, cantidad_usuario){

  ########################
  
  # 1) Tabla externa
  tabla_externa_01 <- input_list[[8]]
  tabla_externa_01 <- tabla_externa_01[c(1,4:ncol(tabla_externa_01))]
  
  # Tabla interna, pero esta no sale
  tabla_interna_01 <- input_list[[9]]
  
  #####################################
  
  
  unidad_tabla <- colnames(tabla_interna_01)[pos_columna_seleccionada]
  sustancia_tabla <- rownames(tabla_interna_01)[pos_fila_seleccionada]
  sustancia_tabla <- gsub("\\\\", "\\\\\\\\", sustancia_tabla)
  sustancia_tabla_mod <- gsub("[\\()]", "", sustancia_tabla)
  
  cantidad_tabla <- tabla_interna_01[pos_fila_seleccionada, pos_columna_seleccionada]
  cantidad_tabla <- as.numeric(as.character(cantidad_tabla))
  unidad_usuario <- colnames(tabla_interna_01)[pos_columna_seleccionada]
  
  la_proporcion_calculada <- (cantidad_usuario*1)/cantidad_tabla
  la_proporcion_calculada <- redondear_como_humano(la_proporcion_calculada, 8)
  la_proporcion_redondeada <- redondear_como_humano(la_proporcion_calculada, 4)
  dt_cambio <- la_proporcion_calculada != la_proporcion_redondeada
  
  #############################
  
  # 2) Frase explicativa
  armado <- "Según la información de la ecuación estequiométrica balanceada 
      _cantidad_tabla_ _mi_unidad_ de _mi_sustancia_ son parte de 1 reacción completa.
      <br>
      La información ingresada de _cantidad_elegida_ _mi_unidad_ de _mi_sustancia_, ¿a qué proporción corresponde respecto de 1 reacción completa?.
      <br>
      Realizamos una regla de 3 simple para calcular un coeficiente de proporción.
      <br>
      <br>
      La cantidad de reactivos consumidos y productos generados que corresponde a los
      datos ingresados será igual al coeficiente de proporción calculado con la regla
      de 3 simple multiplicando a cada valor de la tabla estequimétrica para la ecuación balanceada.
  "
  
  armado <- gsub("_cantidad_tabla_", cantidad_tabla, armado)
  armado <- gsub("_mi_unidad_", unidad_tabla, armado)
  armado <- gsub("_mi_sustancia_", sustancia_tabla, armado)
  armado <- gsub("_cantidad_elegida_", cantidad_usuario, armado)
  
  # 2) Regla de 3 simple
  renglon01 <- c("\\(", cantidad_tabla, "\\;",  unidad_tabla, "\\;",
                 "de", "\\;", sustancia_tabla_mod, " \\rightarrow ", "\\;", 
                 "1\\; reacción\\; completa", "\\)")
  
  renglon01 <- paste0(renglon01, collapse = "")
  
  
  renglon02 <- c("\\(", cantidad_usuario, "\\;",  unidad_tabla, "\\;",
                 "de", "\\;", sustancia_tabla_mod, " \\rightarrow ", "\\;", "x=", "\\)")
  
  renglon02 <- paste0(renglon02, collapse = "")
  
  # 3) Paso a paso
  paso_a_paso <- c()
  paso_a_paso[1] <- "x="
  paso_a_paso[2] <- "\\frac{obj1*obj2}{obj3}="
  paso_a_paso[3] <- "\\frac{obj1}{obj3}="
  paso_a_paso[4] <- paste0(la_proporcion_calculada)
  if(dt_cambio){
    paso_a_paso[5] <- paste0("\\approx",  la_proporcion_redondeada)
  }
  paso_a_paso <- paste0(paso_a_paso, collapse = "")
  paso_a_paso <- paste0("\\(", paso_a_paso, "\\)")
  paso_a_paso <- gsub("obj1", cantidad_usuario, paso_a_paso)
  paso_a_paso <- gsub("obj2", 1, paso_a_paso)
  paso_a_paso <- gsub("obj3", cantidad_tabla, paso_a_paso)
  
  
  frase_extra <- "El coeficiente de proporción es _coef_nuevo_.<br>
                  Los _cantidad_usuario_ _unidad_tabla_ de _sustancia_tabla_mod_
                  implican _coef_nuevo_ veces todas las cantidades de la ecuación balanceada.
                  <br>
                  <br>
                  Para obtener las cantidades que corresponden a la información ingresada 
                  multiplicamos por _coef_nuevo_ a todos los valores de la tabla de estequimetría balaceada.
  "
  
  frase_extra <- gsub("_coef_nuevo_", la_proporcion_redondeada, frase_extra)
  frase_extra <- gsub("_cantidad_usuario_", cantidad_usuario, frase_extra)
  frase_extra <- gsub("_unidad_tabla_", unidad_tabla, frase_extra)
  frase_extra <- gsub("_sustancia_tabla_mod_", sustancia_tabla, frase_extra)
  
  
  tabla_interna_02 <- tabla_interna_01
  
  # Gramos mod
  list_moles   <- as.list(tabla_interna_02$"moles")
  list_moles   <- lapply(list_moles, function(x){
    x1 <- x
    x2 <- paste0(la_proporcion_redondeada, "*", x1)
    x3 <- eval(parse(text= x2))
    salida <- paste0(x2, "=", x3, collapse = "")
    salida <- paste0("\\(", salida, "\\)")
    salida <- paste0(salida, " moles")
    salida
  })
  vector_moles <- unlist(list_moles)
  tabla_interna_02$"moles" <- vector_moles
  
  
  # Gramos mod
  list_gramos   <- as.list(tabla_interna_02$"gramos")
  list_gramos   <- lapply(list_gramos, function(x){
    x1 <- x
    x2 <- paste0(la_proporcion_redondeada, "*", x1)
    x3 <- eval(parse(text= x2))
    salida <- paste0(x2, "=", x3, collapse = "")
    salida <- paste0("\\(", salida, "\\)")
    salida <- paste0(salida, " gramos")
    salida
  })
  vector_gramos <- unlist(list_gramos)
  tabla_interna_02$"gramos" <- vector_gramos
  
  # Litros mod
  list_litros   <- as.list(tabla_interna_02$"litros")
  list_litros   <- lapply(list_litros, function(x){
    x1 <- x
    x2 <- paste0(la_proporcion_redondeada, "*", x1)
    x3 <- eval(parse(text= x2))
    salida <- paste0(x2, "=", x3, collapse = "")
    salida <- paste0("\\(", salida, "\\)")
    salida <- paste0(salida, " litros")
    salida
  })
  vector_litros <- unlist(list_litros)
  tabla_interna_02$"litros" <- vector_litros

  # Avogadro mod
  unidad_avogadro <- tabla_externa_01[,ncol(tabla_externa_01)]
  unidad_avogadro <- strsplit(unidad_avogadro, " ")
  unidad_avogadro <- lapply(unidad_avogadro, function(x){x[length(x)]})
  unidad_avogadro <- unlist(unidad_avogadro)
  
  list_avogadro   <- as.list(tabla_interna_02$"avogadro")
  list_avogadro   <- lapply(list_avogadro, function(x){
    x1 <- x
    x2 <- paste0(la_proporcion_redondeada, "*", x1)
    x3 <- eval(parse(text= x2))
    salida <- paste0(x2, "=", x3, collapse = "")
    salida <- gsub("e([0-9]+)", " \\\\times 10^{\\1}", salida) 
    salida <- gsub("e\\+([0-9]+)", " \\\\times 10^{\\1}", salida) 
    #salida <- paste0(salida, " avogadro")
    salida <- paste0("\\(", salida, "\\)")
    salida
  })
  vector_avogadro <- unlist(list_avogadro)
  vector_avogadro <- paste0(vector_avogadro, " ", unidad_avogadro)
  tabla_interna_02$"avogadro" <- vector_avogadro
  
  
  tabla_externa_02 <- tabla_interna_02
  
  list_output <- list(la_proporcion_redondeada, tabla_externa_01, armado, renglon01, renglon02, 
                      paso_a_paso, tabla_externa_02, tabla_interna_01, frase_extra)  

  names(list_output) <- c("la_proporcion_redondeada", "tabla_externa_01", "armado", "renglon01", 
                          "renglon02", "paso_a_paso", "tabla_externa_02", "tabla_interna_01", 
                          "frase_extra")
  
  return(list_output) 
}      