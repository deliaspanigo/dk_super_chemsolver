
calcular_cantidades <- function(df_PT, df_oxidos, selected_pos){
  # 
  # df_PT <- data00_tabla_periodica
  # df_oxidos <- data01_names[[1]]
  # selected_pos <- 1
  
  vector_final <- na.omit(df_oxidos$balance_final01)
  
  selected_final <- vector_final[selected_pos]
  selected_final
  
  #selected_final <- "$$2H_{2} + O_{2} \\rightarrow 2H_{2}O$$"
  
  step04_A <- selected_final
  step04_A <- strsplit(step04_A, "rightarrow")[[1]]
  step04_A <-strsplit(step04_A, "\\+")
  step04_A <- lapply(step04_A, function(x) gsub("[$\\ ]", "", x))
  step04_A 
  
  step04_B <- step04_A
  step04_B <- lapply(step04_B, function(x) gsub("^\\d+", "", x))
  step04_B 
  
  step03_A <- step04_A
  step03_A <- lapply(step03_A, function(x) gsub("[^a-zA-Z0-9()]", "", x))
  step03_A
  
  step03_B <- step03_A
  step03_B <- lapply(step03_B, function(x) gsub("^\\d+", "", x))
  step03_B
  
  # Coeficientes
  step02 <- step03_A
  step02 <- lapply(step02, function(x) gsub("^[a-zA-Z].*", "1", x))
  step02 <- lapply(step02, function(x) gsub("^(\\d).*", "\\1", x))
  step02
  
  
  step01 <- step03_A
  step01 <- lapply(step01, function(x) gsub("[^a-zA-Z]", "", x))
  step01 <- paste0(unlist(step01), collapse = "")
  step01 <- strsplit(step01, "")[[1]]
  step01 <- unique(step01)
  step01
  
  ################################################################################
  df_step01 <- data.frame(matrix(NA, length(step01), 4))
  vector_pos_rows01 <- which(df_PT$symbol%in%step01)
  df_step01[,1] <- step01
  df_step01[,2] <- df_PT$uma_mod[vector_pos_rows01]
  df_step01[,3] <- paste0(df_step01[,2], "\\; uma")
  df_step01[,4] <- paste0(df_step01[,2], "\\; gramos")
  df_step01
  
  ################################################################################
  cantidad03 <- length(unlist(step03_B))
  df_step03 <- data.frame(matrix(NA, cantidad03, 10))
  df_step03[,1] <- unlist(step04_B)
  df_step03[,2] <- unlist(step03_B)
  
  df_step03[,2] <-   gsub("(\\d)([a-zA-Z])", "\\1 + \\2", df_step03[,2])
  df_step03[,2] <-   gsub("([a-zA-Z])(\\d)", "\\1*\\2", df_step03[,2])
  df_step03[,2] <-   gsub("([a-zA-Z])([a-zA-Z])", "\\1+\\2", df_step03[,2])
  
  df_step03[,3] <- df_step03[,2]
  for (i in 1:nrow(df_step01)) {
    df_step03[,3] <- gsub(df_step01$X1[i], df_step01$X2[i], df_step03[,3])
  }
  
  
  df_step03[,4] <- df_step03[,3]
  for(k4 in 1:nrow(df_step03)){
    selected_row <- df_step03[k4,4]
    list_row <- strsplit(selected_row, "\\+")[[1]]
    list_row <- gsub(" ", "", list_row)
    list_row <- sapply(list_row, function(x) {
      eval(parse(text = x))
    })
    list_row <- as.character(list_row)
    vector_row <- paste0(list_row, collapse = " + ")
    df_step03[k4,4] <- vector_row
  }
  
  
  df_step03[,5] <- df_step03[,4]
  for(k5 in 1:nrow(df_step03)){
    selected_row <- df_step03[k4,4]
    vector_row <- sapply(selected_row, function(x) {
      eval(parse(text = x))
    })
    df_step03[k4,5] <- vector_row
  }
  df_step03[,1] <- paste0(df_step03[,1])
  
  avogadro_standard <- "6.02"
  el_exponente_inicial <- 23
  avogadro_especial <- paste0(avogadro_standard, "e", el_exponente_inicial)
  anexo01 <- paste0("\\times 10^{",el_exponente_inicial,"}")
  anexo02 <- paste0(avogadro_standard, anexo01)
  
  litro_standard <- 22.4
  
  df_step03[,6] <- paste0(df_step03[,5], "\\; uma")
  df_step03[,7] <- paste0(df_step03[,5], "\\; gramos")
  
  df_step03[,8] <- rep("1\\; mol", nrow(df_step03))
  df_step03[,9] <- rep(paste0(anexo02, "\\; átomos"), nrow(df_step03))
  
  df_step03[,10] <- rep(paste0(litro_standard, "\\; litros"), nrow(df_step03))
  ################################################################################
  
  df_step04 <- data.frame(matrix(NA, nrow(df_step03), 14))
  df_step04[, 1] <- unlist(step04_A) 
  df_step04[, 2] <- paste0(unlist(step02), "*", unlist(step04_B))# paste0("$$", unlist(step02),"*", unlist(step04_B), "$$")
  df_step04[, 3] <- paste0(unlist(step02), "*", df_step03[,5])
  df_step04[, 4] <- sapply(df_step04[,3], function(x) {  eval(parse(text = x)) })
  
  df_step04[, 5] <- paste0(df_step04[,4], "\\; uma")
  df_step04[, 6] <- paste0(df_step04[,4], "\\; gramos")
  df_step04[, 7] <- paste0(unlist(step02), "\\; moles")
  
  df_step04[, 8] <- paste0(unlist(step02), "*", paste0(avogadro_standard, anexo01))
  df_step04[, 9] <- paste0(unlist(step02), "*", paste0(avogadro_standard, anexo01))
  
  new_valor <- paste0(unlist(step02), "*", avogadro_especial)
  new_valor <- sapply(new_valor, function(x) {  eval(parse(text = x)) })
  new_valor <- as.character(new_valor)
  new_valor <- strsplit(new_valor, "e[+]")
  los_primeros <- primeros_elementos <- sapply(new_valor, function(x) x[1])
  los_segundos <- primeros_elementos <- sapply(new_valor, function(x) x[2])
  los_terceros <- paste0(los_primeros, "\\times 10^{",los_segundos, "}")
  df_step04[,10] <- los_terceros
  
  df_step04[,11] <- paste0(df_step04[,8], " = ", df_step04[,10], "\\; átomos")
  
  
  df_step04[,12] <- paste0(unlist(step02), "*", litro_standard)
  df_step04[,13] <- paste0(sapply(df_step04[,12], function(x) {eval(parse(text = x))}), "\\; litros")
  df_step04[,14] <- paste0(df_step04[,12], " = ", df_step04[,13])
  #df_step04[,14] <- sapply(df_step04[,13], function(x) {  eval(parse(text = x)) })
  
  
  #df_step04[,1] <- paste0(df_step04[,1])
  
  
  ################################################################################
  df_step05 <- data.frame(matrix(NA, length(step04_A), 3))
  new_list <- lapply(step04_A, length)
  new_fin <- cumsum(new_list)
  new_inicio <- c(1,new_fin[2:length(new_fin)])
  new_pos <- sapply(1:length(new_fin), function(x){
    new_inicio[x]:new_fin[x]
  })
  new_data <- lapply(new_pos, function(x){df_step04[x,4]})
  new_data <- lapply(new_data, function(x){paste0(x, collapse = " + ")})
  df_step05[,1] <- unlist(lapply(step04_A, paste0, collapse = " + "))
  df_step05[,2] <- unlist(new_data)
  df_step05[,3] <- sapply(df_step05[,2], function(x) {  eval(parse(text = x)) })
  
  df_step05[,4] <- paste0(df_step05[,3], "\\; uma")
  df_step05[,5] <- paste0(df_step05[,3], "\\; gramos")
  ################################################################################
  
  vector_ref_moles <- c("un mol", "dos moles", "tres moles", "cuatro moles", 
                        "cinco moles", "seis moles", "siete moles",
                        "ocho moles", "nueve moles", "diez moles")
  
  vector_coef <- as.numeric(as.character(unlist(step02)))
  vector_cantidad_moles <- c("_cantidad01_", "_cantidad02_", "_cantidad03_")
  vector_reemplazo_gramos <- paste0(df_step04[,4], " gramos")
  vector_reemplazo_moles <- vector_ref_moles[vector_coef]
  vector_reemplazo_atomos <- paste0(df_step04[,10], " átomos")
  vector_reemplazo_litros <- paste0(df_step04[,13])
  vector_reemplazo_litros <- gsub(";", "", vector_reemplazo_litros)
  
  vector_sustancia <- c("_sustancia01_", "_sustancia02_", "_sustancia03_")
  vector_reemplazo_sustancias <- unlist(step03_B)
  
  
  frase_explicativa_general <- c("Reaccionan _cantidad01_ de _sustancia01_ y _cantidad02_ 
                              de _sustancia02_ para formar _cantidad03_ de _sustancia03_.")
  
  # 1) Frase gramos
  frase01 <- frase_explicativa_general
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase01 <- gsub(pattern = vector_cantidad_moles[k1],
                    replacement = vector_reemplazo_gramos[k1], 
                    x = frase01)
  }
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase01 <- gsub(pattern = vector_sustancia[k1],
                    replacement = vector_reemplazo_sustancias[k1], 
                    x = frase01)
  }
  
  
  # Frase 2 - Moles
  frase02 <- frase_explicativa_general
  
  for(k1 in 1:length(vector_cantidad_moles)){
    frase02 <- gsub(pattern = vector_cantidad_moles[k1],
                    replacement = vector_reemplazo_moles[k1], 
                    x = frase02)
  }
  
  for(k1 in 1:length(vector_cantidad_moles)){
    frase02 <- gsub(pattern = vector_sustancia[k1],
                    replacement = vector_reemplazo_sustancias[k1], 
                    x = frase02)
  }
  
  
  
  
  #  3) Atomos
  frase03 <- frase_explicativa_general
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase03 <- gsub(pattern = vector_cantidad_moles[k1],
                    replacement = vector_reemplazo_atomos[k1], 
                    x = frase03)
  }
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase03 <- gsub(pattern = vector_sustancia[k1],
                    replacement = vector_reemplazo_sustancias[k1], 
                    x = frase03)
  }
  
  
  #  4) Litros
  frase04 <- frase_explicativa_general
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase04 <- gsub(pattern = vector_cantidad_moles[k1],
                    replacement = vector_reemplazo_litros[k1], 
                    x = frase04)
  }
  
  for(k1 in 1:length(vector_reemplazo_sustancias)){
    frase04 <- gsub(pattern = vector_sustancia[k1],
                    replacement = vector_reemplazo_sustancias[k1], 
                    x = frase04)
  }
  
  
  
  #frase01 <- paste0("$$", frase01, "$$")
  
  ##############################################################################
  df_step01 <- apply(df_step01, 2, as.character)
  df_step01 <- apply(df_step01, 2, function(x){paste0("$$", x, "$$")})
  df_step01 <- t(df_step01)
  #df_step01 <- na.omit(df_step01)
  df_step01 <- apply(df_step01, 2, as.character)
  colnames(df_step01) <-   df_step01[1,]
  df_step01 <- df_step01[-1, , drop = FALSE]
  
  df_step03 <- apply(df_step03, 2, as.character)
  df_step03 <- apply(df_step03, 2, function(x){paste0("$$", x, "$$")})
  df_step03 <- t(df_step03)
  #df_step03 <- na.omit(df_step03)
  df_step03 <- apply(df_step03, 2, as.character)
  colnames(df_step03) <-   df_step03[1,]
  df_step03 <- df_step03[-1, , drop = FALSE]
  
  df_step04 <- apply(df_step04, 2, as.character)
  df_step04 <- apply(df_step04, 2, function(x){paste0("$$", x, "$$")})
  df_step04 <- t(df_step04)
  #df_step04 <- na.omit(df_step04)
  df_step04 <- apply(df_step04, 2, as.character)
  colnames(df_step04) <-   df_step04[1,]
  df_step04 <- df_step04[-1, , drop = FALSE]
  #df_step04 <- df_step04[c(1:6,10,13), , drop = FALSE]
  
  df_step05 <- apply(df_step05, 2, as.character)
  df_step05 <- apply(df_step05, 2, function(x){paste0("$$", x, "$$")})
  df_step05 <- t(df_step05)
  #df_step05 <- na.omit(df_step05)
  df_step05 <- apply(df_step05, 2, as.character)
  colnames(df_step05) <-   df_step05[1,]
  df_step05 <- df_step05[-1, , drop = FALSE]
  
  
  df_step06 <- df_step04[c(4,5,6,9,12),]
  df_step06 <- apply(df_step06, 2, function(x){gsub("uma", "", x)})
  df_step06 <- apply(df_step06, 2, function(x){gsub("gramos", "", x)})
  df_step06 <- apply(df_step06, 2, function(x){gsub("litros", "", x)})
  df_step06 <- apply(df_step06, 2, function(x){gsub("moles", "", x)})
  
  #df_step06 <- apply(df_step06, 2, function(x){gsub("[\\\\$; ]", "", x)})
  #df_step06 <- apply(df_step06, 2, function(x){gsub("10\\^", "e", x)})
  # df_step06 <- apply(df_step06, 2, function(x){gsub("[\\{\\}]", "", x)})
  #df_step06 <- apply(df_step06, 2, function(x){as.numeric(x)})
  
  vector_unidades <- c("uma", "gramos", "moles", "átomos", "litros")
  df_step06 <- cbind.data.frame(vector_unidades, df_step06)
  colnames(df_step06)[1] <- "unidades"
  
  df_step07 <- df_step06[,c(2:ncol(df_step06))]
  df_step07 <- apply(df_step07, 2, function(x){gsub("times", "", x)})
  df_step07 <- apply(df_step07, 2, function(x){gsub("[\\\\$; ]", "", x)})
  df_step07 <- apply(df_step07, 2, function(x){gsub("10\\^", "e", x)})
  df_step07 <- apply(df_step07, 2, function(x){gsub("[\\{\\}]", "", x)})
  df_step07 <- cbind.data.frame(df_step06[,1], df_step07)
  colnames(df_step07)[1] <- "unidades"
  
  # Final mod
  df_step04 <- df_step04[c(1:6,10,13), , drop = FALSE]
  
  ####
  output_list <- list(df_step01, df_step03, df_step04, df_step05, frase01, 
                      frase02, frase03, frase04, df_step06, df_step07)
  
  return(output_list)
}

