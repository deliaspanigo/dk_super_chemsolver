redondear_como_humano <- function(x, digits = 0) {
  # Multiplicar por 10^digits para manejar decimales
  factor <- 10^digits
  x <- x * factor
  
  # Redondear hacia el número par más cercano cuando está exactamente en el medio
  resultado <- ifelse(
    abs(x - floor(x + 0.5)) == 0.5,
    ifelse(floor(x + 0.5) %% 2 == 0, floor(x + 0.5), ceiling(x - 0.5)),
    round(x)
  )
  
  # Dividir por 10^digits para devolver el resultado a la escala original
  resultado <- resultado / factor
  return(resultado)
}