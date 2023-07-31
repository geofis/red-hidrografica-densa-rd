vector_a_lista <- function(vec) {
  n <- length(vec)
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(as.character(vec))
  } else if (n == 2) {
    return(paste(vec, collapse = " y "))
  } else {
    last <- paste("y", vec[n])
    rest <- paste(vec[1:(n-1)], collapse = ", ")
    return(paste(rest, last))
  }
}

estilo_kable <- function(df, titulo = '', cubre_anchura = T) {
  df %>% kable(format = 'latex', escape = F, booktabs = T,
               digits = 2, caption = titulo) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = "HOLD_position",
                  full_width = cubre_anchura, position = "center")
}

# Función de formato personalizado
decimales_y_enteros <- function(x) {
  ifelse(x < 1, number_format(accuracy = 0.01)(x), number_format(accuracy = 1)(x))
}
