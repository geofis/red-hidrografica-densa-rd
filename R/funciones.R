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

estilo_kable <- function(df, titulo = '', cubre_anchura = T,
                         opcion_latex = "HOLD_position") {
  df %>% kable(format = 'latex', escape = F, booktabs = T,
               digits = 2, caption = titulo) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = opcion_latex,
                  full_width = cubre_anchura, position = "center")
}

# Función de formato personalizado
decimales_y_enteros <- function(x) {
  ifelse(x < 1, number_format(accuracy = 0.01)(x), number_format(accuracy = 1)(x))
}

#Extraer tabla desde archivo generado por r.stream.stats, y convertir a data.frame
extraer_rstream_stats <- function(archivo, inicio, fin, dos_filas = F) {
  # inicio: numérico, línea que contiene primera fila donde comienzan encabezados.
  # fin: numérico, línea que contiene última fila de tabla.
  # dos_filas: booleano, si es T, los títulos de la tabla fuente tiene títulos en dos filas
  texto <- readLines(archivo)
  # Extrae lineas
  lineas <- texto[inicio:fin]
  # Líneas de datos
  if(dos_filas)
    lineas_datos <- 3:(3 + fin - inicio - 2)
  else
    lineas_datos <- 2:(2 + fin - inicio - 2)
  # Extrae los nombres de las columnas
  if(dos_filas) {
    nom_1 <- strsplit(trimws(lineas[1]), "\\|")[[1]]
    nom_2 <- strsplit(trimws(lineas[2]), "\\|")[[1]]
    nombres_columnas <- paste0(trimws(nom_1), " ", trimws(nom_2))
  } else {
    nombres_columnas <- strsplit(trimws(lineas[1]), "\\|")[[1]]
  }
  # Divide los datos en la quinta línea
  # datos <- as.matrix(do.call('rbind', sapply(1:length(a), function(x) unlist(strsplit(trimws(a)[x], "\\|")), simplify = F)))
  datos <- data.frame(do.call('rbind',
                              sapply(
                                lineas_datos,
                                function(x) 
                                  matrix(as.numeric(trimws(strsplit(lineas[x], "\\|")[[1]])),
                                         nrow = 1, byrow = T),
                                simplify = F)))
  names(datos) <- nombres_columnas
  return(datos)
}

# Función para eliminar nodos en las puntas
quitar_puntas <- function(objeto_lineas, n){
  coords <- st_coordinates(objeto_lineas)
  
  if (nrow(coords) > 2 * n){
    coords <- coords[-(1:n),]    # Elimina los primeros n nodos
    coords <- coords[-((nrow(coords)-n+1):nrow(coords)),]    # Elimina los últimos n nodos
    lineas_reducidas <- st_linestring(coords)
  }
  
  return(lineas_reducidas)
}

limpiar_nombres <- function(x) {
  # Cambiar caracteres especiales de español a sus equivalentes
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Reemplazar espacios por "_"
  x <- gsub(" |-", "_", x)
  # Eliminar paréntesis, puntos, "/", "$", "^"
  x <- gsub("\\(|\\)|\\.|\\,|\\/|\\$|\\^", "", x)
  # Convertir a minúsculas
  x <- tolower(x)
  # Retornar el resultado
  return(x)
}

preguntar_seguro <- function() {
  opciones <- c("Sí", "No")
  selección <- menu(opciones, title = "¿Estás seguro/a?")
  if (selección == 0) {
    warning("No has hecho una elección válida.")
  } else if (selección == 1) {
    print("Has elegido 'Sí'.")
    # Aquí puedes continuar con lo que quieras que haga la función cuando el usuario elige "Sí"
  } else {
    stop("Has elegido 'No'. Interrumpiendo la ejecución.")
  }
}

asignar_valores_df_a_objetos <- function(df, nombre_dataset = NULL, agrupar_por = NULL, forzar = F){
  if(is.null(nombre_dataset))
    stop('Debe aportarse un nombre de dataset')
  if(is.null(agrupar_por)) {
    cat('Sin agrupamiento especificado. Devolviendo resultados por filas\n')
    agrupar_por <- 'agrupar_por'
    df$agrupar_por <- 1:nrow(df)
  }
  total_objetos <- prod(dim(df) - c(0, 1))
  if(interactive()) cat('Se crearán', total_objetos, '\n')
  if(interactive()) preguntar_seguro()
  limite <- 20
  grupos <- df[, agrupar_por, drop = T]
  grupos_duplicados <- any(duplicated(grupos))
  if(grupos_duplicados)
    stop(paste0('Hay grupos duplicados en la columna ',
                agrupar_por,
                '. Usa un data.frame sin duplicados en este columna para continuar'))
  if(total_objetos > limite && forzar == F)
    stop(paste('Se van a crear más de,',
               limite,
               'objetos. Usa forzar=T bajo tu propio riesgo'))
  df <- df[, -grep(agrupar_por, colnames(df))]
  nombres_limpios <- paste0('RR_', nombre_dataset, '_V_', limpiar_nombres(colnames(df)))
  # "RR", acrónimo único que significa "resumen para RMD
  # nombre_dataset es el nombre general del análisis a extraer
  # "V", de variable
  grupos_limpios <- limpiar_nombres(grupos)
  combinaciones <- expand.grid(nombres_limpios, grupos_limpios)
  nombres_objetos <- apply(combinaciones, 1, paste0, collapse = "_GR_")
  # "GR" de grupo
  datos_extraidos <- as.vector(apply(df, 1, '['))
  if(length(nombres_objetos) != length(datos_extraidos))
     stop('No hay el mismo número de nombres de objetos que de datos')
  for (i in seq_along(nombres_objetos)) {
    if(exists(nombres_objetos[i], )) {
      stop(paste("El objeto", nombres_objetos[i], "ya existe."))
    } else {
      assign(nombres_objetos[i], datos_extraidos[i], envir = .GlobalEnv)
    }
  }
  if(interactive()) cat('Nuevos objetos creados\n', nombres_objetos, sep = '\n')
}

formato <- function(valor = NULL, sci = F, dig = 2){
  salida <- format(x = valor, scientific = sci, digits = dig)
  return(salida)
}
