# =============================================================================
# Vistazo rápido a las bases de DEIS sin descargarlas completas
#
# Los archivos de DEIS son .zip grandes, pero el servidor permite descargar
# solo un trozo (petición HTTP "Range"). Con eso se puede:
#   1. listar_zip():     ver qué archivos trae el zip y cuánto pesan (~64 KB de descarga)
#   2. vistazo_csv():    leer las primeras filas de un CSV dentro del zip (~1-2 MB)
#   3. extraer_archivo(): bajar un solo archivo chico del zip, p. ej. un diccionario
#
# Solo usa R base (más readxl para abrir diccionarios Excel).
# =============================================================================

URL_DEIS <- "https://repositoriodeis.minsal.cl/DatosAbiertos"
URL_DEF  <- file.path(URL_DEIS, "VITALES/DEFUNCIONES_FUENTE_DEIS_1990_2023_CIFRAS_OFICIALES.zip") # base muertes; despues se filtra por suicidios
URL_EGR  <- file.path(URL_DEIS, "EGRESOS/EGRESOS_2024.zip") # hospitalizaciones; después se filtra por psiquiatría
URL_REM  <- file.path(URL_DEIS, "REM/SERIE_REM_2024.zip") # descompensaciones

# ---------------------------------------------------------------------------
# Funciones auxiliares
# ---------------------------------------------------------------------------

# Descarga solo los bytes indicados ("0-999" o "-65536" para los últimos 64 KB)
bajar_rango <- function(url, rango) {
  tmp <- tempfile()
  download.file(url, tmp, mode = "wb", quiet = TRUE, method = "libcurl",
                headers = c(Range = paste0("bytes=", rango)))
  datos <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  datos
}

# Lee un entero little-endian de 2 o 4 bytes
entero <- function(x, pos, n) {
  sum(as.numeric(x[pos:(pos + n - 1)]) * 256^(0:(n - 1)))
}

# Descomprime un bloque "deflate" (aunque esté incompleto) agregándole una
# cabecera gzip, para poder leerlo con gzcon() de R base
inflar <- function(bloque, n_lineas = NULL, binario = FALSE) {
  cabecera <- as.raw(c(0x1f, 0x8b, 0x08, 0, 0, 0, 0, 0, 0, 0x03))
  con <- gzcon(rawConnection(c(cabecera, bloque)))
  on.exit(close(con))
  if (binario) {
    trozos <- list()
    repeat {
      r <- tryCatch(readBin(con, "raw", 1e6), error = function(e) raw(0))
      if (length(r) == 0) break
      trozos[[length(trozos) + 1]] <- r
    }
    return(do.call(c, trozos))
  }
  suppressWarnings(tryCatch(readLines(con, n = n_lineas), error = function(e) character(0)))
}

# ---------------------------------------------------------------------------
# 1. Listar el contenido del zip (lee el "índice" que está al final del archivo)
# ---------------------------------------------------------------------------
listar_zip <- function(url) {
  cola <- bajar_rango(url, "-65536")
  # Fin del directorio central: firma 50 4b 05 06
  firma <- as.raw(c(0x50, 0x4b, 0x05, 0x06))
  eocd <- NA
  for (i in (length(cola) - 21):1) {
    if (all(cola[i:(i + 3)] == firma)) { eocd <- i; break }
  }
  if (is.na(eocd)) stop("No se pudo leer el índice del zip")
  n_archivos <- entero(cola, eocd + 10, 2)
  tam_dir    <- entero(cola, eocd + 12, 4)
  pos        <- eocd - tam_dir

  filas <- vector("list", n_archivos)
  for (i in seq_len(n_archivos)) {
    largo_nombre <- entero(cola, pos + 28, 2)
    largo_extra  <- entero(cola, pos + 30, 2)
    largo_coment <- entero(cola, pos + 32, 2)
    filas[[i]] <- data.frame(
      archivo           = rawToChar(cola[(pos + 46):(pos + 45 + largo_nombre)]),
      mb_comprimido     = round(entero(cola, pos + 20, 4) / 1e6, 1),
      mb_descomprimido  = round(entero(cola, pos + 24, 4) / 1e6, 1),
      offset            = entero(cola, pos + 42, 4),
      bytes_comprimidos = entero(cola, pos + 20, 4))
    pos <- pos + 46 + largo_nombre + largo_extra + largo_coment
  }
  do.call(rbind, filas)
}

# Ubica el inicio de los datos comprimidos de un archivo dentro del zip
inicio_datos <- function(url, entrada) {
  cab <- bajar_rango(url, sprintf("%.0f-%.0f", entrada$offset, entrada$offset + 29))
  entrada$offset + 30 + entero(cab, 27, 2) + entero(cab, 29, 2)
}

buscar_entrada <- function(url, patron) {
  indice <- listar_zip(url)
  entrada <- indice[grepl(patron, indice$archivo), ][1, ]
  if (is.na(entrada$archivo)) stop("No se encontró un archivo con el patrón: ", patron)
  entrada
}

# ---------------------------------------------------------------------------
# 2. Ver las primeras filas de un CSV dentro del zip
# ---------------------------------------------------------------------------
vistazo_csv <- function(url, patron = "\\.csv$", n_lineas = 10, mb = 1) {
  entrada <- buscar_entrada(url, patron)
  ini <- inicio_datos(url, entrada)
  bloque <- bajar_rango(url, sprintf("%.0f-%.0f", ini, ini + mb * 1e6))
  lineas <- inflar(bloque, n_lineas = n_lineas)
  lineas <- iconv(lineas, from = "latin1", to = "UTF-8")  # DEIS usa codificación Latin-1
  read.csv2(text = lineas, colClasses = "character", check.names = FALSE)
}

# ---------------------------------------------------------------------------
# 3. Extraer un solo archivo chico del zip (p. ej. el diccionario de datos)
# ---------------------------------------------------------------------------
extraer_archivo <- function(url, patron, destino = NULL) {
  entrada <- buscar_entrada(url, patron)
  ini <- inicio_datos(url, entrada)
  bloque <- bajar_rango(url, sprintf("%.0f-%.0f", ini, ini + entrada$bytes_comprimidos - 1))
  if (is.null(destino)) destino <- basename(entrada$archivo)
  writeBin(inflar(bloque, binario = TRUE), destino)
  message("Guardado: ", destino)
  invisible(destino)
}

# =============================================================================
# Ejemplos de uso
# =============================================================================

# --- Defunciones: contenido del zip y primeras filas ----------------------
listar_zip(URL_DEF)[, 1:3]
def <- vistazo_csv(URL_DEF, n_lineas = 20)
names(def)          # columnas disponibles
head(def[, c(1, 3:6, 8, 9, 18)])

# --- Egresos hospitalarios 2024 --------------------------------------------
listar_zip(URL_EGR)[, 1:3]
egr <- vistazo_csv(URL_EGR, n_lineas = 20)
names(egr)
head(egr)

# --- REM 2024: qué series trae y cómo se ve la Serie P ----------------------
listar_zip(URL_REM)[, 1:3]
remp <- vistazo_csv(URL_REM, patron = "SerieP", n_lineas = 20)
head(remp[, 1:12])

# Diccionario de códigos de la Serie P (~0,5 MB): cada hoja es un REM (P1, P4, P6, ...)
dicc <- extraer_archivo(URL_REM, "DICCIONARIO CODIGOS SP")
if (requireNamespace("readxl", quietly = TRUE)) {
  print(readxl::excel_sheets(dicc))
  p6 <- readxl::read_excel(dicc, sheet = "P6", col_names = FALSE,
                             .name_repair = "minimal")  # salud mental
  print(p6[1:40, 1:4], n = 40)
}
