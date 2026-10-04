# =============================================================================
# Exploración de la base de egresos hospitalarios DEIS: enfermedad por
# arañazo de gato (CIE-10 A28.1)
#
# Script solo exploratorio: muestra qué valores toma cada variable en los
# casos (años, sexo, edad, región, comuna, previsión, etc.). Todo queda como
# data frames (ver la lista al final), para revisarlos con View(), head(), etc.
#
# Datos: DEIS publica un zip por año (2001-2024), de 7-22 MB comprimido
# (~1,6 millones de filas cada uno). Se descargan completos en DIR_DATOS y,
# después de la primera lectura, los casos quedan guardados en CACHE para no
# tener que leer los ~25 millones de filas otra vez.
#
# Lo que hay que saber de la base (revisado 2010-2024):
#   - Una fila = un egreso (alta o fallecimiento), no una persona. No hay
#     fechas, solo ANO_EGRESO. Edad solo en grupos (GRUPO_EDAD).
#   - Filas ANONIMIZADAS: traen "*" en sexo, edad, región, etc. (~2 % hasta
#     2022, ~7,5 % en 2023-2024).
#   - 2012 y 2021 vienen en otro formato: SEXO como 1/2/3 y edad en grupos de
#     5 años ("15 A 19 AÑOS", "28 DIAS A 2 MES"). Aquí se agregan las columnas
#     armonizadas sexo y edad10, y se mantienen las originales.
#   - REGION_RESIDENCIA ya viene con las 16 regiones actuales en todos los
#     años. Las glosas tienen tildes rotas en 2024, por eso se usan códigos.
#
# Solo usa R base.
# =============================================================================

ANIOS <- 2010:2024            # años a leer (hay de 2001 a 2024)
CODIGO <- "A281"              # enfermedad por arañazo de gato
DIR_DATOS <- "datos_egresos"  # carpeta donde se guardan los zip
CACHE <- file.path(DIR_DATOS, sprintf("casos_%s_%d_%d.rds", CODIGO, min(ANIOS), max(ANIOS)))

URL_EGR <- "https://repositoriodeis.minsal.cl/DatosAbiertos/EGRESOS/EGRESOS_%d.zip"

REGIONES <- c("01" = "Tarapacá", "02" = "Antofagasta", "03" = "Atacama",
              "04" = "Coquimbo", "05" = "Valparaíso", "06" = "O'Higgins",
              "07" = "Maule", "08" = "Biobío", "09" = "La Araucanía",
              "10" = "Los Lagos", "11" = "Aysén", "12" = "Magallanes",
              "13" = "Metropolitana", "14" = "Los Ríos",
              "15" = "Arica y Parinacota", "16" = "Ñuble")

# ---------------------------------------------------------------------------
# 1. Descargar y leer
# ---------------------------------------------------------------------------
dir.create(DIR_DATOS, showWarnings = FALSE)
options(timeout = max(600, getOption("timeout")))

bajar_anio <- function(a) {
  zip <- file.path(DIR_DATOS, sprintf("EGRESOS_%d.zip", a))
  if (!file.exists(zip)) {
    message("Descargando egresos ", a, "...")
    download.file(sprintf(URL_EGR, a), zip, mode = "wb", quiet = TRUE)
  }
  zip
}

# Lleva GRUPO_EDAD a grupos de 10 años: "10 a 19" y "15 A 19 AÑOS" -> "10-19";
# "80 a 89", "85 A MAS" y "90 y más" -> "80+"; días, meses y 1-9 años -> "0-9"
grupo_edad <- function(x) {
  inf <- suppressWarnings(as.integer(sub(" .*", "", x)))
  inf[grepl("D[IÍ]A|MES|^menor", x, ignore.case = TRUE)] <- 0
  g <- pmin(10 * (inf %/% 10), 80)
  ifelse(is.na(g), NA, ifelse(g == 0, "0-9", ifelse(g == 80, "80+", paste0(g, "-", g + 9))))
}

# Lee todo como texto, arregla la codificación y agrega columnas armonizadas
leer_anio <- function(a) {
  zip <- bajar_anio(a)
  csv <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, ignore.case = TRUE, value = TRUE)[1]
  e <- read.csv2(unz(zip, csv), colClasses = "character", check.names = FALSE)
  for (col in names(e)) {
    x <- e[[col]]
    malo <- !validUTF8(x)                  # Latin-1 -> UTF-8 solo donde haga falta
    if (any(malo)) x[malo] <- iconv(x[malo], from = "latin1", to = "UTF-8")
    e[[col]] <- x
  }
  names(e)[grepl("^PERTENENCIA", names(e))] <- "PERTENENCIA"  # ..._SALU o ..._SALUD
  e$anio        <- a                       # en las anonimizadas ANO_EGRESO es "*"
  e$anonimizada <- e$SEXO == "*"
  e$sexo <- ifelse(e$SEXO %in% c("HOMBRE", "1"), "HOMBRE",
            ifelse(e$SEXO %in% c("MUJER", "2"), "MUJER",
            ifelse(e$SEXO == "*", NA, "INDETERMINADO")))     # 3, INTERSEX, DESCONOCIDO
  e$edad10 <- grupo_edad(e$GRUPO_EDAD)
  e$region <- unname(REGIONES[e$REGION_RESIDENCIA])         # NA = ignorada, extranjero o *
  e
}

columnas <- c("anio", "anonimizada", "PERTENENCIA", "SEXO", "sexo", "GRUPO_EDAD",
              "edad10", "GLOSA_PAIS_ORIGEN", "REGION_RESIDENCIA", "region",
              "COMUNA_RESIDENCIA", "GLOSA_COMUNA_RESIDENCIA", "GLOSA_PREVISION",
              "DIAG1", "DIAG2", "DIAS_ESTADA", "CONDICION_EGRESO")

if (file.exists(CACHE)) {
  message("Leyendo casos guardados en ", CACHE)
  guardado <- readRDS(CACHE)
} else {
  por_archivo <- list(); casos <- list()
  for (a in ANIOS) {
    message("Leyendo ", a, "...")
    e <- leer_anio(a)
    por_archivo[[as.character(a)]] <- data.frame(
      anio            = a,
      filas           = nrow(e),
      columnas        = ncol(e) - 5,       # sin las 5 columnas agregadas aquí
      filas_anonim    = sum(e$anonimizada),
      filas_A28       = sum(substr(e$DIAG1, 1, 3) == "A28"),
      casos           = sum(e$DIAG1 == CODIGO),
      casos_anonim    = sum(e$DIAG1 == CODIGO & e$anonimizada),
      casos_en_DIAG2  = sum(e$DIAG2 == CODIGO))
    casos[[as.character(a)]] <- e[e$DIAG1 == CODIGO, columnas]
    rm(e); invisible(gc())
  }
  guardado <- list(por_archivo = do.call(rbind, por_archivo),
                   casos       = do.call(rbind, casos))
  saveRDS(guardado, CACHE)
}

por_archivo <- guardado$por_archivo
gato        <- guardado$casos
rownames(por_archivo) <- rownames(gato) <- NULL
gato$anio <- factor(gato$anio, levels = ANIOS)  # para que los años sin casos (2012) aparezcan con 0

# ---------------------------------------------------------------------------
# 2. Funciones para explorar
# ---------------------------------------------------------------------------

# Una fila por columna: % vacíos, % "*", n.º de valores distintos y los más frecuentes
describir <- function(d, columnas, n_ej = 5) {
  do.call(rbind, lapply(columnas, function(col) {
    x <- as.character(d[[col]])
    x[is.na(x)] <- ""                      # en las columnas armonizadas, NA = sin dato
    frec <- sort(table(x[x != ""]), decreasing = TRUE)
    ej <- head(frec, n_ej)
    data.frame(
      columna    = col,
      pct_vacio  = round(100 * mean(x == ""), 1),
      pct_asteri = round(100 * mean(x == "*"), 1),
      distintos  = length(frec),
      ejemplos   = paste0(names(ej), " (", ej, ")", collapse = "; "),
      row.names  = NULL)
  }))
}

# Frecuencias como data frame, de mayor a menor (incluye NA)
frecuencia <- function(d, vars) {
  t <- as.data.frame(table(d[, vars, drop = FALSE], useNA = "ifany"), responseName = "n")
  t <- t[t$n > 0, ]
  t$pct <- round(100 * t$n / sum(t$n), 1)
  t <- t[order(-t$n), ]
  rownames(t) <- NULL
  t
}

# Tabla cruzada como data frame ancho (filas = primera variable)
cruce <- function(d, fila, columna) {
  t <- table(d[[fila]], d[[columna]], useNA = "ifany")
  dimnames(t) <- lapply(dimnames(t), function(n) ifelse(is.na(n), "(sin dato)", n))
  data.frame(unclass(t), check.names = FALSE)
}

# ---------------------------------------------------------------------------
# 3. Exploración
# ---------------------------------------------------------------------------
descr_columnas <- describir(gato, setdiff(columnas, c("anio", "anonimizada")))

# Años
casos_anio         <- as.data.frame(table(anio = gato$anio), responseName = "n")
anio_anonimizada   <- cruce(gato, "anio", "anonimizada")

# Sexo y edad: valores originales (cambian según el año) y armonizados
sexo_original      <- cruce(gato, "SEXO", "anio")
sexo               <- frecuencia(gato, "sexo")
edad_original      <- cruce(gato, "GRUPO_EDAD", "anio")
edad               <- frecuencia(gato, "edad10")
edad_sexo          <- cruce(gato, "edad10", "sexo")
edad_anio          <- cruce(gato, "edad10", "anio")
sexo_anio          <- cruce(gato, "sexo", "anio")

# Región y comuna
region_codigo      <- frecuencia(gato, "REGION_RESIDENCIA")
region             <- frecuencia(gato, "region")
region_anio        <- cruce(gato, "region", "anio")
region_sexo        <- cruce(gato, "region", "sexo")
region_edad        <- cruce(gato, "region", "edad10")
comuna             <- frecuencia(gato, c("region", "GLOSA_COMUNA_RESIDENCIA"))
regiones_sin_casos <- setdiff(REGIONES, gato$region)

# Otras variables
pais_origen        <- frecuencia(gato, "GLOSA_PAIS_ORIGEN")
prevision          <- frecuencia(gato, "GLOSA_PREVISION")
pertenencia        <- frecuencia(gato, "PERTENENCIA")
condicion_egreso   <- frecuencia(gato, "CONDICION_EGRESO")
dias_estada        <- frecuencia(gato, "DIAS_ESTADA")
dias_estada        <- dias_estada[order(as.numeric(as.character(dias_estada$DIAS_ESTADA))), ]
rownames(dias_estada) <- NULL
diag2              <- frecuencia(gato, "DIAG2")

# Casos anonimizados: qué columnas conservan
anonimizados       <- gato[gato$anonimizada, ]

# ---------------------------------------------------------------------------
# 4. Mostrar
# ---------------------------------------------------------------------------
op <- options(width = 200)
mostrar <- function(titulo, x) {
  cat("\n=====", titulo, "=====\n")
  print(x, right = FALSE)
}

mostrar("Archivos por año (todas las causas y casos A28.1)", por_archivo)
mostrar("Columnas, casos A28.1", descr_columnas)
mostrar("Casos por año", casos_anio)
mostrar("Año x anonimizada", anio_anonimizada)
mostrar("SEXO original x año", sexo_original)
mostrar("Sexo armonizado", sexo)
mostrar("GRUPO_EDAD original x año", edad_original)
mostrar("Edad armonizada (10 años)", edad)
mostrar("Edad x sexo", edad_sexo)
mostrar("Edad x año", edad_anio)
mostrar("Sexo x año", sexo_anio)
mostrar("Código de región", region_codigo)
mostrar("Región", region)
mostrar("Región x año", region_anio)
mostrar("Región x sexo", region_sexo)
mostrar("Región x edad", region_edad)
cat("\nRegiones sin ningún caso:", if (length(regiones_sin_casos)) regiones_sin_casos else "ninguna", "\n")
mostrar("Comunas (primeras 25)", head(comuna, 25))
mostrar("País de origen", pais_origen)
mostrar("Previsión", prevision)
mostrar("Pertenencia al SNSS", pertenencia)
mostrar("Condición al egreso (1 = vivo, 2 = fallecido)", condicion_egreso)
mostrar("Días de estada", dias_estada)
mostrar("DIAG2 (causa externa)", diag2)
mostrar("Casos anonimizados", anonimizados)
options(op)

cat("\nData frames disponibles:\n",
    " por_archivo, gato (un caso por fila), anonimizados, descr_columnas,\n",
    " casos_anio, anio_anonimizada, sexo_original, sexo, sexo_anio, edad_original,\n",
    " edad, edad_sexo, edad_anio, region_codigo, region, region_anio, region_sexo,\n",
    " region_edad, comuna, pais_origen, prevision, pertenencia, condicion_egreso,\n",
    " dias_estada, diag2\n")
