# =============================================================================
# Análisis exploratorio de suicidios en la base de defunciones DEIS 1990-2023
#
# Igual que datos_pois.R, no descarga el zip completo (93 MB comprimido,
# 869 MB descomprimido): lee solo el comienzo del CSV mediante peticiones
# HTTP "Range". Con MB_MUESTRA = 25 se descargan ~25 MB y se obtienen
# ~1 millón de filas.
#
# OJO: la muestra NO es aleatoria. El archivo no está ordenado por año:
#   - 1990-1996 (CIE-9) aparecen casi completos al comienzo.
#   - Desde 1997 (CIE-10) el comienzo del archivo contiene sobre todo muertes
#     por causas externas (DIAG1 en el capítulo S00-T98), que es justamente
#     donde caen los suicidios. Por eso la muestra captura casi todos los
#     suicidios 1997-2019, pero NO sirve para describir la mortalidad general.
#   - 2020-2023 quedan fuera de la muestra.
#
# Pasos:
#   1. Leer la muestra
#   2. Describir las columnas relevantes (vacíos, valores distintos, ejemplos)
#   3. Definir suicidio (CIE-9 E950-E959 / CIE-10 X60-X84) y describirlos
#   4. Agregar a conteos región x año x sexo x grupo de edad (con ceros)
#   5. Ajustar Poisson y comparar devianza vs grados de libertad
#
# Solo usa R base y MASS (viene instalado con R).
# =============================================================================

MB_MUESTRA <- 25              # MB comprimidos a descargar (más = más filas)
ANIOS_MODELO <- 1997:2019     # período CIE-10 bien cubierto por la muestra

URL_DEF <- "https://repositoriodeis.minsal.cl/DatosAbiertos/VITALES/DEFUNCIONES_FUENTE_DEIS_1990_2023_CIFRAS_OFICIALES.zip"

# ---------------------------------------------------------------------------
# Funciones auxiliares (copiadas de datos_pois.R, que al ejecutarse con
# source() también correría sus ejemplos)
# ---------------------------------------------------------------------------

bajar_rango <- function(url, rango) {
  tmp <- tempfile()
  download.file(url, tmp, mode = "wb", quiet = TRUE, method = "libcurl",
                headers = c(Range = paste0("bytes=", rango)))
  datos <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  datos
}

entero <- function(x, pos, n) {
  sum(as.numeric(x[pos:(pos + n - 1)]) * 256^(0:(n - 1)))
}

# Descomprime un bloque "deflate" incompleto agregándole una cabecera gzip
inflar_lineas <- function(bloque) {
  cabecera <- as.raw(c(0x1f, 0x8b, 0x08, 0, 0, 0, 0, 0, 0, 0x03))
  con <- gzcon(rawConnection(c(cabecera, bloque)))
  on.exit(close(con))
  suppressWarnings(tryCatch(readLines(con), error = function(e) character(0)))
}

# Posición donde empiezan los datos comprimidos del primer archivo del zip
inicio_datos <- function(url) {
  cab <- bajar_rango(url, "0-29")
  30 + entero(cab, 27, 2) + entero(cab, 29, 2)
}

# ---------------------------------------------------------------------------
# 1. Leer la muestra
# ---------------------------------------------------------------------------
leer_muestra <- function(url, mb) {
  ini <- inicio_datos(url)
  bloque <- bajar_rango(url, sprintf("%.0f-%.0f", ini, ini + mb * 1e6))
  lineas <- inflar_lineas(bloque)
  lineas <- lineas[-length(lineas)]          # la última línea queda cortada
  lineas <- iconv(lineas, from = "latin1", to = "UTF-8")
  read.csv2(text = lineas, colClasses = "character", check.names = FALSE)
}

message("Descargando ~", MB_MUESTRA, " MB de la base de defunciones...")
def <- leer_muestra(URL_DEF, MB_MUESTRA)
cat("Filas leídas:", format(nrow(def), big.mark = " "), "\n")
cat("Columnas:", ncol(def), "\n\n")

# ---------------------------------------------------------------------------
# 2. Descripción de columnas
# ---------------------------------------------------------------------------

# Una fila por columna: % vacíos, n.º de valores distintos y los más frecuentes
describir <- function(d, columnas, n_ej = 5) {
  do.call(rbind, lapply(columnas, function(col) {
    x <- d[[col]]
    frec <- sort(table(x[x != ""]), decreasing = TRUE)
    ej <- head(frec, n_ej)
    data.frame(
      columna   = col,
      pct_vacio = round(100 * mean(x == ""), 1),
      distintos = length(frec),
      ejemplos  = paste0(names(ej), " (", ej, ")", collapse = "; "),
      row.names = NULL)
  }))
}

columnas_relevantes <- c(
  "AÑO",                 # año de la defunción
  "FECHA_DEF",           # fecha exacta (AAAA-MM-DD)
  "SEXO_NOMBRE",         # Hombre / Mujer / Indeterminado
  "EDAD_TIPO",           # unidad de EDAD_CANT: 1 = años; 2, 3, 4 = unidades
                         # menores (meses, días, horas) para menores de 1 año
  "EDAD_CANT",           # edad en la unidad de EDAD_TIPO
  "COD_COMUNA",          # código de comuna (4-5 dígitos; los 1-2 primeros = región)
  "COMUNA",
  "NOMBRE_REGION",       # ya viene con las 16 regiones actuales (incluye Ñuble)
  "DIAG1",               # causa básica / naturaleza de la lesión
  "DIAG2",               ## causa externa (aquí se identifica el suicidio)
  "CAPITULO_DIAG2",      # desde aquí, solo se completan para CIE-10 (1997+)
  "CODIGO_GRUPO_DIAG2",
  "CODIGO_CATEGORIA_DIAG2",
  "GLOSA_CATEGORIA_DIAG2",
  "LUGAR_DEFUNCION")

op <- options(width = 200)
cat("===== Columnas relevantes (todas las defunciones de la muestra) =====\n")
print(describir(def, columnas_relevantes), right = FALSE)

cat("\nDefunciones por año en la muestra (desde 1997 solo hay causas externas):\n")
print(table(def$AÑO))

cat("\n¿Coincide AÑO con el año de FECHA_DEF? ",
    round(100 * mean(def$AÑO == substr(def$FECHA_DEF, 1, 4)), 2), "%\n")

cat("\nEDAD_CANT según EDAD_TIPO:\n")
edad_cant <- as.numeric(def$EDAD_CANT)
print(data.frame(n      = tapply(edad_cant, def$EDAD_TIPO, length),
                 minimo = tapply(edad_cant, def$EDAD_TIPO, min, na.rm = TRUE),
                 maximo = tapply(edad_cant, def$EDAD_TIPO, max, na.rm = TRUE)))

# ---------------------------------------------------------------------------
# 3. Suicidios
#   - CIE-9 (1990-1996): DIAG2 = código E sin la "E": 950-959
#   - CIE-10 (1997+):    DIAG2 = X60-X84 (lesión autoinfligida intencionalmente)
#   Y87.0 (secuelas de lesión autoinfligida) se reporta aparte y no se incluye.
# ---------------------------------------------------------------------------
anio <- as.integer(def$AÑO)
def$suicidio <- ifelse(anio < 1997,
                       grepl("^95[0-9]", def$DIAG2),
                       grepl("^X(6[0-9]|7[0-9]|8[0-4])", def$DIAG2))
suic <- def[def$suicidio, ]

cat("\n===== Suicidios =====\n")
cat("Total en la muestra:", format(nrow(suic)))
cat("Secuelas Y870 (no incluidas):", sum(def$DIAG2 == "Y870"), "\n\n")

cat("Suicidios por año (revisar que 1997-2019 se vean completos, ~1.500-2.000/año):\n")
print(table(suic$AÑO))

cat("\nDescripción de columnas, solo suicidios:\n")
print(describir(suic, columnas_relevantes), right = FALSE)

cat("\nMétodo (CIE-10, 1997+):\n")
suic10 <- suic[as.integer(suic$AÑO) >= 1997, ]
metodo <- sort(table(paste(suic10$CODIGO_CATEGORIA_DIAG2, "-",
                           suic10$GLOSA_CATEGORIA_DIAG2)), decreasing = TRUE)
print(data.frame(n = as.vector(metodo), pct = round(100 * as.vector(metodo) / sum(metodo), 1),
                 row.names = names(metodo)))

cat("\nSexo (%):\n")
print(round(100 * prop.table(table(suic$SEXO_NOMBRE)), 1))

edad <- as.numeric(suic$EDAD_CANT)
cat("\nEdad (años):\n")
print(summary(edad))

cat("\nMes de la defunción (estacionalidad):\n")
print(table(mes = substr(suic$FECHA_DEF, 6, 7)))

cat("\nSuicidios por región (1997-2019):\n")
print(sort(table(suic10$NOMBRE_REGION), decreasing = TRUE))
options(op)

# ---------------------------------------------------------------------------
# 4. Tabla de conteos para el modelo
#    Celda = región x año x sexo x grupo de edad. Se agregan explícitamente
#    las celdas con 0 suicidios (omitirlas sesgaría el modelo).
# ---------------------------------------------------------------------------
cortes <- c(10, 20, 30, 40, 50, 60, 70, 80, Inf)
etiquetas <- c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

s <- suic10[as.integer(suic10$AÑO) %in% ANIOS_MODELO &
            suic10$SEXO_NOMBRE %in% c("Hombre", "Mujer") &
            !suic10$NOMBRE_REGION %in% c("", "Ignorada") &
            suic10$EDAD_TIPO == "1" & as.numeric(suic10$EDAD_CANT) >= 10, ]
s$edad_grupo <- cut(as.numeric(s$EDAD_CANT), cortes, etiquetas, right = FALSE)
cat("\nSuicidios usados en el modelo:", nrow(s),
    "(se excluyen sexo indeterminado, región ignorada y menores de 10 años)\n")

celdas <- expand.grid(region = sort(unique(s$NOMBRE_REGION)),
                      anio = ANIOS_MODELO,
                      sexo = c("Hombre", "Mujer"),
                      edad_grupo = etiquetas,
                      stringsAsFactors = FALSE)
conteo <- aggregate(list(n = rep(1, nrow(s))),
                    by = list(region = s$NOMBRE_REGION, anio = as.integer(s$AÑO),
                              sexo = s$SEXO_NOMBRE, edad_grupo = as.character(s$edad_grupo)),
                    FUN = sum)
celdas <- merge(celdas, conteo, all.x = TRUE)
celdas$n[is.na(celdas$n)] <- 0
celdas$edad_grupo <- factor(celdas$edad_grupo, etiquetas)

cat("Celdas:", nrow(celdas), "| con 0 suicidios:",
    round(100 * mean(celdas$n == 0), 1), "% | media:", round(mean(celdas$n), 2),
    "| varianza:", round(var(celdas$n), 2), "\n")

# ---------------------------------------------------------------------------
# 5. Poisson: devianza vs grados de libertad
#
# Si el modelo Poisson es adecuado, devianza / gl ≈ 1 (y lo mismo para el
# chi-cuadrado de Pearson / gl, que es más confiable cuando hay muchas celdas
# con conteos esperados pequeños).
#   ~1        -> Poisson razonable
#   1.3 - 2   -> sobredispersión moderada: usar quasi-Poisson o binomial negativa
#   > 2       -> Poisson subestimará bastante los errores estándar
#
# LIMITACIÓN IMPORTANTE: no hay offset de población (este archivo no la trae;
# se necesitan las proyecciones INE por región, año, sexo y edad). Sin offset,
# los efectos de región, sexo y edad absorben en parte el tamaño de cada
# población, pero no cómo esta cambia con los años en cada región. Por eso la
# sobredispersión que se ve aquí es probablemente MAYOR que la que tendrá el
# modelo final con offset: tómalo como una cota superior.
# ---------------------------------------------------------------------------
dispersion <- function(m, nombre) {
  data.frame(
    modelo          = nombre,
    devianza        = round(deviance(m), 1),
    gl              = df.residual(m),
    devianza_gl     = round(deviance(m) / df.residual(m), 2),
    pearson_gl      = round(sum(residuals(m, type = "pearson")^2) / df.residual(m), 2),
    p_bondad_ajuste = signif(pchisq(deviance(m), df.residual(m), lower.tail = FALSE), 3),
    AIC             = round(AIC(m)))
}

# a) Efectos principales: región + sexo + edad + tendencia lineal en años
m1 <- glm(n ~ region + sexo + edad_grupo + anio, family = poisson, data = celdas)
# b) Permite que el perfil de edad difiera por sexo y que cada región tenga su
#    propia tendencia (esto último imita en parte el cambio de población)
m2 <- glm(n ~ region * anio + sexo * edad_grupo, family = poisson, data = celdas)
# c) Año como factor (cambios no lineales, p. ej. el alza de 2008)
m3 <- glm(n ~ region * anio + sexo * edad_grupo + factor(anio), family = poisson, data = celdas)

tabla <- rbind(dispersion(m1, "a) principales"),
               dispersion(m2, "b) + región:año, sexo:edad"),
               dispersion(m3, "c) + año como factor"))

# Mismo modelo c) a nivel nacional (sin región): muestra cuánto depende la
# dispersión del nivel de agregación
nac <- aggregate(n ~ anio + sexo + edad_grupo, data = celdas, FUN = sum)
m_nac <- glm(n ~ sexo * edad_grupo + factor(anio), family = poisson, data = nac)
tabla <- rbind(tabla, dispersion(m_nac, "d) nacional: sexo:edad + año"))

cat("\n===== Poisson: devianza / grados de libertad =====\n")
print(tabla, row.names = FALSE)

esp <- fitted(m3)
cat("\nCeldas con conteo esperado < 1 en el modelo c):", round(100 * mean(esp < 1), 1),
    "% (si es alto, la devianza/gl es poco confiable; mirar Pearson)\n")

# Comparación con binomial negativa (modelo c). theta grande = cercano a Poisson.
m3_nb <- MASS::glm.nb(n ~ region * anio + sexo * edad_grupo + factor(anio), data = celdas)
cat("\nBinomial negativa, modelo c): theta =", round(m3_nb$theta, 1),
    "| AIC =", round(AIC(m3_nb)), "vs Poisson AIC =", round(AIC(m3)), "\n")
# La prueba de razón de verosimilitud está en el borde del espacio de
# parámetros (theta = infinito), por eso el valor p se divide por 2
lr <- 2 * (as.numeric(logLik(m3_nb)) - as.numeric(logLik(m3)))
cat("Razón de verosimilitud NB vs Poisson:", round(lr, 1),
    "| p =", signif(0.5 * pchisq(lr, 1, lower.tail = FALSE), 3), "\n")

cat("\nLectura rápida:\n")
r <- tabla$pearson_gl[3]
cat(if (r < 1.3) {
  "  Pearson/gl cercano a 1: Poisson no parece un mal ajuste a este nivel.\n"
} else if (r < 2) {
  "  Sobredispersión moderada: Poisson sirve para las estimaciones puntuales,\n  pero usa quasi-Poisson o binomial negativa para los errores estándar.\n"
} else {
  "  Sobredispersión marcada: Poisson simple no es adecuado; usa binomial negativa\n  (y revisa de nuevo cuando agregues el offset de población INE).\n"
})
