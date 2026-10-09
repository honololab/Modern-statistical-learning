# =============================================================================
# Población INE para el offset y unión con las bases de conteo
#
# Fuente: INE, "Estimaciones y proyecciones de la población de Chile
# 2002-2035, base 2017, a nivel comunal". Población residente al 30 de junio
# de cada año, por comuna, sexo y edad simple (0 a 80 = "80 y más").
# DEIS no publica la población como dato abierto; usa estas mismas
# proyecciones del INE como denominador de sus tasas.
#
# Pasos:
#   1. Descargar el Excel del INE (~11 MB, una sola vez)
#   2. Pasarlo a formato largo y agrupar edad en 0-9, 10-19, 20+
#   3. Agregar a comuna y región x sexo x edad x año
#   4. Agregar la columna poblacion a dataset_A281_region y dataset_A281_comuna
#
# Ejecutar después de 01_dataset_egresos.R, con el directorio de trabajo en
# tareas/poisson. Usa R base y readxl.
# =============================================================================

DIR_DATOS <- "datos_egresos"
DIR_POB   <- "datos_poblacion"
URL_INE   <- paste0("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/",
                    "cuadros-estadisticos/base-2017/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")
XLSX_INE  <- file.path(DIR_POB, "ine_proyecciones_2002_2035_comunas.xlsx")

ANIOS  <- setdiff(2010:2024, 2012)   # mismos años que la base de casos
EDADES <- c("0-9", "10-19", "20+")
SEXOS  <- c("HOMBRE", "MUJER")

# ---------------------------------------------------------------------------
# 1. Descargar
# ---------------------------------------------------------------------------
dir.create(DIR_POB, showWarnings = FALSE)
if (!file.exists(XLSX_INE)) {
  message("Descargando proyecciones de población INE...")
  options(timeout = max(600, getOption("timeout")))
  download.file(URL_INE, XLSX_INE, mode = "wb", quiet = TRUE, method = "libcurl",
                headers = c("User-Agent" = "Mozilla/5.0"))
}

# ---------------------------------------------------------------------------
# 2. Leer y pasar a formato largo
# ---------------------------------------------------------------------------
ine <- as.data.frame(readxl::read_excel(XLSX_INE, sheet = 1))
names(ine)[grepl("^Sexo", names(ine))] <- "Sexo"   # el nombre trae saltos de línea

cols_anio <- paste("Poblacion", ANIOS)
stopifnot(all(cols_anio %in% names(ine)))

pob <- data.frame(
  comuna_cod = rep(sprintf("%05d", as.integer(ine$Comuna)), length(ANIOS)),
  sexo       = rep(ifelse(ine$Sexo == 1, "HOMBRE", "MUJER"), length(ANIOS)),
  edad       = rep(cut(ine$Edad, c(0, 10, 20, Inf), EDADES, right = FALSE), length(ANIOS)),
  anio       = rep(ANIOS, each = nrow(ine)),
  poblacion  = unlist(ine[cols_anio], use.names = FALSE))
pob$region_cod <- substr(pob$comuna_cod, 1, 2)

# ---------------------------------------------------------------------------
# 3. Agregar a los niveles de las bases de conteo
# ---------------------------------------------------------------------------
pob_comuna <- aggregate(poblacion ~ comuna_cod + sexo + edad + anio, pob, sum)
pob_region <- aggregate(poblacion ~ region_cod + sexo + edad + anio, pob, sum)


saveRDS(pob_comuna, file.path(DIR_POB, "poblacion_comuna.rds"))
saveRDS(pob_region, file.path(DIR_POB, "poblacion_region.rds"))

# ---------------------------------------------------------------------------
# 4. Unir con las bases de conteo
#    Se conserva el orden de filas y los factores de la base original. Si la
#    base ya tenía poblacion (por ejecutar este script de nuevo), se reemplaza.
# ---------------------------------------------------------------------------
agregar_poblacion <- function(nombre, pob, claves) {
  ruta <- file.path(DIR_DATOS, paste0(nombre, ".rds"))
  d <- readRDS(ruta)
  d$poblacion <- NULL
  llave <- function(x) do.call(paste, c(lapply(x[claves], as.character), sep = "|"))
  d$poblacion <- pob$poblacion[match(llave(d), llave(pob))]
  if (anyNA(d$poblacion)) stop(nombre, ": hay celdas sin población en el INE")
  # Celdas sin habitantes (p. ej. niños en Antártica): nadie en riesgo y log(0)
  # no sirve como offset. Se eliminan si no tienen casos.
  sin_pob <- d$poblacion <= 0
  if (any(d$casos[sin_pob] > 0)) stop(nombre, ": hay casos en celdas con población 0")
  if (any(sin_pob)) {
    message(nombre, ": se eliminan ", sum(sin_pob), " celdas con población 0 (sin casos) en ",
            paste(unique(d$comuna[sin_pob]), collapse = ", "))
    d <- d[!sin_pob, ]
    rownames(d) <- NULL
  }
  saveRDS(d, ruta)
  write.csv(d, file.path(DIR_DATOS, paste0(nombre, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
  d
}
region <- agregar_poblacion("dataset_A281_region", pob_region, c("region_cod", "sexo", "edad", "anio"))
comuna <- agregar_poblacion("dataset_A281_comuna", pob_comuna, c("comuna_cod", "sexo", "edad", "anio"))

# ---------------------------------------------------------------------------
# 5. Controles y resumen
# ---------------------------------------------------------------------------
# Las dos bases deben sumar la misma población cada año
stopifnot(all.equal(tapply(region$poblacion, region$anio, sum),
                    tapply(comuna$poblacion, comuna$anio, sum)))

cat("Población total por año (INE):\n")
print(tapply(region$poblacion, region$anio, sum))

tasa <- function(d, por) {
  t <- aggregate(cbind(casos, poblacion) ~ ., d[c(por, "casos", "poblacion")], sum)
  t$tasa_100mil <- round(1e5 * t$casos / t$poblacion, 2)
  t
}
cat("\nTasa observada por 100.000 habitantes:\n")
print(tasa(region, c("sexo", "edad")), row.names = FALSE)
cat("\nCeldas de dataset_A281_region con población < 1.000:",
    sum(region$poblacion < 1000), "de", nrow(region), "\n")
