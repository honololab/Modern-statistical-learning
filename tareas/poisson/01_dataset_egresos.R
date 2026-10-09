# =============================================================================
# Construcción de la base de conteos para la regresión de Poisson:
# egresos hospitalarios por enfermedad por arañazo de gato (CIE-10 A28.1)
#
# Parte de los casos que guardó 00_explorar_egresos.R (CACHE) y genera:
#   1. casos_A281_limpios:    un caso por fila, ya filtrado y armonizado
#   2. dataset_A281_region:   región x sexo x edad x año, con ceros explícitos
#                             (base principal para el modelo)
#   3. dataset_A281_comuna:   comuna x sexo x edad x año, con ceros explícitos
#                             (para análisis más finos o para cruzar con
#                             población comunal)
# Se guardan como .rds (con factores y su orden de niveles) y como .csv.
#
# Decisiones (ver decisiones_dataset.Rmd):
#   - Se excluye 2012: el archivo fuente no trae ningún código A28 (falta de
#     datos, no son ceros reales).
#   - Se excluyen los egresos anonimizados ("*" en sexo, edad y región; la
#     comuna a veces se conserva): sin sexo ni edad no se pueden asignar a
#     ninguna celda.
#   - Edad en 3 grupos (0-9, 10-19, 20+): son los únicos cortes comparables en
#     todos los años y sobre 20 años hay muy pocos casos.
#   - Todavía no se incluye la población (offset): se agrega después.
#
# Ejecutar con el directorio de trabajo en tareas/poisson. Solo usa R base.
# =============================================================================

DIR_DATOS <- "datos_egresos"
CACHE <- file.path(DIR_DATOS, "casos_A281_2010_2024.rds")   # creado por 00_explorar_egresos.R
ANIOS <- setdiff(2010:2024, 2012)
ANIO_COMUNAS <- 2022  # archivo del que se toma la lista completa de comunas

REGIONES <- c("01" = "Tarapacá", "02" = "Antofagasta", "03" = "Atacama",
              "04" = "Coquimbo", "05" = "Valparaíso", "06" = "O'Higgins",
              "07" = "Maule", "08" = "Biobío", "09" = "La Araucanía",
              "10" = "Los Lagos", "11" = "Aysén", "12" = "Magallanes",
              "13" = "Metropolitana", "14" = "Los Ríos",
              "15" = "Arica y Parinacota", "16" = "Ñuble")
# Orden norte a sur, para tablas y gráficos
ORDEN_REGIONES <- c("15", "01", "02", "03", "04", "05", "13", "06", "07",
                    "16", "08", "09", "14", "10", "11", "12")
ZONAS <- c("15" = "Norte", "01" = "Norte", "02" = "Norte", "03" = "Norte",
           "04" = "Centro", "05" = "Centro", "06" = "Centro", "07" = "Centro",
           "13" = "Metropolitana",
           "16" = "Sur", "08" = "Sur", "09" = "Sur", "14" = "Sur", "10" = "Sur",
           "11" = "Sur", "12" = "Sur")
EDADES   <- c("0-9", "10-19", "20+")
SEXOS    <- c("HOMBRE", "MUJER")
PERIODOS <- c("2010-2019", "2020-2021", "2022-2024")

periodo <- function(anio) {
  factor(ifelse(anio <= 2019, PERIODOS[1], ifelse(anio <= 2021, PERIODOS[2], PERIODOS[3])),
         levels = PERIODOS)
}
# Mismo período como código: 1 = prepandemia, 2 = pandemia, 3 = pospandemia.
# En el modelo usar factor(pandemia), no como número.
pandemia <- function(anio) as.integer(periodo(anio))

# ---------------------------------------------------------------------------
# 1. Casos limpios
# ---------------------------------------------------------------------------
if (!file.exists(CACHE)) stop("Falta ", CACHE, ": ejecutar antes 00_explorar_egresos.R")
crudos <- readRDS(CACHE)$casos
crudos$anio <- as.integer(as.character(crudos$anio))

# Cuántos casos se pierden en cada paso (se reporta en el Rmd)
filtro <- data.frame(paso = "Casos A28.1 como diagnóstico principal, 2010-2024", casos = nrow(crudos))
c1 <- crudos[crudos$anio %in% ANIOS, ]
filtro <- rbind(filtro, data.frame(paso = "Sin 2012 (sin datos A28 en la fuente)", casos = nrow(c1)))
c1 <- c1[!c1$anonimizada, ]
filtro <- rbind(filtro, data.frame(paso = "Sin egresos anonimizados", casos = nrow(c1)))
c1 <- c1[c1$sexo %in% SEXOS & !is.na(c1$edad10) & c1$REGION_RESIDENCIA %in% names(REGIONES), ]
filtro <- rbind(filtro, data.frame(paso = "Con sexo, edad y región válidos", casos = nrow(c1)))

casos <- data.frame(
  anio       = c1$anio,
  periodo    = periodo(c1$anio),
  pandemia   = pandemia(c1$anio),
  sexo      = factor(c1$sexo, levels = SEXOS),
  edad       = factor(ifelse(c1$edad10 %in% c("0-9", "10-19"), c1$edad10, "20+"), levels = EDADES),
  zona       = NA,
  region_cod = factor(c1$REGION_RESIDENCIA, levels = ORDEN_REGIONES),
  region     = NA,
  comuna_cod = c1$COMUNA_RESIDENCIA,
  comuna     = NA,
  prevision  = c1$GLOSA_PREVISION,
  dias_estada = as.integer(c1$DIAS_ESTADA))
casos$zona   <- factor(ZONAS[as.character(casos$region_cod)], levels = unique(ZONAS[ORDEN_REGIONES]))
casos$region <- factor(REGIONES[as.character(casos$region_cod)], levels = REGIONES[ORDEN_REGIONES])

# ---------------------------------------------------------------------------
# 2. Lista completa de comunas
#    Para tener las comunas sin casos (con 0) se toma la lista de comunas de
#    residencia de un año completo de egresos. Se lee solo esas columnas y se
#    guarda, para no repetir la lectura.
# ---------------------------------------------------------------------------
LISTA_COMUNAS <- file.path(DIR_DATOS, sprintf("comunas_%d.rds", ANIO_COMUNAS))
if (file.exists(LISTA_COMUNAS)) {
  comunas <- readRDS(LISTA_COMUNAS)
} else {
  message("Leyendo comunas de egresos ", ANIO_COMUNAS, " (una sola vez)...")
  zip <- file.path(DIR_DATOS, sprintf("EGRESOS_%d.zip", ANIO_COMUNAS))
  csv <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, ignore.case = TRUE, value = TRUE)[1]
  cab <- names(read.csv2(unz(zip, csv), nrows = 0, check.names = FALSE))
  usar <- c("COMUNA_RESIDENCIA", "GLOSA_COMUNA_RESIDENCIA")
  clases <- ifelse(cab %in% usar, "character", "NULL")
  e <- unique(read.csv2(unz(zip, csv), colClasses = clases, check.names = FALSE))
  malo <- !validUTF8(e$GLOSA_COMUNA_RESIDENCIA)
  e$GLOSA_COMUNA_RESIDENCIA[malo] <- iconv(e$GLOSA_COMUNA_RESIDENCIA[malo], from = "latin1", to = "UTF-8")
  e <- e[substr(e$COMUNA_RESIDENCIA, 1, 2) %in% names(REGIONES) & nchar(e$COMUNA_RESIDENCIA) == 5, ]
  comunas <- data.frame(comuna_cod = e$COMUNA_RESIDENCIA, comuna = e$GLOSA_COMUNA_RESIDENCIA)
  comunas <- comunas[order(comunas$comuna_cod), ]
  rownames(comunas) <- NULL
  saveRDS(comunas, LISTA_COMUNAS)
}
# Si algún caso tiene una comuna que no está en la lista, se agrega igual
faltan <- setdiff(casos$comuna_cod, comunas$comuna_cod)
if (length(faltan)) {
  warning("Comunas con casos que no están en la lista de ", ANIO_COMUNAS, ": ", paste(faltan, collapse = ", "))
  comunas <- rbind(comunas, data.frame(comuna_cod = faltan, comuna = faltan))
}
# Nombre único por código (las glosas originales vienen con tildes rotas en algunos años)
casos$comuna <- comunas$comuna[match(casos$comuna_cod, comunas$comuna_cod)]

# ---------------------------------------------------------------------------
# 3. Bases de conteo con ceros explícitos
#    Una celda sin casos es un 0 observado y debe estar en la base (omitirla
#    sesgaría el modelo hacia tasas más altas).
# ---------------------------------------------------------------------------
contar <- function(grilla, claves) {
  n <- aggregate(list(casos = rep(1L, nrow(casos))),
                 by = lapply(casos[claves], as.character), FUN = sum)
  g <- merge(grilla, n, by = claves, all.x = TRUE)
  g$casos[is.na(g$casos)] <- 0L
  g
}

# 3a. Región x sexo x edad x año
grilla_region <- expand.grid(region_cod = ORDEN_REGIONES, sexo = SEXOS, edad = EDADES,
                             anio = as.character(ANIOS), stringsAsFactors = FALSE)
dataset_region <- contar(grilla_region, c("region_cod", "sexo", "edad", "anio"))

# 3b. Comuna x sexo x edad x año
grilla_comuna <- expand.grid(comuna_cod = comunas$comuna_cod, sexo = SEXOS, edad = EDADES,
                             anio = as.character(ANIOS), stringsAsFactors = FALSE)
dataset_comuna <- contar(grilla_comuna, c("comuna_cod", "sexo", "edad", "anio"))
dataset_comuna$comuna     <- comunas$comuna[match(dataset_comuna$comuna_cod, comunas$comuna_cod)]
dataset_comuna$region_cod <- substr(dataset_comuna$comuna_cod, 1, 2)

# Columnas, tipos y orden comunes a ambas bases
ordenar <- function(d, con_comuna) {
  d$anio       <- as.integer(d$anio)
  d$periodo    <- periodo(d$anio)
  d$pandemia   <- pandemia(d$anio)
  d$sexo      <- factor(d$sexo, levels = SEXOS)
  d$edad       <- factor(d$edad, levels = EDADES)
  d$zona       <- factor(ZONAS[d$region_cod], levels = unique(ZONAS[ORDEN_REGIONES]))
  d$region     <- factor(REGIONES[d$region_cod], levels = REGIONES[ORDEN_REGIONES])
  d$region_cod <- factor(d$region_cod, levels = ORDEN_REGIONES)
  cols <- c("anio", "periodo", "pandemia", "zona", "region_cod", "region",
            if (con_comuna) c("comuna_cod", "comuna"), "sexo", "edad", "casos")
  sub_region <- if (con_comuna) d$comuna_cod else rep("", nrow(d))
  d <- d[order(d$anio, d$region_cod, sub_region, d$sexo, d$edad), cols]
  rownames(d) <- NULL
  d
}
dataset_region <- ordenar(dataset_region, FALSE)
dataset_comuna <- ordenar(dataset_comuna, TRUE)

# Control: las tres bases deben sumar lo mismo
stopifnot(sum(dataset_region$casos) == nrow(casos),
          sum(dataset_comuna$casos) == nrow(casos))

# ---------------------------------------------------------------------------
# 4. Guardar
# ---------------------------------------------------------------------------
guardar <- function(d, nombre) {
  saveRDS(d, file.path(DIR_DATOS, paste0(nombre, ".rds")))
  write.csv(d, file.path(DIR_DATOS, paste0(nombre, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
}
guardar(casos,          "casos_A281_limpios")
guardar(dataset_region, "dataset_A281_region")
guardar(dataset_comuna, "dataset_A281_comuna")
saveRDS(filtro, file.path(DIR_DATOS, "filtro_casos.rds"))

# ---------------------------------------------------------------------------
# 5. Resumen
# ---------------------------------------------------------------------------
resumen <- function(d, nombre) {
  data.frame(base = nombre, filas = nrow(d), casos = sum(d$casos),
             pct_ceros = round(100 * mean(d$casos == 0), 1),
             media = round(mean(d$casos), 2), varianza = round(var(d$casos), 2),
             maximo = max(d$casos))
}
print(filtro, right = FALSE, row.names = FALSE)
cat("\n")
print(rbind(resumen(dataset_region, "region x sexo x edad x año"),
            resumen(dataset_comuna, "comuna x sexo x edad x año")), row.names = FALSE)
cat("\nComunas en la lista:", nrow(comunas), "| con al menos un caso:", length(unique(casos$comuna_cod)), "\n")
