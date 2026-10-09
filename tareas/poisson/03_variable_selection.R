# =============================================================================
# Modelos de Poisson: egresos por enfermedad por arañazo de gato (A28.1)
#
# Unidad de observación: región x sexo x edad x año
# Respuesta Y: casos (egresos con DIAG1 = A28.1)
# Exposición E: poblacion (INE, 30 de junio) -> offset(log(poblacion))
#
# Ejecutar después de 02_poblacion_ine.R, con el directorio de trabajo en
# tareas/poisson. Usa R base.
# =============================================================================

DIR_DATOS <- "datos_egresos"

# ---------------------------------------------------------------------------
# 1. Cargar datos
# ---------------------------------------------------------------------------
# Se lee el .rds (y no el .csv) para conservar los factores y el orden de sus niveles
datos_region <- readRDS(file.path(DIR_DATOS, "dataset_A281_region.rds"))
datos_comuna <-readRDS(file.path(DIR_DATOS, "dataset_A281_comuna.rds"))

head(datos_region)
head(datos_comuna)

length(unique(datos_region$poblacion[datos_region$anio == "2011"]))
length(unique(datos_comuna$comuna))
length(unique(datos_comuna$comuna_cod))
