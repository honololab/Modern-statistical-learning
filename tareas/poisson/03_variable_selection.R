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

# AIC selection

modelo_pois <- glm(
  casos ~ sexo + edad + zona + region + periodo + pandemia + anio,
  offset = log(poblacion),
  family = poisson,
  data = datos_region
)

modelo_completo <- glm(
  casos ~ region + periodo + pandemia + anio + sexo + edad + zona,
  offset = log(poblacion),
  family = poisson,
  data = datos_region
)

modelo_nulo <- glm(
  casos ~ 1,
  offset = log(poblacion),
  family = poisson,
  data = datos_region
)

modelo_backward <- step(modelo_completo, direction = "backward")
# modelo_forward <- step(modelo_nulo, scope = alcance, direction = "forward")

# edad + region + periodo + anio
# region + periodo + anio + edad

summary(modelo_backward)

class(datos_region$anio)
class(datos_region$periodo)

