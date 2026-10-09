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
datos <- readRDS(file.path(DIR_DATOS, "dataset_A281_region.rds"))
datos_comuna <-readRDS(file.path(DIR_DATOS, "dataset_A281_comuna.rds"))

head(datos)
# anio   periodo pandemia  zona region_cod             region   sexo  edad casos poblacion

# ---------------------------------------------------------------------------
# 2. Categorías de referencia
# ---------------------------------------------------------------------------
datos$sexo    <- relevel(datos$sexo,    ref = "HOMBRE")
datos$edad    <- relevel(datos$edad,    ref = "20+")
datos$zona    <- relevel(datos$zona,    ref = "Metropolitana")
datos$periodo <- relevel(datos$periodo, ref = "2010-2019")

# ---------------------------------------------------------------------------
# 3. Modelos
# ---------------------------------------------------------------------------
# Modelo 1: afectan el sexo, edad, zona y periodo el ratio?

modelo_pois1 <- glm(
  casos ~ sexo + edad + zona + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois1)

exp(coef(modelo_pois1))

irr1 <- exp(cbind(IRR = coef(modelo_pois1), confint(modelo_pois1)))
round(irr1, 2)

# Ejemplo: IRR de la pandemia (2020-2021 frente a 2010-2019)
irr1["periodo2020-2021", ] # 2010-2019 va vacío porque es la variable de referencia

modelo_pois1_2 <- glm(
  casos ~ sexo + edad + region + periodo, # region
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois1_2)

exp(coef(modelo_pois1_2))

# Modelo 2: depende el sexo de la edad? 
# añadimos la interacción entre sexo y edad.

modelo_pois2 <- glm(
  casos ~ sexo * edad + zona + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois2)

irr2 <- exp(cbind(IRR = coef(modelo_pois2), confint(modelo_pois2)))
round(irr2, 2)

irr2["sexoMUJER", ]

# Con la interacción, sexoMUJER es el IRR mujer/hombre SOLO en 20+ (la referencia
# de edad). Para obtenerlo en cada grupo de edad, con su IC, se vuelve a ajustar
# el mismo modelo cambiando la referencia de edad y se lee la fila sexoMUJER.
irr_sexo_por_edad <- t(sapply(levels(datos$edad), function(ref_edad) {
  d <- datos
  d$edad <- relevel(d$edad, ref = ref_edad)
  m <- update(modelo_pois2, data = d)
  exp(c(IRR = unname(coef(m)["sexoMUJER"]), confint(m)["sexoMUJER", ]))
}))
round(irr_sexo_por_edad, 2)

# 
modelo_pois2_2 <- glm(
  casos ~ sexo * edad + region + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

# Modelo 3: la pandemia afectó a cada rango de edad de forma separada?

modelo_pois3 <- glm(
  casos ~ sexo * edad + edad * periodo + zona,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois3)

#
modelo_pois3_2 <- glm(
  casos ~ sexo * edad + edad * periodo + region,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

#
modelo_pois3_3 <- glm(
  casos ~ sexo * edad + edad * periodo + comuna,
  offset = log(poblacion),
  family = poisson,
  data = datos_comuna
)

#AIC
AIC(modelo_pois1, modelo_pois1_2, modelo_pois2, modelo_pois2_2, modelo_pois3, modelo_pois3_2, modelo_pois3_3)


# ---------------------------------------------------------------------------
# 4. Pruebas de razón de verosimilitud (modelos anidados)
# ---------------------------------------------------------------------------
# M1 vs M2 -> H0: el IRR mujer/hombre es igual en todas las edades (2 gl)
anova(modelo_pois1, modelo_pois2, test = "Chisq")

# M2 vs M3 -> H0: el IRR de cada período es igual en todas las edades (4 gl)
anova(modelo_pois2, modelo_pois3, test = "Chisq")

# ---------------------------------------------------------------------------
# 5. Tabla comparativa: AIC, devianza/gl y ECM
# ---------------------------------------------------------------------------
# fitted() incluye el offset, así que el ECM queda en escala de conteos
comparar <- function(m) c(
  AIC        = AIC(m),
  dev_gl     = deviance(m) / df.residual(m),
  ECM        = mean((datos$casos - fitted(m))^2),
  pearson_gl = sum(residuals(m, type = "pearson")^2) / df.residual(m)
)
tabla_modelos <- rbind(M1 = comparar(modelo_pois1),
                       M2 = comparar(modelo_pois2),
                       M3 = comparar(modelo_pois3))
round(tabla_modelos, 4)

# ---------------------------------------------------------------------------
# 6. Período en el modelo final (M2): ¿cambió la tasa y se mantuvo?
# ---------------------------------------------------------------------------
# H0: no hay diferencias entre los tres períodos (2 gl)
drop1(modelo_pois2, test = "Chisq")

# 2022-2024 vs 2010-2019 ya está en irr2 (periodo2022-2024).
# 2022-2024 vs 2020-2021: mismo modelo con la pandemia como referencia
datos_pand <- datos
datos_pand$periodo <- relevel(datos_pand$periodo, ref = "2020-2021")
modelo_pois2_pand <- update(modelo_pois2, data = datos_pand)
exp(c(IRR = unname(coef(modelo_pois2_pand)["periodo2022-2024"]),
      confint(modelo_pois2_pand)["periodo2022-2024", ]))

# ---------------------------------------------------------------------------
# 7. Predicción para un perfil
# ---------------------------------------------------------------------------
# Con poblacion = 100.000, el conteo esperado es directamente la tasa por 100.000.
# El IC se calcula en escala log y luego se exponencia.
perfiles <- data.frame(sexo = "MUJER", edad = "0-9", zona = "Sur",
                       periodo = c("2010-2019", "2020-2021"), poblacion = 1e5)
pred <- predict(modelo_pois2, newdata = perfiles, type = "link", se.fit = TRUE)
cbind(perfiles[c("sexo", "edad", "zona", "periodo")],
      tasa_100mil = exp(pred$fit),
      li = exp(pred$fit - 1.96 * pred$se.fit),
      ls = exp(pred$fit + 1.96 * pred$se.fit))