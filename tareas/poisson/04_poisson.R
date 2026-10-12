# =============================================================================
# Modelos de Poisson: egresos por enfermedad por arañazo de gato (A28.1)
#
# Unidad de observación: región x sexo x edad x año
# Respuesta Y: casos (egresos con DIAG1 = A28.1)
# Exposición E: poblacion (INE, 30 de junio) -> offset(log(poblacion))
# =============================================================================

DIR_DATOS <- "datos_egresos"

datos <- readRDS(file.path(DIR_DATOS, "dataset_A281_region.rds"))

head(datos)
# anio   periodo pandemia  zona region_cod             region   sexo  edad casos poblacion

# categorías de referencia
datos$sexo    <- relevel(datos$sexo,    ref = "HOMBRE")
datos$edad    <- relevel(datos$edad,    ref = "20+")
datos$zona    <- relevel(datos$zona,    ref = "Metropolitana")
datos$periodo <- relevel(datos$periodo, ref = "2010-2019")
datos$region  <- relevel(datos$region,  ref = "Metropolitana")

# Región con Aysén y Magallanes unidas: Magallanes no tiene casos (su coeficiente
# diverge) y Aysén tiene uno.
datos$region_austral <- datos$region
levels(datos$region_austral)[levels(datos$region_austral) %in% c("Aysén", "Magallanes")] <- "Aysén y Magallanes"

## Modelos Poisson

# Modelo 1: afectan el sexo, edad, region y periodo el ratio?
modelo_pois1 <- glm(
  casos ~ sexo + edad + region_austral + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois1)

# Modelo 2: depende el sexo de la edad? 
# añadimos la interacción entre sexo y edad.
modelo_pois2 <- glm(
  casos ~ sexo * edad + region_austral + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois2)

# Modelo 3: la pandemia afectó a cada rango de edad de forma separada?
modelo_pois3 <- glm(
  casos ~ sexo + edad * periodo + region_austral,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois3)

# Modelo 4: interacción sexo:edad y periodo:edad juntas
modelo_pois4 <- glm(
  casos ~ sexo * edad + edad * periodo + region_austral,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois4)

# años
modelo_pois2_3 <- glm(
  casos ~ sexo * edad + region_austral + periodo + anio,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

## Summaries y coeficientes

summary(modelo_pois1) # sexo + edad + region_austral + periodo
irr1 <- exp(cbind(IRR = coef(modelo_pois1), confint(modelo_pois1)))
round(irr1, 2)
irr1[c("periodo2020", "periodo2021"), ]
irr1["sexoMUJER", ]

summary(modelo_pois2) # sexo * edad + region_austral + periodo (reference group: hombre 20+ años)
# cambio grupo de referencia (hombre)
irr2 <- exp(cbind(IRR = coef(modelo_pois2), confint(modelo_pois2)))
round(irr2, 2)
irr_sexo_por_edad <- t(sapply(levels(datos$edad), function(ref_edad) {
  d <- datos
  d$edad <- relevel(d$edad, ref = ref_edad)
  m <- update(modelo_pois2, data = d)
  exp(c(IRR = unname(coef(m)["sexoMUJER"]), confint(m)["sexoMUJER", ]))
}))
round(irr_sexo_por_edad, 2)

summary(modelo_pois3) # casos ~ sexo + edad * periodo + region_austral
irr3 <- exp(cbind(IRR = coef(modelo_pois3), confint(modelo_pois3)))
round(irr3, 2)
coefs_periodo <- c("periodo2020", "periodo2021", "periodo2022-2024")
# reference group (2010 - 2019)
irr_periodo_por_edad <- t(sapply(levels(datos$edad), function(ref_edad) {
  d <- datos
  d$edad <- relevel(d$edad, ref = ref_edad)
  m <- update(modelo_pois3, data = d)
  ic <- confint(m, parm = coefs_periodo)
  res <- exp(cbind(IRR = coef(m)[coefs_periodo], IC_inf = ic[, 1], IC_sup = ic[, 2]))
  setNames(as.vector(t(res)),
           paste(rep(sub("^periodo", "", coefs_periodo), each = 3),
                 colnames(res), sep = "_"))
}))
round(irr_periodo_por_edad, 2)

summary(modelo_pois4) # casos ~ sexo * edad + edad * periodo + region_austral
irr4 <- exp(cbind(IRR = coef(modelo_pois4), confint(modelo_pois4)))
round(irr4, 2)

modelos <- list(
  M1                = modelo_pois1,
  # M1_2         = modelo_pois1_2,
  M2                = modelo_pois2,
  # M2_2         = modelo_pois2_2,
  # M2_3 = modelo_poi2_3,
  M3                = modelo_pois3,
  # M3_2         = modelo_pois3_2,
  M4 = modelo_pois4
  # M4_2 = modelo_pois4_2
)


## Tabla AIC, devianza residual / gl, ECM y Pearson / gl (control de sobredispersión).
tabla_comparacion <- data.frame(
  parametros = sapply(modelos, function(m) m$rank),
  AIC        = sapply(modelos, AIC),
  dev_gl     = sapply(modelos, function(m) deviance(m) / df.residual(m)),
  ECM        = sapply(modelos, function(m) mean((datos$casos - fitted(m))^2)),
  pearson_gl = sapply(modelos, function(m) sum(residuals(m, type = "pearson")^2) / df.residual(m))
)
round(tabla_comparacion, 4) # M2

tabla_comparacion[order(tabla_comparacion$AIC), ] # M2, M4, M1, M3
tabla_comparacion[order(tabla_comparacion$dev_gl), ] # M4, M2, M3, M1
tabla_comparacion[order(tabla_comparacion$ECM), ] # M2, M1, M4, M3

m1 <- modelo_pois1   # efectos principales
m2 <- modelo_pois2   # + sexo:edad
m3 <- modelo_pois3   # + edad:periodo
m4 <- modelo_pois4 # sexo:edad + edad:periodo

## Pruebas de razón de verosimilitud (modelos anidados)
####### porqué esto y no AIC?
# If H0 is true, the big model only gains a little by fitting noise, so LR is small.
# If H0 is false, the big model captures real structure, so LR is large.

anova(m1, m2, test = "Chisq") # rechaza H0
anova(m3, m4, test = "Chisq")  # rechaza H0
anova(m1, m3, test = "Chisq")  # no rechaza H0
anova(m2, m4, test = "Chisq")  # no rechaza H0

drop1(m2, test = "Chisq")
drop1(m3, test = "Chisq")

## Predicción
# Con poblacion = 100.000, el conteo esperado es directamente la tasa por 100.000.
# El IC se calcula en escala log y luego se exponencia.
perfiles <- data.frame(sexo = "MUJER", edad = "0-9", region_austral = "Metropolitana",
                       periodo = levels(datos$periodo), poblacion = 1e5)
pred <- predict(m2, newdata = perfiles, type = "link", se.fit = TRUE)
cbind(perfiles[c("sexo", "edad", "region_austral", "periodo")],
      tasa_100mil = exp(pred$fit),
      li = exp(pred$fit - 1.96 * pred$se.fit),
      ls = exp(pred$fit + 1.96 * pred$se.fit))

# Observado vs. predicho por M2 para el mismo perfil, por período.
# casos_esp suma los conteos esperados de las filas del perfil (con su poblacion real).
# p_poisson: prueba exacta de que los casos observados vienen de una Poisson(casos_esp).
obs_perfil <- subset(datos, sexo == "MUJER" & edad == "0-9" & region_austral == "Metropolitana")
obs_perfil$casos_esp <- predict(m2, newdata = obs_perfil, type = "response")
agg <- aggregate(cbind(casos, casos_esp, poblacion) ~ periodo, data = obs_perfil, FUN = sum)

pred_perfil <- data.frame(periodo = perfiles$periodo,
                          tasa_m2 = exp(pred$fit),
                          li      = exp(pred$fit - 1.96 * pred$se.fit),
                          ls      = exp(pred$fit + 1.96 * pred$se.fit))
tabla_perfil <- merge(agg, pred_perfil, by = "periodo")
tabla_perfil$tasa_obs  <- 1e5 * tabla_perfil$casos / tabla_perfil$poblacion
tabla_perfil$O_E       <- tabla_perfil$casos / tabla_perfil$casos_esp
tabla_perfil$dentro_IC <- tabla_perfil$tasa_obs >= tabla_perfil$li & tabla_perfil$tasa_obs <= tabla_perfil$ls
tabla_perfil$p_poisson <- mapply(function(o, e) poisson.test(o, r = e)$p.value,
                                 tabla_perfil$casos, tabla_perfil$casos_esp)
tabla_perfil <- tabla_perfil[match(levels(datos$periodo), tabla_perfil$periodo), ]
tabla_perfil$poblacion <- NULL
num <- sapply(tabla_perfil, is.double)
tabla_perfil[num] <- round(tabla_perfil[num], 3)
tabla_perfil

# Mismo perfil, error de cada modelo (filas región-sexo-edad-año del perfil)
sapply(list(M1 = m1, M2 = m2, M3 = m3, M4 = m4), function(m) {
  e <- predict(m, newdata = obs_perfil, type = "response")
  c(ECM = mean((obs_perfil$casos - e)^2), O_E_total = sum(obs_perfil$casos) / sum(e))
})



# sensitivity m2
datos$t <- datos$anio - 2010
m2_trend <- update(m2, . ~ . + t);                  anova(m2, m2_trend, test = "Chisq")
m2_anio  <- update(m2, . ~ . - periodo + factor(anio)); anova(m2, m2_anio, test = "Chisq"); BIC(m2, m2_anio)
m2_qp    <- update(m2, family = quasipoisson);      summary(m2_qp)$dispersion
m2_nb    <- MASS::glm.nb(casos ~ sexo * edad + region_austral + periodo + offset(log(poblacion)), data = datos)