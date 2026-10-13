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
datos$edad    <- relevel(datos$edad,    ref = "0-9")
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
  casos ~ sexo + edad + sexo * edad + region_austral + periodo,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois2)

# Modelo 3: la pandemia afectó a cada rango de edad de forma separada?
modelo_pois3 <- glm(
  casos ~ sexo + edad + periodo + edad * periodo + region_austral,
  offset = log(poblacion),
  family = poisson,
  data = datos
)

summary(modelo_pois3)

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
round(irr1, 5)
irr1[c("periodo2020", "periodo2021", "periodo2022-2024"), ]
irr1["sexoMUJER", ]
irr1[c("edad10-19", "edad20+"), ]

summary(modelo_pois2) # sexo + edad + sexo * edad + region_austral + periodo (reference group: hombre 20+ años)
# cambio grupo de referencia (hombre)
irr2 <- exp(cbind(IRR = coef(modelo_pois2), confint(modelo_pois2)))
round(irr2, 2)
irr_sexo_por_edad <- t(sapply(levels(datos$edad), function(ref_edad) {
  d <- datos
  d$edad <- relevel(d$edad, ref = ref_edad)
  m <- update(modelo_pois2, data = d)
  exp(c(IRR = unname(coef(m)["sexoMUJER"]), confint(m)["sexoMUJER", ]))
}))
round(irr_sexo_por_edad, 5)
irr2[c("periodo2020", "periodo2021", "periodo2022-2024"), ]
irr2[grep("^region_austral", rownames(irr2)), ] # reference group (Metropolitana)

summary(modelo_pois3) # casos ~ sexo + edad * periodo + region_austral
irr3 <- exp(cbind(IRR = coef(modelo_pois3), confint(modelo_pois3)))
round(irr3, 2)
# reference group (20+ años), dentro de cada periodo
coefs_edad <- c("edad10-19", "edad20+")
irr_edad_por_periodo <- t(sapply(levels(datos$periodo), function(ref_periodo) {
  d <- datos
  d$periodo <- relevel(d$periodo, ref = ref_periodo)
  m <- update(modelo_pois3, data = d)
  ic <- confint(m, parm = coefs_edad)
  res <- exp(cbind(IRR = coef(m)[coefs_edad], IC_inf = ic[, 1], IC_sup = ic[, 2]))
  setNames(as.vector(t(res)),
           paste(rep(sub("^edad", "", coefs_edad), each = 3),
                 colnames(res), sep = "_"))
}))
round(irr_edad_por_periodo, 5)


modelos <- list(
  M1                = modelo_pois1,
  # M1_2         = modelo_pois1_2,
  M2                = modelo_pois2,
  # M2_2         = modelo_pois2_2,
  # M2_3 = modelo_poi2_3,
  M3                = modelo_pois3
  # M3_2         = modelo_pois3_2,
  # M4 = modelo_pois4
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
# m4 <- modelo_pois4 # sexo:edad + edad:periodo

## Pruebas de razón de verosimilitud (modelos anidados)
# If H0 is true, the big model only gains a little by fitting noise, so LR is small.
# If H0 is false, the big model captures real structure, so LR is large.

anova(m1, m2, test = "Chisq") # rechaza H0
# anova(m3, m4, test = "Chisq")  # rechaza H0
anova(m1, m3, test = "Chisq")  # no rechaza H0
# anova(m2, m4, test = "Chisq")  # no rechaza H0
AIC(m2, m3)

drop1(m2, test = "Chisq")
drop1(m3, test = "Chisq")

## Predicción
perfiles <- data.frame(sexo = "MUJER", edad = "0-9", region_austral = "Metropolitana",
                       periodo = levels(datos$periodo), poblacion = 1e5)
pred <- predict(m2, newdata = perfiles, type = "link", se.fit = TRUE)
cbind(perfiles[c("sexo", "edad", "region_austral", "periodo")],
      tasa_100mil = exp(pred$fit),
      li = exp(pred$fit - 1.96 * pred$se.fit),
      ls = exp(pred$fit + 1.96 * pred$se.fit))

# real vs. predicho por M2 para el mismo perfil, por período.
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
sapply(list(M1 = m1, M2 = m2, M3 = m3), function(m) {
  e <- predict(m, newdata = obs_perfil, type = "response")
  c(ECM = mean((obs_perfil$casos - e)^2), O_E_total = sum(obs_perfil$casos) / sum(e))
})

# Observado vs. esperado por edad x período (suma sobre sexo y región).
oe_edad_periodo <- function(m) {
  d <- datos
  d$casos_esp <- fitted(m)                      # conteo esperado de cada celda (incluye el offset)
  agg <- aggregate(cbind(casos, casos_esp, poblacion) ~ edad + periodo, data = d, FUN = sum)
  agg$tasa_obs  <- 1e5 * agg$casos / agg$poblacion
  agg$tasa_esp  <- 1e5 * agg$casos_esp / agg$poblacion
  agg$O_E       <- agg$casos / agg$casos_esp
  agg$p_poisson <- mapply(function(o, e) poisson.test(o, r = e)$p.value, agg$casos, agg$casos_esp)
  agg$poblacion <- NULL
  agg <- agg[order(agg$edad, agg$periodo), ]
  rownames(agg) <- NULL
  num <- sapply(agg, is.double)
  agg[num] <- round(agg[num], 3)
  agg
}
oe_m2 <- oe_edad_periodo(m2)
oe_m2

# Mismo cálculo con M3: al incluir edad:periodo, O/E = 1 en cada combinación
oe_m3 <- oe_edad_periodo(m3)
oe_m3[, c("edad", "periodo", "casos", "casos_esp", "O_E")]
