library("ISLR")
library("caret")

set.seed(123)

n <- nrow(Default)

# cross validation

id_train <- sample(
  1:n,
  size = 0.7 * n
)

train <- Default[id_train, ]
test <- Default[-id_train, ]

modelo_logit <- glm(
  default ~ student + balance,
  family= binomial,
  data = train
)

prob_test <- predict(
  modelo_logit,
  newdata = test,
  type = "response"
)

# umbral 0.5
pred_test05 <- ifelse(
  prob_test >= 0.5,
  "Yes",
  "No"
)

tabla05 <- table(
  Predicho = pred_test05,
  Real = test$default
)

tabla05

# umbral 0.2
pred_test02 <- ifelse(
  prob_test >= 0.2,
  "Yes",
  "No"
)

tabla02 <- table(
  Predicho = pred_test02,
  Real = test$default
)

tabla02

metricas <- function(tabla) {
  TP <- tabla["Yes", "Yes"]
  TN <- tabla["No", "No"]
  FP <- tabla["Yes", "No"]
  FN <- tabla["No", "Yes"]
  accuracy <- (TP + TN) / sum(tabla)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2 * precision * recall / (precision + recall)
  c(accuracy = accuracy, precision = precision, recall = recall, F1 = F1)
}

metricas(tabla05)
metricas(tabla02)

# grilla de umbrales
umbrales <- setdiff(round(seq(0.05, 0.95, by = 0.05), 2), c(0.2, 0.5))
real <- test$default

resultados <- data.frame()

for (u in umbrales) {
  pred <- ifelse(prob_test >= u, "Yes", "No")

  TP <- sum(pred == "Yes" & real == "Yes")
  TN <- sum(pred == "No" & real == "No")
  FP <- sum(pred == "Yes" & real == "No")
  FN <- sum(pred == "No" & real == "Yes")

  accuracy <- (TP + TN) / length(real)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2 * precision * recall / (precision + recall)

  resultados <- rbind(
    resultados,
    data.frame(umbral = u, TP, TN, FP, FN, accuracy, precision, recall, F1)
  )
}

resultados

# umbral con mayor F1
resultados[which.max(resultados$F1), ]

#curva roc (IA)

umbrales_roc <- sort(c(umbrales, 0.2, 0.5))

TPR <- sapply(umbrales_roc, function(u) {
  sum(prob_test >= u & real == "Yes") / sum(real == "Yes")
})
FPR <- sapply(umbrales_roc, function(u) {
  sum(prob_test >= u & real == "No") / sum(real == "No")
})

plot(
  c(1, FPR, 0), c(1, TPR, 0),
  type = "l",
  xlim = c(0, 1), ylim = c(0, 1),
  xlab = "Tasa de falsos positivos (FPR)",
  ylab = "Tasa de verdaderos positivos (TPR)",
  main = "Curva ROC"
)
abline(0, 1, lty = 2, col = "gray")


"
al disminuir el umbral, aumenta recall, ya que hay más holgura para detecatr más casos positivos,
sin embargo se pierde precisión, ya que podemos caer en falsos negativos.

si el objetivo es detectar la mayor cantidad de clientes en mora, nos interesa más la 
métrica recall, por lo que el umbral que tuvo mejor resultado es 0.2, ya que nos permite
detectar la mayor cantidad de clientes default sin comprometer demasiado las otras metricas.
el umbral con mejor F1 score es 0.3, encontrado por la grilla de valores.
"

