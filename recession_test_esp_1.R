library(tidyverse)
library(caret)
library(pROC)
library(ranger)
library(glmnet)
library(xgboost)

set.seed(123)
#Mola esta función file.choose() que me abre un desplegable para seleccionar el archivo que quiero
data <- read_csv(file.choose())

data <- data %>%
  mutate(
    target = factor(target, levels = c(0, 1)),
    target = relevel(target, ref = "0")
  ) %>%
  drop_na()

data <- data %>%
  select(-TIME_PERIOD, -gdp_future)

split <- createDataPartition(data$target, p = 0.8, list = FALSE)
train_data <- data[split, ]
test_data  <- data[-split, ]

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

metric <- "ROC"

#La librería caret ajusta utilizando como targets variables de nombres de clase, no
#como factores de 0 y 1 como está codificada, con esto lo reconfiguramos a no y a sí
train_data <- train_data %>%
  mutate(target = factor(target, levels = c(0, 1), labels = c("no", "si")))

test_data <- test_data %>%
  mutate(target = factor(target, levels = c(0, 1), labels = c("no", "si")))


model_glm <- train(
  target ~ .,
  data = train_data,
  method = "glm",
  family = binomial(),
  trControl = ctrl,
  metric = metric
)

model_rf <- train(
  target ~ .,
  data = train_data,
  method = "ranger",
  trControl = ctrl,
  metric = metric,
  tuneLength = 5,
  importance = "impurity"
)

model_glmnet <- train(
  target ~ .,
  data = train_data,
  method = "glmnet",
  trControl = ctrl,
  metric = metric,
  tuneLength = 10
)

model_xgb <- train(
  target ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = ctrl,
  metric = metric,
  tuneLength = 5
)

resumen <- resamples(list(
  GLM = model_glm,
  RF = model_rf,
  GLMNET = model_glmnet,
  XGB = model_xgb
))

summary(resumen)

results <- tibble(
  Modelo = c("GLM", "RF", "GLMNET", "XGB"),
  ROC = c(
    max(model_glm$results$ROC, na.rm = TRUE),
    max(model_rf$results$ROC, na.rm = TRUE),
    max(model_glmnet$results$ROC, na.rm = TRUE),
    max(model_xgb$results$ROC, na.rm = TRUE)
  )
) %>%
  arrange(desc(ROC))

print(results)

best_model_name <- results$Modelo[1]
best_model <- switch(
  best_model_name,
  GLM = model_glm,
  RF = model_rf,
  GLMNET = model_glmnet,
  XGB = model_xgb
)

pred_probs <- predict(best_model, newdata = test_data, type = "prob")
pred_class <- predict(best_model, newdata = test_data)

conf_matrix <- confusionMatrix(pred_class, test_data$target, positive = "si")
print(conf_matrix)

roc_obj <- roc(response = test_data$target, predictor = pred_probs[, "si"], levels = c("no", "si"))
auc_value <- auc(roc_obj)
print(auc_value)

#Vemos los graficos de estos resultados

library(ggplot2)
library(dplyr)
library(tidyr)

cm <- as.data.frame(conf_matrix$table)

ggplot(cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Matriz de confusión",
    x = "Clase real",
    y = "Clase predicha"
  ) +
  theme_minimal()


metricas <- data.frame(
  metric = c("Accuracy", "Sensitivity", "Specificity", "Balanced Accuracy"),
  value = c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Balanced Accuracy"]
  )
)

ggplot(metricas, aes(x = metric, y = value, fill = metric)) +
  geom_col(width = 0.7) +
  ylim(0, 1) +
  labs(
    title = "Métricas del modelo",
    x = NULL,
    y = "Valor"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(pROC)

roc_obj <- roc(test_data$target, pred_probs[, "si"])

plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC")
auc(roc_obj)

#EL modelo presenta una baja precisión general, no es un buen modelo porque falla
#mucho al detectar recesiones económicas como positivos que es nuestro objetivo
#Sería interesante buscar más variables u observaciones y construir el proceso desde cero
#A ver que se obtiene.

write_csv(results, "resultados_modelos.csv")
write_csv(as_tibble(conf_matrix$table), "matriz_confusion.csv")
