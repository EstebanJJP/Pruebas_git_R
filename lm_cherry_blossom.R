#Quiero predecir el día de floración de cerezo en kyoto a partir de los datos dados por 

#Cargamos las librerías necesarias

library(rethinking)


#Cargamos los datos de la librería rethinking
data("cherry_blossoms")
d<-cherry_blossoms

#Vamos a empezar ploteando los datos que tenemos
#Con esto preparamos para generar una imagen con 2 filas y 1 columna para poner nuestros dos graficos
#el segundo parametro es para ajustar márgenes
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

#Primero hacemos un plot clásico de puntos relacionando años con día de floración
plot(
  d$year, d$doy,
  pch = 16, col = col.alpha("pink", 0.7),
  xlab = "Año", ylab = "Día del año de primera floración",
  main = "Floración del cerezo en el tiempo"
)

#Ahora hacemos este otro en el que al marcar type=l hacemos lineas para medir la evolución de la temperatura (min, max y media)
plot(
  d$year, d$temp,
  type = "l", lwd = 2, col = "steelblue",
  xlab = "Año", ylab = "Temperatura de marzo",
  main = "Reconstrucción de temperatura de marzo"
)
lines(d$year, d$temp_lower, col = "gray70", lty = 2)
lines(d$year, d$temp_upper, col = "gray70", lty = 2)

#---DEPURACIÓN---

#Vamos a comenzar con una depuración de los datos, para ello vemos NA y outliers. 
#Vamos a generar a partir de ello dos datasets, uno sin NA y otro con valores imputados y a ver cual da mejor resultado

#Comenzamos contando los NA por columna
colSums(is.na(d))
#Vamos a eliminar aquellos en los que no tenemos nuestra variable de respuesta, esto es, el día de floración. 
d2<-d[complete.cases(d$doy), ]
#Sobre este vamos a hacer primero el que tenga todos los datos, sin ningún nulo
d_complete<-d2[complete.cases(d2), ]

#Vamos a ver los boxplots para ver la distribución de los datos
par(mfrow=c(2,1))
boxplot(d$doy, main = "Outliers en doy", horizontal = TRUE)
boxplot(d$temp, main = "Outliers en temp", horizontal = TRUE)

hist(d$temp, breaks = 30)

#Vemos que hay bastanetes outliers en temp, aunque esto no tiene por qué ser malo por eso haremos un modelo con estos
#y otro sin los outliers

#Generamos un nuevo dataset sin outliers

#Calculamos el primer y tercer cuartil y con IQR calculamos el rango intercuartilico (Q3-Q1)
q1 <- quantile(d$temp, 0.25, na.rm = TRUE)
q3 <- quantile(d$temp, 0.75, na.rm = TRUE)
iqr <- IQR(d$temp, na.rm = TRUE)

#Con esto sacamos los valores extremos definiendo los limites inferiores y superiores
lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

#Ahora guardamos estos datos en una nueva variable en nuestro dataset
#que es la temperatura pero si se excede el limite se pone este en lugar del valor outlier
d_complete$temp_cap <- pmin(pmax(d_complete$temp, lim_inf), lim_sup)

hist(d_complete$temp_cap, main="Temperaturas limitadas", na.rm=TRUE)

#---MODELOS---
#Usamos la librería caret para usar la validación cruzada
library(caret)
library(splines)

#Seteamos una semilla de aleatoriedad
set.seed(123)

#Generamos nuestros grupos de control para la cv
control <- trainControl(
  method = "cv",
  number = 5
)

#Primer modelo, regresión lineal con doy y año
m1 <- train(
  doy ~ year,
  data = d_complete,
  method = "lm",
  trControl = control,
  metric = "RMSE"
)
#Segundo modelo, regresión lineal con doy y año + temp 
m2 <- train(
  doy ~ year + temp,
  data = d_complete,
  method = "lm",
  trControl = control,
  metric = "RMSE"
)
#Segundo modelo, regresión lineal con doy y año + temp_cap
m3<-train(
  doy ~ year + temp_cap,
  data = d_complete, 
  method = "lm",
  trControl=control,
  metric="RMSE"
)
#Modelo con de polinomio de grado 2
m4 <- train(
  doy ~ poly(temp_cap, 2),
  data = d_complete,
  method = "lm",
  trControl = control,
  metric = "RMSE"
)

m5 <- train(
  doy ~ bs(temp_cap, df = 5),
  data = d_complete,
  method = "lm",
  trControl = control,
  metric = "RMSE"
)

m1$results
m2$results
m3$results
m4$results
m5$results
#Parece que el mejor de momento es m3

#---PLOTS DE LOS MODELOS
#Importamos la librería ggplot2
library(ggplot2)

#Generamos los valores sobre los datos según nuestro modelo
pred_m3 <- predict(m3, newdata = d_complete)
#Guardamos en un df los datos reales frente a los predichos
plot_m3 <- data.frame(
  real = d_complete$doy,
  predicho = pred_m3
)
#Con esto ploteamos este dataset añadiendo una linea de coincidencia y con geom_point generamos un grafico de dispersión

ggplot(plot_m3, aes(x = real, y = predicho)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(
    x = "Valor real",
    y = "Valor predicho",
    title = "Comparación de predicción del modelo m3"
  ) +
  theme_minimal()

#Ahora uno perro viendo la diferencia año a año
plot_m3_year <- data.frame(
  year = d_complete$year,
  real = d_complete$doy,
  predicho = pred_m3
)

#Aquí ploteamos los puntos el valor  real y en la linea azul el valor predicho en cada año
ggplot(plot_m3_year, aes(x = year)) +
  geom_point(aes(y = real), color = "gray50", alpha = 0.7) +
  geom_line(aes(y = predicho), color = "blue", linewidth = 1) +
  labs(
    x = "year",
    y = "doy",
    title = "Real vs predicho por año"
  ) +
  theme_minimal()

#Vamos a hacer uno sobre m4 que tiene un error muy similar pero queremos ver si la dispersión
#respecto a no considerar los datos sin outliers merece la pena
pred_m4 <- predict(m4, newdata = d_complete)
#Ahora uno pero viendo la diferencia año a año
plot_m4_year <- data.frame(
  year = d_complete$year,
  real = d_complete$doy,
  predicho = pred_m4
)

#Aquí ploteamos los puntos el valor  real y en la linea azul el valor predicho en cada año
ggplot(plot_m4_year, aes(x = year)) +
  geom_point(aes(y = real), color = "gray50", alpha = 0.7) +
  geom_line(aes(y = predicho), color = "blue", linewidth = 1) +
  labs(
    x = "year",
    y = "doy",
    title = "Real vs predicho por año"
  ) +
  theme_minimal()

#---ANALISIS DE RESIDUOS

#Por último vamos a ver cual se comporta mejor según un análisis de los residuos

#Generamos un nuevo dataframe sobre el que generamos los residuos
#Para ello le ponemos los valores predecidos y la diferencia con los reales
#en una nueva columna
d_resid<-d_complete
d_resid$pred_m3 <- predict(m3, newdata = d_resid)
d_resid$resid_m3 <- d_resid$doy - d_resid$pred_m3

#Con esto ploteamos en torno a una linea de 0 de diferencia los residuos obtenidos
#Buscamos que los residuos no tengan un comportamiento no errático, queremos ver una
#nube aleatoria de puntos como la que efectivamente obtenemos

ggplot(d_resid, aes(x = pred_m3, y = resid_m3)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs valores ajustados"
  ) +
  theme_minimal()

#Por último ponemos un Q-Q plot
ggplot(d_resid, aes(sample = resid_m3)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q plot de los residuos") +
  theme_minimal()

#Obtenemos que los valores se agrupan en torno a la linea es que se puede asumir 
#normalidad en los residuos, que es lo que buscamos. 
#El comportamiento de desviación suave en los extremos puede indicar un comportamiento
#atípico o problemático relacionado con valores atípicos o colas pesadas. 
#Por último, no es el caso, pero si vieramos un comportamiento en forma de S
#sería problemáticoporque el supuesto de normalidad fallaría. 
