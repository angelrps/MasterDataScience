####################################################################
# Considera el dataset Auto, que tienes disponible en el fichero auto.csv del paquete ISRL
# Ajusta un modelo lineal de mpg frente a horsepower y comenta los resultados.
library(dplyr)
library(ISLR)
data("Auto")
lm_auto <- lm(mpg ~horsepower, Auto)
lm_auto

glimpse(Auto)
summary(Auto)
pairs(Auto, col="red")

# no utilizaremos las variables discretas "cylinders" y "origin" porque pueden dar problemas.
# tambpoco "name" que son charaters.

####################################################################
# ¿Existe relación entre el predictor y la respuesta?
# ¿Cómo de fuerte es esa relación?
# ¿Es la relación entre el predictor y la respuesta positiva o negativa?

cor(Auto$mpg, Auto$horsepower)
# Sí, su valor es de -0.77
# Es una correlación alta y negativa.
summary(lm_auto)
# Por cada unidad de horsepower, la respuesta mpg disminuye -0.158

####################################################################
# ¿Cuál es el mpg predicho para un horsepower de 98?
lm_auto <- lm(mpg ~horsepower, Auto)
lm_auto
new_value <- data.frame(horsepower = 98)
pred_Auto <- predict(lm_auto, new_value, interval = "confidence", level = 0.99)
pred_Auto
# El valor predicho es 24.46

# Da un intervalo de confianza del 99% para ese valor
confint(lm_auto, level =  0.99)

####################################################################
# Representa gráficamente la respuesta y el predictor, así como el modelo que has ajustado.
plot(x = Auto$horsepower, y = Auto$mpg, col = "blue", xlab = "HORSEPOWER", ylab = "MPG")
abline(a = 39.9359, b = -0.1578, col="red")

####################################################################
# Representa gráficamente los residuos del modelo y comenta posibles problemas que puedas encontrar
plot(lm_auto)

plot(lm_auto, lm_auto$residuals)
# Al dibujar horsepower con los residuos se aprecia una gran dispersión de los residuos en valores bajos
# Se debería intentar reducir esto, por ej añadiendo información o escalando los datos.

####################################################################
# Intenta otro tipo de análisis en busca de algún insight sorprendente.
# Vamos a linealizar la dependencia entre mpg y horsepower aplicando escala logaritmica

log_hp <- log(Auto$horsepower)
log_mpg <- log(Auto$mpg)

lm_fit_log <- lm(log_mpg ~ log_hp)
summary(lm_fit_log)

plot(log_hp, log_mpg)
abline(lm_fit_log, col = "red")

# determinamos nuevo valor de horsepower = 98
new_hp <- data.frame(log_hp = log(98))
predicted_values <- predict(lm_fit_log, new_hp, interval = "confidence", level = 0.99)
exp(predicted_values)

# estudiamos los residuos
plot(lm_fit_log)


#######################################################################
# AJUSTE MODELO LINEAL MÚLTIPLE

#######################################################################
adv <- read.delim("advertising.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
View(adv)

# AJUSTAR SALES FRENTE A TV, RADIO Y NEWSPAPER
lm_fit_sales_all <- lm(adv$Sales ~ ., data = adv)
summary(lm_fit_sales_all)

# AJUSTAR SALES FRENTE A TV Y RADIO
lm_fit_sales_TV_Radio <- lm(adv$Sales ~ . - adv$Newspaper, data = adv)
summary(lm_fit_sales_TV_Radio)

# REPRESENTAR VALORES REALES FRENTE A PREDICHOS Y RESIDUOS FRENTE A VALORES REALES
plot(adv$Sales, lm_fit_sales_TV_Radio$fitted.values, col = "red")
plot(adv$Sales,lm_fit_sales_TV_Radio$residuals, col = "red")



#######################################################################
# AJUSTE MODELO LINEAL MÚLTIPLE
# SOBRE EL DATASET DE AUTO
#######################################################################

# Realiza un analisis exploratorio sencillo
DescTools::Desc(Auto)

# Representa todos los pares de variables numéricas.
auto_num <- Auto[,-which(colnames(Auto) == "name")]
pairs(auto_num, col = "red")

# Y calcula la matriz de correlaciones.
cor(auto_num)
# existe cierta correlación entre todas las variables.
# A priori eliminaremos 'displacement' por tener mucha correlación con el resto de variables.
# También quitamos 'origin' por ser una variable categórica.

lm_fit_auto_all <- lm(auto_num$mpg ~ . -origin -displacement, data = auto_num)
summary(lm_fit_auto_all)

# Eliminamos la variable con el P valor más alto: horsepower
lm_fit_auto_cway <- update(lm_fit_auto_all, . ~ . - horsepower)
summary(lm_fit_auto_cway)

# De nuevo extraemos la que tiene el P valor más alto: cylinders
lm_fit_auto_way <- update(lm_fit_auto_cway, . ~ . - cylinders)
summary(lm_fit_auto_way)

# De nuevo extraemos la que tiene el P valor más alto: acceleration
lm_fit_auto_wy <- update(lm_fit_auto_way, . ~ . - acceleration)
summary(lm_fit_auto_wy)

# ¿Existen problemas en el ajuste del modelo?
plot(lm_fit_auto_wy)





