library(car)
library(psych)
library(lmtest)
library(ggplot2)
library(gridExtra)

# Importamos los datos a analizar
datos <- read.table("data.csv", sep = ",", header=TRUE)

# # Calculamos el coeficiente de correlación
# cor(datos)

# # Graficamos la relación entre las variables
# multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
#            main = "")

# # Generamos el modelo de regresión múltiple
# multi.fit <- lm(formula = datos$Fuel.flow..mf...kg.s.. ~ datos$Hight.Pressure..HP..Turbine.exit.temperature..T48...C. +
#                datos$Lever.position + datos$Ship.speed..v. + datos$Gas.Turbine..GT..shaft.torque..GTT...kN.m.. + 
#                datos$GT.rate.of.revolutions..GTn...rpm.. + datos$Gas.Generator.rate.of.revolutions..GGn...rpm.. + 
#                datos$Starboard.Propeller.Torque..Ts...kN.. + datos$Port.Propeller.Torque..Tp...kN.. +
#                datos$GT.Compressor.inlet.air.temperature..T1...C.. +  datos$GT.Compressor.outlet.air.temperature..T2...C..+
#                datos$HP.Turbine.exit.pressure..P48...bar.. + datos$GT.Compressor.inlet.air.pressure..P1...bar.. +
#                datos$GT.Compressor.outlet.air.pressure..P2...bar.. + datos$GT.exhaust.gas.pressure..Pexh...bar.. +
#                datos$Turbine.Injecton.Control..TIC...... + datos$GT.Compressor.decay.state.coefficient. + 
#                datos$GT.Turbine.decay.state.coefficient, data = datos)
# summary(multi.fit)

# # Seleccionamos los mejores predictores
# step(object = multi.fit, direction = "both", trace = 1)

# El mejor modelo resultante del proceso de selección es
multi.fit = lm(formula = datos$Fuel.flow..mf...kg.s.. ~ datos$Ship.speed..v. + 
               datos$Gas.Turbine..GT..shaft.torque..GTT...kN.m.. +
               datos$GT.rate.of.revolutions..GTn...rpm.., data = datos)
summary(multi.fit)

# # Hallamos los intervalos de confianzas
# confint(lm(formula = datos$Fuel.flow..mf...kg.s.. ~ datos$Ship.speed..v. +
#              datos$Gas.Turbine..GT..shaft.torque..GTT...kN.m.. +
#              datos$GT.rate.of.revolutions..GTn...rpm.., data = datos))

# # Validación de condiciones para la regresión múltiple lineal
# plot1 <- ggplot(data = datos, aes(datos$Ship.speed..v., modelo$residuals)) +
#   geom_point() +
#   labs(title = "", x = "Ship speed", y = "Residuals") +
#   geom_smooth(formula = "y ~ x", method = "loess", color = "firebrick") +
#   geom_hline(yintercept = 0) +
#   theme_bw()
# plot2 <- ggplot(data = datos, aes(datos$Gas.Turbine..GT..shaft.torque..GTT...kN.m.., modelo$residuals)) +
#   geom_point() +
#   labs(title = "", x = "Gas Turbine", y = "Residuals") +
#   geom_smooth(formula = "y ~ x", method = "loess", color = "firebrick") +
#   geom_hline(yintercept = 0) +
#   theme_bw()
# plot3 <- ggplot(data = datos, aes(datos$GT.rate.of.revolutions..GTn...rpm.., modelo$residuals)) +
#   geom_point() +
#   labs(title = "", x = "GT revolutions", y = "Residuals") +
#   geom_smooth(formula = "y ~ x", method = "loess", color = "firebrick") +
#   geom_hline(yintercept = 0) +
#   theme_bw()
# grid.arrange(plot1, plot2, plot3)


# Análisis de residuos
mean(multi.fit$residuals)
sum(multi.fit$residuals)

ks.test(multi.fit$residuals, pnorm, mean(multi.fit$residuals), sd(multi.fit$residuals))

bptest(multi.fit)

dwtest(multi.fit)

# Graficando los residuos
layout(matrix(c(1,2,3,4),2,2,byrow = T))

plot(multi.fit$fitted.values, rstandard(multi.fit), 
     main = "Multi Fit Standarized Residuals",
     xlab = "Predictions", ylab = "Standarized Resid", 
     ylim = c(-2.5, 2.5))

abline(h = 0, lty = 2)

res = multi.fit$residuals

hist(res, main = "Residuals histogram")

qqnorm(res)
qqline(res)


#Creating a data frame
variable_fuel_flow <- data.frame(datos$Fuel.flow..mf...kg.s..)

# Make predictions
predictions <- predict(modelo, newdata = variable_fuel_flow)

# Model performance
# Compute the prediction error, RMSE

RMSE = sqrt(mean((datos$Fuel.flow..mf...kg.s.. - predictions)^2))



# Interpretación de resultados
# Lo primero que observamos es que el coeficiente del intercepto no es mucho más grande que los coeficientes del resto de las variables, 
# lo que quiere decir que hay una gran parte del flujo de combustible que está explicada a partir de las variables independientes.

# Se puede observar que los coeficientes de las variables independientes son significativos al 0%, lo que es muy bueno.

# Por cada aumento de la velocidad de la planta de producción disminuye en 0.02 el flujo de combustible. 

# Por cada aumento de la variable independiente 2 aumenta en 3.057 * 10^(-5) el flujo de combustible.

# Por cada aumento de la variable independiente 3 disminuye en 2.997 * 10^(-5) el flujo de combustible.

# El R-cuadrado ajustado es 0.998 por lo que el modelo podemos decir es casi ideal.

# Teniendo en cuenta que el nivel de significación del coeficiente del intercepto es muy bueno, 
# indica que no es necesario considerar otros factores en la producción de flujo de combustible.

# El p-valor del estadígrafo F es menor que 0.05 por lo que podemos afirmar que existe al menos una variable 
# significativamente diferente a cero en el modelo.

# En cuanto al análisis de residuos:
# El p-valor del test de Kolmogorov-Smirnov es menor que 0.05, se rechaza la hipótesis nula y por tanto, 
# los errores no siguen una distribución normal.

# El p-valor del test de Durbin-Watson es menor que 2.2e-16, menor que 0.05, 
# se afirma que los errores nos son independientes.

# El p-valor del test de Breusch-Pagan es menor que 0.05, afirmamos entonces 
# que se cumple la heterocedasticidad.

