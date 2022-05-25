library(EnvStats)


data <- read.table(file.choose(), header = TRUE)
# d <- read.csv(header = T)


# Estadísticos descriptivos de las variables
# a)
InfoEstadisticosDescriptivos("Fuel Flow ", data$Fuel.flow..mf...kg.s..)
InfoEstadisticosDescriptivos("GT Compressor decay state coefficient ", data$GT.Compressor.decay.state.coefficient.)
InfoEstadisticosDescriptivos("GT Turbine decay state coefficient ", data$GT.Turbine.decay.state.coefficient)


# Intervalos de confianza
# b)
IntervaloDeConfianzaMedia("Fuel Flow ", data$Fuel.flow..mf...kg.s..)
IntervaloDeConfianzaMedia("GT Compressor decay state coefficient ", data$GT.Compressor.decay.state.coefficient.)
IntervaloDeConfianzaMedia("GT Turbine decay state coefficient ", data$GT.Turbine.decay.state.coefficient)

IntervaloDeConfianzaVarianza("Fuel Flow ", data$Fuel.flow..mf...kg.s..)
IntervaloDeConfianzaVarianza("GT Compressor decay state coefficient ", data$GT.Compressor.decay.state.coefficient.)
IntervaloDeConfianzaVarianza("GT Turbine decay state coefficient ", data$GT.Turbine.decay.state.coefficient)

# Gráficos de las variables
# c)
Graficos("Fuel Flow ", "Values", data$Fuel.flow..mf...kg.s..)
Graficos("GT Compressor decay state coefficient ", "Values", data$GT.Compressor.decay.state.coefficient.)
Graficos("GT Turbine decay state coefficient ", "Values", data$GT.Turbine.decay.state.coefficient)

# 2)

print(paste("Prueba de Hipótesis para varianza "))
require(stests)
res <- stests::var.test(x = data$Fuel.flow..mf...kg.s.., alternative = "two.sided")
res$conf.int

MediaAritmetica <- function(x) 
{
  return (sum(x) / length(x))
}

Moda <- function(x)
{
  return(as.numeric(names(which.max(table(x)))))
  # set <- unique(x)
  # freq <- tabulate(match(x, set))
  # m <- max(freq)
  # return (set[freq == m])
}

Mediana <- function(x)
{
  return (median(x))
}

DesviacionEstandar <- function(x)
{
  return (sd(x))
}

Varianza <- function(x)
{
  return (DesviacionEstandar(x) ^ 2)
}

CoeficienteVariacion <- function(x)
{
  return (DesviacionEstandar(x) / MediaAritmetica(x))
}

InfoEstadisticosDescriptivos <- function(text, x)
{
  print(text)
  # print(summary(x))
  print(paste("Media Aritmética = ", MediaAritmetica(x)))
  print(paste("Moda = ", Moda(x)))
  print(paste("Mediana = ", Mediana(x)))
  print(paste("Varianza = ", Varianza(x)))
  print(paste("Desviación Estándar = ", DesviacionEstandar(x)))
  print(paste("Coeficiente de Variación = ", CoeficienteVariacion(x)))
  Quartiles(x)
}

Quartiles <- function(x)
{
  quantile(x)
}

IntervaloDeConfianzaMedia <- function(text, x)
{
  nivel_confianza = 0.95
  n <- length(x) # El tamaño válido de la muestra
  media <- MediaAritmetica(x) # la media 
  desv <- DesviacionEstandar(x) # La desviación estándar
  
  error.est <- desv/sqrt(n) # Calculamos el error estándar
  margen.error <- 1.644854 * error.est # nivel de confianza de 95% 
  
  lim.inf <- media - margen.error # Límite inferior del intervalo
  lim.sup <- media + margen.error # Límite superior del intervalo
  
  print(text)
  print(paste("Límite inferior = ", lim.inf))
  print(paste("Límite superior = ", lim.sup))
}

IntervaloDeConfianzaVarianza <- function(text, x)
{
  print(text)
  vt <- varTest(x, conf.level = 0.95)
  print(vt$conf.int)
}

Histograma <- function(name, abscisa, x)
{
  hist(x, ylab = "Frequency", xlab = abscisa, main = name)
}

CajaBigote <- function(name, x)
{
  boxplot(x, main = name)
}

Graficos <- function(name, abscisa, x)
{
  Histograma(name, abscisa, x)
  CajaBigote(name, x)
}
