library(ggplot2)

# Importamos los datos a analizar
d <- read.table("data.csv", sep= ",", header=TRUE)


# Graficamos el diagrama de dispersión
ggplot(data = d, mapping = aes(x = d$Fuel.flow..mf...kg.s.., 
                                   y = d$Hight.Pressure..HP..Turbine.exit.temperature..T48...C.)) + 
  geom_point(color = "firebrick", size = 2) +
  labs(title = "Diagrama de dispersión", x = "Flujo de combustible", 
       y = "Temperatura de salida de la turbina") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Test de correlación mediante el método de Pearson
cor.test(x = d$Fuel.flow..mf...kg.s.., y = d$Hight.Pressure..HP..Turbine.exit.temperature..T48...C., method = "pearson")

# 0.9863178 => hay correlación lineal positiva fuerte