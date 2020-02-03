# 2020 02 01
#╔ vamos a hacer un ejercicio
# co2 es un vector medidas de 12 meses de niveles de co2 a alo largo de varios años

library(dplyr)
library(tidyverse)
co2_wide <-data.frame(matrix(co2, ncol=12,byrow=TRUE)) %>%
  setNames(1:12) %>% # nombre de las columnas del 1 al 12 , los meses
  mutate(year = as.character(1959:1997)) # preferimos que los años sean character

head(co2_wide)

# co2_wide está en formato wide y lo tenemos que poner en tidy
co2_tidy <- co2_wide %>%
  pivot_longer(
    cols=-year,
    names_to = "month",
    values_to = "co2_level",
    names_ptypes = list(month=integer()) # queremos este valor como integer para que ggplot lo dibuje bien
  )
co2_tidy

co2_tidy %>% ggplot(aes(month, co2_level, color=year)) + geom_line()

###################################
library(dslabs)
data("murders") # cargamos la base de datos murders

# este plot es muy básico porque es sin tidyverse
plot(murders$total, murders$population/10^5)

plot(murders$total,murders$population/10^5,pch="*",col="red")
points(murders$total,murders$population/10^5,lty=2)

# la funcion lm nos calcula los valores a y b para dibujar la recta regresion lineal
lm(formula=murders$population/10^5~murders$total)
# con abline dibujamos esa línea
abline(a=9.138, b=0.28,col="red")

# histogramas
hist(murders$total)
# cambiamos las bins con el atributo breaks digo CUANTAS bins quiero
hist(murders$total, breaks = 10)

# box plot
boxplot(murders$total)

boxplot(murders$total~murders$region)

# vamos a trabajar con el dataset de los babies
babies <- read.delim("Datasets/babies.txt", header = T, sep = "\t")
plot(babies$gestation, babies$bwt)
# salen valores de gestacion errones de 999 días
# vamos a quitarlos

babies_noNA <- babies %>% filter(gestation != 999)

plot(babies_noNA$gestation, babies_noNA$bwt)
# hazme una regresion del peso del bebe en funcion del tiempo de gestacion
summary(lm(formula = babies_noNA$bwt~babies_noNA$gestation))
# fijarse en Multiple R-Square, valor que va de -1 a 1.
# Cuanto más se acerca a 1 más relación linea hay, y cunado más se acerque a 0 menos.

abline(a=-10, b=0.46,col="red")

# quitamos los valores smoke de nueve (que están mal)
babies_noNA <- babies_noNA %>% filter(smoke!=9)
boxplot(babies_noNA$bwt~babies_noNA$smoke)
hist(babies_noNA$bwt)

# library(gganimate) permite crear gifts animados teniendo los distintos frames
# EMPEZAMOS CON GGPLOT2
# necesitamos definir para el plot 3 cosas:
# 1. data
# 2. informacion gráfica: coordenadas, tamaño de punto, etc. Todo esto se hace 
# con la función aes(). Aes vienen de aesthetics.
# 3. Tipo de geometría, es decir, si quiero histograma, boxplot, scatter, etc.
# Lo importante son lo spuntos 1 y 2. A partir de ellos se pueden dibujar diferentes gráficos.
# por eso el punto 3 es un "añadido" y ponemos un "+"

data(murders)

p <-  murders %>% 
  ggplot(aes(population/10^6, total, label = abb))+
  geom_point(aes(col=region), size = 3) +  
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

p

p <-  murders %>% 
  ggplot(aes(population/10^6, total, label = abb, col=region))+
  geom_point(size = 3) +  # dibuja los puntitos
  geom_text(nudge_x = 0.05)+ # pone los labels a una distancia del punto
  scale_x_log10()+          # utiliza escala logaritmica en x
  scale_y_log10()+          
  xlab("Populations in millions (log scale)")+
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in 2010")+
  theme_bw()    # hay mucho themes que cambian la regilla, el fondo, etc.
  
p

# geom_point no lleva aes() porque esa información gráfica no depende de los datos
# aes solo se ponen cuando hace falta mapear la información con los gráficos
# despues de definir el objeto ggplot es como si fueramos añadiendo capas gráficas con +
# usamos scale_x_log() para desapelmazar los datos
# todo lo que esté dentro de ggplot() es global y lo que esté dentro de cada capa
# solo es visto por cada capa

# una vez definido p podemos seguir añadiendo capas, por ejemplo un texto
p + geom_label(x=1, y=1, label="Hello")
# o añadimos una linea
p + geom_abline(intercep=log10(2))
# o las dos a la vez
p + geom_label(x=1, y=1, label="Hello") + geom_abline(intercep=log10(2))

#COMO CAMBIAR LOS THEMES
# paquete ggrepel pone una leader a los labels
library(ggrepel)
# ggthemes otro paquete con muchos themes
library(ggthemes)

# pero no hacen falta esos paquetes! ya hay muchos themes disponibles por defecto

# geom_text_repel() es la capa que hay que añadir para que ponga el leader

library(ggplot2movies)
install.packages("ggplot2movies")

###############################
# cuantas pelis tiene?
#############################
data(movies)+
View(movies)
dim(movies) # salen 58788 peliculas (filas)
str(movies)
? movies

#############################
# quitar todas las columnas que empiezan por R
#############################
dummy <- movies %>%
  select(-starts_with("r"), -mpaa) %>%
  add_column(Romance<-movies %>% select(Romance))
dummy

# nos ha quitado la columna Ratings y Romance. La necesitamos. Las volvemos a añadir
dummy <- movies %>%
  select(-starts_with("r"), -mpaa) %>%
  mutate(Romance=movies$Romance, Ratings=movies$rating)
dummy


####################
# 3. por año extraee la peli con el peor rating excluyendo las de duración < 30 min o sin presupuesto conocido
####################

m <- movies %>%
  filter(!is.na(budget) & length>30) %>%    # quitamos las que no tienen presupuesto o duran <30min
  group_by(year) %>%  # agrupamos por año
  slice(which.min(rating)) %>%  # nos quedamos sólo con las filas del mínimo rating
  select(title, year, length, budget, rating) %>% # nos quedamos con las columnas que nos interesan
  arrange(desc(year))


# scatter plot con rating frente al budget

pmovies <- m %>%
  ggplot(aes(budget, rating, col=year, size=rating)) + # coloreamos por año y tamaño de punto segun rating
  geom_point() +
  scale_x_log10() +
  theme_classic()
  
pmovies

#####################
# gif
#####################

install.packages("gganimate")
library(gganimate)
data(gapminder) # cargamos el dataset gapminder que está dentro del paquete dslabs
View(gapminder)

# hacer scatter entre esperanza de vida y fertilidad para Europa y Asia
# colorear puntos en funcion del continente
# un plot para cada año

# nos creamos la tabla personalizada.
g <- gapminder %>%
  select(continent, country, year, life_expectancy, fertility) %>%
  filter(continent=="Europe" | continent == "Asia" )

#creamos el plot para todos los años
g %>%
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  facet_wrap(~year) + # esto te pinta un plot por año
  geom_point(size=5)

# quitamos face wrap y usamos transition_states del paquete gganimate
library(gganimate)

gplot <- g %>%
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  geom_point(size=5) +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("Year {closest_state}")

animate(gplot)



