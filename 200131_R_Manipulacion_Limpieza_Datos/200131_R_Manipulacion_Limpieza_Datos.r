# vamos a almacenar el resultado de 100 tiradas del script play que hicimos en la clase pasada
tiradas<-numeric()
for (i in 1:100){tiradas[i]<-play()}

# vamos a guardar 100 tiradas en una matriz llamada jugadas
jugadas<-matrix(nrow=100, ncol=3)
for (i in 1:100){jugadas[i,]<-get_symbols()}
colnames(jugadas)<-c("first","second","third")

for (i in colnames(jugadas)){print(jugadas[1,i])}

s1<-numeric()
for (i in 1:100){
  s[i]<-get_score(jugadas[i,])
}

# aplica la funcion score a la columna 1 de jugadas...??? revisar
s2<-apply(jugadas,1,get_score)


# trabajando con el fichero babys.
# calcular la media del peso al nacer para los bebes de madres
# fumadoras y no fumadoras

babies <- read.delim("Datasets/babies.txt", header=T, sep="\t", stringsAsFactors = F)
head(babies)
mean(babies$bwt[babies$"smoke"==0])
mean(babies$bwt[babies$"smoke"==1])


# De las siguientes parejas, cual dirias que tuvo la mayor mortalidad infantil en 2015?
# nos aseguramos de tener cargado el paquete dplyr
library(dplyr)

#  ++ Sri Lanka or Turkey
# llamamos a gapminder que es un data.frame
gapminder %>% # lo pasamos por pipe a filter()
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>% # ojo poner el pipe al final de la fila
  select(country, infant_mortality)

#++ Poland or South Korea
gapminder %>% # lo pasamos por pipe a filter()
  filter(year == 2015 & country %in% c("Poland", "South Korea")) %>% # ojo poner el pipe al final de la fila
  select(country, infant_mortality)

#++ Malaysia or Russia
gapminder %>% # lo pasamos por pipe a filter()
  filter(year == 2015 & country %in% c("Malaysia", "Russia")) %>% # ojo poner el pipe al final de la fila
  select(country, infant_mortality)

#++ Pakistan or Vietnam
gapminder %>% # lo pasamos por pipe a filter()
  filter(year == 2015 & country %in% c("Pakistan", "Vietnam")) %>% # ojo poner el pipe al final de la fila
  select(country, infant_mortality)

#++ Thailand or South Africa
gapminder %>% # lo pasamos por pipe a filter()
  filter(year == 2015 & country %in% c("Thailand", "South Africa")) %>% # ojo poner el pipe al final de la fila
  select(country, infant_mortality)


# podemos decir que seleccione columnas que "start_with" las letras que sean
gp.reduced<-gapminder %>% 
  filter(year == 2015 ) %>% 
  select(country, starts_with("reg"), infant_mortality)

# ejemplos de sumarize() con dslabs
library(dslabs)

# heiught es un data set que vienen con dslabs
heights %>%
  filter(sex=="Female") %>% summarize((mean(height)))

# se pueden correr varias funciones dentro de sumarize
# IMPORTANTE: el resultado de la funcion que metemos en sumarize debe devolver UN UNICO VALOR.
s <- heights %>%
  summarize(
    median = median(height),
    mad=mad(height),
    min=min(height),
    max=max(height))
s


# tambien podemos agrupar con groupby antes de calcular valores con sumarize
heights %>% 
  group_by(sex) %>%
  summarize(
    average = mean(height),
    standard_deviation = sd(height)
  )

# calculamos la media del ejercicio de antes pero con el nuevo método
babies %>%
  filter(smoke==1) %>% summarize(media_peso=mean(bwt))


### *dot*
# Recordemos en el último ejercicio de la sesión II habiamos descargado la tabla 
# con el rate de asesinatos en todo el mundo. 
# abrimos la tabla de la wikipedia
url="https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate"
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes("table")
tab <- tab[[4]] %>% html_table
head(tab)
class(tab)
tab <- tab %>% 
  select(starts_with("Country"),
         Region,Count,Rate,starts_with("Year")) %>%
  
  # cambiamos el nombre a las columnas
  setNames(c("country", "continent", "total", "murder_rate","year"))

head(tab)

# añadimo suna nueva variable llamada "rate" al dataframe murders para que coincida con la misma medida
# de la tabla de la wikipedia
s <- murders %>% 
  mutate(rate=total/population*100000) %>%
  summarize(mean(rate))
s
# el resultado s es tambien un data frame, aunque es en realidad un numero
str(s)

# para seleccionar sólo el valor al final del pipe ponemos
# .$average (average es el numbre de la columna.Importante poner el punto.)


# NHANES datos de una encuesta relativos a la salud
# tiene muchos missing mvalues
# is.na(seleccion de columna) nos devuelve un vector logico segun si es na o no

#Vamos a explorar los datos en __NHANES__

#1. Tensión sanguínea: Seleccionemos a las mujeres entre 20 y 29 años.
#`AgeDecade` es una variable categórica que contiene las edades. 
#La categoría es " 20-29", con un espacio delante! Cual es su media? 
#(`BPSysAve` variable). Guardalo en una variable llamada`ref`.

#media de la presión sistólica para hombres y mujeres segun decada de edad
NHANES <- read.delim("Datasets/NHANES.txt", sep='\t', stringsAsFactors = F)
head(NHANES)
NHANES %>%
  select(Gender, AgeDecade,BPSysAve) %>% # me quedo con las columnas que necesito
  filter(!is.na(AgeDecade)) %>% # quito las filas con AgeDecade NA
  filter(!is.na(Gender)) %>% # quito los NA que haya en Gender
  group_by(Gender, AgeDecade)%>%
  summarize(mean(BPSysAve, na.rm=T)) #na.rm=T no tienen en cuenta los NA de BPSysAve


# case_when
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE ~ "Zero") # al final pongo true que es lo mismo que decir "el resto de los casos"ç


between(1,-2,2)

# pasar wide data a tidy data with pivot_longer
wide_data %>%
  pivot_longer(-country, # utiliza todas las columnas menos la primera que se llama country
               names_to = "year", # los nombres de las columnas lo metes en la variable nueva "year"
               values_to = "fertility") # los valores los metes en la variable nueva "fertility"

# otro ejemplo de pivot longer usando algunos atributos más
billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    names_prefix = "wk", # quita este prefijo de los nombres
    names_ptypes = list(week = integer()), # defino tipo de objecto de la variable week
    values_to = "rank",
    values_drop_na = TRUE # si una fila son todos NA la elimina
  )


# este es un ejemplo en que creamos 3 columnas nuevas y los nombres se crean con una expresion regular
who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )

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
    values_to = "co2_level"
  )
co2_tidy
