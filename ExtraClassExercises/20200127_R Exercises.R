# Genera un vector que reproduzca las cartas repartidas a un jugador de mus en una mano jugando a la baraja española.
# Recuerda que hay cuatro palos: bastos, oros, copas y espadas. ¿Qué tipo de vector es? ¿Con que funcion lo conﬁrmamos?

mano <- c('as_oros', "as_copas", "rey_expadas", "rey_bastos")
class(mano)


moneda <- list("cruz"=1, "cara"=0)
attributes(moneda)
names(moneda)

p<-data.frame(figura=c("sota","caballo","rey"),puntos=c(10,11,12))
p
 

# Ejercicio 4: Construye una baraja española de cartas La baraja española tiene cuatro palos y 10 cartas por palo.
# Cada una de ellas tiene una puntuación: As (11 puntos), Tres (10 puntos), Rey (4 puntos), Caballo (3 puntos) y Sota (2 puntos).
# El resto de cartas no tienen valor puntuable.

# 4.1. Construye una lista que contenga todas las posibles cartas que hay en la baraja y sus puntuaciones
# 4.2. Construye un data frame donde cada ﬁla represente una combinacion de carta/palo/puntuacion 
# 4.3. Cuantos puntos suma en total la baraja española?

carta <- c('as', 2:7, 'sota', 'caballo', 'rey')
palos <- c('oros', 'copas', 'espadas', 'bastos')
puntos <- c(11, 0, 10, rep(0,4), 2, 3, 4)
df_lista <- list(rep(carta,4), rep(palos, each=10), rep(puntos,4))

df_baraja <- data.frame(carta=df_lista[[1]], palo=df_lista[[2]], puntos=df_lista[[3]])

sum(df_baraja$puntos)
df_baraja
df_baraja[,1]
df_baraja$carta


# Ejercicio 5: Baraja y reparte 
# 5.1. Construye una función deal<-function(){return()} que seleccione la primera carta de tu baraja 
deal <- function(b){
  return (b[1,])
}
deal(df_baraja)

# 5.2. Baraja de manera que queden ordenadas de otra forma. 
# Genera una función llamada shuﬄe que pueda ser utilizada para esto más veces.
suffle <- function(b){
  return(b[sample(1:40, 40),])
}
t <- suffle(df_baraja)
t

# 5.3. Selecciona la primera carta de esta baraja revuelta
deal(suffle(df_baraja))


# Ejercicio 6: ¿Cuántos ases hay en mi baraja? 
# 6.1. Selecciona la columna carta del objeto baraja que contiene los nombres de las cartas
# 6.2. Usa el operador lógico correspondiente para identiﬁcar todos los que tengan como nombre “as”. 
# (Hint: recuerda que los caracteres se escriben entre comillas) 
# 6.3. Crea un nuevo objeto baraja2 en el que pongas a 0 la puntuacion de los ases 

df_baraja2 <- df_baraja
df_baraja2[df_baraja2$carta == 'as','puntos'] <- 0

df_baraja2


# Ejercicio Máquina Tragaperras
# Para simular estos datos necesitamos hacer dos cosas:

# 1. Generar combinaciones de tres elementos de entre los siguientes símbolos: 
# diamonds (DD), sevens (7), triple bars (BBB), double bars (BB), single bars (B), cherries (C), and zeroes (0). 
# Cada símbolo aparece según su probabilidad en la rueda.

#2. Asignar un premio a cada combinación Las máquinas tragaperras de la marca Manitoba tienen el siguiente esquema de premios:
#PREMIOS
# DD DD DD                100
# 7 7 7                   80
# BBB BBB BBB             40
# BB BB BB                25
# B B B                   10
# C C C                   10
# Any combination of bar  5
# C C *                   5
# C * C                   5
# * C C                   5
# C * *                   2
# * C *                   2
# * * C                   2

#PROBABILIDADES
# DD  0.03
# 7   0.03
# BBB 0.06
# BB  0.1
# B   0.25
# C   0.01
# 0   0.52



# declaramos vector de simbolos y de scores
wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
probability <- c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)

# la función get_symbols nos devolverá una tirada de la máquina (simulación de una jugada)
get_symbols <- function(){
  return (sample(wheel,3, replace=TRUE, prob = probability))
}

# la función get_score nos dirá el premio ganado
get_score <- function(s){
  same <- function(s){
    return(s[1]==s[2] & s[2]==s[3])
  }
  bars <- sum(s %in% c('BBB','BB','B'))
  Cs <- sum(s %in% c('C', 'C', 'C'))
  
  # if symbols are the same, except 0, 0, 0
  if(same(s) & s[1]!=0){
    if(s[1]=='DD'){
      prize = 100
    }else if(s[1]=='7'){
      prize = 80
    }else if(s[1]=='BBB'){
      prize = 40
    }else if(s[1]=='BB'){
      prize = 25
    }else{
      prize = 10
    }
  }else if(bars==3 | Cs==2){ # if all symbols are bars or 2 Cs
    prize = 5
  }else if(Cs==1){ # si hay una C
    prize = 2
  }else{
    prize = 0
  }
}

play <- function(){
  #get the symbols
  s <- get_symbols()
  #show the symbols
  print(s)
  #get the score
  score <- get_score(s)
  #show the score
  paste('Prize: ', score)
}

combos <- expand.grid(wheel,wheel,wheel, stringsAsFactors = FALSE)
View(combos)

dir <- system.file(package = 'dslabs')
dir
install.packages('dslabs')
library('dslabs')


# trabajando con el fichero babys.
# calcular la media del peso al nacer para los bebes de madres
# fumadoras y no fumadoras
babies <- read.delim("Datasets/babies.txt", sep = '\t', header=T, stringsAsFactors = F)
head(babies)
mean(babies$bwt[babies$smoke==1])
mean(babies$bwt[babies$smoke==0])



# De las siguientes parejas, cual dirias que tuvo la mayor mortalidad infantil en 2015?
# nos aseguramos de tener cargado el paquete dplyr
library(dplyr)
data(gapminder)
gm <- read.csv("Datasets/Gapminder.csv")
gm2015 <- gm%>%
  filter(year==2015)
gm2015

#++ Sri Lanka or Turkey
gm2015 %>%
  filter(country=="Sri Lanka" | country=="Turkey")
#++ Poland or South Korea
gm2015 %>%
  filter(country %in% c("Poland", "South Korea"))
#++ Malaysia or Russia
gm2015 %>%
  filter(country %in% c("Malaysia", "Russia"))
#++ Pakistan or Vietnam
gm2015 %>%
  filter(country %in% c("Pakistan", "Vietnam"))
#++ Thailand or South Africa
gm2015 %>%
  filter(country %in% c("Thailand", "South Africa"))

# EJERCICIO CON SUMMARIZE()
library(dslabs)
heights
heights %>%
  summarize(media=mean(height),
            max=max(height),
            min=min(height),
            mediana=median(height),
            standard_deviation=sd(height))

# agrupando antes de aplicar summarize()
heights %>%
  group_by(sex) %>%
  summarize(mean(height))

# CALCULA LA MEDIA DEL PESO AL NACER PARA MADRES FUMADORAS Y NO FUMADORAS
# USANDO SUMMARIZE()
babies %>%
  group_by(smoke)%>%
  summarize(media_peso = mean(bwt))


# SETNAMES()
b_setname <- babies
b_setname %>% setNames(c("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete"))


h <- heights %>%
  filter(sex=="Female") %>%
  summarize(media=mean(height)) %>%
  .$'media'

h$'media'

# Ejercicio NCHS DATA.Paquete de datos: NHANES. 
# 1. Tensión sanguínea: Seleccionemos a las mujeres entre 20 y 29 años.
# AgeDecade es una variable categórica que contiene las edades. 
# La categoría es " 20-29", con un espacio delante! Cual es su media? 
# (BPSysAve variable). Guardalo en una variable llamada ref. 
# Hint: Usa filter y summarize y luego usa na.rm = TRUE al calcular su meda y desviacion estandard. 
# Tambien podrias quitar los missing usando filter.

nhanes <- read.delim("Datasets/NHANES.txt", stringsAsFactors = F)
nhanes %>%
  select(Gender, AgeDecade, BPSysAve) %>%
  filter(!is.na(nhanes$BPSysAve)) %>%
  filter(Gender == "female" & AgeDecade == " 20-29") %>%
  summarize(media= mean(BPSysAve))

 
# 2. Usando pipe asigna ese valor a una variable llamada ref_avg. 
# Hint: Use the code similar to above and then pull. 
# 3. min, max, media y desviación estándar para cada grupo de edad dentro de las mujeres
nhanes %>%
  select(Gender, AgeDecade, BPSysAve) %>%
  filter(!is.na(nhanes$BPSysAve)) %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(minimo = min(BPSysAve), 
            maximo = max(BPSysAve),
            media = mean(BPSysAve),
            desv_std = sd(BPSysAve))

# 5. Hacer lo mismo para los hombres
nhanes %>%
  select(Gender, AgeDecade, BPSysAve) %>%
  filter(!is.na(nhanes$BPSysAve)) %>%
  filter(Gender == "male") %>%
  group_by(AgeDecade) %>%
  summarize(minimo = min(BPSysAve), 
            maximo = max(BPSysAve),
            media = mean(BPSysAve),
            desv_std = sd(BPSysAve))
# 6. Usando group_by(AgeDecade, Gender), repite 4 y 5 con una sola linea de código.
nhanes %>%
  select(Gender, AgeDecade, BPSysAve) %>%
  filter(!is.na(nhanes$BPSysAve)) %>%
  group_by(AgeDecade, Gender) %>%
  summarize(minimo = min(BPSysAve), 
            maximo = max(BPSysAve),
            media = mean(BPSysAve),
            desv_std = sd(BPSysAve))
# 7. Para los hombres entre 40-49 compara su presion sistolica para las distintas 
# razas reportadas en la variable Race1 y ordenalas de mayor a menor.
nhanes %>%
  select(Gender, AgeDecade, Race1, BPSysAve) %>%
  filter(!is.na(nhanes$BPSysAve)) %>%
  filter(Gender == "male" & AgeDecade == " 40-49") %>%
  group_by(Race1) %>%
  summarize(media = mean(BPSysAve)) %>%
  arrange(desc(media))


# EJERCICIOS TIDY / WIDE
# 1. Niveles de CO2
# co2 es un vector CON medidas de 12 meses de niveles de co2 a alo largo de varios años
library(dplyr)
library(tidyverse)
View(co2)

# definimos los datos en formato wide,
#esto lo hace la profe que conoce cómo están definidos los datos en co2
co2_wide <- data.frame(matrix(co2, ncol=12, byrow=TRUE)) %>%
  setNames(1:12) %>%
  mutate(year=as.character(1959:1997))
co2_wide

# ahora pasamos la tabla a formato tidy
co2_tidy <- co2_wide %>%
  pivot_longer(-year,
               names_to = "month",
               values_to = "co2_level")
co2_tidy                        