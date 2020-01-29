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
