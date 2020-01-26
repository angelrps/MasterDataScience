num <- -1
if (num < 0) {
  print("num is negative.")
  print("Don't worry, I'll fix it.")
  num <- num * -1
  print("Now num is positive.")
  print(num)
}else {
  print("soy positivo")
}
#ojo, el else debe estar en la misma línea del corchete de cierre del if para que R continue ejecutando

num <- 0
if (num < 0) {
  print("num is negative.")
  print("Don't worry, I'll fix it.")
  num <- num * -1
  print("Now num is positive.")
  print(num)
}else if(num>0){
  print("soy positivo")
}else{
  print("soy 0")
}

#EJERCICIO MÁQUINA TRAGAPERRAS

#PREMIOS
# DD DD DD                100
# 7 7 7                   80
# BBB BBB BBB             40
# BB BB BB                25
# B B B                   10
# C C C                   10
# Any combination of bar  5
# C C *                   2
# C * C                   2
# * C C                   2
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
#la función get_symbols nos devolverá una tirada de la máquina (simulación de una jugada)
#la función score nos dirá el premio ganado

#declaramos vector de simbolos y de scores
wheel<-c('DD','7','BBB','BB','B','C','0')
probability<-c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)

get_symbols<-function(wheel,p){
  sample(wheel, size = 3, replace = TRUE,
         prob = p)
}

get_symbols(wheel, probability)

score<-function(x){
  same<-x[1]==x[2] & x[1]==x[3]
  #excluimos si los 3 son cero no puntúa
  if(same & x[1]!=0){
    if(x[1]=='DD'){
      puntos<-100
    }else if(x[1]=='7'){
      puntos<-80
    }else if(x[1]=='BBB'){
      puntos<-40
    }else if(x[1]=='BB'){
      puntos<-25
    }else{
      puntos<-10
    }
    print(paste('premio', puntos, sep=':'))
  }else{
    print('no premio')
  }
}

#otra manera de definir score
score<-function(x){
  #son los simbolos iguales
  same<-x[1]==x[2] & x[1]==x[3]
  #cualquier combinación de Bs da premio.
  # el operador %in% dice de todos los elementos de x están o no el el siguiente vector
  #si la suma da 3 hay premio
  #cuántas bars tengo?
  bars<-sum(x %in% c('B','BB', 'BBB'))
  #combinaciones de C. Si numa 2 o 3 tendrán premio
  #cuantas cherries tengo?
  nc<-sum(x == 'C')
  
  #x[1]!=0 excluimos si los 3 son cero no puntúa
  if(same & x[1]!=0){
    #creamos un vector con nombres, es decir como un diccionario
    payout=c('DD'=100, '7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10)
    #unname me quita la key y deja el value
    prize<-unname(payout[x[1]]) #uso x[1] para escoger el premio adecuado
  }else if(bars==3 & !same){
    puntos<-5
  }else if(nc==2){
    puntos<-5
  }else if(nc==1){
    puntos<-2
  }else{puntos<-0}
  
  if(puntos>0){
    print(paste('Premio', puntos, sep=':'))
  }else{
    print('No premio')
  }
  return(puntos)
}


sc<-score(get_symbols(wheel, probability))

play<-function(){
  symbols<-get_symbols(wheel, probability)
  print(symbols)
  puntos<-score(symbols)
  return(puntos)
}

a<-numeric()
for (i in 1:100){
  a[i]<-play()
}

puntos
