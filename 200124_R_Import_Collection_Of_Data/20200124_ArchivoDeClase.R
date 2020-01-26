2*2
871*1.21

precio<-871*1.21

n<-8
(((n+2)*3)-6)/3
c(1,2,3,4,5,6)
dado <- c(1,2,3,4,5,6)
dado <- 1:6
dado+2
dado/dado
#esto es un comentario
moneda<-c(1,2)
moneda+dado

Dado<-c(1,2,3)
Dado<-c(1L,2L,3L)
typeof(Dado)

text<-c("Hello", "Bye")
typeof(text)
v2<- 2<1
dado<3
mano<-c("rey_bastos", "rey_de_oros", "as_de_oros", "as_de_copas")
dado<3
sum(dado<3)
#la funcion matrix() crea una matriz a través de unvector de dimension 1.
#byrow para que rellene por fila. T es TRUE
matrix(dado,nrow = 2, byrow = T)
#array me crea un vector de la dimension que queramos
array(c(11:14,21:24,31:34), dim = c(2,3,3))
mano2
dado[1]
text[1]
mat<-matrix(dado,nrow=2, byrow=T)
mat[1,2]
mat[1,]
mat[1,c(1,2,3)]
dado<3
dado[dado<3]
p<-data.frame(triunfo=c("as", "sota","caballo", "rey"), puntos=c(10,2,3,4))
gender<-factor("M","F","F","M")
gender1<-c("M","F","F","M")
list1=list(100:130, "R", list(TRUE,FALSE))
list1[[1]]
list1[[2]]
list1[[3]]
baraja_list<-list(carta=c(1:7,10,11,12),
                  palo=c("oros","copas","espadas","bastos"),
                  puntos=c(11,0,10,rep(0,4),2,3,4))
baraja_list
? rep
p
baraja_dataframe<-data.frame(carta=rep(baraja_list$carta,4),
                             palo=c(rep(baraja_list$palo,each=10)),
                                    puntos=rep(baraja_list$puntos,4))
View(baraja_dataframe)
sum(baraja_dataframe$puntos)
#muestra fila uno
baraja_dataframe[1,]
#fila uno, columna 2
baraja_dataframe[1,2]
#muestra primera fila y columnas 2 y 3
baraja_dataframe[1,c(2,3)]
#selecciona sólo los ases
baraja_dataframe[baraja_dataframe$carta==1,]
lr<-baraja_dataframe$carta==1
which(lr)
throw<-function(){
  return(sample(dado,1))
}
throw()

throw2<-function(x){
  return(sample(x,1))
}
throw2(dado)

throw3<-function(){
  dado<-7:12
  return(sample(dado,1))
}

deal<-function(x){return(x[1,])}
deal(baraja_dataframe)

dealAndShuffle<-function(x){return(x[sample(1:40,1),])}
dealAndShuffle(baraja_dataframe)

shuffle<-function(x){return(x[sample(1:40,40),])}
shuffle(baraja_dataframe)

baraja_new<-shuffle(baraja_dataframe)
deal(baraja_new)

#vamos a definir una nueva baraja donde los ases valen 0
baraja_df2<-baraja_dataframe
baraja_df2$puntos[baraja_df2$carta==1]<-0

baraja_dataframe$carta==1 & baraja_dataframe$palo=="oros"
baraja_dataframe[which(baraja_dataframe$carta==1 & baraja_dataframe$palo=="oros"),]

? mtcars
