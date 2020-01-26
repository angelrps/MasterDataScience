data(mtcars)
? mtcars
View(mtcars)

#str() nos da mucha información sobre el data set
str(mtcars)

#cuantos modelos tenían 4 cilindros
sum(mtcars$cyl==4)
length(which(mtcars$cyl==4))

#y qué modelos son?
mtcars[mtcars$cyl==4,]


#cuáles tienen 4 cilindros y además transmisión automática
mtcars[mtcars$cyl==4 & mtcars$am==0,]

#cuál sería el consumo medio de los coches de más de cuatro cilindros y que tienen transmisión automáticas
#querremos hacer la media de miles per galo (mpg)
mean(mtcars$mpg[mtcars$cyl>4 & mtcars$am==0])
