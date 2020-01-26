#read.delim lee cualquier tipo de archivo de texto plano
#csv, tsv, txt, ...
dat2<-read.delim("DataSets/murders.csv",sep=",", header=T)
head(dat2)
str(dat2)
View(dat2)

#read.csv es una particularización de read.delim que viene son sep=',' y header=T.
#si el separador fuera otro habría que especificar el separador.
dat<-read.csv("DataSets/murders.csv")

#read.csv ha transformado alguna variables que eran string a factores
#para que eso no suceda hacemos stringsAsFactors = FALSE
dat<-read.csv("DataSets/murders.csv", stringsAsFactors = F)

dat$region2<-dat$region

#mutate es una función del paquete dplyer.
#añade una columna con el valor que le digamos 'regionf', y en este caso lo pasamos como factor
dat<-mutate(dat,regionf=as.factor(region))

#otra forma es:
dat %>% mutate(pop2=population/10^5)
#%>% este es el operador 'pipe'. La nueva columna es pop2. Population es la columna existente

#otpaquete xlsx es para leer archivos de excel
dat<-read.xlsx("DataSets/murders.xlsx", sheetIndex = 1)


#paquete data.table para crear data frames
n = 1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )


head(DT)


write.table(DT,
            "DataSets/test.csv",
            sep=",",
            row.names=FALSE,
            quote=FALSE)  #si no pones quote false te pone comillas en cada elemento


dt1<-read.csv("DataSets/test.csv") #esta función tarda bastante
dt1<-fread("DataSets/test.csv") #fread es mucho más rápido!! Lee archivos de texto plano (no excel)

#PAQUETE TIDYVERSE
murders<-read_csv("DataSets/murders.csv")
#vemos que murders es un objeto tibble. Este objeto no muestra todas las lineas en pantalla y me da más información.
#para trabajar con el objeto es igual que el data frame
murders

#cómo inicializar un objeto tibble
tibble(x = letters)
tibble(x = 1:3, y = list(1:5, 1:10, 1:20))

tibble(x = letters)
tibble(x = 1:3, y = list(1:5, 1:10, 1:20))
