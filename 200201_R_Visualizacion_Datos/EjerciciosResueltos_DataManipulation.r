#################################################
# repaso clase 1: apply
#################################################
setwd("C:/Users/fscabo/Desktop/MasterDataScience_KSchool/Intro2R_KaggleSchool/Class2")
source("play.r")

a<-numeric()
for (i in 1:100){a[i]<-play()}

jugadas<-matrix(nrow=100,ncol=3)
for (i in 1:100){jugadas[i,]<-get_symbols()}
colnames(jugadas)<-c("first","second","third")

for (i in colnames(jugadas)){print(jugadas[1,i])}

s1<-numeric()
for (i in 1:100){
  s1[i]<-score(jugadas[i,])
}

s2<-apply(jugadas,1,score)

#################################################
# repaso clase 1: ejemplo
#################################################
#Calculate the mean weight for the mothers that smoke and those who do not smoke

babies<-read.delim("DataSets/babies.txt",header=T,sep="\t",stringsAsFactors = F)

head(babies)

#Q1
mean(babies$bwt[babies$smoke==1])
mean(babies[babies$smoke==1,"bwt"])
mean(babies[babies$smoke==1,"bwt"])

mean(babies[babies$smoke==0,"bwt"])


#################################################
#       MOTIVATIONAl EXAMPLE: GAPMINDER
#################################################
library(dslabs)
library(dplyr)
library(ggplot2)
library(tidyverse)
data(gapminder)

tidy_data<-gapminder %>% 
  filter(country %in% c("South Korea","Germany","United States"))%>%
  select(country,year,fertility)

head(tidy_data)

tidy_data %>%
  ggplot(aes(year,fertility,color=country))+geom_line()


# pasarlo a wide_data: pivot_wider
wide_data<-tidy_data %>% pivot_wider(names_from=year,values_from = fertility)
head(wide_data)

# pasarlo a tidy otra vez: pivot_longer
tidy_data <- wide_data %>% pivot_longer(-country,names_to="year",values_to = "fertility")
head(tidy_data)


# si los nombres de las columnas son numericos
billboard

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE)
    
##Ahora queremos convertir la variable semana en numerica
billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    names_prefix = "wk",
    names_ptypes = list(week = integer()),
    values_to = "rank",
    values_drop_na = TRUE,
  )

# Variables dentro de los nombres de columna
who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )

who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    names_ptypes = list(
      gender = factor(levels = c("f", "m")),
      age = factor(
        levels = c("014", "1524", "2534", "3544", "4554", "5564", "65"), 
        ordered = TRUE
      )
    ),
    values_to = "count"
  )

# Varias observaciones por fila

anscombe
anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

#################################################
#      ejercicio 1: NHANES
#################################################
data(NHANES)

# 1. Tensión sanguínea: Seleccionemos a las mujeres entre 20 y 29 años.
# `AgeDecade` es una variable categórica que contiene las edades. 
# La categoría es " 20-29", con un espacio delante! Cual es su media? 
# (`BPSysAve` variable). Guardalo en una variable llamada`ref`.

# Hint: Usa `filter` y `summarize` y luego usa `na.rm = TRUE` al calcular 
#su media y desviacion estandard. Tambien podrias quitar los missing usando `filter`.

NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% filter(!is.na(BPSysAve)) %>% summarize(mean(BPSysAve))

#2. Usando pipe asigna ese valor a una variable llamada `ref_avg`. 
#Hint: Use the code similar to above and then `pull`.

ref<-NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% filter(!is.na(BPSysAve)) %>% summarize(mean(BPSysAve))

ref<-NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% summarize(mean(BPSysAve,na.rm=T))


ref_avg<-NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% 
  filter(!is.na(BPSysAve)) %>% 
  summarize(mean(BPSysAve)) %>% 
  pull()

#4. Media y desviacion estandar para las mujeres en cada grupo de edad. 
#Hint: filtra por genero y luego usa `group_by`.
female_BPSysAve<-NHANES %>% 
  filter(Gender=="female" ) %>%
  select(AgeDecade,BPSysAve) %>% group_by(AgeDecade) %>% summarize(mean(BPSysAve,na.rm=T))


#5. Hacer lo mismo para los hombres 
male_BPSysAve<-NHANES %>% 
  filter(Gender=="male" ) %>%
  select(AgeDecade,BPSysAve) %>% group_by(AgeDecade) %>% summarize(mean(BPSysAve,na.rm=T))


#6. Usando `group_by(AgeDecade, Gender)`, repite 4 y 5 con una sola linea de código.

all_BPSysAve<-NHANES %>% 
  select(Gender,AgeDecade,BPSysAve) %>% 
  group_by(AgeDecade,Gender) %>% 
  summarize(mean(BPSysAve,na.rm=T))

#7. Para los hombres entre 40-49 compara su presion sistolica para las distintas 
#razas reportadas en la variable `Race1` y ordenalas de mayor a menor. 

male_40_BPSysAve<-NHANES %>% 
  filter(Gender=="male" & AgeDecade==" 40-49") %>%
  select(Race1,BPSysAve) %>% 
  group_by(Race1) %>% 
  summarize(mean(BPSysAve,na.rm=T)) %>% arrange()


NHANES %>% select(Gender,AgeDecade,BPSysAve)%>%
  filter(!is.na(Gender))%>%
  filter(!is.na(AgeDecade))%>%
  group_by(Gender,AgeDecade)%>%summarize(mean(BPSysAve,na.rm=T))


#################################################
#      ejercicio 2
#################################################
# Para el dataset babies, calcula la media de los dos grupos usando dplyr
# Identifica los valores extraños en la variable peso y cmabialos por NA
# vuelve a calcular las medias

#################################################
#      ejercicio 3
#################################################
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide%>%
  pivot_longer(-year,names_to="month",values_to="co2")
co2_tidy

# plot
co2_tidy %>% ggplot(aes(month, co2, color = year)) + geom_line()

class(co2_tidy$month)

#change type of month to integer
co2_tidy <- co2_wide%>%
  pivot_longer(-year,
               names_to="month",
               values_to="co2",
               names_ptypes = list(month=integer()))
co2_tidy

co2_tidy %>% ggplot(aes(month, co2, color = year)) + geom_line()

#################################################
#      ejercicio 3
#################################################
admissions
dat <- admissions %>% select(-applicants)

#Transformalo usando *pivot_wider()* de manera que haya una fila para cada major.

adm_wide<-admissions %>% pivot_wider(names_from = gender,
                           values_from = c(admitted,applicants))

#cambia las columnas admitted and applicants por el porc. admitidos

adm_wide <- adm_wide %>% 
  mutate(rate_men=admitted_men/applicants_men,
         rate_women=admitted_women/applicants_women) %>% select(major,rate_men,rate_women)

#vuelve a poner la tabla como la original pero con una columna que indique el porcentaje 
#de admitidos

adm_tidy<-adm_wide %>% 
  pivot_longer(-major,
               names_to="gender",
               values_to = "rate")

adm_tidy<-adm_tidy %>% mutate(gender=sub("rate_","",gender))


X11()
adm_wide %>% 
  ggplot(aes(rate_men,rate_women,col=major),size=10)+geom_point()




