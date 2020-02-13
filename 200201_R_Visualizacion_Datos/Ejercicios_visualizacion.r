####################################################################
# BABIES
####################################################################
#1.
setwd("C:/Users/fscabo/Desktop/MasterDataScience_KSchool/Ejercicios")
babies=read.delim("babies.txt",header=T,sep="\t",stringsAsFactors = F)
plot(babies$bwt,babies$gestation)

#necesitamos poner a NA los missing (999)
babies$gestation[which(babies$gestation=="999")]=NA
plot(babies$bwt,babies$gestation)

plot(log(babies$bwt),log(babies$gestation))

#2.
boxplot(babies$bwt~babies$smoke)

#3.
hist(babies$bwt)
hist(murders$total)

####################################################################
# alturas de la clase: utilizando ggplot2 pinta la relacion entre
# la altura y la edad
####################################################################


####################################################################
# gif
####################################################################
library(ggplot2)
library(gganimate)
data(gapminder)

dummy <- c("Europe", "Asia")
gapminder %>% 
  filter(year=="2010" & continent %in% dummy) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() 

continents <- c("Europe", "Asia")
gapminder %>% 
  filter(continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

continents <- c("Europe", "Asia")
g<-gapminder %>% 
  #filter(year %in% years & continent %in% continents) %>%
  filter(continent %in% continents) %>%
  ggplot(aes(fertility,life_expectancy, col = continent)) +
  geom_point(size=5)+
  transition_states(year, transition_length = 1, state_length = 1)+
  ggtitle("Year {closest_state}")


#+facet_wrap(~year) 
animate(g)


####################################################################
# gif2
####################################################################
library(gapminder)
library(ggplot2)
library(gganimate)
p <- ggplot(
  gapminder,
  aes(x = gdp, y=life_expectancy, size = population, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p + transition_time(year)

