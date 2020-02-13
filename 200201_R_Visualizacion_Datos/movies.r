library(ggplot2movies)
data(movies)

# The internet movie database, \url{http://imdb.com/}, is a website devoted
# to collecting movie data supplied by studios and fans.  It claims to be the
# biggest movie database on the web and is run by amazon.  More about
# information imdb.com can be found online,
# \url{http://imdb.com/help/show_leaf?about}, including information about
# the data collection process,
# \url{http://imdb.com/help/show_leaf?infosource}.
# }
# \details{
#   Movies were selected for inclusion if they had a known length and had been
#   rated by at least one imdb user.
# }

######################################
# 1. Cuantas peliculas tiene este archivo?
# Que informacion hay de cada una de ellas?
# Que tipo de objeto es?
#####################################
dim(movies)
str(movies)
? movies

######################################
# 2. Quita todas las columnas que empiezan con r
#####################################
dummy<-movies %>% 
  select(-starts_with("r"),-mpaa)%>% 
  add_column(Romance<-movies %>% select(Romance))

dummy<-movies %>% 
  select(-starts_with("r"),-mpaa)%>% 
  mutate(Romance=movies$Romance,Ratings=movies$rating)

##########################################################################
# 3. Por año, extrae la pelicula con el peor rating. Excluye aquellas 
# con un metraje corto (<30min) 
#########################################################################
movies %>% 
  group_by(year) %>%
  slice(which.min(rating)) %>% 
  select(title,year,length,budget,rating)%>%
  arrange(desc(year))
#  filter(!is.na(budget)) 
  
movies %>% 
  group_by(year) %>%
  slice(which.max(rating)) %>% 
  select(title,year,length,budget,rating)%>%
  arrange(desc(year))
##########################################################################
# 3. Por año, extrae la pelicula con el peor rating. Excluye aquellas 
# con un metraje corto (<30min) o sin presupuesto conocido
#########################################################################
movies %>% 
  group_by(year) %>%
  slice(which.min(rating)) %>% 
  select(title,year,length,budget,rating)%>%
  filter(!is.na(budget)) 

##########################################################################
# 4. Por año, extrae la pelicula con el mejor rating. Excluye aquellas 
# con un metraje corto (<30min) o sin presupuesto conocido
#########################################################################
movies %>% 
  group_by(year) %>%
  slice(which.max(rating)) %>% 
  select(title,year,length,budget,rating)%>%
  filter(!is.na(budget)) 

##########################################################################
# 5. En general, hay ua correlacion entre budget y rating? 
# TOma solo los datos a partir de los años 70
#########################################################################
movies %>% 
  select(title,year,length,budget,rating)%>%
  filter(!is.na(budget) & year>2000) %>%
  ggplot(aes(budget,rating,col=year))+geom_point()
