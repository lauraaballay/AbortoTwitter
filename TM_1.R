#Aborto Twitter - preliminar
#Base Junio

library(dplyr)
library(tidyr)
library(ggplot2)

options(scipen=999)

tweets_junio <- read.csv("C:/Users/User02/Google Drive/DiploDatos/Mentoria/tweets_junio.csv", 
                         encoding = 'latin1')

#existen id duplicados

tweets_junio$created_at <- as.character(tweets_junio$created_at)

Sys.setlocale("LC_TIME", "English")
tweets_junio$fecha <- as.POSIXct(tweets_junio$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")
tweets_junio$dia <- as.Date(tweets_junio$fecha)

summary(tweets_junio[, c(2:4, 6:8, 11:13)])

#casos con id duplicados

duplicados <- tweets_junio %>%
  group_by(id) %>%
  mutate(cant = n())
duplicados <- duplicados[duplicados$cant > 1 , ]

length(unique(duplicados$id))

#Tomar desicion sobre estos casos (por ahora los dejo)

ggplot(tweets_junio, aes(x=dia)) +
  geom_histogram(binwidth=.5) + 
  scale_x_date(breaks = unique(tweets_junio$dia)) +
  xlab("Fecha de creación del Tweet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

#Tweet por días

td <- tweets_junio %>%
  group_by(dia) %>%
  summarise(cant = n())

summary(td$cant)

ggplot(td, aes("Tweets por día", cant)) +
  geom_boxplot(show.legend = F) +
  theme_classic()

#Favoritos - Me gusta

summary(tweets_junio$favorite_count)

#porcentaje de tw con al menos 1 me gusta:

nrow(tweets_junio[tweets_junio$favorite_count > 0, ])/nrow(tweets_junio)

ggplot(tweets_junio, aes("Cantidad de me gusta", favorite_count)) +
  geom_boxplot(show.legend = F) +
  theme_classic()

#porcentaje de tw con al menos 1 retw:

summary(tweets_junio$retweet_count)
nrow(tweets_junio[tweets_junio$retweet_count > 0, ])/nrow(tweets_junio$retweet_count)

ggplot(tweets_junio, aes("Cantidad de retweet", retweet_count)) +
  geom_boxplot(show.legend = F) +
  theme_classic()

ggplot(tweets_junio, aes(x=retweet_count)) +
  geom_histogram(binwidth = 1000) + 
  xlab("Retweet") +
  theme_classic() 

#Cantidad de Tw por usuario

tu <- tweets_junio %>%
  group_by(user_id) %>%
  summarise(cant = n())

#Usuarios unicos
nrow(tu)
summary(tu$cant)

ggplot(tu, aes("Tweets por usuario", cant)) +
  geom_boxplot(show.legend = F) +
  theme_classic()

ggplot(tu, aes(x=cant)) +
  geom_histogram(stat = "count", bins = 1000) + 
  xlab("usuarios") +
  theme_classic() 