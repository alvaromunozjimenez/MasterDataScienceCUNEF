# 1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr(import data), o mediante los menus de RStudio, leer los datasets sleep.csv  y activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado
library(tidyverse)
library(readr)
library(dplyr)
library(TeachingDemos)

activities <- read_csv("C:/Users/alvar/Desktop/CUNEF/PROGRAMACIÓN EN R/Practica 4/activities.csv")

#En primer lugar creo el objeto activities mediante la lectura del archivo csv "activities".

# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities

View(activities)
sum(is.na(activities$GPS))

#Calculo los NAs correspondientes a la columna GPS dentro del objeto activities.

# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6

act_new<-select(activities, 1, 2, 5, 6)
view(act_new)

#Creo el objeto act_new realizando un select para seleccionar las variables 1, 2 ,5 y 6.

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'

#Utilizar rename

act_new<-rename(act_new, tipo= "Activity type")
act_new<-rename(act_new, ciudad ="Timezone")
view(act_new)

#Utilizo rename para renombrar las variables "Activity type" y "Timezone".

# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad

#Hacer sumary y gráfico de frecuencias
#<-as.factor.

act_new$tipo<-as.factor(act_new$tipo)
view(act_new$tipo)
summary(act_new$tipo)
plot(act_new$tipo, main="activities")

act_new$ciudad<-as.factor(act_new$ciudad)
view(act_new$ciudad)
summary(act_new$ciudad)
plot(act_new$ciudad, main="activities")

#Inicialmente transformo la variable tipo a factor con as.factor.
#Realizo un recuento mediante la función summary del resultado obtenido previamente.
#Mediante plot creo un grafico de barras para visualizar las frecuencias.

#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y s? en Madrid y viceversa. Genera graficos para visualizar los resultados

#Vamos a tener dos nuevos objetos:
#act_new_ams
#act_new_mad
#Determinar de deporte se practica en una ciudad o en la otra.

act_new_ams<-filter(act_new, ciudad=="Europe/Amsterdam")
view(act_new_ams)

act_new_mad<-filter(act_new, ciudad=="Europe/Madrid")
view(act_new_mad)

summary(act_new_ams$tipo)
summary(act_new_mad$tipo)

#Inicialmente filtro por ciudad Amsterdam dentro del objeto act_new.
#Procedo igual con Madrid.
#Mediante summary veo que deportes se practican en cada ciudad.

setdiff(act_new_ams$tipo, act_new_mad$tipo) 

#Deportes que solo se practican en Madrid.

setdiff(act_new_mad$tipo, act_new_ams$tipo) 

#Deportes que solo se practican en Amsterdam

ggplot(act_new_ams, aes(tipo)) + geom_bar()


ggplot(act_new_mad, aes(tipo)) + geom_bar()

#Genero graficos para visualizar los resultados.

#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el a?o 2019

#Hacer un filter. Si no sale quitar el año 2019. El año viene en un formato fecha hora que hay que transformar.

filter(act_new_ams, act_new_ams$tipo== "Cycling" | act_new_ams$tipo== "Pilates")

#Utilizo filter para filtrar las fechas en las que se ha practicado bicicleta o pilates.

#9. Crear una nueva variable dif con los minutos de realizaci?n de cada actividad en Amsterdam
# y realizar una representaci?n gr?fica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas

act_new_ams <- mutate(act_new_ams, dif= a - de)
view(act_new)

deportes_ams<-act_new_ams %>%
  group_by(tipo) %>%
  summarize(tiempo=as.integer(sum(dif)))

view(deportes_ams)

#Creo la variable dif=a-de que representa los minutos de cada actividad en Amsterdam.


ggplot(deportes_ams, aes(x=tipo, y=tiempo))+
  geom_bar(stat = "identity", fill = "green")

#Visualizo graficamente los resultados.

filter(deportes_ams, tiempo>=120)

#Realizo un filter para saber que deportes se han practicado durante
#2 o mas horas dentro del objeto deportes_ams creado anteriormente.


#10. Guardar los nuevo dataset en archivos llamados  "act_new.csv", etc.

write.csv(act_new, file = "act_new.csv", row.names = FALSE)



#-------------------------------
#-----SEGUNDA PARTE-------------
# 11. Cargar el dataset sleep en un objeto llamado sleep

sleep<-read_csv("C:/Users/alvar/Desktop/CUNEF/PROGRAMACIÓN EN R/Practica 4/sleep.csv")


#Localizar variables para que no sean todo ceros. Si la suma es 0 es que no hay nada.

#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informaci?n, que no sean todo cero.

summary(sleep)

sleep_new<-select(sleep, 1, 2 ,3 ,4 ,6 ,7 ,8 ,9)
view(sleep_new)

#Creo el data set denominado sleep_new, el cual contiene las variables con informacion.

#13. Renombrar las variables de sleep_new a nombres cortos:

sleep_new<-rename(sleep_new, ligero= "ligero (s)")
                  
sleep_new<-rename(sleep_new, profundo= "profundo (s)")                            
                  
sleep_new<-rename(sleep_new, despierto= "despierto (s)")
                  
sleep_new<-rename(sleep_new, duration.sleep= "Duration to sleep (s)")

sleep_new<-rename(sleep_new, duration.wakeup= "Duration to wake up (s)")

view(sleep_new)

#Cambio el nombre de las variables a nombres cortos a traves de rename.

#14. Eliminar todas las filas que contengan alg?n NA

sleep_new<-na.omit(sleep_new)
view(sleep_new)

#Elimino las filas que contienen na.

# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo

sleep_new<-mutate(sleep_new, tiempo.total= ligero + profundo)
view(sleep_new)

#Creo la columna tiempo.total que sume el sueño ligero+profundo para calcular
#el tiempo total que se ha dormido cada noche.

# 16. Visualizacion de la relacion ligero-profundo-total

#3 graficos, ligerego profundo, profundo total...

# A la vista de los resultados, que tipo de sue?o es mas relevante?

ggplot(data = sleep_new) + geom_smooth(mapping = aes(x= ligero, y = tiempo.total))

ggplot(data = sleep_new) + geom_smooth(mapping = aes(x=ligero, y = profundo))

ggplot(data = sleep_new) + geom_smooth(mapping = aes(x=profundo, y = tiempo.total))

#Visualizo los tres graficos correspondientes para ver la relación existente
#entre sueño ligero, profundo y total.

#Existe correlacion positiva entre profundo y ligero.

#El tipo de sueño más relevante es profundo.

# 17. Realizar un analisis de diferencias entre los dos tipos de sue?o e interpretar los resultados
# usar la funci?n ICalpha o el 'One sample t-test' de TeachingDemos: t.test()

#Intervalo de confianza para la fiferencia de las medias.Si el 0 no pertenece al intervalo hay diferencias significativas. 

t.test(x=((sleep_new$profundo)-(sleep_new$ligero)), mu=0)
      

#Hay diferrencias significativas entre sueño profundo y ligero.
#La probabilidad de que un dia duermas mas tiempo profundo que ligero es del 95%.

#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.

sleep_new$ciudad<-NA
act_new$ciudad <- as.character(act_new$ciudad)
for (i in 1:nrow(sleep_new)) {
  for (j in 1:nrow(act_new)) {
    if (as.Date(sleep_new$de[i])==as.Date(act_new$de[j])) {
      sleep_new$ciudad[i]<-act_new$ciudad[j]
      
    }
    
  }
  
}

#En el primer bucle: quiero que me de el valor por dia de sleep_new.

#En el segundo bucle compara el valor de dia de sleep_new con todos los dias de act_new.

#Por ultimo, si encuentra una coincidencia entre las anteriores señalo que escriba la ciudad en la columna nueva de sleep new.

#19. Representar la relaci?n totalsleep y profundo usando como facetas el factor ciudad

#Crear la variable e introducir contenido. Tenemos que coger sleep, coger el primer registro(01/11/2018) y buscar en activities, que no esta. En sleep lo dejo como en na. Cuando llego al 5 de nov. que esta em activities en madrid, cojo en sleep y pongo Europe/Madrid. Tengo que hacer un for qe recoge sleep. Por cada uno de elos elementos coger la fecha ver si y esta y si si esta copiarlo en sleep.


#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"

write.csv(sleep_new, file = "sleep_new.csv", row.names=FALSE)

#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.