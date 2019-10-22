# Individual work with  DATA FRAME

#1. Creating a new data frame

edad <- c(22, 34, 29, 25, 30, 33, 31, 27, 25, 25)
tiempo <- c(14.21, 10.36, 11.89, 13.81, 12.03, 10.99, 12.48, 13.37, 12.29, 11.92)
sexo <- c("M","H","H","M","M","H","M","M","H","H")
misDatos <- data.frame(edad,tiempo,sexo)
misDatos
View(misDatos)
str(misDatos)    # Estructura de 'misDatos'
names(misDatos)  # Nombre de las variables contenidas en 'misDatos'

#2. Checking variables and data within the data frame

misDatos[3:6,]
misDatos[,1]
misDatos$edad
misDatos[,"edad"]

#3. Some operations 

mean(misDatos[,1])
mean(misDatos$edad)
mean(misDatos[,"edad"])
mean(misDatos[["edad"]])

#4. Functions ATTACH and DETACH

attach(misDatos)
table(tiempo)
table(sexo)
table(edad)
table(sexo,edad)
mean(edad[sexo=="M"])
mean(edad[sexo=="H"])
detach(misDatos)

#5. New dataframe 'currencies' store some currencies

currencies = data.frame(amount=c(1,2),currency=c("Dolar", "Euro"), exchange=c(1, 0.9))
currencies
Countries<-data.frame(names=c("UK","Spain","Russia"),currency=c("Pound", "Euro", "Rublo"), exchange=c(1.2, 1, 0.02))
attach(currencies) # 
currency          
attach(Countries)  #  'detach' to  'currencies' in common fields
currency          # from Countries
exchange           
amount   # from currencies

detach(Countries)  # 
currency          # from currencies
detach(currencies) # 
#currency          # error

#6. In short

longitud<-c(12,10,11,13,14,17)
longitud

medidas<-data.frame(longitud=c(6,4,7), peso=c(240,326,315), diametro=c(8,9,9))
mean(longitud)
mean(medidas$longitud)
mean(medidas$peso)
mean(medidas$diametro)

attach(medidas)
mean(peso)
mean(diametro)
mean(longitud)
detach(medidas)

# 7. Function WITH

with(medidas,{
  vol=longitud*pi*(diametro/2)^2  # volume
  dens=peso/vol               # density
  dens       # local variable                     # 
})

medidas
medidas$dens=with(medidas,{
  vol=longitud*pi*(diametro/2)^2  # volume
  dens=peso/vol               #  density
  dens                          # 
})
medidas # 

#8. SUBSET

hombres<-subset(misDatos,sexo=="H")
hombres

mujeres<-subset(misDatos,sexo=="M")
mujeres

mayores<-subset(misDatos,sexo=="H" & edad>30)
mayores

jov_habladores<-subset(misDatos,sexo=="H" & edad<30 & tiempo>12)
jov_habladores

extremos<-subset(misDatos,edad<25|edad>30)
extremos

hombres<-subset(misDatos,sexo=="H", select=c(edad, tiempo))
hombres


# 9. MERGE - RBIND

animales1 <- data.frame(animal=c("vaca","perro","rana","lagarto","mosca","jilguero"), 
                       clase=c("mamífero","mamífero","anfibio","reptil","insecto","ave"))
animales1

animales2 <- data.frame(animal=c("atún", "cocodrilo", "gato","rana"), clase=c("pez", "reptil", "mamífero","anfibio"))
animales2

animales3 <- rbind(animales1,animales2)
animales3

animales4<-merge(animales1,animales2)
animales4

animales5<-merge(animales1,animales2,all=TRUE)
animales5

superficieAnimales<-data.frame(animal=c("perro","tortuga","jilguero", 
                                       "cocodrilo","vaca","lagarto","sardina"),
superficie=c("pelo","placas óseas","plumas", "escamas","pelo","escamas","escamas"))
superficieAnimales
View(superficieAnimales)


merge(animales3,superficieAnimales) # Muestra sólo los animales comunes a ambos dataframes
merge(animales3,superficieAnimales, all.x=TRUE) # Muestra todos los animales del primer dataframe.
merge(animales3,superficieAnimales, all.y=TRUE) # Muestra todos los animales del segundo dataframe.
merge(animales3,superficieAnimales, all=TRUE) # Muestra todos los animales de ambos dataframes.

# 10. sorting DATAFRAMES

ordenacion<-order(animales1$animal)
ordenacion  # Posiciones dentro del dataframe 'animales1' de los animales ordenados alfabéticamente
View(ordenacion)
animales1<-animales1[ordenacion,]  # Se reordenan las filas del dataframe animales1
animales1
View(animales1)
animales1=animales1[order(animales1$animal),]  
misDatos=misDatos[order(misDatos$edad,misDatos$tiempo),]
misDatos
