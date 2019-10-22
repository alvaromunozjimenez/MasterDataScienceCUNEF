# EJERCICIO  1

#1. INTRODUCIR DATOS Y CREAR OBJETOS CON METADATOS

edad<-c(18,19,NA,18,24,17,22,15,22,25,NA,16,23,16)
sexo<-c(0,1,0,0,1,0,0,1,1,0,0,1,0,1)
estudios<-c(1,2,0,1,3,2,3,1,2,3,1,2,3,0)

#Creamos los objetos edad, sexo y estudios en los que introducimos los valores correspondientes de cada objeto mediante la creación del vector c.

sexo<-factor(sexo, levels=c(0,1), labels=c("Hombre","Mujer"))
sexo
View(sexo)

#Categorizamos la variable sexo otorgando el valor 0 para hombres y el valor 1 para mujeres. En este caso hemos modificado la variable sexo añadiendo nuevos valores.

estudios<-factor(estudios, levels=c(0,1,2,3), labels=c("Ninguno","Primarios", "Secundarios","Superiores"))
estudios
View(estudios)

#Categorizamos la variable estudios otorgando el valor 0 para ninguno, valor 1 para primarios, valor 2 para secundarios y valor 3 para superiores. La variable estudios como consecuencia cambia de valores. 

#2. ESTUDIAR LOS DATOS: TABLA DE FRECUENCIAS UNIDIMENSIONALES

table(edad)
tedad<-table(edad)
tedad
prop.table(table(edad))
table(edad,useNA="ifany")

#Introducimos la variable tedad con una tabla de clasificación de edades. El argumento ifany incorpora los valores na existentes en la tabla.

#3.ESTUDIAR LOS DATOS:  TABLA DE REFERENCIAS CRUZADAS

table(estudios,sexo)
testudios.sexo<-table(estudios,sexo)
prop.table(table(estudios,sexo))
prop.table(table(estudios,sexo),1)
prop.table(table(estudios,sexo),2)

#Creamos el objeto testudios.sexo con una tabla de clasificación de edad y sexo.Introducimos prop.table para establecer si la proporción de la tabla va a ser sobre las filas(1) y sobre las columnas(2).

#4. ESTUDIAR LOS DATOS: SIMPLIFICACAR USANDO OBJETOS DE ALMACENAMIENTO

prop.table(testudios.sexo)
prop.table(testudios.sexo,1)
prop.table(testudios.sexo,2)

#No es necesario crear el objeto t ya que el objeto ya estaba anteriormente creado en el apartado anterior.

#5. AGRUPAR DATOS PARA VARIABLES CONTINUAS

range(edad,na.rm=TRUE)
nc<-nclass.Sturges(edad)  # N. de intervalos+1 (limites)
nc
lc<-seq(15,25,length=nc)  # Limites de los intervalos-clases
lc

#Mediante la función range eliminamos los valores Na dando un valor mínimo y máximo a los elementos que lo componen. Creamos el objeto nc con el número de intervalos necesarios para dividir los datos en rango.Creamos el objeto lc con una secuencia numérica de 5 elementos.

#6. CONSTRUIR INTERVALOS CON cut():

?cut

intervalosEdad<-cut(edad,breaks=lc,include.lowest=TRUE)
intervalosEdad # Se muestran los intervalos de edad, cada uno correspondiente a cada edad observada.
edad
table(intervalosEdad)

#Creamos el objeto intervalosEdad dividiendo en intervalos cada edad observada.

#7. ESTADISTICOS DESCRIPTIVOS

mean(edad)
sd(edad)
summary(edad)
summary(estudios)
summary(sexo)
misDatos<-data.frame(edad,estudios,sexo)
summary(misDatos)

#Inicialmente mediante la funcion summary obtenemos las propiedades de las listas. Después, establecemos la estructura de datos bidimensional con los objetos edad, sexo y estudios.

#8. DATOS AGREGADOS POR GRUPOS

aggregate(edad,by=list(sexo),mean)

aggregate(edad,by=list(sexo),mean,na.rm=TRUE)

#A través de la función aggregate dividimos los datos en conjuntos y nos devuelve los resultados. En este caso solicitamos la media de las edades según el sexo excluyendo los datos Na.

#9. REPRESENTACIONES GRAFICAS

#help(pie)
pie(table(estudios))
#help(barplot)
barplot(table(sexo))
#help(hist)
hist(edad)
#help(boxplot)
boxplot(edad~sexo)
#Representación de los datos obtenidos mediante distintas gráficas.
