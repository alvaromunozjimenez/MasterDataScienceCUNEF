babymales<-read.csv("ontario_top_baby_names_male_1917-2016_english.csv", skip=1)
getwd()              
head(babymales)
tail(babymales)
str(babymales)
nombresmasrecientes<-babymales[babymales$Year==max(babymales$Year),]
nombresmasrecientes
nrow(nombresmasrecientes)
sum(nombresmasrecientes$Frequency)
#nº de niños que se han registrado
nombresmasrecientes<-subset(babymales, babymales$Year==max(babymales$Year),)
nombresmasrecientes<-nombresmasrecientes[,c("Name", "Frequency")]
#Estamos haciendo un filtrado por columnas
nombresmasrecientes<-nombresmasrecientes[order(nombresmasrecientes$Frequency, decreasing = TRUE),]
write.csv(nombresmasrecientes, file = "2016_male_popular.csv",row.names = FALSE)
getwd()
#name.in<-readline(prompt =" Qué nombre quieres buscar? : ")
name.in<-"PETER"
freq.year<-babymales[babymales$Name == toupper(name.in),c("Year", "Frequency")]
freq.year<-freq.year[order(freq.year$Year),]
freq.year
plot.title<-paste("Babies named", toupper(name.in))
g<-plot(freq.year, main=plot.title, type="s")
