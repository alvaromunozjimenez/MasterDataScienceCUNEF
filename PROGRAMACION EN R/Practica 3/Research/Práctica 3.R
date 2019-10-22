#Ejercicio 1

fib <- function(n){
  if (n==0) {
    return(c(0))
  }
  if (n==1) {
    return(c(1))
  }
  if (n>=2) {
    lista <- c(0:n)
    for (i in 3:(n+1)) {
      lista[i] <- lista[i-2]+lista[i-1]
    }
    return(lista)
  }
}

fib(10)

#En primer lugar creo el objeto fib en referencia a la funcion de fibonacci.

#Impongo la condici?n de que si n=0 me devuelva el primer n? de fibonacci, que es 0.

#Lo mismo sucede con n=1, me devuelve 1.

#Pra n>=2 creo el objeto lista que va de 0 a n y ejecuto un bucle for,
#donde el elemento i a partir n>=2,es decir 3,  va a ser el resultado
#de la operaci?n lista[i-2] + lista[i-1].


nternas<-function(n) {
  if (n < 1) {
    return("Selecciona valores mayor o igual a 1")
  }
  else
    cuaternaf<-c(fib(n+4))
  
  #Creo un objeto con números necesarios de fibonacci.
  
  m<-matrix(nrow = n, ncol = 3)
  for (i in 1:n) {
    v1<-cuaternaf[(i+1)]
    v2<-cuaternaf[(i+2)]
    v3<-cuaternaf[(i+3)]
    v4<-cuaternaf[(i+4)]
    a<-v1*v4
    b<-2*v2*v3
    h<-(v2*v2)+(v3*v3)
    m[i, ]<-c(a,b,h)
  }
  return(m)
}

nternas(10)

#En segundo lugar creo el objeto nternas que se trata de una funci?n donde impongo 
#la condici?n de que si n<1 me devuelva "valores>=1" y para todo lo demas, creo
#el objeto cuaternaf, procedente de una 4-secuencia de la serie de fibonacci.

#Por ?ltimo creo la matriz m donde me incluya los objetos v1:v4 como cuaternasf,
#obteniendo como resultado de operar dichos objetos los catetos y la hipotenusa   
#de los triangulos procedentes de la serie de fibonacci.

