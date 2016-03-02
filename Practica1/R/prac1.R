
###################################################
##################EJERCICIO 4.2.1##################
###################################################
simula_unif <- function(N=3, dim=5, rang=0:9) {
  if(N<0){                                                  #Si paso tamaño de la lista negativo, no creo lista con valores buenos. 
    print("La lista no puede tener un tamaño negativo.")
    return(NA)
  }else if(dim<0){                                          #Si paso dimension del vector negativa, no creo lista con valores buenos.
    print("La dimension del vector no puede ser negativa.")
    return(NA)
  }
  ini_vector = runif(dim, rang[1], rang[length(rang)])                          #vector de tamaño dim con valores aleatorios uniformes en el intervalo 0-rango
  mi_lista = list(ini_vector)                               #creo mi lista con el vector
  if(N>1){                                                  #Si nos piden más de un vector en la lista....
     for( i in 2:N){                                        #Para cada nuevo vector de la lista...
       ini_vector = runif(dim, rang[1],rang[length(rang)])                    #vector de tamaño dim con valores aleatorios uniformes en el intervalo 0-rango
       segunda = list(ini_vector)                           #creo una lista auxiliar con el nuevo vector
       mi_lista = c(mi_lista, segunda)                      #concateno a mi lista anterior la nueva lista que contiene el nuevo vector.
     }
  }
  return(mi_lista)
}

###################################################
##################EJERCICIO 4.2.2##################
###################################################
simula_gaus <- function(N=3, dim=5, sigma=1:9){
  if(N<0){                                                  #Si paso tamaño de la lista negativo, no creo lista con valores buenos. 
    print("La lista no puede tener un tamaño negativo.")
    return(NA)
  }else if(dim<0){                                          #Si paso dimension del vector negativa, no creo lista con valores buenos.
    print("La dimension del vector no puede ser negativa.")
    return(NA)
  }else if(sigma[1]<0 || sigma[length(sigma)]<0){
    print("Sigma no puede ser negativo.")
    return(NA)
  }
  ini_vector = rnorm(dim, mean=0, sd=sample(sigma,1))                 #vector de tamaño dim con valores aleatorios gaussianos con media 0 y varianza sigma
  mi_lista = list(ini_vector)                               #creo mi lista con el vector
  if(N>1){                                                  #Si nos piden más de un vector en la lista....
    for( i in 2:N){                                         #Para cada nuevo vector de la lista...
      ini_vector = rnorm(dim, mean=0, sd=sample(sigma,1))             #vector de tamaño dim con valores aleatorios gaussianos con media 0 y varianza sigma
      segunda = list(ini_vector)                            #creo una lista auxiliar con el nuevo vector
      mi_lista = c(mi_lista, segunda)                       #concateno a mi lista anterior la nueva lista que contiene el nuevo vector.
    }
  }
  return(mi_lista)
}

###################################################
##################EJERCICIO 4.2.3##################
###################################################
#Genero la lista de vectores con la funcion y los valores que nos indican
N3=50
dim3=2
intervalo3 = -50:50
lista3 = simula_unif(N3,dim3,intervalo3)

#Creo un vector con los valores y (valores aleatorios generados impares).
#Creo un vector con los valores x (valores aleatorios generados pares).
lista33y = NULL #en lista33y tendre todos los valores y.
lista33x = NULL #en lista33y tendre todos los valores x.
contador=2
for(j in 1:N3){
  for(i in 1:dim3){
    if((contador%%2) == 0){
     lista33y = c(lista33y, lista3[[j]][i])
    }
    else{
     lista33x = c(lista33x, lista3[[j]][i])
    }
     contador = contador+1
  }
} 
plot(lista33x, lista33y, 
     main = "Valores función uniforme",col="purple")

###################################################
##################EJERCICIO 4.2.4##################
###################################################
#Genero la lista de vectores con la funcion y los valores que nos indican
N4=50
dim4=2
intervalo4 = 5:7
lista4 = simula_gaus(N4,dim4,intervalo4)

#Creo un vector con los valores y (valores aleatorios generados).
#Creo un vector con los valores x (indice de lista del valor).
lista42y = NULL #en lista42y tendre todos los valores y.
lista42x = NULL #en lista42y tendre todos los valores x.
contador4=2
for(j in 1:N4){
  for(i in 1:dim4){
    if((contador4%%2) == 0){
      lista42y = c(lista42y, lista4[[j]][i])
    }else{
      lista42x = c(lista42x, lista4[[j]][i])
    }
    contador4 = contador4 +1
  }
} 
plot(lista42x,lista42y, 
     main = "Valores función gaussiana", col="orange")


###################################################
##################EJERCICIO 4.2.5##################
###################################################
simula_recta <- function(intervalo=-50:50, a=NA){
  #Tomo dos puntos aleatorios del cuadrado con un aleatorio
  primerx = sample(intervalo,1)
  primery = sample(intervalo,1)
  segundox = sample(intervalo, 1)
  while(segundox == primerx){ #Evito la división por 0
    segundox=sample(intervalo,1)
  }
  segundoy = sample(intervalo, 1)
  
  #Añado los dos puntos del cuadrado para dibujarlos
  x=c(primerx, segundox) 
  y=c(primery, segundoy)
  
  #Calculo a, b que definen la recta que pasa por los dos puntos
  a= ((segundoy - primery)/(segundox - primerx))
  b= (primery - (a*primerx))
  
  #Represento el cuadrado de trabajo, los puntos y la recta que los une
  plot(x,y, col= "red", 
       xlim = c(intervalo[1], intervalo[length(intervalo)]), 
       ylim = c(intervalo[1], intervalo[length(intervalo)]),
       main="Recta cortando al cuadrado. ") 
  abline(b,a) # Recta ax+b (pendiente a)(corte b)

  val=c(a,b)
  return(val) #devuelvo los coeficientes a y b
}

###################################################
##################EJERCICIO 4.2.6##################
###################################################
#Genero la recta que corta al cuadrado
val = simula_recta(-50:50)
#Obtengo los coeficientes de la recta
a = val[1]
b = val[2]

#Genero los datos aleatorios uniformes y los represento junto a la recta
N6=50
dim6=2
intervalo6 = -50:50
lista6 = simula_unif(N6,dim6,intervalo6)
lista61y = NULL #en lista61y tendre todos los valores y.
lista61x = NULL #en lista61y tendre todos los valores x.
contador6 =2
for(j in 1:N6){
  for(i in 1:dim6){
    if((contador6%%2) == 0){
      lista61y = c(lista61y, lista6[[j]][i])
    }
    else{
      lista61x = c(lista61x, lista6[[j]][i])
    }
    contador6 = contador6+1
  }
}

lista61xpos = NULL
lista61ypos = NULL
lista61xneg = NULL
lista61yneg = NULL
for(k in 1:length(lista61x)){
  num = lista61y[k] -a*lista61x[k] -b
  if(num>0){
    lista61xpos = c(lista61xpos, lista61x[k])
    lista61ypos = c(lista61ypos, lista61y[k])
  }
  if(num<0){
    lista61xneg = c(lista61xneg, lista61x[k])
    lista61yneg = c(lista61yneg, lista61y[k])
  }
}

points(lista61xpos,lista61ypos, col = "purple")
points(lista61xneg,lista61yneg, col = "green")

###################################################
##################EJERCICIO 4.2.7##################
###################################################
lista71xpos1 = NULL
lista71ypos1 = NULL
lista71xneg1 = NULL
lista71yneg1 = NULL
lista71xpos2 = NULL
lista71ypos2 = NULL
lista71xneg2 = NULL
lista71yneg2 = NULL
lista71xpos3 = NULL
lista71ypos3 = NULL
lista71xneg3 = NULL
lista71yneg3 = NULL
lista71xpos4 = NULL
lista71ypos4 = NULL
lista71xneg4 = NULL
lista71yneg4 = NULL
for(k in 1:length(lista61x)){
  num1 = (lista61x[k] - 10)^2 + (lista61y[k] - 20)^2 -400
  num2 = 0.5*(lista61x[k] + 10)^2 + (lista61y[k] - 20)^2 -400
  num3 = 0.5*(lista61x[k] - 10)^2 - (lista61y[k] + 20)^2 -400
  num4 = lista61y[k] - 20*((lista61x[k])^2) - 5*(lista61x[k]) +3
  #Para primera funcion
  if(num1>0){
    lista71xpos1 = c(lista71xpos1, lista61x[k])
    lista71ypos1 = c(lista71ypos1, lista61y[k])
  }
  else if(num1<0){
    lista71xneg1 = c(lista71xneg1, lista61x[k])
    lista71yneg1 = c(lista71yneg1, lista61y[k])
  }
  #Para segunda funcion
  if(num2>0){
    lista71xpos2 = c(lista71xpos2, lista61x[k])
    lista71ypos2 = c(lista71ypos2, lista61y[k])
  }
  else if(num2<0){
    lista71xneg2 = c(lista71xneg2, lista61x[k])
    lista71yneg2 = c(lista71yneg2, lista61y[k])
  }
  #Para tercera funcion
  if(num3>0){
    lista71xpos3 = c(lista71xpos3, lista61x[k])
    lista71ypos3 = c(lista71ypos3, lista61y[k])
  }
  else if(num3<0){
    lista71xneg3 = c(lista71xneg3, lista61x[k])
    lista71yneg3 = c(lista71yneg3, lista61y[k])
  }
  #Para cuarta funcion
  if(num4>0){
    lista71xpos4 = c(lista71xpos4, lista61x[k])
    lista71ypos4 = c(lista71ypos4, lista61y[k])
  }
  else if(num4<0){
    lista71xneg4 = c(lista71xneg4, lista61x[k])
    lista71yneg4 = c(lista71yneg4, lista61y[k])
  }
}

x7=-50
contador=0
while(contador< 1000){
  x7 = c(x7, x7[length(x7)] + 0.10)
  contador=contador+1; 
}

plot(lista71xpos1,lista71ypos1, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "Primera funcion")
points(lista71xneg1,lista71yneg1, col = "green")
y71 = 20 - sqrt(400-(x7-10)^2)
y72 = 20 + sqrt(400-(x7-10)^2)
points(x7, y71, col="orange", type="l")
points(x7, y72, col="orange", type="l")

plot(lista71xpos2,lista71ypos2, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "Segunda funcion")
points(lista71xneg2,lista71yneg2, col = "green")
y72a = 20 - sqrt(400-0.5*(x7+10)^2)
y72b= 20 + sqrt(400-0.5*(x7+10)^2)
points(x7, y72a, col="orange", type="l")
points(x7, y72b, col="orange", type="l")

plot(lista71xpos3,lista71ypos3, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "Tercera funcion")
points(lista71xneg3,lista71yneg3, col = "green")
y73a = -sqrt(0.5*(x7-10)^2 -400) -20
y73b = sqrt(0.5*(x7-10)^2 -400) -20
points(x7, y73a, col="orange", type="l")
points(x7, y73b, col="orange", type="l")

plot(lista71xpos4,lista71ypos4, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "Cuarta funcion")
points(lista71xneg4,lista71yneg4, col = "green")
y74 = 20*x7^2 +5*x7 -3
points(x7, y74, col="orange", type="l")


###################################################
##################EJERCICIO 4.2.8##################
###################################################
#pinto las funciones del ejercicio 7
#funcion 4









