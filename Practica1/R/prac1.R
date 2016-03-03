
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
  ini_vector = runif(dim, rang[1], rang[length(rang)])      #vector de tamaño dim con valores aleatorios uniformes en el intervalo rang
  mi_lista = list(ini_vector)                               #creo mi lista con el vector
  if(N>1){                                                  #Si nos piden más de un vector en la lista....
     for( i in 2:N){                                        #Para cada nuevo vector de la lista...
       ini_vector = runif(dim, rang[1],rang[length(rang)])  #vector de tamaño dim con valores aleatorios uniformes en el intervalo rang
       segunda = list(ini_vector)                           #creo una lista auxiliar con el nuevo vector
       mi_lista = c(mi_lista, segunda)                      #concateno a mi lista anterior la nueva lista que contiene el nuevo vector.
     }
  }
  return(mi_lista)
}
print("Salima simula_unif por defecto:")
print(simula_unif())

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
  }else if(sigma[1]<0 || sigma[length(sigma)]<0){           #sigma no puede ser negativo
    print("Sigma no puede ser negativo.")
    return(NA)
  }
  ini_vector = rnorm(dim, mean=0, sd=sample(sigma,1))       #vector de tamaño dim con valores aleatorios gaussianos con media 0 y varianza sigma
  mi_lista = list(ini_vector)                               #creo mi lista con el vector
  if(N>1){                                                  #Si nos piden más de un vector en la lista....
    for( i in 2:N){                                         #Para cada nuevo vector de la lista...
      ini_vector = rnorm(dim, mean=0, sd=sample(sigma,1))   #vector de tamaño dim con valores aleatorios gaussianos con media 0 y varianza sigma
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
lista3 = simula_unif(N3,dim3,intervalo3)      #Obtengo la lista de vectores de la funcion

#Creo un vector con los valores y (valores aleatorios generados pares).
#Creo un vector con los valores x (valores aleatorios generados impares).
lista3y = NULL                               #en lista3y tendre todos los valores y.
lista3x = NULL                               #en lista3y tendre todos los valores x.
contador=2
for(j in 1:N3){
  for(i in 1:dim3){
    if((contador%%2) == 0){                   #Si es un elemento par...
     lista3y = c(lista3y, lista3[[j]][i])     #Lo añado a lista3y
    }
    else{                                     #Si es un elemento impar...
     lista3x = c(lista3x, lista3[[j]][i])     #Lo añado a lista3x  
    }
     contador = contador+1
  }
} 
#Represento los datos
plot(lista3x, lista3y, 
     main = "4.2.3: Valores función uniforme",
     col="purple")

###################################################
##################EJERCICIO 4.2.4##################
###################################################
#Genero la lista de vectores con la funcion y los valores que nos indican
N4=50
dim4=2
intervalo4 = 5:7
lista4 = simula_gaus(N4,dim4,intervalo4)         #Obtengo la lista de vectores de la funcion gaussiana

#Creo un vector con los valores y (valores aleatorios generados pares).
#Creo un vector con los valores x (valores aleatorios generados impares).
lista4y = NULL                                   #en lista4y tendre todos los valores y.
lista4x = NULL                                   #en lista4x tendre todos los valores x.
contador4=2
for(j in 1:N4){
  for(i in 1:dim4){
    if((contador4%%2) == 0){                     #Si es un elemento par...
      lista4y = c(lista4y, lista4[[j]][i])       #Lo añado en lista4y
    }else{                                       #Si es un elemento impar...
      lista4x = c(lista4x, lista4[[j]][i])       #Lo añado en lista4x
    }
    contador4 = contador4 +1
  }
} 
plot(lista4x,lista4y, 
     main = "4.2.4: Valores función gaussiana",
     col="purple")


###################################################
##################EJERCICIO 4.2.5##################
###################################################
simula_recta <- function(intervalo=-50:50){
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

#Genero los datos aleatorios uniformes y los represento junto a la recta anterior
N6=50
dim6=2
intervalo6 = -50:50
lista6 = simula_unif(N6,dim6,intervalo6)
lista61y = NULL #en lista61y tendre todos los valores y.
lista61x = NULL #en lista61x tendre todos los valores x.
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

#Etiqueto los datos mediante la función y las almaceno en etiquetas6
etiquetas6 = NULL                                  #Será un vector con valores 1 y -1
for(k in 1:length(lista61x)){
  num = lista61y[k] -a*lista61x[k] -b
  if(num>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange") #Los pinto en color naranja
    etiquetas6 = c(etiquetas6, 1)
  }
  if(num<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")  #Los pinto en color verde
    etiquetas6 = c(etiquetas6, -1)
  }
}

###################################################
##################EJERCICIO 4.2.7##################
###################################################
etiquetas71 = NULL
etiquetas72 = NULL
etiquetas73 = NULL
etiquetas74 = NULL

x7=-50
contador=0
while(contador< 1000){
  x7 = c(x7, x7[length(x7)] + 0.10)
  contador=contador+1; 
}

#PRIMERA FUNCION.
y71a = 20 - sqrt(400-(x7-10)^2)
y71b = 20 + sqrt(400-(x7-10)^2)
plot(x7,y71a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.7:Primera funcion.", type="l")
points(x7, y71b, col="purple", type="l")

for(k in 1:length(lista61x)){
  num1 = (lista61x[k] - 10)^2 + (lista61y[k] - 20)^2 -400
  if(num1>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange") #Los pinto en color naranja
    etiquetas71 = c(etiquetas71, 1)
  }
  if(num1<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")  #Los pinto en color verde
    etiquetas71 = c(etiquetas71, -1)
  }
}

#SEGUNDA FUNCION
y72a = 20 - sqrt(400-0.5*(x7+10)^2)
y72b= 20 + sqrt(400-0.5*(x7+10)^2)
plot(x7,y72a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.7:Segunda funcion.", type="l")
points(x7, y72b, col="purple", type="l")

for(k in 1:length(lista61x)){
  num2 = 0.5*(lista61x[k] + 10)^2 + (lista61y[k] - 20)^2 -400
  if(num2>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange") #Los pinto en color naranja
    etiquetas72 = c(etiquetas72, 1)
  }
  if(num2<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")  #Los pinto en color verde
    etiquetas72 = c(etiquetas72, -1)
  }
}

#TERCERA FUNCION
y73a = -sqrt(0.5*(x7-10)^2 -400) -20
y73b = sqrt(0.5*(x7-10)^2 -400) -20
plot(x7,y73a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.7:Tercera funcion.", type="l")
points(x7, y73b, col="purple", type="l")

for(k in 1:length(lista61x)){
  num3 = 0.5*(lista61x[k] - 10)^2 - (lista61y[k] + 20)^2 -400
  if(num3>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange") #Los pinto en color naranja
    etiquetas73 = c(etiquetas73, 1)
  }
  if(num3<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")  #Los pinto en color verde
    etiquetas73 = c(etiquetas73, -1)
  }
}

#CUARTA FUNCION
y74 = 20*x7^2 +5*x7 -3
plot(x7,y74, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.7:Cuarta funcion.", type="l")

for(k in 1:length(lista61x)){
  num4 = lista61y[k] - 20*((lista61x[k])^2) - 5*(lista61x[k]) +3
  if(num4>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange") #Los pinto en color naranja
    etiquetas74 = c(etiquetas74, 1)
  }
  if(num4<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")  #Los pinto en color verde
    etiquetas74 = c(etiquetas74, -1)
  }
}


###################################################
##################EJERCICIO 4.2.8##################
###################################################
#PARTE A
auxiliar6 = etiquetas6
numeropositivos8 =0
for(i in 1:length(etiquetas6)) {
 if(etiquetas6[i] == 1)
  numeropositivos8 = numeropositivos8+1
}
numeronegativos8 = length(etiquetas6) - numeropositivos8

positivosacambiar8 = numeropositivos8%/%10
negativosacambiar8 = numeronegativos8%/%10

#Cambio positivos aleatorios a negativos.
aleatorio6 = NULL
for(j in 1:positivosacambiar8){
  aleatorio6 = sample(1:length(etiquetas6),1)
   while( etiquetas6[aleatorio6] != 1){
     aleatorio6 = sample(1:length(etiquetas6),1)
   }
    etiquetas6[aleatorio6] = -1;
}

#Cambio negativos aleatorios a positivos.
aleatorio6b = NULL
for(j in 1:negativosacambiar8){
  aleatorio6b = sample(1:length(etiquetas6),1)
  while( etiquetas6[aleatorio6b] != -1 && auxiliar6[aleatorio6b] == etiquetas6[aleatorio6b] ){
    aleatorio6b = sample(1:length(etiquetas6),1)
  }
  etiquetas6[aleatorio6b] = 1;
}

plot(lista61x, lista61y, col = "orange", main="Ejercicio 8")
abline(b,a) # Recta ax+b (pendiente a)(corte b)
#En etiquetas6 ya tengo la muestra con las etiquetas cambiadas
for(i in 1:length(etiquetas6)){
  if(etiquetas6[i] == 1){
    points(lista61x[i], lista61y[i], col = "purple")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}


#PARTE B
#con primera funcion:
plot(lista61x, lista61y, col = "orange", main="Ejercicio 8-b. Primera")
for(i in 1:length(etiquetas6)){
  if(etiquetas6[i] == 1){
    points(lista61x[i], lista61y[i], col = "purple")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}
points(x7, y71a, col="orange", type="l")
points(x7, y71b, col="orange", type="l")

#Con segunda funcion
plot(lista61x, lista61y, col = "orange", main="Ejercicio 8-b. Segunda")
for(i in 1:length(etiquetas6)){
  if(etiquetas6[i] == 1){
    points(lista61x[i], lista61y[i], col = "purple")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}
points(x7, y72a, col="orange", type="l")
points(x7, y72b, col="orange", type="l")

#Con tercera funcion
plot(lista61x, lista61y, col = "orange", main="Ejercicio 8-b. Tercera")
for(i in 1:length(etiquetas6)){
  if(etiquetas6[i] == 1){
    points(lista61x[i], lista61y[i], col = "purple")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}
points(x7, y73a, col="orange", type="l")
points(x7, y73b, col="orange", type="l")

#Con cuarta funcion
plot(lista61x, lista61y, col = "orange", main="Ejercicio 8-b. Cuarta")
for(i in 1:length(etiquetas6)){
  if(etiquetas6[i] == 1){
    points(lista61x[i], lista61y[i], col = "purple")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}
points(x7, y74, col="orange", type="l")
