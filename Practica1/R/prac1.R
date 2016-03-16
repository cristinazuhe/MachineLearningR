########################################################################
############################### SECCION 1 ##############################
########################################################################
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

lista3x=NULL
lista3y=NULL
for(k in 1:N3){
  lista3x = c(lista3x,lista3[[k]][1])                    #en lista3x tendre todos los valores x.(primeros)
  lista3y = c(lista3y,lista3[[k]][2])                    #en lista3y tendre todos los valores y.(segundos)
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
lista4x = NULL
lista4y= NULL
for(k in 1:N4){
    lista4x = c(lista4x,lista4[[k]][1])          
    lista4y = c(lista4y,lista4[[k]][2])                                 
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

lista61x=NULL
lista61y=NULL
for(k in 1:N6){
    lista61x = c(lista61x,lista6[[k]][1]) #en lista61y tendre todos los valores y.
    lista61y = c(lista61y,lista6[[k]][2]) #en lista61x tendre todos los valores x.
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
etiquetas71 = NULL     #Vector con etiquetas para funcion1
etiquetas72 = NULL     #Vector con etiquetas para funcion2
etiquetas73 = NULL     #Vector con etiquetas para funcion3
etiquetas74 = NULL     #Vector con etiquetas para funcion4

#x7 es un vector con valores en el intervalo de trabajo.
#Util para dibujar las funciones1, 2, 3 y 4.
x7=intervalo6[1]
contador=0
while(contador< 1000){
  x7 = c(x7, x7[length(x7)] + 0.10)
  contador=contador+1; 
}

#PRIMERA FUNCION.
#Valores en eje y de la primera funcion
y71a = 20 - sqrt(400-(x7-10)^2)
y71b = 20 + sqrt(400-(x7-10)^2)
#Dibujo la primera funcion en una nueva gráfica
plot(x7,y71a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.7:Primera funcion.", type="l")
points(x7, y71b, col="purple", type="l")

#Dibujo los puntos del apartado 6 etiquetándolos de acuerdo a la segunda función
for(k in 1:length(lista61x)){
  num1 = (lista61x[k] - 10)^2 + (lista61y[k] - 20)^2 -400
  if(num1>0){                                                 #valores positivos de la funcion--> etiqueta 1.
    points(lista61x[k], lista61y[k],col= "orange")            #Los pinto en color naranja
    etiquetas71 = c(etiquetas71, 1)
  }
  if(num1<0){                                                 #valores negativos de la funcion-->etiqueta -1
    points(lista61x[k], lista61y[k], col="green")             #Los pinto en color verde
    etiquetas71 = c(etiquetas71, -1)
  }
}

#Analogo para...
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


#Analogo para...
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


#Analogo para...
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
etiquetas8 = etiquetas6
#Obtengo el número de muestras positivas y negativas a cambiar 
#Un 10% de todas las que hay de positivas y negativas. 
numeropositivos8 =0
for(i in 1:length(etiquetas6)) {
 if(etiquetas6[i] == 1)
  numeropositivos8 = numeropositivos8+1
}
numeronegativos8 = length(etiquetas6) - numeropositivos8

#positivosacamiar8 muestras han de pasar de etiquetado 1 a -1
positivosacambiar8 = numeropositivos8%/%10  
#negativosacambiar8 muestras han de pasar de etiquetado -1 a 1
negativosacambiar8 = numeronegativos8%/%10

#Cambio positivos aleatorios a negativos.
aleatorio6 = NULL
for(j in 1:positivosacambiar8){
  aleatorio6 = sample(1:length(etiquetas8),1)
   while( etiquetas8[aleatorio6] != 1){
     aleatorio6 = sample(1:length(etiquetas8),1)
   }
  etiquetas8[aleatorio6] = -1;
}

#Cambio negativos aleatorios a positivos.
aleatorio6b = NULL
for(j in 1:negativosacambiar8){
  aleatorio6b = sample(1:length(etiquetas8),1)
  while( etiquetas8[aleatorio6b] != -1 && 
         etiquetas6[aleatorio6b] == etiquetas8[aleatorio6b] ){
    aleatorio6b = sample(1:length(etiquetas8),1)
  }
  etiquetas8[aleatorio6b] = 1;
}

plot(lista61x, lista61y,
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     col = "orange", main="4.2.8:10% muestras modificadas")
abline(b,a) # Recta ax+b (pendiente a)(corte b)
#En etiquetas8 ya tengo la muestra con las etiquetas cambiadas
for(i in 1:length(etiquetas8)){
  if(etiquetas8[i] == 1){
    points(lista61x[i], lista61y[i], col = "orange")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}


#PARTE B
#con primera funcion:
plot(x7,y71a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.8-b. Primera funcion", type="l")
points(x7, y71b, col="purple", type="l")
for(i in 1:length(etiquetas8)){
  if(etiquetas8[i] == 1){
    points(lista61x[i], lista61y[i], col = "orange")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}

#Con segunda funcion
plot(x7,y72a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.8-b. Segunda funcion", type="l")
points(x7, y72b, col="purple", type="l")
for(i in 1:length(etiquetas8)){
  if(etiquetas8[i] == 1){
    points(lista61x[i], lista61y[i], col = "orange")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}

#Con tercera funcion
plot(x7,y73a, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.8-b. Tercera funcion", type="l")
points(x7, y73b, col="purple", type="l")
for(i in 1:length(etiquetas8)){
  if(etiquetas8[i] == 1){
    points(lista61x[i], lista61y[i], col = "orange")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}

#Con cuarta funcion
plot(x7,y74, col = "purple",
     xlim = c(intervalo6[1], intervalo6[length(intervalo6)]), 
     ylim = c(intervalo6[1], intervalo6[length(intervalo6)]),
     main = "4.2.8-b. Cuarta funcion", type="l")
for(i in 1:length(etiquetas8)){
  if(etiquetas8[i] == 1){
    points(lista61x[i], lista61y[i], col = "orange")
  }else
    points(lista61x[i], lista61y[i], col = "green")
}
########################################################################
############################### SECCION 2 ##############################
########################################################################
###################################################
##################EJERCICIO 4.3.1##################
###################################################
ajusta_PLA <- function(datos, label, max_iter=10, vini){
  vini = c(vini, 1)
  d = dim(datos)
  datos = cbind(datos, rep(1,d[1]))
  contador = 0
  numerocambiadas=NULL
  while(contador<max_iter){
    H = sign(datos%*%vini)
    indexa = which(H!=label) #Guarda los indices de los que son distintos
    if(length(indexa)==0){
      print("Se ha encontrado un buen ajuste.")
      break
    }else{
      i = sample(indexa, 1)
      vini = vini + datos[i,]*label[i]
      print(length(indexa))
    }
    contador= contador+1
  }
  if(contador==max_iter)
    print("No se ha encontrado un buen ajuste.")
  print("Numero de iteracción en la que para: ")
  print(contador)
  print("Numero de errores")
  
  H = sign(datos%*%vini)
  indexa = which(H!=label) #Guarda los indices de los que son distintos
  print(length(indexa))
  return(vini)
}


###################################################
##################EJERCICIO 4.3.2##################
###################################################
print("********************************Ejercicio 4.3.2************************************")
matriz_datoss2e2 = cbind(lista61x, lista61y) #creo la matriz de datos
mi_labels2e2 = etiquetas6
max_itera100 = 100
d = dim(matriz_datoss2e2)

vector_inicial0 = rep(0,d[2])
writeLines("\n***Ajuste PLA con vector inicial 0:***")
print(ajusta_PLA(matriz_datoss2e2,mi_labels2e2, max_itera100, vector_inicial0))

for(i in 1:10){
writeLines("\n***Ajuste PLA con vector aleatorio:***")
    vector_inicial_random = runif(d[2],0,1)
    print(vector_inicial_random)
    ajusta_PLA(matriz_datoss2e2,mi_labels2e2, max_itera100, vector_inicial_random)
}

###################################################
##################EJERCICIO 4.3.3##################
###################################################
print("********************************Ejercicio 4.3.3************************************")
max_iter10 = 10
max_iter1000 = 1000
matriz_datoss2e3 = cbind(lista61x, lista61y) #creo la matriz de datos
etiquetasorigs2e3 = etiquetas8
intervalors2e3 = intervalo6
d = dim(matriz_datoss2e3)


#Representamos las muestras con las etiquetas originales
plot(NULL,NULL, col= "red", 
     xlim = c(intervalors2e3[1], intervalors2e3[length(intervalors2e3)]), 
     ylim = c(intervalors2e3[1], intervalors2e3[length(intervalors2e3)]),
     main="4.3.3.Etiquetas antes de PLA ") 
for(k in 1:length(matriz_datoss2e3[,1])){
  if(etiquetasorigs2e3[k] == 1)
    points(matriz_datoss2e3[k,1], matriz_datoss2e3[k,2],col= "orange")
  else
    points(matriz_datoss2e3[k,1], matriz_datoss2e3[k,2],col= "green")
}

#Realizo PLA
vector_inicial0 = rep(0,d[2])
writeLines("\n***Ajuste PLA con vector inicial 0:***")
hiperplanos3e3i10 = ajusta_PLA(matriz_datoss2e3,etiquetasorigs2e3, max_iter10, vector_inicial0)
coefas2e3 = -hiperplanos3e3i10[3]/hiperplanos3e3i10[2]
coefbs2e3 = -hiperplanos3e3i10[1]/hiperplanos3e3i10[2]

#Representamos las muestras con las nuevas etiquetas generadas por el hiperplano de PLA
plot(NULL,NULL, col= "red", 
     xlim = c(intervalors2e3[1], intervalors2e3[length(intervalors2e3)]), 
     ylim = c(intervalors2e3[1], intervalors2e3[length(intervalors2e3)]),
     main="4.3.3.Etiquetas tras PLA ") 
abline(coefbs2e3,coefas2e3) # Recta ax+b (pendiente a)(corte b)

etiquetass2e3 = NULL                                  #Será un vector con valores 1 y -1
for(k in 1:length(matriz_datoss2e3[,1])){
  numi = matriz_datoss2e3[k,2] -coefas2e3*matriz_datoss2e3[k,1] -coefbs2e3
  if(numi>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(matriz_datoss2e3[k,1], matriz_datoss2e3[k,2],col= "orange") #Los pinto en color naranja
    etiquetass2e3 = c(etiquetass2e3, 1)
  }
  if(numi<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(matriz_datoss2e3[k,1], matriz_datoss2e3[k,2], col="green")  #Los pinto en color verde
    etiquetass2e3 = c(etiquetass2e3, -1)
  }
}

#Obtengo etiquetas cambiadas por PLA
etiquetas_cambiadas = which(etiquetasorigs2e3!=etiquetass2e3) 
num_etiquetas_cambiadas = length(etiquetas_cambiadas)
print("Número de etiquetas cambiadas: ")
print(num_etiquetas_cambiadas)


###################################################
##################EJERCICIO 4.3.4##################
###################################################
print("********************************Ejercicio 4.3.4************************************")
matriz_datoss2e4 = cbind(lista61x, lista61y) #creo la matriz de datos
etiquetasorigs2e4 = etiquetas71
intervalors2e4 = intervalo6
ds2e4 = dim(matriz_datoss2e4)

#Representamos las muestras con las etiquetas originales
plot(NULL,NULL, col= "red", 
     xlim = c(intervalors2e4[1], intervalors2e4[length(intervalors2e4)]), 
     ylim = c(intervalors2e4[1], intervalors2e4[length(intervalors2e4)]),
     main="4.3.4:Etiquetas antes de PLA. Datos de funcion1 de 4.2.7 ") 
for(k in 1:length(matriz_datoss2e4[,1])){
  if(etiquetasorigs2e4[k] == 1)
    points(matriz_datoss2e4[k,1], matriz_datoss2e4[k,2],col= "orange")
  else
    points(matriz_datoss2e4[k,1], matriz_datoss2e4[k,2],col= "green")
}

#Realizo PLA
vector_inicial0s2e4 = rep(0,ds2e4[2])
writeLines("\n***Ajuste PLA con vector inicial 0:***")
hiperplanos2e4i10 = ajusta_PLA(matriz_datoss2e4,etiquetasorigs2e4, max_iter10, vector_inicial0s2e4)
coefas2e4 = -hiperplanos2e4i10[3]/hiperplanos2e4i10[2]
coefbs2e4 = -hiperplanos2e4i10[1]/hiperplanos2e4i10[2]

#Representamos las muestras con las nuevas etiquetas generadas por el hiperplano de PLA
plot(NULL,NULL, col= "red", 
     xlim = c(intervalors2e4[1], intervalors2e4[length(intervalors2e4)]), 
     ylim = c(intervalors2e4[1], intervalors2e4[length(intervalors2e4)]),
     main="4.3.4.Etiquetas tras PLA ") 
abline(coefbs2e4,coefas2e4) # Recta ax+b (pendiente a)(corte b)

etiquetass2e4 = NULL                                  #Será un vector con valores 1 y -1
for(k in 1:length(matriz_datoss2e4[,1])){
  numi = matriz_datoss2e4[k,2] -coefas2e4*matriz_datoss2e4[k,1] -coefbs2e4
  if(numi>0){                                       #valores positivos de la funcion--> etiqueta 1.
    points(matriz_datoss2e4[k,1], matriz_datoss2e4[k,2],col= "orange") #Los pinto en color naranja
    etiquetass2e4 = c(etiquetass2e4, 1)
  }
  if(numi<0){                                       #valores negativos de la funcion-->etiqueta -1
    points(matriz_datoss2e4[k,1], matriz_datoss2e4[k,2], col="green")  #Los pinto en color verde
    etiquetass2e4 = c(etiquetass2e4, -1)
  }
}

#Obtengo etiquetas cambiadas por PLA
etiquetas_cambiadass2e4 = which(etiquetasorigs2e4!=etiquetass2e4) 
num_etiquetas_cambiadass2e4 = length(etiquetas_cambiadass2e4)
print("Número de etiquetas cambiadas: ")
print(num_etiquetas_cambiadass2e4)


###################################################
##################EJERCICIO 4.3.5##################
###################################################
PLA_grafica <- function(datos, label, max_iter=10, vini, intervalo){
  #Dibujo los datos etiquetados
  plot(datos[1,1], datos[1,2], main="4.3.5: Ajuste PLA",
       xlim = c(intervalo[1], intervalo[length(intervalo)]), 
       ylim = c(intervalo[1], intervalo[length(intervalo)]),)
  for(i in 1:length(label)){
    if(label[i] == 1)
      points(datos[i,1], datos[i,2], col="orange")
    else
      points(datos[i,1], datos[i,2], col="green")
  }
  vini = c(vini, 1)
  d = dim(datos)
  datos = cbind(datos, rep(1,d[1]))
  contador = 0
  
  while(contador<max_iter){
    H = sign(datos%*%vini)
    indexa = which(H!=label) #Guarda los indices de los que son distintos
    if(length(indexa)==0){
      print("Se ha encontrado un buen ajuste.")
      break
    }else{
      i = sample(indexa, 1)
      vini = vini + datos[i,]*label[i]
      #abline(-vini[3]/vini[2], -vini[1]/vini[2], col="thistle1")
    }
    contador= contador+1
  }
  if(contador==max_iter)
    print("No se ha encontrado un buen ajuste.") 
  
  H = sign(datos%*%vini)
  indexa = which(H!=label) #Guarda los indices de los que son distintos
  print("Numero de errores:")
  print(length(indexa))
  
  abline(-vini[3]/vini[2], -vini[1]/vini[2], col="thistle3")
  print("Vector de parada:")
  vini
}

print("********************************Ejercicio 4.3.5************************************")
matriz_datoss2e5 = matriz_datoss2e3 #creo la matriz de datos
mi_labels2e5 = etiquetasorigs2e3
ds2e5 = dim(matriz_datoss2e5)

vector_inicial0s2e5 = rep(0,ds2e5[2])
writeLines("\n***Ajuste PLA con vector inicial 0:***")
print(PLA_grafica(matriz_datoss2e5,mi_labels2e5, 10,
                  vector_inicial0s2e5, intervalors2e3))


###################################################
##################EJERCICIO 4.3.6##################
###################################################
PLA_grafica_MOD <- function(datos, label, max_iter=10, vini, intervalo){
  #Dibujo los datos etiquetados
  plot(datos[1,1], datos[1,2], main="4.3.6: Ajuste PLA",
       xlim = c(intervalo[1], intervalo[length(intervalo)]), 
       ylim = c(intervalo[1], intervalo[length(intervalo)]),)
  for(i in 1:length(label)){
    if(label[i] == 1)
      points(datos[i,1], datos[i,2], col="orange")
    else
      points(datos[i,1], datos[i,2], col="green")
  }
  vini = c(vini, 1)
  d = dim(datos)
  datos = cbind(datos, rep(1,d[1]))
  contador = 0
  mejorvini=NULL
  mejorbuenas=length(label)
  indexa=NULL
  while(contador<max_iter){
    H = sign(datos%*%vini)
    indexa = which(H!=label) #Guarda los indices de los que son distintos
    if(length(indexa)==0){
      print("Se ha encontrado un buen ajuste.")
      break
    }else{
      i = sample(indexa, 1)
      vini = vini + datos[i,]*label[i]
      if(length(which(sign(datos%*%vini)!=label))<mejorbuenas){
        H = sign(datos%*%vini)
        indexa = which(H!=label) #Guarda los indices de los que son distintos
        mejorvini=vini
        mejorbuenas=length(indexa)
      }
      #abline(-vini[3]/vini[2], -vini[1]/vini[2], col="thistle1")

    }
    contador= contador+1
  }
  if(contador==max_iter)
    print("No se ha encontrado un buen ajuste.")

  print("Numero de errores:")
  print(mejorbuenas)
  
  abline(-mejorvini[3]/mejorvini[2], -mejorvini[1]/mejorvini[2], col="thistle3")
  print("Vector de parada:")
  mejorvini
}


print("********************************Ejercicio 4.3.6************************************")
matriz_datoss2e6 = matriz_datoss2e3 #creo la matriz de datos
mi_labels2e6 = mi_labels2e5
ds2e6 = dim(matriz_datoss2e6)

vector_inicial0s2e6 = rep(0,ds2e6[2])
writeLines("\n***Ajuste PLA con vector inicial 0:***")
print(PLA_grafica_MOD(matriz_datoss2e6,mi_labels2e6, max_itera100,
                  vector_inicial0s2e6, intervalors2e3))
