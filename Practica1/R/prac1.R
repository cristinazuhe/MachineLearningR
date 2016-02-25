#EJERCICIO 4.2.1
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

#EJERCICIO 4.2.2
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

#EJERCICIO 4.2.3
lista3 = simula_unif(50,5,-50:50)
x=runif(10, 0, 1) 
print(x)
hist(x,probability=TRUE,col=gray(.5),main="uniform on [0,1]") 
#curve(dunif(x,0,1),add=T) # Densidad 