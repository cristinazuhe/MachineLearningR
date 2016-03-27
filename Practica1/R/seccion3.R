readkey <- function(){
  cat ("Press [enter] to continue")
  line <- readline()
}
########################################################################
############################### SECCION 3 ##############################
########################################################################
###################################################
##################EJERCICIO 4.4.2##################
###################################################
#Leo el fichero
lectura_fichero <- read.table("zip.train", header=FALSE)

#indices de los vectores de datos que representan 1's o 5's
indicess3e2= which((lectura_fichero[,1]) ==5 |  (lectura_fichero[,1]) ==1)

#matrizdatostodos es una lista con las matrices que representan las imagenes de 1's y 5's
matrizdatostodos = NULL
for(k in 1:length(indicess3e2)){
    matrizdatos=matrix(as.numeric(lectura_fichero[indicess3e2[k],2:ncol(lectura_fichero)]), 
                       nrow=16,ncol=16)
    matrizdatostodos = c(matrizdatostodos, list(matrizdatos))
}
image(matrizdatostodos[[100]], main="4.4.2: Representacion 1") #Así veo la imagen (matriz) 100 de la lista

print("********************************Ejercicio 4.4.2*********************************")
print("Ver imagen 4.4.2")
readkey()

###################################################
##################EJERCICIO 4.4.3##################
###################################################
#Vector con el valor medio para cada matriz
vectormedias = NULL
for(k in 1:length(indicess3e2)){
   vectormedias = c(vectormedias, mean(apply(matrizdatostodos[[k]],1, mean)))
}

#Vector con grado de simetria vertical para cada matriz
total=ncol(matrizdatostodos[[1]])
mitad= total/2
vectorsimetrias =NULL
for(k in 1:length(indicess3e2)){
   sumadifabs = 0
   for(i in 1: mitad){
      vectorderecho = matrizdatostodos[[k]][,i]
      vectorizquierdo=matrizdatostodos[[k]][,total - i+1]
      sumadifabs = sumadifabs + 2*(sum(abs(vectorderecho - vectorizquierdo))) #Multiplico por 2 porque me piden que recorra toda la imagen
   }
   vectorsimetrias = c(vectorsimetrias, sumadifabs)
}
vectorsimetrias = -vectorsimetrias


###################################################
##################EJERCICIO 4.4.4##################
###################################################
matrizdatoss3e4 = cbind(vectormedias, vectorsimetrias)
etiquetass3e4 = lectura_fichero[indicess3e2,1]
plot(matrizdatoss3e4[,1], matrizdatoss3e4[,2], 
     main="4.4.4. Representacion 1's y 5's", col=(etiquetass3e4-1)/2 +2,
     xlab="Media", ylab="Simetria")

print("********************************Ejercicio 4.4.4*********************************")
print("Ver grafica 4.4.4")

###################################################
##################EJERCICIO 4.4.5##################
###################################################
regresion <- function(MatrizDatos, Etiquetas){
  for(i in 1:length(Etiquetas)){
   if(Etiquetas[i] == 5)
     Etiquetas[i] =-1
  }
  d = dim(MatrizDatos)
  MatrizDatos = cbind(rep(1,d[1]), MatrizDatos)
  
  X = MatrizDatos
  sv = svd(t(X)%*%X)
  U = sv$u
  D = diag(sv$d)
  V = sv$v

  XtraX_inv = V%*%solve(D)%*%t(V)
  Xpseudo = XtraX_inv%*%t(X)
  #Obtengo w:
  w = Xpseudo%*%Etiquetas
  return(w)
}

###################################################
##################EJERCICIO 4.4.6##################
###################################################
hiperplanow = regresion(matrizdatoss3e4, etiquetass3e4)

plot(matrizdatoss3e4[,1], matrizdatoss3e4[,2], main="4.4.6. Regresión 1's y 5's", 
     col=(etiquetass3e4-1)/2 +2, xlab="Media", ylab="Simetria")

abline( a=-hiperplanow[1,]/hiperplanow[3,],
        b=-hiperplanow[2,]/hiperplanow[3,], 
        col="thistle3")

print("********************************Ejercicio 4.4.6*********************************")
print("Ver grafica 4.4.6")