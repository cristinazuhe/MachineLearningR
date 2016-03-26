
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
    matrizdatos=matrix(as.numeric(lectura_fichero[indicess3e2[k],2:ncol(lectura_fichero)]), nrow=16,ncol=16)
    matrizdatosderecha=NULL
    for(i in nrow(matrizdatos):1){
      matrizdatosderecha = cbind(matrizdatosderecha, matrizdatos[,i])
    }
    matrizdatostodos = c(matrizdatostodos, list(matrizdatos))
}
#image(matrizdatostodos[[200]]) #Así veo la imagen (matriz) 200 de la lista

###################################################
##################EJERCICIO 4.4.3##################
###################################################
print("********************************Ejercicio 4.4.3*********************************")
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

###################################################
##################EJERCICIO 4.4.4##################
###################################################
matrizdatoss3e4 = cbind(vectormedias, vectorsimetrias)
etiquetass3e4 = lectura_fichero[indicess3e2,1]
plot(matrizdatoss3e4[,1], matrizdatoss3e4[,2], main="4.4.5. Representacion 1's y 5's", col=(etiquetass3e4-1)/2 +2)


###################################################
##################EJERCICIO 4.4.5##################
###################################################
regresion <- function(MatrizDatos, Etiquetas){
  plot(MatrizDatos[,1], MatrizDatos[,2], main="4.4.5. Representacion 1's y 5's", 
       col=(Etiquetas-1)/2 +2)
  
  d = dim(MatrizDatos)
  MatrizDatos = cbind(rep(1,d[1]), MatrizDatos)

  sv = svd(MatrizDatos)
  U = sv$u
  D = diag(sv$d)
  V = sv$v

  Dpseudo = solve(t(D)%*%D)%*%t(D)
  XtraX_inv = V%*%Dpseudo%*%t(V)
  Xcruz = XtraX_inv%*%t(MatrizDatos)

  #Obtengo w:
  w = Xcruz%*%as.matrix(Etiquetas)
  print(w)
  abline( -w[1,]/w[2,],-w[3,]/w[2,],  col="thistle3")
}

regresion(matrizdatoss3e4, etiquetass3e4)

