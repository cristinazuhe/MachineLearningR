#######################################################################
################## Cristina Zuheros Montes - 2016 #####################
#######################################################################

########################################################################
############################## AUXILIARES ##############################
########################################################################
#Funcion auxiliar para ir parado la ejecucion
pulsaTecla <- function(){
  cat ("Pulse Intro para continuar...")
  line <- readline()
}

pintar_grafica = function(f) {
  x=y=seq(-50,50,by=0.1)
  z = outer(x,y,FUN=f)
  contour(x,y,z, levels=0:3, drawlabels = TRUE,add = TRUE, col="purple")
}

simula_unifM = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
}

simula_gaus <- function(N=2, dims=2, mean=0, sd=1){
  m = matrix(rnorm(N*dims, mean=mean, sd=sd),
             nrow = N, ncol=dims, byrow=T)
}

simula_recta = function (intervalo = c(-1,1),visible=F, ptos = NULL){
  if(is.null(ptos)) m = simula_unifM(2,2,intervalo)
  a = (m[1,2] - m[2,2]) / (m[1,1]-m[2,1]) # calculo de la pendiente
  b = m[1,2]-a*m[1,1]
  if (visible) {
  #  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
    plot(NULL, NULL, type="n", xlim=intervalo, ylim=intervalo)
    abline(b,a)
    points(m,col=3) #pinta en verde los puntos
  }
  return(c(a,b))
}

pinta_puntos = function(m,intervalo = c(-1,1) ,etiqueta=1){
  nptos=nrow(m)
  plot(m,xlim=intervalo, ylim = intervalo, xlab=paste("Pinta ",nptos," Puntos"), ylab="",col=etiqueta+3)
}

fsimetria1 <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}


fsimetria2 <- function(A){
  A = abs(A-A[nrow(A):1,])
  -sum(A)
}

regresionlineal <- function(MatrizDatos, Etiquetas){
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

PLA_pocket <- function(datos, label, max_iter=50, vini){
  for(i in 1:length(label)){
    if(label[i] == 5)
      label[i] =-1
  }
  d = dim(datos)
  datos = cbind(rep(1,d[1]), datos)
  contador = 0
  mejorvini=vini
  mejorbuenas=length(label)
  indexa=NULL
  while(contador<max_iter){
    H = sign(datos%*%vini)
    indexa = which(H!=label) #Guarda los indices de los que son distintos
    if(length(indexa)!=0){
      if(contador!=0){
      i = sample(indexa, 1)
      vini = vini + datos[i,]*label[i]
      }else{
        mejorbuenas = length(indexa)
      }
      if(length(which(sign(datos%*%vini)!=label))<mejorbuenas){
        H = sign(datos%*%vini)
        indexa = which(H!=label) #Guarda los indices de los que son distintos
        mejorvini=vini
        mejorbuenas=length(indexa)
      }
    }else{
       return(mejorvini)
    }
    contador= contador+1
  }
  return(mejorvini)
}
########################################################################
############################### SECCION 1 ##############################
########################################################################
print("###############################SECCION 1###################################")
###################################################
###################EJERCICIO 1.1###################
###################################################
print("###############################Ejercicio 1###################################")
fs1e1f1 = function(x,y) ((x*exp(y))- (2*y*exp(-x)))^2  #defino la funcion
fs1e1f2 = function(x,y) x^2 +  2*y^2 +2*sin(2*pi*x)*sin(2*pi*y)  #defino la funcion

graddesc = function(
  FUN = function(x, y) ((x*exp(y))- (2*y*exp(-x)))^2, interx = c(-1.25, 1.25), intery=c(-1.25,1.25),
  val_ini=c(1,1), tasa=0.01, tope = 10^(-14), mimain="", maxiter=200){
    plot(NULL,NULL, xlim = interx, ylim=intery, xlab="x", ylab="y", main = mimain)
    pintar_grafica(FUN)
    coste_funcion=FUN(val_ini[1], val_ini[2])
    contador=0
    while(coste_funcion>tope && contador<maxiter){
      dxy_fs1e1 = deriv(as.expression(body(FUN)), c("x","y"), function.arg=TRUE)
      val_sig = val_ini - tasa*attr(dxy_fs1e1(val_ini[1],val_ini[2]), 'gradient')
      coste_funcion = FUN(val_sig[1], val_sig[2])
      val_ini = val_sig
      contador=contador+1
      points(val_sig, col="orange")
    }
    points(val_sig, col="red")
    print("Numero de iteraciones realizadas:")
    print(contador)
    print("Valor obtenido:")
    print(val_ini)
}
print("*************************Primera funcion*************************")
#graddesc(mimain="1.1.a)Primera funcion", interx = c(-2,2), intery=c(-2,2), tasa=0.1)
print("*************************Segunda funcion*************************")
print("*************************Inicial (1,1)0.01*************************")
#graddesc(FUN = fs1e1f2,mimain="1.1.b)Segunda funcion (1,1) 0.01", maxiter=50)
print("*************************Inicial (1,1) 0.1*************************")
#graddesc(FUN = fs1e1f2,mimain="1.1.b)Segunda funcion (1,1) 0.1", maxiter=50, tasa=0.1)

print("*************************Inicial (-1,-1)***********************")
#graddesc(FUN = fs1e1f2, val_ini=c(-1,-1),mimain="1.1.b)Segunda funcion (-1,-1)", maxiter=50)

print("***********************Inicial (0.1,0.1)************************")
#graddesc(FUN = fs1e1f2, val_ini=c(0.1,0.1),mimain="1.1.b)Segunda funcion (0.1,0.1)", maxiter=50)

print("**********************Inicial (-0.5,-0.5)***********************")
#graddesc(FUN = fs1e1f2, val_ini=c(-0.5,-0.5),mimain="1.1.b)Segunda funcion (-0.5,-0.5)", maxiter=50)

###################################################
###################EJERCICIO 1.2###################
###################################################
print("###############################Ejercicio 2###################################")
coorddesc = function(
  FUN = function(x, y) ((x*exp(y))- (2*y*exp(-x)))^2, interx = c(-3, 3), intery=c(-3,3),
  val_ini=c(1,1), tasa=0.1, tope = 10^(-14), mimain="", maxiter=15){
  plot(NULL,NULL, xlim = interx, ylim=intery, xlab="x", ylab="y", main = mimain)
  pintar_grafica(FUN)
  coste_funcion=FUN(val_ini[1], val_ini[2])
  contador=0
  val_sig = val_ini
  while(coste_funcion>tope && contador<maxiter){
    dxy_fs1e1 = deriv(as.expression(body(FUN)), c("x","y"), function.arg=TRUE)
    val_sig[1] = val_ini[1] - tasa*(attr(dxy_fs1e1(val_ini[1],val_ini[2]), 'gradient'))[1]
    val_sig[2] = val_ini[2] - tasa*(attr(dxy_fs1e1(val_sig[1],val_ini[2]), 'gradient'))[2]
    coste_funcion = FUN(val_sig[1], val_sig[2])
    val_ini = val_sig
    contador=contador+1
    points(val_sig, col="orange")
  }
  points(val_sig, col="red")
  print("Numero de iteraciones realizadas:")
  print(contador)
  print("Valor obtenido:")
  print(val_ini)
}

print("*************************15 Iteraciones*************************")
#coorddesc(mimain="1.2. 15 Iteraciones.")
print("*************************30 Iteraciones*************************")
#coorddesc(maxiter=30, mimain="1.2. 30 Iteraciones.")

###################################################
###################EJERCICIO 1.3###################
###################################################
print("###############################Ejercicio 3###################################")
newton = function(
  FUN = function(x, y) x^2 +  2*y^2 +2*sin(2*pi*x)*sin(2*pi*y), interx = c(-2, 2), intery=c(-2,2),
  val_ini=as.matrix(rbind(1,1)), tasa=0.1, tope = 0.00001, mimain="", maxiter=15){

  plot(NULL,NULL, xlim = interx, ylim=intery, xlab="x", ylab="y", main = mimain)
  pintar_grafica(FUN)
  coste_funcion=FUN(val_ini[1], val_ini[2])
  contador=0
  val_sig = val_ini
  diferencia_coste = 200
  valores = coste_funcion
  while(diferencia_coste>tope && contador<maxiter){
    hess_fs1e1f1 = deriv(as.expression(body(FUN)), c("x","y"), hessian=TRUE, function.arg=TRUE)
    hessiana_fs1e1f1 = as.matrix(rbind(c((attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'hessian'))[1],
                                         (attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'hessian'))[2]),
                                       c((attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'hessian'))[3],
                                         (attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'hessian'))[4])))
    gradiente_fs1e1f1 = as.matrix(rbind((attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'grad'))[1],
                                        (attr(hess_fs1e1f1(val_ini[1],val_ini[2]), 'grad'))[2]))
    hessiana_fs1e1f1 = solve(hessiana_fs1e1f1)

    val_sig = val_ini -hessiana_fs1e1f1%*%gradiente_fs1e1f1
    diferencia_coste = abs(FUN(val_sig[1], val_sig[2]) - FUN(val_ini[1], val_ini[2]))
    val_ini = val_sig
    contador=contador+1
    valores =c(valores, FUN(val_sig[1], val_sig[2]))
  }
  points((val_sig[1]:val_sig[2]), col="red")
  print("Numero de iteraciones realizadas:")
  print(contador+1)
  print("Punto obtenido:")
  print(val_sig)
  print("Valor alcanzado")
  print(FUN(val_sig[1], val_sig[2]))
  iteraciones = 1:(contador+1)
  plot(x=iteraciones, y=valores, col="purple", main="Descenso valor función.")
}

#print("Newton partiendo de (1,1)")
#newton(mimain="Sec1 Ejer3 (1,1)")
#print("Newton partiendo de (-1,-1)")
#newton(val_ini=as.matrix(rbind(-1,-1)), mimain="Sec1 Ejer3 (-1,-1)")
#print("Newton partiendo de (-0.5,-0.5)")
#???newton(val_ini=as.matrix(rbind(-0.5,-0.5)), mimain="Sec1 Ejer3 (-0.5,-0.5)")

###################################################
###################EJERCICIO 1.4###################
###################################################
print("###############################Ejercicio 4###################################")
N=100
rango = c(-1,1)
datos = simula_unifM (N,2,rango)
coef = simula_recta(rango) 
f4 = function(pto,coef) {          #funcion de la recta
  pto[2]-pto[1]*coef[1]-coef[2]
}
z0 = apply(datos,1,f4,coef)        #obtiene los valores de la funcion para datos
etiqueta = sign(z0)                # apartir de ellos crea las etiquetas
#pinta_puntos(datos, intervalo = rango,etiqueta=etiqueta) # utiliza las etiquetas 
#abline(coef[2],coef[1])            # pinta la recta

reglog = function(datos,etiqueta,vector_w_ini=c(0,0,0), tasa=0.01){
  N= dim(datos)[1]
  diferencia = 1
  w = vector_w_ini
  d = dim(datos)
  datos = cbind(rep(1,d[1]), datos)
  etiqueta = t(t(etiqueta))
  contador=0
  
  while(diferencia > 0.01 && contador<100000){
    vector_w = w
  permutacion = sample(1:N,N,replace=FALSE)
  for(i in 1:N){
    indice = permutacion[i]
    gradientelog = (-etiqueta[indice,]*datos[indice,])/(1+exp(etiqueta[indice,]*w%*%datos[indice,]))
    w = w - tasa*gradientelog 
    contador=contador+1
    
  }
  diferencia = sqrt(sum((w-vector_w)^2))
  }
  return(w)
}

vector_final = reglog(datos=datos, etiqueta=etiqueta)


#Obtener g
a= -vector_final[2]/vector_final[3]
b= -vector_final[1]/vector_final[3]
#curve(a*x + b, col="orange", add=T)

#Obtener Eout
nuevos_datos = simula_unifM (N,2,rango)
real_z0 = apply(nuevos_datos,1,f4,coef)        #obtiene los valores de la funcion para datos
etiquetas_real = sign(real_z0)                # apartir de ellos crea las etiquetas

coef_estimados = c(a,b)
estimado_z0 = apply(nuevos_datos,1,f4,coef_estimados)
etiquetas_estimadas = sign(estimado_z0)

error=mean(etiquetas_real!=etiquetas_estimadas)
#print("Error obtenido regresion logistica:")
#print(error)

###################################################
###################EJERCICIO 1.5###################
###################################################
print("###############################Ejercicio 5###################################")
#Datos train
digit.train <- read.table("datos/zip.train", header=FALSE) 
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
etiquetas_digitos_train = digitos15.train[,1]
ndigitos_train = nrow(digitos15.train)
matriz_Digitos_train = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos_train,16,16))
rm(digit.train)
rm(digitos15.train)

intensidad_train = apply(matriz_Digitos_train[1:ndigitos_train,,],1, mean)
simetria_train = apply(matriz_Digitos_train[1:ndigitos_train,,],1,fsimetria1)
datos_train = as.matrix(cbind(intensidad_train,simetria_train))
#plot(datos_train,xlab="Intensidad Promedio",ylab="Simetria",main="SEC1:ejer5.train",
#     col=etiquetas_digitos_train,pch=etiquetas_digitos_train+3)

#Regresion
hiperplanow = regresionlineal(datos_train, etiquetas_digitos_train)
hiperplanow = PLA_pocket(datos_train, etiquetas_digitos_train, vini=hiperplanow)
#abline( a=-hiperplanow[1,]/hiperplanow[3,],
#        b=-hiperplanow[2,]/hiperplanow[3,], col="orange")


#Datos test
digit.test <- read.table("datos/zip.test", header=FALSE)
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]
etiquetas_digitos_test = digitos15.test[,1]
ndigitos_test = nrow(digitos15.test)
matriz_Digitos_test = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos_test,16,16))
rm(digit.test)
rm(digitos15.test)

intensidad_test = apply(matriz_Digitos_test[1:ndigitos_test,,],1, mean)
simetria_test = apply(matriz_Digitos_test[1:ndigitos_test,,],1,fsimetria1)
simetria_test=simetria_test #porque...
datos_test = as.matrix(cbind(intensidad_test,simetria_test))
#plot(datos_test,xlab="Intensidad Promedio",ylab="Simetria",main="SEC1:ejer5.test",
#    col=etiquetas_digitos_test,pch=etiquetas_digitos_test+3)
#abline( a=-hiperplanow[1,]/hiperplanow[3,],
#        b=-hiperplanow[2,]/hiperplanow[3,], col="orange")


#Apartado b:
#Para datos_train:
coefw = c(-hiperplanow[1,]/hiperplanow[3,],-hiperplanow[2,]/hiperplanow[3,]) 
f4 = function(pto,coefw) {          #funcion de la recta
  pto[2]-pto[1]*coefw[2]-coefw[1]
}
z0trainfin = apply(datos_train,1,f4,coefw)        #obtiene los valores de la funcion para datos
etiquetas_train_fin = sign(z0trainfin)                # apartir de ellos crea las etiquetas
for(i in 1:length(etiquetas_train_fin)){
  if(etiquetas_train_fin[i] == -1)
    etiquetas_train_fin[i] =5
}
distintas_train = length(which(etiquetas_train_fin!=etiquetas_digitos_train))
Ein_train = (distintas_train*100)/nrow(datos_train)
#print("Ein en train:")
#print(Ein_train)

#Para datos_test:
z0testfin = apply(datos_test,1,f4,coefw)        #obtiene los valores de la funcion para datos
etiquetas_test_fin = sign(z0testfin)                # apartir de ellos crea las etiquetas
for(i in 1:length(etiquetas_test_fin)){
  if(etiquetas_test_fin[i] == -1)
    etiquetas_test_fin[i] =5
}
distintas_test = length(which(etiquetas_test_fin!=etiquetas_digitos_test))
Ein_test = (distintas_test*100)/nrow(datos_test)
#print("Ein en test")
#print(Ein_test) 

#Apartado c:


########################################################################
############################### SECCION 2 ##############################
########################################################################
print("###############################SECCION 2###################################")
###################################################
###################EJERCICIO 2.1###################
###################################################
print("###############################Ejercicio 1###################################")
library(orthopolynom)
reg_lineal <- function(MatrizDatos, Etiquetas){
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

sobreajusteg2g10 <- function(Qf=4, N=50, sigma=0.5){
  #Obtengo los polinomios de Lagrange normalizados
  pol.Leg.Normalizados <- legendre.polynomials(n= Qf-1, normalized=T)
  #normalizo los coeficientes
  a = simula_gaus(N=Qf,1,mean=0, sd=1)
  a=a/sqrt(sum(a^2))

  #obtengo la función f(x)
  fun_f=0
  for(i in 1:Qf){
     fun_f = fun_f + pol.Leg.Normalizados[[i]]*a[i]
  }
  #print(integral(fun_f^2,limits=c(-1,1))) #Compruebo que está bien normalizado

  #Añado error a las muestras
  epsilon = simula_gaus(N=N,1,mean=0, sd=1)
  X = simula_unifM (N=N,1,rango=c(-1,1))
  f = as.function((fun_f))
  Y = f(X) + sigma*epsilon
  #plot(X, Y, xlim=c(-1,1), main="Sec1 Ejer1:Ajusto con g2 y g10")
  #curve(expr=f, add=T, col="red", lw=2)

  #Ya tengo la función y los datos. Ahora tengo que encontrar g2 y g10
  datos2 = as.matrix(cbind(X,X^2))
  hiperplanow2= reg_lineal(datos2,Y)
  #curve(hiperplanow2[3,]*x^2 + hiperplanow2[2,]*x + hiperplanow2[1,], add=T,
  #      col="orange", lw=2, lty=3)

  datos10 = as.matrix(cbind(X,X^2,X^3,X^4,X^5,X^6, X^7,X^8,X^9,X^10))
  hiperplanow10= reg_lineal(datos10,Y)
  #curve(hiperplanow10[11,]*x^10 + hiperplanow10[10,]*x^9 +
  #      hiperplanow10[9,]*x^8 + hiperplanow10[8,]*x^7 + hiperplanow10[7,]*x^6+
  #      hiperplanow10[6,]*x^5 + hiperplanow10[5,]*x^4 + hiperplanow10[4,]*x^3 +
  #      hiperplanow10[3,]*x^2 + hiperplanow10[2,]*x + hiperplanow10[1,], add=T,
  #      col="blue", lw=2, lty=3)
  
  #Error g2
  fung2 = as.function(as.polynomial(c(hiperplanow2[1,], hiperplanow2[2,],hiperplanow2[3,])))
  Eg2_real=(sum((fung2(X)-Y)^2))/N
  #print("Error con la muestra original usando g2")
  #print(Eg2_real)
  
  nueva_X = simula_unifM (N=N,1,rango=c(-1,1))
  nueva_Y = f(X) + sigma*epsilon
  Eg2_fuera=(sum((fung2(nueva_X)-nueva_Y)^2))/N
  #print("Error con muestra nueva usando g2")
  #print(Eg2_fuera)
  
  #Error g10
  fung10 = as.function(as.polynomial(c(hiperplanow10[1,], hiperplanow10[2,],
                                       hiperplanow10[3,], hiperplanow10[4,],
                                       hiperplanow10[5,], hiperplanow10[6,],
                                       hiperplanow10[7,], hiperplanow10[8,],
                                       hiperplanow10[9,], hiperplanow10[10,],
                                       hiperplanow10[11,])))
  Eg10_real=(sum((fung10(X)-Y)^2))/N
  #print("Error con la muestra original usando g10")
  #print(Eg10_real)
  
  #print("Error con muestra nueva usando g10")
  Eg10_fuera=(sum((fung10(nueva_X)-nueva_Y)^2))/N
  #print(Eg10_fuera)
  
  return(c(Eg2_fuera,Eg10_fuera))
}

errores = sobreajusteg2g10(Qf=20, N=50, sigma=1)
#Hago sobreajuste 150 veces
limite=149
for(i in 1:limite){
   errores = rbind(errores,sobreajusteg2g10(Qf=20, N=50, sigma=1))
}

#Calculo la media de Eout para g2 y g10
media_errores = apply(errores,2,sum)/(limite+1)
print("Media de Eout para g2 y para g10")
print(media_errores)

sobreajuste=media_errores[2]-media_errores[1]
print("Sobreajuste")
print(sobreajuste)