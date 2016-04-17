#######################################################################
################## Cristina Zuheros Montes - 2016 #####################
#######################################################################

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
print("*************************Inicial (1,1)*************************")
#graddesc(FUN = fs1e1f2,mimain="1.1.b)Segunda funcion (1,1)", maxiter=50)

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
###############################################
print("###############################Ejercicio 3###################################")