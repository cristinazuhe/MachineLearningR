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

simula_unifM = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
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
###################################################
print("###############################Ejercicio 3###################################")
newton = function(
  FUN = function(x, y) x^2 +  2*y^2 +2*sin(2*pi*x)*sin(2*pi*y), interx = c(-3, 3), intery=c(-3,3),
  val_ini=as.matrix(rbind(1,1)), tasa=0.1, tope = 0.00001, mimain="", maxiter=15){

  plot(NULL,NULL, xlim = interx, ylim=intery, xlab="x", ylab="y", main = mimain)
  pintar_grafica(FUN)
  val_ini = as.matrix(rbind(1,1)) 
  coste_funcion=FUN(val_ini[1], val_ini[2])
  contador=0
  val_sig = val_ini
  diferencia_coste = 200
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
    points(val_sig, col="orange")
    print(FUN(val_sig[1], val_sig[2]))
  }
  points(val_sig, col="red")
  print("Numero de iteraciones realizadas:")
  print(contador)
  print("Valor obtenido:")
  print(val_sig)
}

#newton()

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
pinta_puntos(datos, intervalo = rango,etiqueta=etiqueta) # utiliza las etiquetas 
abline(coef[2],coef[1])            # pinta la recta

tasa = 0.01
vector_w=c(0,0) 

reglog = function(
  matriz_datos,etiquetas, interx = c(-1.25, 1.25), intery=c(-1.25,1.25),
  vector_w=c(0,0), tasa=0.01, tope = 0.01, mimain="", maxiter=20){
  
  numerador = etiquetas%*%matriz_datos
  denominador = 1+exp(etiquetas%*%(matriz_datos%*%vector_w))
  gradientelog = (numerador/denominador[1,1])/(dim(matriz_datos)[1])
  
  vector_w = vector_w + tasa%*%gradientelog
  
  print("Valor obtenido:")
  print(vector_w)
}
reglog(matriz_datos=datos, etiquetas=etiqueta)

numerador = etiqueta%*%datos
denominador = 1+exp(etiqueta%*%(datos%*%vector_w))
gradientelog = -(numerador/denominador[1,1])/(dim(datos)[1])