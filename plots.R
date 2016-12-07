source("func.R")

input <- list(
  max.time=100,
  init.rad=100,
  alpha=-7,
  beta=-2.1,
  alpha2=-10,
  beta2=-4,
  gamma=-6,
  mu=-3.5,
  K=-7,
  prod=3,
  rho=0,
  nu=0)

times <- seq(0,input$max.time,length.out=1000)
y0 <- c(B1=0,B2=0,r=input$init.rad,M=0)
parms <- lapply(input,function(x) 10**x)

fit.deg <- function(alpha,parms) {
  parms$alpha <- alpha
  out <- ode(y0,times,f,parms)
  V <- 4/3*pi*out[,4]**3
  V <- V/V[1]
  k <- 1
  Tau <- as.numeric(out[which(V<0.5)[1],1])
  tryCatch({
    y.nls <- nls(V ~ sigmoid(out[,1],.k,.Tau),start=c(.k=k,.Tau=Tau))
    k <- coef(y.nls)[1]
    Tau <- coef(y.nls)[2]
  },error=function(e) {
    message("Coulnd't fit model")
  })
  c(k,Tau)
}

x <- sapply(A <- seq(1e-7,1e-6,by=1e-7),function(a) fit.deg(a,parms))
plot(A,x[1,])
plot(A,x[2,])


par(mfrow=c(3,1))
xlim <- c(0,input$max.time)

plot(out[,1],V,type="l",xlim=xlim,ylim=c(0,V[1]),main="Particle volume")
lines(out[,1],sigmoid(out[,1],k,Tau),lty=2)
legend("topright",c(sprintf("T = %.2f",Tau),sprintf("k = %.2f",k)),cex=2)

plot(out[,1],out[,2]+out[,3],xlim=xlim,type="l", main="Bacteria")
lines(out[,1],out[,2],type="l", main="Primary",col="red")
lines(out[,1],out[,3],type="l", main="Secondary",col="blue")
legend("topright",c("Primary","Secondary"),lty=1,
       col=c("red","blue"),cex=2)

plot(out[,1],out[,5],xlim=xlim,type="l",main="Monomers")

