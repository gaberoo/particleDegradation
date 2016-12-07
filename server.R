library(shiny)
library(deSolve)

f <- function(t,y,p) {
  dy <- y*0
  S <- ifelse(y[3]>0.0,4*pi*y[3]**2,1.0)
  .qr <- ifelse(y[3]>0.0,S,0.0)
  totB <- y[1]+y[2]
  b1 <- ifelse(y[1]>0,y[1]/totB,0.0)
  b2 <- ifelse(y[2]>0,y[2]/totB,0.0)
  totK <- (1-totB/(p$K*S))
  dy[1] <- (p$alpha  + p$beta *p$gamma*y[4]*b1)*totK - p$mu*y[1]
  dy[2] <- (p$alpha2 + p$beta2*p$gamma*y[4]*b2)*totK - p$mu*y[2]
  dy[3] <- -p$prod*y[1]*.qr/(S*p$rho)
  dy[4] <- p$prod*y[1]*.qr - p$gamma*y[4]*totK - p$nu*y[4]
  list(dy)
}

if (FALSE) {
  input <- list(
    max.time=100,
    init.rad=100,
    alpha=-7,
    beta=2,
    alpha2=-10,
    beta2=-4,
    gamma=-6,
    mu=0,
    K=-7,
    prod=3,
    rho=0,
    nu=0)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$degrade.plot <- renderPlot(height=1000,
  {
    times <- seq(0,input$max.time,length.out=1000)
    y0 <- c(B1=0,B2=0,r=input$init.rad,M=0)
    parms <- lapply(input,function(x) 10**x)

    out <- ode(y0,times,f,parms)
    V <- 4/3*pi*out[,4]**3
    V <- V/V[1]

    hill <- function(x,k,Tau) { 1 - x**k/(Tau**k+x**k) }
    sigmoid <- function(x,k,Tau) { 1/(1+exp(k*(x-Tau))) }

    k <- 1
    Tau <- as.numeric(out[which(V<0.5)[1],1])
    tryCatch({
      y.nls <- nls(V ~ sigmoid(out[,1],.k,.Tau),start=c(.k=k,.Tau=Tau))
      k <- coef(y.nls)[1]
      Tau <- coef(y.nls)[2]
    },error=function(e) {
      message("Coulnd't fit model")
    })

    par(mfrow=c(5,1))
    plot(out[,1],V,type="l",ylim=c(0,V[1]),main="Particle volume")
    lines(out[,1],sigmoid(out[,1],k,Tau),lty=2)
    legend("topright",c(sprintf("T = %.2f",Tau),sprintf("k = %.2f",k)),cex=2)

    plot(out[,1],out[,2]+out[,3],type="l", main="Bacteria")
    lines(out[,1],out[,2],type="l", main="Primary",col="red")
    lines(out[,1],out[,3],type="l", main="Secondary",col="blue")
    legend("topright",c("Primary","Secondary"),lty=1,
           col=c("red","blue"),cex=2)

    plot(out[,1],out[,5],type="l", main="Monomers")
  })
})
