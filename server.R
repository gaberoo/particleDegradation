library(shiny)

source("func.R")

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
shinyServer(function(input, output, session) {

  output$degrade.plot <- renderPlot(height=600,
  {
    times <- seq(0,input$max.time,length.out=1000)
    y0 <- c(B1=0,B2=0,r=input$init.rad,M=0)

    parms <- list(
      alpha   = 10**input$alpha,
      beta    = 10**input$beta,
      alpha2  = 10**input$alpha2,
      beta2   = 10**input$beta2,
      gamma   = 10**input$gamma,
      mu      = 10**input$mu,
      K       = 10**input$K,
      prod    = 10**input$prod,
      rho     = 10**input$rho,
      nu      = 10**input$nu
   )

    if (input$eqTabs == 1) {
      out <- lsoda(y0,times,f.cheat,parms)
    } else {
      out <- lsoda(y0,times,f.parasite,parms)
      #out <- lsoda(y0,times,f.parasite,parms)
    }
    V <- 4/3*pi*out[,4]**3
    V <- V/V[1]

    k <- 1
    Tau <- as.numeric(out[which(V<0.5)[1],1])
    #try(silent=TRUE,{
    #  h.nls <- nls(V ~ hill(out[,1],.k,.Tau),start=c(.k=k,.Tau=Tau))
    #})
    tryCatch({
      y.nls <- nls(V ~ sigmoid(out[,1],.k,.Tau),start=c(.k=k,.Tau=Tau))
      k <- coef(y.nls)[1]
      Tau <- coef(y.nls)[2]
    },error=function(e) {
      message("Coulnd't fit model")
    })

    par(mfrow=c(3,1))
    xlim <- c(0,input$max.time)

    plot(out[,1],V,type="l",xlim=xlim,ylim=c(0,V[1]),main="Particle volume")
    lines(out[,1],sigmoid(out[,1],k,Tau),lty=2,col="blue")
    if (exists("h.nls")) {
      lines(out[,1],hill(out[,1],coef(h.nls)[1],coef(h.nls)[2]),lty=2,col="red")
    }
    legend("topright",c(sprintf("T = %.2f",Tau),sprintf("k = %.2f",k)),cex=2)

    plot(out[,1],out[,2]+out[,3],xlim=xlim,type="l", main="Bacteria")
    lines(out[,1],out[,2],type="l", main="Primary",col="red")
    lines(out[,1],out[,3],type="l", main="Secondary",col="blue")
    legend("topright",c("Primary","Secondary"),lty=1,
           col=c("red","blue"),cex=2)

    plot(out[,1],out[,5],xlim=xlim,type="l",main="Monomers")
  })
})
