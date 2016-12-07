library(deSolve)

f.cheat <- function(t,y,p) {
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

f.parasite <- function(t,y,p) {
  dy <- y*0
  S <- ifelse(y[3]>0.0,4*pi*y[3]**2,1.0)
  .qr <- ifelse(y[3]>0.0,S,0.0)
  totB <- y[1]+y[2]
  totK <- (1-totB/(p$K*S))
  dy[1] <- (p$alpha  + p$beta *p$gamma*y[4])*totK - p$mu*y[1]
  dy[2] <- (p$alpha2 + 100000*p$beta2*y[2]*y[1])*totK - p$mu*y[2]
  dy[3] <- -p$prod*y[1]*.qr/(S*p$rho)
  dy[4] <- p$prod*y[1]*.qr - p$gamma*y[4]*totK - p$nu*y[4]
  list(dy)
}

hill <- function(x,k,Tau) { 1 - x**k/(Tau**k+x**k) }
sigmoid <- function(x,k,Tau) { 1/(1+exp(k*(x-Tau))) }


