rm(list = ls(all=T)) # This code clears all.

y <- vector(length=1000) #to save simulated data

for (i in 1:length(y)){
  y[i] <- rnorm(1, mean=0, sd=1) 
}
dev.new()
plot(y, type="l", xlab="period", col="blue")
legend("bottomleft", inset=0.05,  "white noise process", lwd=2, lty=1, col="blue")


#------------AR(1) model: simulation exercise-----------------
# See how the parameter phi1 affects the dynamics of y. 

set.seed(123)
a <- rnorm(length(y), mean=0, sd=1) 

y[1] <- 0
phi1 <- 0.98  
for (i in 2:length(y)){
  y[i] <- phi1*y[i-1] + a[i]
}

dev.new()
plot(y, type="l", xlab="period", col="blue")
legend("bottomleft", inset=0.05,  c(paste("Phi1=", phi1)), lwd=2, lty=1, col="blue")

#Autocorrelation Function
#자기상관함수(주기성 파악)
#dev.new()
#acf(y, lag=12, main="ACF")

#Partial Autocorrelation Function
#편자기상관함수(개벽시차의 영향력 파악)
#dev.new()
#pacf(y, lag=12, main="PACF")


#-------Given various values of phi1, generate y---------------------
dev.new()
par(mfcol=c(2,2)) # 2 by 2
#options: mfcol and mfrow

#y[1] <- 0 # initial value 
for (phi1 in c(0.0, 0.5, 0.9, 1)){
  
  for (i in 2:length(y)){
    y[i] <- phi1*y[i-1] + a[i]
  }
  
  plot(y, type="l", xlab="period", col="blue")
  legend("bottomleft", inset=.05, c(paste("Phi1=", phi1)), lwd=2, lty=1, col="blue")
}

par(mfcol=c(1,1))

#------------AR(1) model with a constant term: simulation exercise-----------------
# See how the parameter phi0 affects the dynamics of y. 

phi0 <- 0.3
phi1 <- 1 

#y[1] <- 0 # initial value
for (i in 2:length(y)){
  y[i] <- phi0 + phi1*y[i-1] + a[i]
}

dev.new()
plot(y, type="l", xlab="period", col="blue")
legend("bottomright", inset=0.05,  "non-stationary process", lwd=2, lty=1, col="blue")


#------Simulation of a random walk process: phi=1 ----------------------------------
#y[1] <- 0 # initial value 
for (j in 1:1000){     #number of simulations
  
  a <- rnorm(length(y), mean=0, sd=1) # Increase sd to see what happens.  
  for (i in 2:length(y)){
    y[i] <- 0.1+y[i-1] + a[i] #random walk with a drift
  }
  
  if (j==1) {
    dev.new()
    plot(y, type="l", col="blue", xlab="period", ylim=c(-100, 200), main="stock price")
  } else {
    lines(y, type="l", col="blue")
  }
  
}


#------------AR(2) model--------------------------------------------

dev.new()
par(mfcol=c(2,2)) # 2 by 2 

y[1]=0 # initial value
y[2]=0
for (phi in c(0.0, 0.5, 0.9, 1)){
  
  if (phi==0) {
    phi1=0  #phi=phi1+phi2
    phi2=0
  }else if (phi==0.5) {
    phi1=0.25
    phi2=0.25
  }else if (phi==0.9) {
    phi1=0.45
    phi2=0.45
  } else {
    phi1=0.5
    phi2=0.5
  }
  
  for (i in 3:length(y)){
    y[i] <- phi1*y[i-1]+phi2*y[i-2] + a[i]
  }
  
  plot(y, type="l", xlab="period", col="blue")
  legend("bottomleft", inset=.05, c(paste("phi1+phi2=", phi)), lwd=2, lty=1, col="blue")
}

par(mfcol=c(1,1))

#---------------MA(1)-------------------------
y <- vector(length=50)
set.seed(123)
a <- rnorm(length(y), mean=0, sd=1) 

dev.new()
par(mfcol=c(1,2)) # 2 by 2 

y[1]=0 # first value 
for (theta in c(0.8, -0.8)){
  
  for (i in 2:length(y)){
    y[i] <- a[i]+theta*a[i-1]
  }
  
  plot(y, type="l", xlab="period", col="blue")
  legend("topleft", inset=.05, c(paste("theta=", theta)), lwd=2, lty=1, col="blue")
}
par(mfcol=c(1,1))

