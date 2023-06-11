rm(list=ls())

rate <- read.csv(file="C:/Users/daeun/OneDrive/바탕 화면/시계열/FEDFUNDS.csv")

date <- rate[,1]
fedfunds <- log(rate[,2])

n0 <- length(fedfunds)

#Compute the growth rate
growth = 100*(fedfunds[2:n0]-fedfunds[1:(n0-1)])
growth <- as.matrix(growth)

#Plot the growth rate 
date <- as.Date(date)

plot(date[2:n0], growth, type="l", col="blue", lwd=2, xlab="date", ylab="")

n <- length(growth) 

#OLS for AR(1) model
y <- growth[2:n]
X <- cbind(as.matrix(rep(1,(n-1))), growth[1:(n-1),1])

#Estimated parameters
phi.hat <- solve(t(X)%*%X)%*%t(X)%*%y 
phi.hat

fitted <- X%*%phi.hat #fitted values
resid.ols <- y-fitted #residuals

#plot the growth rate and its fitted value
date.new <- date[2:n]
plot(date.new, y, type="l", col="blue", lwd=2, xlab="date", ylab="")
lines(date.new, fitted, col="red", lwd=2)
text(date.new[200], 4, "growth rate", cex=1.5, col="blue")
text(date.new[200], 2.6, "fitted value", cex=1.5, col="red")

