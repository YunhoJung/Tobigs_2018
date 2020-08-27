# --- Monte Carlo Method ---
n <- 1e4 # n = 10000
r <- 1 # radius 1

# The use of pdf is different for each situation
carculate_area <- function(n, r){
  x <- runif(n,-r,r) # x
  y <- runif(n,-r,r) # y
  # Boundary condition for accept / reject based on circle                                                                                                                                                                          cond <- x^2 + y^2 <= r^2
  cond <- x^2+y^2 <r^2 # boundary condition
  # area of circle = (area of square) * the number of dots located inside the circle / the number of total dots                                                                                                                                                           area <- (2*r)^2 * length(x[cond]) / n
  area <- (2*r)^2*length(x[cond])/n
  # Monte Carlo
  
  # Visualization
  par(pty='s') # 1:1 ratio
  plot(x[cond], y[cond], pch=20, cex=0.1, xlim=c(-r-1,r+1), ylim=c(-r-1,r+1), col='red',
       xlab='X-axis', ylab='Y-axis', main='Monte Carlo estimating circle area')
  par(new=T)
  plot(x[!cond], y[!cond], pch=20, cex=0.1, xlim=c(-r-1,r+1), ylim=c(-r-1,r+1), col='blue',
       xlab='X-axis', ylab='Y-axis', main='Monte Carlo estimating circle area')
  abline(h=c(-r, r), v=c(-r, r), lty='dotted')
  
  return(area)
}

carculate_area(n, r=1)

# --- Metropolis Hastings ---
# Using normal proposal
# Generate a Markov chain {Xk,k=1,2,...,n} so that the normal distribution of the Markov chain is f(x)

metroNormal=function(N,b,init){
  x<-rep(init,N) # vector x full of initial values (init)
  for (i in 2:N){
    # Select one distribution and draw one Y=y from it                                                                                                                                                                                                   y<-runif(1,x[i-1]-1,x[i-1]+1)
    y <- runif(1,x[i-1]-1,x[i-1]+1)
    # Acceptance probability                                                                                                                                                                                                   alpha<-exp((x[i-1]^2 - y^2)/2)
    alpha <- min(alpha<-exp((x[i-1]^2 - y^2)/2),1)
    u<-runif(1) # extract one variable in Unif(0, 1) to be used for comparison 
    if (u<alpha) x[i] <- y # update y
    else x[i] <- x[i-1] # previous
  }
  return(x)
}

res1=metroNormal(2*10^3,1,-10) # samples that follow a standard normal distribution are produced


hist(res1,nclass=20,main="Simulation of Standard Normal with MCMC (uniform proposal)",prob=T,xlim=c(-4,4))
curve(dnorm(x),-4,4,add=T, col='red') # built-in standard normal distribution

ts.plot(res1,col=3, main = "Traceplot")
# Due to the nature of the normal distribution, most of the data should be located between -2 and 2. Draw a red line to check it out.
# arguments : (x or y, 0-row or 1-col, lwd=..., col=...)
abline(2,0,lwd=2,col=2);abline(-2,0,lwd=2,col=2) 
plot(res1, main = "Traceplot", xlab='Time', type="l") # similar with ts.plot

# --- Gibbs Sampling ---

if(!require(mnormt)) install.packages("mnormt"); library(mnormt)
Sig=matrix(c(1,0.6,0.6,1),2,2)  # the definition of the variance matrix(the definition of the covariance matrix)
Sig
x1=seq(-3,3,0.1)                 #############################################################################
x1
x2=seq(-3,3,0.1)                 # Extracted from the actual multivariate normal distribution for comparison #
x2
cbind(x1[1],x2)
cbind(x1[2],x2)
n=length(x1)                     #############################################################################
n
z=matrix(0,n,n)
z
for (i in 1:n){
  z[i,]=dmnorm(cbind(x1[i],x2),c(0,0),Sig)
} # built-in bivariate normal distribution
z
contour(x1,x2,z,levels=c(0.005,0.05,0.1,0.15),lwd=2,main="Contour of Bivariate Normal : N(0,0,1,1,0.6)") # contour

# Using Gibbs Sampler

Nsim=6 # by splitting the burning period At first, only 6 are drawn and then an empty vector is defined
Nsim
x1=numeric(Nsim) # numeric vector
x1
x2=numeric(Nsim)
x2
for (i in 2:Nsim){
  x1[i]=rnorm(1,0.6*x2[i-1],sqrt(0.64)) # *= x2. gets one random number with mean and standard deviation
  x2[i]=rnorm(1,0.6*x1[i],sqrt(0.64))
}
x1 # x
x2 # y
for (i in 1:Nsim){
  segments(x1[i],x2[i],x1[i+1],x2[i],lwd=2,col="blue")
  segments(x1[i+1],x2[i],x1[i+1],x2[i+1],lwd=2,col="blue")
} # segments 

text(x1[1],x2[1],"0",cex=2,col="red")
text(x1[2],x2[2],"1",cex=2,col="red")
text(x1[3],x2[3],"2",cex=2,col="red")
text(x1[4],x2[4],"3",cex=2,col="red")
text(x1[5],x2[5],"4",cex=2,col="red")
text(x1[6],x2[6],"5",cex=2,col="red")


Nsim=101
x1=numeric(Nsim)
x2=numeric(Nsim)
for (i in 2:Nsim){
  x1[i]=rnorm(1,0.6*x2[i-1],sqrt(0.64)) # x2 - previous state
  x2[i]=rnorm(1,0.6*x1[i],sqrt(0.64)) # x1 - next state
}

for (i in 1:Nsim){
  segments(x1[i],x2[i],x1[i+1],x2[i],lwd=2,col="blue")
  segments(x1[i+1],x2[i],x1[i+1],x2[i+1],lwd=2,col="blue")
}
# gradually follows toward the contour line

Nsim=10001
x1=numeric(Nsim) 
x2=numeric(Nsim)

for (i in 2:Nsim){
  x1[i]=rnorm(1,0.6*       x2[i-1]    ,sqrt(0.64))
  x2[i]=rnorm(1,0.6*       x1[i]    ,sqrt(0.64))
}
points(x1,x2,pch=20,col="green") # scatter plot

library(MASS)
contour(kde2d(x1, x2, lims= c(-3,3,-3,3)), xlab='x1', ylab='x2',main='Estimated Kernel Density') # draw the contour line directly using samples from the function we made
# kde2d - approximate pdf and estimated
# prior probability, posteriori probability, bayesian statistics