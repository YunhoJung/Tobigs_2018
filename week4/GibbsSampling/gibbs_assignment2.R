### 1. marginal pdf
### f(x1) = 2/(x1+1) * exp(-x1)
### f(x2) = 2/(x2+1) * exp(-x2)
### f(x1|x2) = f(x1,x2)/f(x2) = (x2+1)*exp(-(x2+1)*x1) -> exponential distribution with lambda is x2+1
### f(x2|x1) = f(x1,x2)/f(x1) = (x1+1)*exp(-(x1+1)*x2) -> exponential distribution with lambda is x1+1
### Therefore, sampling using rexp

# Gibbs Sampler
### random vector with n=10
Nsim=10
x1=rep(1,Nsim)
x2=rep(1,Nsim)
for (i in 2:Nsim) {
  x1[i]=rexp(1,rate=1)
  x2[i]=rexp(1,rate=1)
}
x1
x2

### random vector with n=10^2
Nsim=100
x1=rep(1,Nsim)
x2=rep(1,Nsim)
for (i in 2:Nsim) {
  x1[i]=rexp(1,rate=1)
  x2[i]=rexp(1,rate=1)
}
x1
x2

### random vector with n=10^3 
Nsim=1000
x1=rep(1,Nsim)
x2=rep(1,Nsim)
for (i in 2:Nsim) {
  x1[i]=rexp(1,rate=1)
  x2[i]=rexp(1,rate=1)
}
x1
x2

### 2-D coutour visualization
library(MASS)
contour(kde2d(x1,x2,lims= c(1,2,1,2)),xlab='x1',ylab='x2',main='Estimated Kernel Density')

### 2-D scatter plot
points(x1,x2,pch=20,col="green")

