# We beed the multivariate normal for our first demonstration
library(mvtnorm)

multivariateNormalDensity1 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(-1,-1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw1 = function(n) {
  mean = c(-1,-1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity2 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(2,2)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw2 = function(n) {
  mean = c(2,2)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity3 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(3,2)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw3 = function(n) {
  mean = c(3,2)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity4 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(0,1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw4 = function(n) {
  mean = c(0,1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(4,2,2,3), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

# A 3D visualization using the rgl library
library(rgl)
x = seq(-4,4,length.out=50)
y = seq(-4,4,length.out=50)
z1a = outer(x, y, multivariateNormalDensity1)
z1b = outer(x, y, multivariateNormalDensity2)
z1 = z1a+z1b

z2a = outer(x, y, multivariateNormalDensity3)
z2b = outer(x, y, multivariateNormalDensity4)
z2 = z2a+z2b

d1 = multivariateNormalDraw1(500)
d2 = multivariateNormalDraw2(500)
d3 = multivariateNormalDraw3(500)
d4 = multivariateNormalDraw4(500)

clear3d()

points3d(d1[,1], d1[,2], 0*d1[,1],color='red')
points3d(d2[,1], d2[,2], 0*d2[,1],color='red')
points3d(d3[,1], d3[,2], 0*d3[,1],color='green')
points3d(d4[,1], d4[,2], 0*d4[,1],color='green')

surface3d(x,y,z1*20-3.5,alpha=1.0,color='red')
surface3d(x,y,z2*20-3.5,alpha=1.0,color='green')



