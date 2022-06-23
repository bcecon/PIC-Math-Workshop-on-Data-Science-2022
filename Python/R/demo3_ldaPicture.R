# We beed the multivariate normal for our first demonstration
library(mvtnorm)

multivariateNormalDensity1 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(-1,-1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(2,1,1,1), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw1 = function(n) {
  mean = c(-1,-1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(2,1,1,1), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity2 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(1,1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(2,1,1,1), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw2 = function(n) {
  mean = c(1,1)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(1,1,1,1), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity3 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(1,0)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(1,1,1,1), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw3 = function(n) {
  mean = c(1,0)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(1,1,1,1), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}

multivariateNormalDensity4 = function(x,y) {
  # cbind takes two columns and merges them into 
  # one matrix
  mean = c(0.5,-0.5)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(1,1,1,1), ncol=2)
  dmvnorm(cbind(x,y),mean=mean,sigma=sigma) 
}

multivariateNormalDraw4 = function(n) {
  mean = c(0.5,-0.5)
  sigma = matrix(c(1,0,0,1), ncol=2)
  #sigma = matrix(c(1,1,1,1), ncol=2)
  rmvnorm(n,mean=mean,sigma=sigma) 
}


# A 3D visualization using the rgl library
library(rgl)
x = seq(-3,3,length.out=50)
y = seq(-3,3,length.out=50)
z1 = outer(x, y, multivariateNormalDensity1)
z2 = outer(x, y, multivariateNormalDensity2)
z3 = outer(x, y, multivariateNormalDensity3)
z4 = outer(x, y, multivariateNormalDensity4)

draw1 = multivariateNormalDraw1(1000)
draw2 = multivariateNormalDraw2(1000)
draw3 = multivariateNormalDraw3(1000)
draw4 = multivariateNormalDraw4(1000)

col <- rainbow(length(z1))[rank(z1)]
clear3d()
points3d(draw1[,1],draw1[,2],0,col='red')
points3d(draw2[,1],draw2[,2],0,col='green')
#points3d(draw3[,1],draw3[,2],0,col='blue')
#points3d(draw4[,1],draw4[,2],0,col='cyan')

surface3d(x,y,z1*20,alpha=1.0,color='red')
surface3d(x,y,z2*20,alpha=1.0,color='green')
#surface3d(x,y,z3*20,alpha=1.0,color='blue')
#surface3d(x,y,z4*20,alpha=1.0,color='cyan')

