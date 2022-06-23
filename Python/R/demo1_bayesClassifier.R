# The probabilty density function (PDF) for the red data
redDensity = function(x) {
  dnorm(x,mean=-2,sd=1)
}
# The function which lets you make draws from a distribution
redDraw = function(x) {
  rnorm(x,mean=-2,sd=1)
}

# The probabilty density function (PDF) for the red data
greenDensity = function(x) {
  dnorm(x,mean=2,sd=1.5)
}
# The function which lets you make draws from a distribution
greenDraw = function(x) {
  rnorm(x,mean=2,sd=1.5)
}

# Let's plots the data
x=seq(-6,6,length=200)
# So some plots
plot(redDraw(x),0*x, col='red', pch=3, xlim=c(-6,6),xlab='X',ylab='type')
# The '+1' here  is just to seperate the two sets so you can see them
points(greenDraw(x),0*x, col='green', pch=3)

# Plot the densities so you can see where they cross
lines(x,redDensity(x),col='red')
lines(x,greenDensity(x),col='green')

##########################################################################

# To plot a more complicated density we need to use the "distr" library.
library(distr)

# The probabilty density function (PDF) for the red data
# Note this has two "humps".  Technically, this is called
# multi-modal.  We divide by 2 to keep this a PDF!
redDensity = function(x) {
  (dnorm(x,mean=-2,sd=0.5) + dnorm(x,mean=1,sd=0.5))/2 
}
plot(x,redDensity(x),col='red',type='l')
# Using the distr package you can define distributions from their PDF
redDistribution = AbscontDistribution(d=redDensity)
# The 'r' function lets you make draws from a distribution
redDraw = r(redDistribution)

# The probabilty density function (PDF) for the red data
greenDensity = function(x) {
  dnorm(x,mean=2,sd=1.5)
}
greenDistribution = AbscontDistribution(d=greenDensity)
greenDraw = r(greenDistribution)

# Let's plots the densities
x=seq(-6,6,length=200)

# So some plots
plot(redDraw(x),0*x, col='red', pch=3, xlim=c(-6,6),xlab='X',ylab='type')
empiricalDensity = function(x) {
  dnorm(x,mean=mean(x),sd=sd(x)) 
}
lines(x,redDensity(x),col='red')
lines(x,empiricalDensity(x),col='red',lty=2,lwd=2)

points(greenDraw(x),0*x, col='green', pch=3)
lines(x,greenDensity(x),col='green')

# Save a plot
plot(redDraw(x),0*x, col='red', pch=3, xlim=c(-6,6),xlab='X',ylab='type')
points(greenDraw(x),0*x, col='green', pch=3)
lines(x,redDensity(x),col='red')
lines(x,greenDensity(x),col='green')

