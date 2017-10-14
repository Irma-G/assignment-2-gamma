#generating random variables from mixture of Normal distributions
estimates1 <- list(mu=c(3, 10), sigma=c(6, 4), lambda=c(0.75, 0.25)) 
#note lambda is a probability!
#Source: -include name of person who posted it
#https://stack.stackexchange.com/questions/70855/generating-random-variables-from-a-mixture-of-normal-distributions
N <- 300
components <- sample(1:2, prob=c(estimates1$lambda[1], estimates1$lambda[2]), size=N, replace=TRUE)
samples <- rnorm(N)*estimates1$mu+estimates1$sigma
#end sourced code

plotHist <- function(sampling, estimates){
  spread <- range(sampling)
  #nr bins -> 10 per 10 units
  bins <- seq(spread[1], spread[2], l=spread[2]-spread[1]+1)
  #pre-plot, then get highest density and use to set ylim
  sampleHist <- hist(plot=FALSE, sampling, breaks=bins)

  hist(sampling, freq=FALSE, breaks=bins, xlim=c(spread[1],spread[2]), ylim=c(0,max(sampleHist$density)), col=rgb(0, 1, 0, 0.5), main="Time (min) between Old Faithful eruptions", xlab="Minutes", ylab="Density")
  
  curve(dnorm(x, mean=estimates$mu[1], sd=estimates$sigma[1]), col="blue", add=TRUE)
  curve(dnorm(x, mean=estimates$mu[2], sd=estimates$sigma[2]), col="red", add=TRUE)
  #write out what formula is for common curve, then can code it
  curve(sampling, col="black", lwd=2, add=TRUE) #TODO doesn't work
}
plotHist(samples, estimates1)




#Other method for two independent distributions
estimates1 <- list(mu=c(3, 5), sigma=c(1.5, 2.5), lambda=c(0.5, 0.5)) 
#note lambda is a probability!
data1 <- rnorm(500, estimates1$mu[1], estimates1$sigma[1])
data2 <- rnorm(500, estimates1$mu[2], estimates1$sigma[2])

plotHist2 <- function(dataA, dataB, estimates){
  spreadA <- range(dataA)
  spreadB <- range(dataB)
  #nr bins -> 10 per 10 units
  binsA <- seq(spreadA[1], spreadA[2], l=spreadA[2]-spreadA[1]+1)
  binsB <- seq(spreadB[1], spreadB[2], l=spreadB[2]-spreadB[1]+1)
  if (spreadA[1] <= spreadB[1]){
    low <- spreadA[1]
  } else{
    low <- spreadB[1]
  }
  if (spreadA[2] <= spreadB[2]){
    high <- spreadB[2]
  } else{
    high <- spreadA[2]
  }
  totalspread <- c(low, high)
  
  hist(dataA, prob=TRUE, breaks=binsA, xlim=c(totalspread[1],totalspread[2]), ylim=c(0,0.25), col=rgb(0, 1, 0, 0.5), main="Time (min) between Old Faithful eruptions", xlab="Minutes", ylab="Density")
  hist(dataB, prob=TRUE, breaks=binsB, col=rgb(0, 0, 1, 0.5), add=TRUE)
  
  curve(dnorm(x, mean=estimates$mu[1], sd=estimates$sigma[1]), col="blue", add=TRUE)
  curve(dnorm(x, mean=estimates$mu[2], sd=estimates$sigma[2]), col="red", add=TRUE)
}
plotHist2(data1, data2, estimates1)

#official algorithm to get parameters of mixed Normal distributions - case n = 2
mixtools::normalmixEMcomp()


#overlapping histograms with transparency
a=rnorm(1000, 3, 1)
b=rnorm(1000, 6, 1)
hist(a, xlim=c(0,10), col="red")
hist(b, add=T, col=rgb(0, 1, 0, 0.5) )

# Histogram Grey Color
hist(h1, col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,10), ylim=c(0,200), main="Overlapping Histogram")
hist(h2, col=rgb(0.8,0.8,0.8,0.5), add=T)
box()
