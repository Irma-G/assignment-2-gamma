#teamEM<-function(data, episilon=1e-08, maxit=1000){
data<-c(1,2,3,3,4,5,6,7,7,8,9)

#UPDATE NOTES Sat 14th Oct
# - changed 'posterior' from vector to matrix, updated calls to that object in code
#   -> also added code at very end of file to remove third column before returning object
# - made named list 'estimates', changed calls to mu, sigma, lambda in code:
#   -> updated calls in Ellen's code, and in assignment of inits object
#   -> haven't touched Maki's calls to mu, sigma, lambda
# - added a couple of TODOs at start second block Maki's code:
#   -> extract mu/sigma/lambda from estimates object at start?
#   -> need check order operations in overall teamEM structure
#
# Hope this is clear, msg me with any questions - Irma.

#Initialisation:
data.n <-length(data)
#maxit<-50
likelihood<-rep(0,times=maxit)
#named list estimates:
estimates <- list(mu=c(0,0),sigma=c(0,0),lambda=c(0,0))
#TODO - make calls to estimates vector!
posterior <- matrix(nrow=data.n,ncol=3)
#assigning column names
for (q in 1:3){ 
  #avoids col name assignment casting matrix entries to 'character'
  posterior[1,q] <- 0
}
#col1 = P(xi in k=1), col2 = P(xi in k=2), col3 = current k assigned
colnames(posterior) <- c("prob_k1", "prob_k2", "is_k")

#Assign initial values of k
#Using the median - assumption that the data contains the same number
#of observations from the two distributions.
data_sorted<-sort(data,decreasing = FALSE)

if(data.n%%2==0){
  for (i in 1:data.n){
    if (i <= data.n/2){ #above median
      posterior[i,"is_k"] <- 1
    }else{ #below median
      posterior[i,"is_k"] <- 2
    }
  }
}else{
  mid <- as.integer(data.n/2)
  for(i in 1:mid){ #above median
    posterior[i,"is_k"]<-1
  }
  posterior[mid+1,"is_k"]<-sample(1:2,1)#I assign the middle value 1 or 2 at random.
  for(j in mid+1:data.n){ #below median
    posterior[j,"is_k"]<-2
  }#The posterior vector now has the value 1 or 2 at every third position.
}
print(posterior)

#Calculating means:
sum_mu1<-0
sum_mu2<-0

data1<-rep(0,times=sum(posterior[,"is_k"]==1))
data2<-rep(0,times=sum(posterior[,"is_k"]==2))
#Need this later to calculate sd`s.
for(i in 1:data.n){
  if(posterior[i,"is_k"]==1){ 
    sum_mu1<-sum_mu1+data_sorted[i]
    data1[i]<-data_sorted[i]
  }else{
    sum_mu2<-sum_mu2+data_sorted[i]
    data2[i]<-data_sorted[i]
  }
}
data1<-data1[data1!=0]
data2<-data2[data2!=0]
estimates$mu[1]<-sum_mu1/sum(posterior[,"is_k"]==1)
estimates$mu[2]<-sum_mu2/sum(posterior[,"is_k"]==2)
#calculate sd0`s

estimates$sigma[1]<-sd(data1)
estimates$sigma[2]<-sd(data2)

#Initial Lambda
estimates$lambda[1]<-length(data1)/data.n
#Did I understand the formula correctly? -Ellen TODO
estimates$lambda[2]<-length(data2)/data.n
count<-1
converged<-FALSE

##------Maki--------##
#Create inits: list of initial values used for mu, sigma and lambda  

#estimates: list of mu, sigma (standard deviation, not variance), and lambda)
### I'm not sure why we need estimates...

if(count == 1){ 
  inits <- estimates
}
##----end Maki-----##

#while(converged==FALSE){
#adding conditional probabilities to posterior
#for(j in seq(1,length(posterior), step=3)){
for(i in 1:data.n){
  Pk1<-sum(posterior[,"is_k"]==1)/data.n
  Pk2<-sum(posterior[,"is_k"]==2)/data.n
  Pxi<-dnorm(i,estimates$mu[1],estimates$sigma[1])*Pk1+dnorm(i,estimates$mu[2],estimates$sigma[2])*Pk2
  cond_prob1<-(dnorm(i,estimates$mu[1],estimates$sigma[1])*Pk1)/Pxi
  cond_prob2<-(dnorm(i,estimates$mu[2],estimates$sigma[2])*Pk2)/Pxi
  posterior[i,"prob_k1"]<-cond_prob1
  posterior[i,"prob_k2"]<-cond_prob2
  if(cond_prob1>cond_prob2){
    posterior[i,"is_k"]<-1
  }else if(cond_prob1==cond_prob2){
    posterior[i,"is_k"]<-sample(1:2,1) 
    #note: source of extra variation, definitely set a seed when testing teamEM()
  }else{
    posterior[i,"is_k"]<-2
  }
  
}
#Up to here all debugged

sum1<-0
sum2<-0
SUM1<-0
SUM2<-0
for(i in 1:data.n){
  #TODO - should posterior P(x in k=2) also be updated? :o
  #or should it be preserved... ?
  if(posterior[i,"is_k"]==1){
    sum1<-sum1+posterior[i,"prob_k1"]*data_sorted[i]
    SUM1<-SUM1+posterior[i,"prob_k1"]
  }else{
    sum2<-sum2+posterior[i,"prob_k2"]*data_sorted[i]
    SUM1<-SUM1+posterior[i,"prob_k2"]
  }
}
estimates$mu[1]<-sum1/SUM1
estimates$mu[2]<-sum2/SUM2

#Now calculating sigma
Sum1<-0
Sum2<-0
for(i in 1:data.n){
  if(posterior[i,"is_k"]==1){
    Sum1<-Sum1+posterior[i,"prob_k1"]*((data_sorted[i]-estimates$mu[1])**2)
  }else{
    Sum2<-Sum1+posterior[i,"prob_k2"]*((data_sorted[i]-estimates$mu[2])**2)
  }
}
estimates$sigma[1]<-sqrt(Sum1/sum1)
estimates$sigma[2]<-sqrt(Sum2/sum2)

#Calculate lambda
estimates$lambda[1]<-(1/data.n)*sum1
estimates$lambda[2]<-(1/data.n)*sum2

##------Maki2-------##
#Calculating likelihood:

# Function to calculate log of likelihood and return scalar
# change to use estimate_vector

mu <- c(mu1,mu2)
sigma <- c(sd1,sd2)
lambda <- c(lambda1,lambda2)
TODO - can use estimates named list
TODO - if first iteration -> make sure use inits (with initial lambda) instead!
  check order operations

likelihoodF <- function(x,mu,sigma,lambda){
  sum(log(lambda[1]*dnorm(x,mu[1],sigma[1]) + lambda[2]*dnorm(x,mu[2],sigma[2])))
}

# Update (log)likelihood vector with the scalar
likelihood[count] <- likelihoodF(x, mu, sigma, lambda)

# Stop-function rule (update converged)
###can the break stop the while loop??
###need return??

if(abs(likelihood[count] - likelihood[count-1]) < episilon){
  converged <- TRUE
}else{
  converged <- FALSE
  count <- count + 1
  if (count > maxit) break
}

##------End Maki2-------##

#remove third column before returning posterior matrix
posterior <- posterior[,c("prob_k1","prob_k2")] 
