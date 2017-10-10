#teamEM<-function(data, epilson=1e-08, maxit=1000){
#data<-c(1,2,3,3,3,4,5,6,7,7,8,9)
#Initialisation:
#maxit<-50
likelihood_vector<-rep(0,times=maxit)
posterior<-rep(0,times=length(data)*3)
#(structure: for each xi: P(xi belongs in k=1), P(xi belongs in k=2),which k? (integer 1 or 2))
data_sorted<-sort(data,decreasing = FALSE)
#I´m gonna use the median - making the assumption that there data contains the same number
#of observations from the two distributions. The mean is not better though. Any opinions?
x<-length(data)
if(x%%2==0){
  even<-TRUE
  for(i in seq(3, x*3/2, by = 3)){
    posterior[i]<-1
  }
  for(j in seq(((x*3/2))+3,x*3, by=3 )){
    posterior[j]<-2
  }
  
}else{
  even<-FALSE
  for(i in seq(3,(x*3-3)/2, by = 3)){
    posterior[i]<-1
  }
  posterior[(((x*3)-3)/2)+3]<-sample(1:2,1)#I assign the middle value 1 or 2 at random.
  for(j in seq((3*x+9)/2,x*3, by=3)){
    posterior[j]<-2
  }#The posterior vector now has the value 1 or 2 at every third position.
}
print(posterior)
#Calculating means:
sum_mu1<-0
sum_mu2<-0

data1<-rep(0,times=sum(posterior==1))
data2<-rep(0,times=sum(posterior==2))
#Need this later to calculate sd`s.
for(i in(seq(3,x*3, by = 3))){
  if(posterior[i]==1){
    sum_mu1<-sum_mu1+data_sorted[i/3]
    data1[i/3]<-data_sorted[i/3]
  }else{
    sum_mu2<-sum_mu2+data_sorted[i/3]
    data2[i/3]<-data_sorted[i/3]
  }
}
data1<-data1[data1!=0]
data2<-data2[data2!=0]
mu1<-sum_mu1/sum(posterior==1)
mu2<-sum_mu2/sum(posterior==2)
#calculate sd0`s

sd1<-sd(data1)
sd2<-sd(data2)

lamda1<-length(data1)/x#Did I understand the formula correctly?
lamda2<-length(data2)/x
count<-0
converged<-FALSE


#while(converged==FALSE){
#adding conditional probabilities to posterior
#for(j in seq(1,length(posterior), step=3)){
for(i in seq(1,length(data_sorted), by=1)){
  Pk1<-sum(posterior==1)/x
  Pk2<-sum(posterior==2)/x
  Pxi<-dnorm(i,mu1,sd1)*Pk1+dnorm(i,mu2,sd2)*Pk2
  cond_prob1<-(dnorm(i,mu1,sd1)*Pk1)/Pxi
  cond_prob2<-(dnorm(i,mu2,sd2)*Pk2)/Pxi
  posterior[3*i-2]<-cond_prob1
  posterior[3*i-1]<-cond_prob2
  if(cond_prob1>cond_prob2){
    posterior[3*i]<-1
  }else if(cond_prob1==cond_prob2){
    posterior[3*i]<-sample(1:2,1)
  }else{
    posterior[3*i]<-2
  }
  
}
#Up to here all debugged

sum1<-0
sum2<-0
SUM1<-0
SUM2<-0
for(i in seq(1,length(posterior),by=3)){
  if(posterior[i+2]==1){
    sum1<-sum1+posterior[i]*data_sorted[(i+2)/3]
    SUM1<-SUM1+posterior[i]
  }else{
    sum2<-sum2+posterior[i+1]*data_sorted[(i+2)/3]
    SUM1<-SUM1+posterior[i+1]
  }
}
mu1<-sum1/SUM1
mu2<-sum2/SUM2

#Now calculating sigma
Sum1<-0
Sum2<-0
for(i in seq(1,length(posterior),by=3)){
  if(posterior[i+2]==1){
    Sum1<-Sum1+posterior[i]*((data_sorted[(i+2)/3]-mu1)**2)
  }else{
    Sum2<-Sum1+posterior[i+1]*((data_sorted[(i+2)/3]-mu2)**2)
  }
}
sd1<-sqrt(Sum1/sum1)
sd2<-sqrt(Sum2/sum2)

#Calculate lamda
lamda1<-(1/x)*sum1
lamda2<-(1/x)*sum2

