
teamEM<-function(data, epilson=1e-08, maxit=1000){
  #Initialisation:
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
        posterior[((x-3*3)/2)+3]<-sample(1:2,1)#I assign the middle value 1 or 2 at random.
        for(i in seq(((x*3-3)/2)+6,x*3)){
          posterior[i]<-2
        }#The posterior vector now has the value 1 or 2 at every third position.
      }
  #Calculating means:
  sum_mu1<-0
  sum_mu2<-0
  
  for(i in(seq(3,x*3, by = 3))){
    for(j in seq(1,x,by=1))
    if(posterior[i]==1){
      sum_mu1<-sum_mu1+data_sorted[j]
    }else{
      sum_mu2<-sum_mu2+data_sorted[j]
    }
  }
  mu1<-sum_mu1/sum(posterior==1)
  mu2<-sum_mu2/sum(posterior==2)
  #calculate sd0`s
  if(even==FALSE){
    data1<-data_sorted[1:x/2]#get list of all data from 1
    data2<-data.sorted[x/2+1,x]
  }else{
    if(posterior[(((x*3)-3)/2)+3]==1){
      data1<-data_sorted[1:((x-1)/2)+1]
      data2<-data_sorted[((x-1)/2)+2:x]
    }else{
        data1<-data_sorted[1:((x-1)/2)]
        data2<-data_sorted[((x-1)/2)+1:x]
    }
   
  }
  sd1<-sd(data10)
  sd2<-sd(data20)  
  lamda1<-length(data10)/x#Did I understand the formula correctly?
  lamda2<-length(data20)/x
  count<-0
  converged<-FALSE
  
  while(converged==FALSE){
    #adding conditional probabilities to posterior
    for(j in seq(1,length(posterior), step=3)){
      for(i in seq(1,data_sorted, by=1)){
        Pk1<-sum(posterior==1)/x
        Pk2<-sum(posterior==2)/x
        Pxi<-dnorm(i,mu1,sd1)*Pk1+dnorm(i,mu2,sd2)*Pk2
        cond_prob1<-(dnorm(i,mu1,sd1)*Pk1)/Pxi
        cond_prob2<-(dnorm(i,mu2,sd2)*Pk2)/Pxi
        posterior[j]<-cond_prob1
        posterior[j+1]<-cond_prob2
        if(cond_prob1>cond_prob2){
          posterior[i+2]<-1
        }else if(cond_prob1==cond_prob2){
          posterior[i+2]<-sample(1:2,1)
        }else{
          posterior[i+2]<-2
        }
          
      }
    }
    #Maximization
    sum1<-0
    sum2<-0
    SUM1<-0
    SUM2<-0
    for(i in seq(1,length(posterior),by=3)){
      for(j in seq(1,x,by=1)){
      if(posterior[i+2]==1){
        sum1<-sum1+posterior[i]*data_sorted[j]
        SUM1<-SUM1+posterior[i]
      }else{
        sum2<-sum2+posterior[i+1]*data_sorted[j]
        SUM1<-SUM1+posterior[i+1]
      }
    }}
    mu1<-sum1/SUM1
    mu2<-sum2/SUM2
    
    #Now calculating sigma
    Sum1<-0
    Sum2<-0
    for(i in seq(1,length(posterior),by=3)){
      for(k in seq(1,x, by=1)){
      if(posterior[i+2]==1){
        Sum1<-Sum1+posterior[i]*((data_sorted[k]-mu1)**2)
      }else{
        Sum2<-Sum1+posterior[i+1]*((data_sorted[k]-mu2)**2)
      }
      }
      sd1<-sqrt(Sum1/sum1)
      sd2<-sqrt(Sum2/sum2)
      
      #Calculate lamda
      lamda1<-(1/x)*sum1
      lamda2<-(1/x)*sum2
    }
      
    
  }

}