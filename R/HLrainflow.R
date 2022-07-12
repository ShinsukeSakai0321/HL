HLrainflow<-function(Peak){
  peak_num <- length(Peak)
  p <- numeric(peak_num)
  j<-0
  res<-NULL
  for(i in 1:peak_num){
    j <- j+1
    pk1<- Peak[i]
    p[j]<-pk1
    while(j>3){ #2->3 2022.7.5
      r0 <- abs(p[j-2]-p[j-3]) #added 2022.7.5
      r1 <- abs(p[j-1]-p[j-2])
      r2 <- abs(p[j]-p[j-1])

      if(r1>r2)break
      if(r1>r0)break #added 2022.7.5
      r<-r1
      m<-(p[j-1]+p[j-2])/2
      res <- rbind(res,c(r,m))
      res <- rbind(res,c(r,m))
      j<-j-2
      p[j]<-pk1
    }
  }
  for(i in 1:(j-1)){
    r <- abs(p[i+1]-p[i])
    m <- (p[i+1]+p[i])/2
    res <- rbind(res,c(r,m))
  }
  colnames(res)<-c("halfR","halfM")
  as.data.frame(res)
}
