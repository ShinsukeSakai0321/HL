PeakCalc <- function(Wave){
  n <- length(Wave)
  m <- n
  wn <- numeric(n)
  wn<-Wave
  #skip same value in Wave
  j<- 1
  wn[j] <- Wave[1]
  for(i in 2:n){
    if(Wave[i]!=wn[j]){
      j <- j+1
      wn[j]<-Wave[i]
    }
  }
  pk<-NULL
  num <- j
  aa=data.frame(flag=FALSE,time=0,value=0)
  for(i in 1:(num-2)){
      aa <- peak(wn[i],wn[i+1],wn[i+2])
      if(aa$flag){
        pk<-rbind(pk,c(aa$value))
      }
  }
  pk
}
peak<-function(y0,y1,y2){
  aa=data.frame(flag=FALSE,time=0,value=0)
  pflag <- (y2-y1)*(y1-y0)
  if(pflag<=0 ){
    a <- (y0+y2-2*y1)/2.0
    b <- -(y0-y2)/2.0
    c <- y1
    time <- -b/2/a
    value <- c-b*b/4.0/a
    aa=data.frame(flag=TRUE,time=time,value=value)
  }
  return(aa)
}
