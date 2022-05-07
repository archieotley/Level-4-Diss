

E=runif(min = 0,max = 20,10)
lambda=runif(min = 0,max = 2,10)
V=30
C=runif(min = 0,max = 200000,10)

  Cs<-0.02*C
  
 time_safe=function(Cs,C,E,lambda,V) {
  Ts<-log((Cs-E/(lambda*V))/(C-E/(lambda*V)))/(-lambda)
  return(Ts)
 }  
 
 p<-c()
 
for (i in 1:10) {
  p[i]<-time_safe(Cs,C,E[i],lambda,V)
  print(p)
}
 #time_safe(Cs,C,E,lambda,V)

 p<-log((Cs-E/(lambda*V))/(C-E/(lambda*V)))/(-lambda)
boxplot(p) 

