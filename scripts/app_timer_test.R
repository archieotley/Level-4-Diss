

E=0
Lambda=0.5   
V=30
C=2000

  Cs<-0.05*C
  
  Ts<-log((Cs-E/(lambda*V))/(C-E/(lambda*V)))/(-lambda)
  Ts
  
sprintf(Ts,Ts)
  

source('app_timer_test.R', echo=TRUE)
