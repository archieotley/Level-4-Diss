

#dose=0:1000,0.1
#k=410
#risk<-1-exp(-dose*(k))
 #          plot(dose,risk,
    #            main="Dose Response Curve",
      #          ylab="risk)",
    #            type="l",
      #          col="blue")
      
           
#plot.function(risk<-1-exp(-dose*(410)) ~ risk.lim=range(0,0.9))           
           
#curve(  1-exp(-y/410), from=1, to=1000, log="x", n=3000, xlab="xvalue", ylab="yvalue", 
 #       col="blue", lwd=2, main="Dose Response Curve"  )


equation1 = function(x){sin(x)+cos(x)}

curve(equation1, from=0, to=2*pi, n=300, xlab="X", ylab="Y", col="blue",lwd=2, 
      main="Plot of  Y = sin(X)+cos(X) ")

equation2 = function(x){0.5*sin(x)+cos(x)}

curve(equation2, from=0, to=2*pi, n=300, add=TRUE, xlab="X", ylab="Y", col="red",lwd=2, 
      main="Plot of  Y = sin(X)+cos(X) ")
