#import files
library(readxl)
ex1 <- read_excel("C:/Users/Downloads/examples.xlsx", sheet = "dist")
ex1

#EXP
evx <-  sum(ex1$X * ex1$prob2) ; evx
evy <-  sum(ex1$Y * ex1$prob2) ; evy

#VAR.P, STDEV.P
varp <-  sum((ex1$X-evx)^2*ex1$prob2)/length(ex1$X) ; varp
stdp <- sqrt(varp) ; stdp

#DEV
dex <- ex1$X - evx
dey <-ex1$Y - evy

#COV
coxy <- sum(dex*dey*ex1$prob2); coxy

#make the function
covf <-  function(x, y, p){
  
  evx <-  sum(x*p)
  evy <-  sum(y*p)
  
  dex <-  x - evx
  dey <-  y - evy
  
  sum (dex * dey * p)
}

covf(ex1$X, ex1$Y, ex1$prob2)

