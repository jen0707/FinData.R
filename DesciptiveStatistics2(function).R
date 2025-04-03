#import files
library(readxl)

ex1 <- read_excel("examples.xlsx", sheet = "dist")
ex1

#set parameter
x  <- ex1$X
y  <- ex1$Y
p  <- ex1$prob2

#prob function
#NULL을 통해 입력 있어도 되고 없어도 되도록 set
pp <- function(x,p=NULL){
  if (is.null(p)) {
    pp <- rep(1/length(x), length(x)) #미입력시 동등확률 
  }
  else {pp <- p}
}

#EXP function
evf <- function(x,p=NULL){
  
  pp <- pp(x,p) #pp(변수이름) <- pp(x,p)(함수)
  sum(x*pp)
}

evf(x); evf(x,p) #동등확률 기댓값; prob2 기댓값 
evf(y); evf(y,p)  

#COV function
#samp = 표본 공분산 계산 여부 parameter
covf <- function(x,y,p=NULL,samp=F){
  
  pp <- pp(x,p)
  
  evx <- evf(x,pp)
  evy <- evf(y,pp)
  
  dex <- x - evx
  dey <- y - evy
  
  cov <- sum(dex*dey*pp)
  
  if (samp==T) {
    cov*length(x)/(length(x)-1) #표본인 경우 n-1 로 나눔
  } else {
    cov
  }
}
#VAR.P = 자기 자신과의 COV
covf(x,x); covf(x,x,samp=T); covf(x,x,p) #분산; samp=T; prob2
covf(y,y); covf(y,y,samp=T); covf(y,y,p)
covf(x,y); covf(x,y,samp=T); covf(x,y,p)

#CORR.COEF
corr1 <- covf(x,y)  /sqrt(covf(x,x)  *covf(y,y));   corr1
corr2 <- covf(x,y,p)/sqrt(covf(x,x,p)*covf(y,y,p)); corr2