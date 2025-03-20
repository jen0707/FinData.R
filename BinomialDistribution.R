## Binomial distribution 이항분포

# 확률
k <-  3 # 앞면이 3회
n <-  5 # 시행 횟수 5회
p <- 0.4 # 앞면이 나올 확률 

dbinom(k, n, p) # 이항분포함수(앞면이 3회 나올 확률)
pbinom(k, n, p) # 누적(앞면이 3회 이하로 나올 확률)

# Recursive Cumulative Function 재귀적 누적함수
cupr <- function(k, n, p) {
  if(k>0){
    cupr(k-1, n, p) + dbinom(k, n, p)
  }
  else if(k==0){
    dbinom(0, n, p)
  }
  else
    stop("k should be non-negative.")
}
cupr(k, n, p); pbinom(k, n, p)

#Plot 도표 
success <- 0:n 

pr  <- sapply(success, function(k) dbinom(k,n,p)) 
cpr <- sapply(success, function(k) pbinom(k,n,p))

plot(success, pr, type = 'h')

plot(success,cpr, type = 'l')

plot( success, pr, type = 'h', ylim = c(0,1)) 
lines(success,cpr, type = 'l')
