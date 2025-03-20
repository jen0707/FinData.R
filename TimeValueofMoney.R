# Time Value of Money

r <- 0.1 # 이자율
P <- 100 # 원금
n <- 2 # 기간 수

n
r
P
# multi line exec
n; r; P

# vector
c(n,r,P)

# 원리합계
P*(1+r) 
# 복리
P*(1+r)^n 
# 단리
P*(1+r*n)
# 복리와 단리의 차이
P*(1+r)^n - P*(1+r*n)

x <- "Alt + - "
abcdefg <- 5

abc <-  function(x,y){
  z <-  x + y
  print(z^4)
}

abc(3,6)

# 복리 미래가치 함수
# P=1 은 default값. 값이 두 개 주어지면 P=1로 계산됨.
fvn <- function(n,r,P=1){
    P*(1+r)^n
} 

fvn(3,0.2,100)

# 단리 미래가치 함수
fvns <- function(n,r,P=1){
    P*(1+r*n)
} 

fvns(3,0.2,100)
fvns(3,0.2)*100

# 연속된 값 처리
1:30
7:4

(1:5)*2 -5 

n <-  1:4

2^n

li <- 0:30
rr <- 0.05

# 반복적인 함수 실행 
# li를 function의 x에 대입
sapply(li, function(x){fvn(x,rr)})

ff <- sapply(li, function(x){fvn(x,rr)})
ff

gg <- sapply(li, function(x){fvns(x,rr)})

# 복리와 단리의 차이
ff - gg


plot(ff)
plot(ff, type = 'l')

plot(gg)
plot(gg, type = 'l', col = 'red')

# 겹쳐 그리기
plot(ff, type = 'l', col = 'black')
lines(gg, col="blue",lty=2)
lines(ff-gg, col = 'red')

# 정리 ################################ 

mtitle <- paste('미래가치 : 이자율 = ',
                as.character(rr*100), '%')

plot(x = li, 
     y = ff, 
     type = 'b',
     pch = 18,
     col = 'black', 
     ylim = c(0, max(ff)*1.1 ),
     main = mtitle, 
     xlab = 'year', 
     ylab = 'value')

lines(x=li, y=gg, col="blue", pch=19, type='b')

lines(x=li, y=ff-gg, col='red', type='l', lty=2, lwd=3)

legend(max(li)*0.05,max(ff),
       title = '구분',
       legend=c("복리","단리","차이"),
       col=c("black","blue","red"), 
       lty=c(1,1,2), pch = c(18,19,0),
       border="white",box.lty=0, 
       cex=1.2, text.font=4, bg='lightyellow')

# 참고 : 복잡한 그래프 그리기는 ggplot2 패키지 

# 현재가치도 계산 및 그래프 그려보기
