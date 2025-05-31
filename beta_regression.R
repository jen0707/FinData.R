#################################
# 주식 가격, 수익률, 그리고 베타
#################################

library(quantmod)

start_date <- '2020-01-01'
end_date   <- '2023-12-31'

######################
# 1. 주식 하나씩 처리
######################

getSymbols('AAPL',from=start_date,to=end_date)
 # [1] "AAPL"
head(AAPL)
head(Op(AAPL)) # Open 시가 
head(Cl(AAPL)) # Close 종가 
head(Hi(AAPL)) # High 고가 
head(Lo(AAPL)) # Low 저가 
head(Ad(AAPL)) # Adjusted 수정종가 

# 월별 수익률: monthlyReturn
# leading: 수익률 계산 구조상 첫 줄은 무조건 NA 
# leading=T 하면 첫 줄 표시 안함 
head(monthlyReturn(AAPL,leading=F))

# 어떤 가격을 기준으로 계산할 지 지정
# default = Adjust
head(monthlyReturn(AAPL$AAPL.Close,leading=F))

# type = 'log' : 로그수익률 -> 장기 시계열일 수록 중요함
# 그냥 수익률의 경우 복리효과를 인지하기 어려워 혼동을 야기할 수 있음 
head(monthlyReturn(AAPL,leading=F,type ='log'))

AAPL['2020-12-01::2020-12-07'] # 슬라이스 ::
AAPL$AAPL.Adjusted['2020-12-01::2020-12-07']
Ad(AAPL)['2020-12-01::2020-12-07'] # 위와 완전히 같은 식 
# 결과는 같으나, 컴퓨터의 계산 처리 속도가 다를 수 있으므로 순서에 유의하면 좋다 
AAPL['2020-12-01::2020-12-07',6] # 6열만 뽑아내라
AAPL['2020-12-01::2020-12-07',c(4,6)] # 4열, 6열을 뽑아라 
# C( , )로 하나의 묶음으로 만들어줌 

# 시각화 
chartSeries(AAPL,subset='2020-12::2021-01',theme='white') # theme = 배경색 지정 
chartSeries(AAPL['2020-12::2021-01',],type='bars',TA=NULL) # type = 그래프 형식 지정 
# TA = 거래량 표시 여부 

chartSeries(AAPL$AAPL.Adjusted,subset='2020-12::2021-01')
chartSeries(Ad(AAPL)['2020-12::2021-01'])

######################
# 2. 주식 자료명 변경
######################
# auto.assign=F 이름 자동 지정 여부 
aap <- getSymbols('AAPL',from=start_date,to=end_date,auto.assign=F)

aap['2020-12-01::2020-12-07',c(4,6)]

chartSeries(aap['2020-12::2021-01',],type='bars',TA=NULL)

#########################
# 3. 여러 주식 베타 계산
#########################
# getsymbols 는 컴퓨터 안에서 일어나는 일이 아니라,
# 인터넷을 이용하여 컴퓨터 밖에서 데이터를 받아오는 것이므로
# 사용할 때 주의할 것 
getSymbols(c('AAPL','TSLA','^GSPC'), 
           from = start_date, 
           to   = end_date)
# 동시 처리시애는 무조건 auto.assign 됨
# c(, )로 묶어 벡터 처리할 것 -> 여러 개를 하나로 취급 
# [1] "AAPL"  "TSLA"  "^GSPC"
# 주가 지수 자료는 ^을 붙임 (다른 주가들과 차이를 주기 위함)
# 불러오면 ^ 없어지기 때문에 주의할 것 (R 문법상: 변수명 문제)

# lapply( , ): 뒤에 적어놓은 함수를 list에 적용시켜 결과 저장 
dat1 <- lapply(list(AAPL,TSLA,GSPC), Ad) # 수정종가 
ret1 <- lapply(dat1, monthlyReturn) # 월별 수익률
# 위의 두 가지를 한 번에 시행할 수도 있다(객체지향: 함수 안에 함수를 넣을 수 있음)
x1   <- do.call(data.frame, ret1) # 데이터프레임 형식
# lapply는 list임, R은 주로 df를 씀 -> 바꿔줘야만: do.call 

names(x1) <- c('mret.AAPL','mret.TSLA','mret.GSPC') # 변수명 변경

# lm: linear Regression
# 변수 지정: Y~X
# <- 으로 결과 저장 
res1 <- lm(data = x1, mret.AAPL ~ mret.GSPC); res1 
res2 <- lm(data = x1, mret.TSLA ~ mret.GSPC); res2
 # 상수값(intercept), 기울기
 # beta(기울기가 크다)는 것은 시장으로부터 훨씬 영향을 많이 받는다는 뜻 

# summary: 더 자세한 값(잔차, 계수값, 표준오차, t-value 까지 표시 )
# t-value가 크면, 변동성에 비해서 추정되는 영향력의 크기가 크다는 것을 의미 
# t값이 클 수록, p 값이 작을 수록 좋음 
# Signif.codes (의미 표시) 참고 (보통 *부터 유의하다고 인정) 
summary(res1)

coef1 <- summary(res1)$coefficients; coef1
coef2 <- summary(res2)$coefficients; coef2

# 회귀분석 beta = xy공분산/x분산 n= 기울기 
cov(x1)

beta1 <- cov(x1)[1,3]/cov(x1)[3,3]; beta1
beta2 <- cov(x1)[2,3]/cov(x1)[3,3]; beta2

c(beta1, coef1[2,1]) 
c(beta2, coef2[2,1])

# 회귀분석 그래프
## fitted plot
# 점이 data, line이 회귀분석 그래프 
plot(x1$mret.GSPC,x1$mret.AAPL, col='blue', 
     abline(res1, col='red'))

## residual plot 
# 잔차 표시 
plot(fitted(res1),resid(res1),abline(0,0))

#################################
# 4. 자료 보존 및 엑셀 파일 변환 
#################################

# (1) RData(rda) 파일로 저장하기
## getSymbols 함수는 인터넷 연결해서 자료를 불러옴
## 한번 불러온 자료를 R 자료 형식으로 파일 저장했다가
## 다음 코딩 수정보완시 바로 활용하면 편리함
## 또는 여러 전단계 조작을 거쳐 어렵게 만든 자료 역시
## 이후에는 전단계 조작 없이 바로 데이터를 활용할 수 있음

save(dat1, file='beta_regression_dat1_AAPL_TSLA_GSPC.rda')
# 나중에 불러 올 때는 아래 load() 함수 이용
# load('beta_regression_dat1_AAPL_TSLA_GSPC.rda')

# (2) 엑셀 파일이나 텍스트파일로 저장하기
## 다른 프로그램에서 활용하기 편리함

write.csv(x1, file = 'Adjusted_AAPL_TSLA_GSPC.csv')
# 나중에 불러 올 때는 아래 read.csv() 함수 이용
# x1 <- read.csv('Adjusted_AAPL_TSLA_GSPC.csv')

# The End

