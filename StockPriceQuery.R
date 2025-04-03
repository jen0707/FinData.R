library(xts)
library(quantmod)

#삼성전자(samsung electronics) 주가자료 불러오기
getSymbols('005930.KS',
           from = '2019-01-01',
           to   = '2023-05-01')

#이름 붙여주기 (숫자로 변수명을 설정하는 경우 불편함) 
#주의 -> 숫자 변수명 출력 시 `` sign을 이용하여 error 방지
sse <- getSymbols('005930.KS',
  from = '2019-01-01',
  to   = '2023-05-01',
  auto.assign = F
  )
#auto.assign = F <- 이름이 자동 지정되어 중복되는 현상 방지 

#열 이름 변경 (data set 안의 이름임을 유의)
#c() = combine 7개의 자료를 묶어 vector 취급 
colnames(sse) <- c( 'open', 'high', 'low', 'close', 'volume', 'adjusted' )
## high: 고가(당일 가장 높았던 가격) / low: 저가(당일 가장 낮은 가격)
## open: 시가(시작 가격) / close: 종가(일별 주가 자료 = 종가 기준)
## volume: 거래량 / adjusted: 수정 종가(수정 가격 <- 분할 등 요인)

head(sse,5) # 시작일 확인(5개) 
tail(sse,5) # 종료일 확인(5개)

#배당 자료 불러오기 (패키지에서 불러오기 때문에 기존이름 사용)
div.sse <- getDividends(
  '005930.KS',  from='2019-01-01',
  ) 
div.sse

#(종가 - 수정가)
zzz <- sse$close - sse$adjusted
zzz

#수정종가 설명: https://psystat.tistory.com/158 참고 

####################################################

#monthly data 불러오기
sse_m <- getSymbols('005930.KS',
                    from = '2019-01-01',
                    to   = '2023-05-01',
                    auto.assign = F,
                    periodicity = 'monthly') #월별로 지정(default: daily)

#열 변수 이름 변경 
colnames(sse_m) <- c(
  'open', 'high', 'low', 'close', 'volume', 'adjusted')

head(sse_m) #시작점 확인 
tail(sse_m) #종점 확인 

#candle chart 출력 (4가지 정보를 표현: 시가, 종가, 고가, 저가) 
chart_Series(sse_m['2019-01/2023-05'])

#ROC() = 수익률 반환 함수(return rate function)
sse_m$rtn <- ROC(sse_m$adjusted)

#수정종가의 기댓값(평균 <- 확률이 존재하지 않기 때문)
mean(sse_m$adjusted) 

#수익률의 평균 
mean(sse_m$rtn, na.rm=T) #NA(Not Available) 값 제외하기
#약 10.75%

var(sse_m$adjusted) #분산 
sqrt(var(sse_m$adjusted)) #표준편차 

######################################################

#코스피 주가지수(^) 자료 불러오기
kospi_m <- getSymbols('^KS11',
                    from = '2019-01-01',
                    to   = '2023-05-01',
                    auto.assign = F,
                    periodicity = 'monthly')
colnames(kospi_m) <- c(
  'open', 'high', 'low', 'close', 'volume', 'adjusted'
)
head(kospi_m)
tail(kospi_m)

chart_Series(kospi_m['2019-1/2023-05'])

kospi_m$rtn <- ROC(kospi_m$adjusted)

mean(kospi_m$adjusted)
mean(kospi_m$rtn, na.rm=T)


var(kospi_m$adjusted)
