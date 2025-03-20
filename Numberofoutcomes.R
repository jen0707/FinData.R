## number of outcomes 경우의 수

# Sampling with replacement 복원 추출
n <- 2; k <- 5; n^k
n <- 6; k <- 3; n^k
# Sampling without replacement 비복원 추출
n <- 3; factorial(n)
n <- 10; factorial(n)
# Permutation 순열
n <- 5; k <- 3; factorial(n)/factorial(n-k)
# Combination 조합
n <- 5; k <- 3; factorial(n)/(factorial(k)*factorial(n-k))
n <- 5; k <- 2; factorial(n)/(factorial(k)*factorial(n-k))
# R명령어
n <- 5; k <- 3;
choose(n,k) # 조합
choose(n,k)*factorial(k) # 순열: 조합 줄 세우기
# Recursive function: 계승 재귀적 함수
factr <-  function(n){
  if(n>1){
    n*factr(n-1)}
  else if(n==1)
    1
  else
    stop('Error!')
}
n <- 5
factr(n); factorial(n) # 비교

