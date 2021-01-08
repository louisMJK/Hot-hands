library(tidyverse)
library(mosaic)
library(foreach)


p = 0.5684932
trial = c()
m = 1

n = 20
for (s in 1:10){
  m = s
  coin_vals = do(100000)*{
    coin = c(1,0)
    coins = sample(coin, size=n, replace=TRUE, prob=c(p,1-p))
    K = c()
    k = 1
    i = 1
    cnt = 0
    while (i <= n){
      if (coins[i]==0){
        i = i + 1
        cnt = 0
      }
      else{
        while(i<=n && coins[i]==1){
          cnt = cnt + 1
          if (i<n && coins[i+1]==0){
            K[k] = cnt
            k = k + 1
          }
          i = i + 1
        }
      }
    }
    k = k - 1
    
    ALL = 0
    HIT = 0
    if (k>1){
      for (j in 1:k){
        if(K[j] >= m){
          ALL = ALL + K[j] - m + 1
          HIT = HIT + K[j] - m
        }
      }
    }
    c(ALL,HIT)
  }
  sum = colSums(coin_vals)
  trial[s] = sum[2]/sum[1]
}
