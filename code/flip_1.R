library(tidyverse)
library(mosaic)
library(foreach)

p = 0.4356
trial = c()
N = 50000

n = 15
for (m in 1:6){
  coin_vals = do(N)*{
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
        flag = 0
      }
      else{
        while(i<=n && coins[i]==1){
          cnt = cnt + 1
          if ((i<n && coins[i+1]==0) || i==n){
            K[k] = cnt
            k = k + 1
          }
          if(i==n){
            flag=1
            i = i + 1
          }
          i = i + 1
        }
      }
    }
    
    k = k - 1
    
    ALL = 0
    HIT = 0

    if (k>0){
      for (j in 1:k){
        if(K[j] >= m){
          ALL = ALL + K[j] - m + 1
          if(flag==1 && j==k){
            ALL = ALL - 1
          }
          HIT = HIT + K[j] - m
        }
      }
    }
    if(ALL != 0){
      a = HIT/ALL
      a
    }
  }
  trial[m] = colMeans(coin_vals)
}
trial


df = data.frame(trial=trial)
#write.csv(df,'../trial_3.csv')
