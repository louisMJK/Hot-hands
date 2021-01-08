library(tidyverse)
library(mosaic)
library(foreach)
library(corrplot)

# load data
path = "E:/repo/Hot-hands/result/FG_K_13-14.csv"
df = read.csv(path)
path = "E:/repo/Hot-hands/result/FG_13-14.csv"
df_all = read.csv(path)

# Curry 201939  
# KT    202691  
# James harden 201935  
# LBJ 2544  
# KD   

# plot LBJ
df_LBJ = subset(df, PLAYER_ID==2544)
df_all_LBJ = subset(df_all, PLAYER_ID==2544)
FG = df_LBJ[2:8]
FG = c(0.45744,0.44366,0.43974,0.433387,0.42616,0.41618)

df_LBJ = data.frame(K=c(1,2,3,4,5,6), FG=FG, line=trial[1:6]*100)

ggplot(df_LBJ, aes(x=K, y=FG*100)) +
  geom_col(fill='#46ACC9', color='black', width=0.7) +
  scale_x_continuous(labels=df_LBJ$K, breaks=df_LBJ$K) +
  scale_y_continuous(limits=c(0,80), breaks= seq(0,100,by=10)) +
  geom_hline(yintercept=43.56, color='blue', size=1) +
  xlab('Hit Streak') + ylab('Percentage (%)') +
  geom_smooth(aes(x=K,y=line), size=1, span=0.7, color='red') +
  theme(axis.text.x=element_text(size=15,color='black'),
        axis.text.y=element_text(size=15,color='black'),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.title=element_text(size=25)) +
  ggtitle('League Average')


# plot curry
df_SC = subset(df, PLAYER_ID==201939)
df_all_SC = subset(df_all, PLAYER_ID==201939)
FG = c(0.5948276,0.5539216,0.4767442,0.4375,0.4166667,0.75,1,1,0)

df_SC = data.frame(K=c(1,2,3,4,5,6,7,8,9), FG=FG, line=trial[1:9]*100)

ggplot(df_SC, aes(x=K, y=FG*100)) +
  geom_col(fill='#46ACC9', color='black', width=0.7) +
  scale_x_continuous(labels=df_SC$K, breaks=df_SC$K) +
  scale_y_continuous(limits=c(0,100), breaks= seq(0,100,by=10)) +
  geom_hline(yintercept=38.12405, color='blue', size=1) +
  xlab('Hit Streak') + ylab('Percentage (%)') +
  geom_smooth(aes(x=K,y=line), size=1, span=1, color='red') +
  theme(axis.text.x=element_text(size=15,color='black'),
        axis.text.y=element_text(size=15,color='black'),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.title=element_text(size=25)) +
  ggtitle('Stephen Curry')


# plot KD
df_KB = subset(df, PLAYER_ID==202694)
df_all_KB = subset(df_all, PLAYER_ID==202694)
FG = c(0.5857143,0.5164835,0.4166667,0.3333333,0.3333333,0)

df_KB = data.frame(K=c(1,2,3,4,5,6), FG=FG, line=trial[1:6]*100)

ggplot(df_KB, aes(x=K, y=FG*100)) +
  geom_col(fill='#46ACC9', color='black', width=0.7) +
  scale_x_continuous(labels=df_KB$K, breaks=df_KB$K) +
  scale_y_continuous(limits=c(0,100), breaks= seq(0,100,by=10)) +
  geom_hline(yintercept=48.62932, color='blue', size=1) +
  xlab('Hit Streak') + ylab('Percentage (%)') +
  geom_smooth(aes(x=K,y=line), size=1, span=1, color='red') +
  theme(axis.text.x=element_text(size=15,color='black'),
        axis.text.y=element_text(size=15,color='black'),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.title=element_text(size=25)) +
  ggtitle('Marcus Morris')

# plot KT
df_KT = subset(df, PLAYER_ID==203095)
df_all_KT = subset(df_all, PLAYER_ID==203095)
FG = c(0.5389222,0.4705882,0.3846154,0.5714286,0.3333333,0)

df_KT = data.frame(K=c(1,2,3,4,5,6), FG=FG, line=trial[1:6]*100)

ggplot(df_KT, aes(x=K, y=FG*100)) +
  geom_col(fill='#46ACC9', color='black', width=0.7) +
  scale_x_continuous(labels=df_KT$K, breaks=df_KT$K) +
  scale_y_continuous(limits=c(0,100), breaks= seq(0,100,by=10)) +
  geom_hline(yintercept=42.37288, color='blue', size=1) +
  xlab('Hit Streak') + ylab('Percentage (%)') +
  geom_smooth(aes(x=K,y=line), size=1, span=1, color='red') +
  theme(axis.text.x=element_text(size=15,color='black'),
        axis.text.y=element_text(size=15,color='black'),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        plot.title=element_text(size=25)) +
  ggtitle('Evan Fournier')
