library(tidyverse)
library(mosaic)
library(foreach)
library(corrplot)

# load data
path = "E:/repo/Hot-hands/result/trial_4.csv"
df = read.csv(path)

n = c(1:40)
ggplot(df) +
  geom_smooth(aes(x=n,y=trial_1),span=0.1,size=1) +
  geom_smooth(aes(x=n,y=trial_2),span=0.1,size=1) +
  geom_smooth(aes(x=n,y=trial_3),span=0.1,size=1) +
  scale_y_continuous(limits=c(0.2,0.7), breaks= seq(0,1,by=0.1)) +
  geom_hline(yintercept=0.4, color='red' ,size=1) +
  ylab('Probability') + xlab("N") +
  theme(axis.text.x=element_text(size=25,color='black'),
        axis.text.y=element_text(size=25,color='black'),
        axis.title.x=element_text(size=30),
        axis.title.y=element_text(size=30))
