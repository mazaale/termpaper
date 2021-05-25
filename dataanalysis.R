### importing and preparing
rm(list=ls())
library(dplyr)
library(ggplot2)
library(nnet)
library(AER)
library(mclogit)
library(reshape2)

wd <- 'C:/Users/admin/Desktop/study/gm_std'

setwd(wd)

### preparing data
datamc <- read.csv('rpgfull.csv', header=T, encoding = 'UTF-8')
data2<- datamc %>%
  select(im1, im2, im3, im4, im5, im6, ac1, ac2, ac3, ac4, ac5, ac6, cul1, cul2, cul3, ec1, ec2, ec3, soc1, soc2, soc3, sym1, sym2, sym3, rec1, rec2, rec3, idk1, idk2, idk3, loy1, loy2, loy3, r1, r2, cont1, cont2, id1, id2, id3, id4, id5, id6, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
data3 <- scale(data2) # scaling
data3 <- data.frame(data3) #making a dataframe from scale

# creating a dataframe for analysis with average of independent variables of the same concept; adding long responses
data4 <- data.frame(resp1=datamc$longresp1)
data4$resp2 <- datamc$longresp2 
data4$resp3 <- datamc$longresp3
data4$resp4 <- datamc$longresp4
data4$resp5 <- datamc$longresp5
data4$IM <- rowMeans(data3[,c('im1', 'im2', 'im3', 'im4', 'im5', 'im6')], na.rm=T)
data4$AC <- rowMeans(data3[,c('ac1', 'ac2', 'ac3', 'ac4', 'ac5', 'ac6')], na.rm=T)
data4$CUL <- rowMeans(data3[,c('cul1', 'cul2', 'cul3')], na.rm=T)
data4$ECO <- rowMeans(data3[,c('ec1', 'ec2', 'ec3')], na.rm=T)
data4$SOC <- rowMeans(data3[,c('soc1', 'soc2', 'soc3')], na.rm=T)
data4$SYM <- rowMeans(data3[,c('sym1', 'sym2', 'sym3')], na.rm=T)
data4$REC <- rowMeans(data3[,c('rec1', 'rec2', 'rec3')], na.rm=T)
data4$IDK <- rowMeans(data3[,c('idk1', 'idk2', 'idk3')], na.rm=T)
data4$LOY <- rowMeans(data3[,c('loy1', 'loy2', 'loy3')], na.rm=T)
data4$REA <- rowMeans(data3[,c('r1', 'r2')], na.rm=T)
data4$CON <- rowMeans(data3[,c('cont1', 'cont2')], na.rm=T)
data4$IDR <- rowMeans(data3[,c('id1', 'id2', 'id3', 'id4', 'id5', 'id6')], na.rm=T)

#multinomenal logic models
#1 + resp1
mult <- nnet::multinom(resp1~IM+AC+CUL+ECO+SOC+SYM+REC+IDK+LOY+REA+CON+IDR, data = data4)
summary(mult)
z <- summary(mult)$coefficients/summary(mult)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
#2 + resp2
mult2 <- nnet::multinom(resp2~IM+AC+CUL+ECO+SOC+SYM+REC+IDK+LOY+REA+CON+IDR, data = data4)
summary(mult2)
z2 <- summary(mult2)$coefficients/summary(mult2)$standard.errors
p2 <- (1 - pnorm(abs(z), 0, 1)) * 2
p2
#resp3
mult3 <- nnet::multinom(resp3~IM+AC+CUL+ECO+SOC+SYM+REC+IDK+LOY+REA+CON+IDR, data = data4)
summary(mult3)
z3 <- summary(mult3)$coefficients/summary(mult3)$standard.errors
p3 <- (1 - pnorm(abs(z), 0, 1)) * 2
p3
#resp4
mult4 <- nnet::multinom(resp4~IM+AC+CUL+ECO+SOC+SYM+REC+IDK+LOY+REA+CON+IDR, data = data4)
summary(mult4)
z4 <- summary(mult4)$coefficients/summary(mult4)$standard.errors
p4 <- (1 - pnorm(abs(z), 0, 1)) * 2
p4
#resp5
mult5 <- nnet::multinom(resp5~IM+AC+CUL+ECO+SOC+SYM+REC+IDK+LOY+REA+CON+IDR, data = data4)
summary(mult5)
z5 <- summary(mult5)$coefficients/summary(mult5)$standard.errors
p5 <- (1 - pnorm(abs(z), 0, 1)) * 2
p5

##experiment 

exp <- read.csv('exp1.csv', header=T, encoding = 'UTF-8') 
ggplot (data = exp) +
  geom_count(aes(x = ind, y = ref, color = ind))

####### graphs
socio <- read.csv('socio.csv', header=T, encoding = 'UTF-8') 

with_socio <- data4
with_socio$age = socio$age
with_socio$activity = socio$activity
with_socio$placebrith = socio$placebrith
with_socio$gender = socio$gender
with_socio$town = socio$town
with_socio$genre = socio$genre
with_socio$frequency_of_play = socio$frequency_of_play

with_socio$gender[131] = 'Иначе'

ggplot(data=with_socio, aes(x=factor(gender)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()



with_socio %>%
  mutate(frequency_of_play = factor(frequency_of_play, levels=c("один раз в месяц и реже", 
                                                                "несколько раз в месяц",
                                                                "несколько раз в неделю",
                                                                "от двух до пяти дней в неделю",
                                                                "каждый день"))) %>%
  ggplot()+
  geom_count(data=with_socio, mapping=aes(x=gender, y=factor(frequency_of_play, levels=c("один раз в месяц и реже", 
                                                                                         "несколько раз в месяц",
                                                                                         "несколько раз в неделю",
                                                                                         "от двух до пяти дней в неделю",
                                                                                         "каждый день"))))

library(reshape2)

df.melted.gender <- melt(with_socio[c('gender', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'gender')
df.melted.gender

pl <- ggplot(data = df.melted.gender, aes(x=variable, y=value)) + geom_boxplot(aes(fill=gender))
pl + facet_wrap( ~ variable, scales="free")

df.melted.freq <- melt(with_socio[c('frequency_of_play', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'frequency_of_play')
pf <- ggplot(data=df.melted.freq, aes(x=variable, y=value)) + geom_boxplot(aes(fill=frequency_of_play))
pf + facet_wrap( ~ variable, scales='free')

df.melted.resp1 <- melt(with_socio[c('resp1', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'resp1')
pf <- ggplot(data=df.melted.resp1, aes(x=variable, y=value)) + geom_boxplot(aes(fill=resp1))
pf + facet_wrap( ~ variable, scales='free')

df.m <- melt(with_socio[c('resp2', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'resp2')
pf <- ggplot(data=df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=resp2))
pf + facet_wrap( ~ variable, scales='free')


df.m <- melt(with_socio[c('resp3', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'resp3')
pf <- ggplot(data=df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=resp3))
pf + facet_wrap( ~ variable, scales='free')


df.m <- melt(with_socio[c('resp4', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'resp4')
pf <- ggplot(data=df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=resp4))
pf + facet_wrap( ~ variable, scales='free')


df.m <- melt(with_socio[c('resp5', 'IM', "AC", "CUL", "ECO", "SOC", "SYM", "REC", "LOY", "REA", 'CON', "IDR")], id.var = 'resp5')
pf <- ggplot(data=df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=resp5))
pf + facet_wrap( ~ variable, scales='free')