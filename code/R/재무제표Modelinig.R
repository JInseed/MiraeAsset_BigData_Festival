install.packages('ggcorrplot')
library(ggcorrplot)
library(tidyverse)
library(moonBook)
library(car)
library(caret)
rm(list=ls())

df=read.csv('final_stock.csv')

df = df %>% 
  filter(!is.na(종가)) %>% 
  group_by(기업) %>% 
  mutate(n = n()) %>% 
  filter(n == 6) %>% 
  ungroup()


one = df %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 != 2022) %>% 
  select(유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A,일년뒤증감율) %>% 
  na.omit()


one = one %>% 
  filter( (!유동비율 %in% boxplot(유동비율)$out) & (!EPS %in% boxplot(EPS)$out) &
            (!PER %in% boxplot(PER)$out) & (!PBR %in% boxplot(PBR)$out) &
            (!PCR %in% boxplot(PCR)$out) &
            (!POR %in% boxplot(POR)$out) & (!PSR %in% boxplot(PSR)$out) &
            (!GP.A %in% boxplot(GP.A)$out) & (!GP.A %in% boxplot(GP.A)$out)) %>% 
  mutate(PER = ifelse (PER >= 0, PER, max(PER) - PER),
         PBR = ifelse (PBR >= 0, PBR, max(PBR) - PBR),
         PCR = ifelse (PCR >= 0, PCR, max(PCR) - PCR),
         POR = ifelse (POR >= 0, POR, max(POR) - POR),
         GP.A = ifelse (GP.A >= 0, GP.A, max(GP.A) - GP.A),
         ROE = PBR/PER)
  
past = df %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 != 2017) %>% 
  select(유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A,전년도대비증감율) %>%
  na.omit() 

past = past%>% 
  filter( (!유동비율 %in% boxplot(유동비율)$out) & (!EPS %in% boxplot(EPS)$out) &
            (!PER %in% boxplot(PER)$out) & (!PBR %in% boxplot(PBR)$out) &
            (!PCR %in% boxplot(PCR)$out) &
            (!POR %in% boxplot(POR)$out) & (!PSR %in% boxplot(PSR)$out) &
            (!GP.A %in% boxplot(GP.A)$out) & (!GP.A %in% boxplot(GP.A)$out)) %>% 
  mutate(PER = ifelse (PER >= 0, PER, max(PER) - PER),
         PBR = ifelse (PBR >= 0, PBR, max(PBR) - PBR),
         PCR = ifelse (PCR >= 0, PCR, max(PCR) - PCR),
         POR = ifelse (POR >= 0, POR, max(POR) - POR),
         GP.A = ifelse (GP.A >= 0, GP.A, max(GP.A) - GP.A),
         ROE = PBR/PER)


summary(one)
summary(past)

ggcorrplot(cor(one),
           hc.order = F, 
           type = "upper",
           lab = T)

ggcorrplot(cor(past),
           hc.order = F, 
           type = "upper",
           lab = T)


colnames(one[,1])

for(i in 1:ncol(one)){
  if (colnames(one[,i]) != '일년뒤증감율'){
    one[,i] = scale(one[,i], center = T, scale = T)
  }
}

for(i in 1:ncol(past)){
  if (colnames(past[,i])!='전년도대비증감율'){
    past[,i] = scale(past[,i], center = T, scale = T)
  }
}


summary(one)
summary(past)



fm=lm(일년뒤증감율~.,data=one)
summary(fm)
vif(fm)


pm=lm(전년도대비증감율~.,data=past)
summary(pm)
vif(pm)







