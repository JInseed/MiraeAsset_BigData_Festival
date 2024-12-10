#install.packages("NbClust")
#install.packages("caret")
#install.packages("factoextra")
#install.packages('kableExtra')

library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)




rm(list=ls())

stocks = read.csv('final_stock.csv')


#2018년도로 해서 일년뒤, 전년도대비 증감율 둘 다 봐보기
df = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2018) %>% 
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)

summary(df)
quantile(df$PER, 0.99)


df = df %>%
  filter((유동비율 > quantile(유동비율, 0.01) & 유동비율 < quantile(유동비율, 0.99)) &
           (EPS > quantile(EPS, 0.01) & EPS < quantile(EPS, 0.99))  &
           (PER > quantile(PER, 0.01) & PER < quantile(PER, 0.99)) &
           (PBR > quantile(PBR, 0.01) & PBR < quantile(PBR, 0.99)) &
           (PSR > quantile(PSR, 0.01) & PSR < quantile(PSR, 0.99)) &
           (PCR > quantile(PCR, 0.01) & PCR < quantile(PCR, 0.99))
  ) %>%  
  mutate(PER = ifelse (PER >= 0, PER, max(PER) - PER),
         PBR = ifelse (PBR >= 0, PBR, max(PBR) - PBR),
         PCR = ifelse (PCR >= 0, PCR, max(PCR) - PCR),
         POR = ifelse (POR >= 0, POR, max(POR) - POR),
         GP.A = ifelse (GP.A >= 0, GP.A, max(GP.A) - GP.A),
         ROE = PBR/PER) %>% 
  na.omit()



#표준화
for(i in 1:ncol(df)){
  
  df[,i] = scale(df[,i], center = T, scale = T)
  
}



################################################################################
#dbscan
library(dbscan)

df_c = df

set.seed(0802)
db=dbscan(df_c,  eps = .67, MinPts = log(nrow(df)))


fviz_cluster(db, df_c, geom = "point",
             palette = "Set2", ggtheme = theme_minimal())

table(db$cluster)


#통합데이터 만들기
df_c$cluster = db$cluster
df_c$기업 = rownames(df_c)

cl_trans = data.frame(기업 = rownames(df_c), cluster_trans = df_c$cluster)

stock = stocks %>% 
  filter(연도 == 2018)

stock = stock %>% 
  left_join(cl_trans, by='기업')

table(stock$cluster_trans)

 
stock = stock %>% 
  mutate(cluster_trans = as.factor(cluster_trans)) %>% 
  mutate(cluster_trans = ifelse(is.na(cluster_trans), 'big_Outlier',
                          ifelse(cluster_trans == '0', 'Outlier', cluster_trans)))



#군집 해석
result = stock %>%
  group_by(cluster_trans) %>% 
  summarise(유동비율=mean(유동비율),
            EPS=mean(EPS, na.rm = T),
            PER=mean(PER, na.rm = T),
            PBR=mean(PBR, na.rm = T),
            PCR=mean(PCR, na.rm = T),
            POR=mean(POR, na.rm = T),
            PSR=mean(PSR, na.rm = T),
            GP.A=mean(GP.A, na.rm = T),
            ROE=mean(ROE, na.rm = T),
            일년뒤증감율=mean(일년뒤증감율, na.rm = T),
            전년도대비증감율=mean(전년도대비증감율, na.rm = T),
            군집수 = n()) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_trans']

result %>% 
  select(-cluster_trans) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)


#저장
stocks = stocks %>% 
  left_join(cl_trans, by='기업')

write.csv(stocks, file = "재무제표_전처리.csv", row.names = F)

############################################################################



stocks %>% 
  select(일년뒤증감율, 전년도대비증감율, 기업, 연도, 종가) %>% 
  view()















