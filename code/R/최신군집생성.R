library(dbscan)
library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)

rm(list=ls())


#변수변환 군집화
stocks = read.csv('final_stock.csv')

#2022년도로 해서 일년뒤, 전년도대비 증감율 둘 다 봐보기
df = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)

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


df_c = df

set.seed(0802)
db=dbscan(df_c,  eps = .73, MinPts = log(nrow(df)))


#통합데이터 만들기
df_c$cluster = db$cluster
df_c$기업 = rownames(df_c)

cl_trans = data.frame(기업 = rownames(df_c), cluster_trans = df_c$cluster)


#지표양수군집화
#2022년도로 해서 일년뒤, 전년도대비 증감율 둘 다 봐보기
df = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A) %>%
  filter(유동비율 > 0 & PER > 0 & PBR > 0 & PSR > 0 & PCR > 0) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)


df = df %>%
  filter((유동비율 < quantile(유동비율, 0.99)) &
           (EPS < quantile(EPS, 0.99))  &
           (PER < quantile(PER, 0.99)) &
           (PBR < quantile(PBR, 0.99)) &
           (PSR < quantile(PSR, 0.99)) &
           (PCR < quantile(PCR, 0.99))
  ) %>%
  na.omit()


#표준화
for(i in 1:ncol(df)){
  
  df[,i] = scale(df[,i], center = T, scale = T)
  
}


################################################################################
#kmeans

df_c = df



set.seed(0802)
df_c_km = kmeans(df_c, centers = 9)


#통합데이터 만들기
df_c$cluster = df_c_km$cluster
df_c$기업 = rownames(df_c)

cl_plus = data.frame(기업 = rownames(df_c), cluster_plus = df_c$cluster)

#지표음수군집화
#2022년도로 해서 일년뒤, 전년도대비 증감율 둘 다 봐보기
df = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A)  %>%
  filter(!(유동비율 > 0 & PER > 0 & PBR > 0 & PSR > 0 & PCR > 0)) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)



df = df %>%
  filter((유동비율 > quantile(유동비율, 0.01) & 유동비율 < quantile(유동비율, 0.99)) &
           (EPS > quantile(EPS, 0.01) & EPS < quantile(EPS, 0.99))  &
           (PER > quantile(PER, 0.01) & PER < quantile(PER, 0.99)) &
           (PBR > quantile(PBR, 0.01) & PBR < quantile(PBR, 0.99)) &
           (PSR > quantile(PSR, 0.01) & PSR < quantile(PSR, 0.99)) &
           (PCR > quantile(PCR, 0.01) & PCR < quantile(PCR, 0.99))
  ) %>%
  na.omit()


#표준화
for(i in 1:ncol(df)){
  
  df[,i] = scale(df[,i], center = T, scale = T)
  
}

colSums(is.na(df))

################################################################################
#kmeans

df_c = df


set.seed(0802)
df_c_km = kmeans(df_c, centers = 8)


#통합데이터 만들기
df_c$cluster = df_c_km$cluster
df_c$기업 = rownames(df_c)

cl_out = data.frame(기업 = rownames(df_c), cluster_out = df_c$cluster)



#저장
stocks = stocks %>% 
  left_join(cl_out, by='기업')

stocks = stocks %>% 
  left_join(cl_plus, by='기업')

stocks = stocks %>% 
  left_join(cl_trans, by='기업')

stocks = stocks %>% 
  mutate(cluster_out = as.factor(cluster_out),
         cluster_plus = as.factor(cluster_plus),
         cluster_trans = as.factor(cluster_trans)) %>% 
  mutate(cluster_out = ifelse(is.na(cluster_out), 'Other', cluster_out),
         cluster_plus = ifelse(is.na(cluster_plus), 'Other', cluster_plus),
         cluster_trans = ifelse(is.na(cluster_trans), 'big_Outlier',
                                ifelse(cluster_trans == '0', 'Outlier', cluster_trans)))


table(stocks$cluster_out)
table(stocks$cluster_plus)
table(stocks$cluster_trans)

write.csv(stocks, file = "재무제표_cl.csv", row.names = F)


 




