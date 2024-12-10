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
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A)  %>%
  filter(!(유동비율 > 0 & PER > 0 & PBR > 0 & PSR > 0 & PCR > 0)) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)

summary(df)
quantile(df$PER, 0.01)


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

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(df_c, kmeans, method = "wss", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(df_c, kmeans, method = "silhouette", k.max = 20) +
  theme_minimal() + 
  ggtitle("Silhouette Method")

set.seed(0802)
df_c_km = kmeans(df_c, centers = 8)
sil = silhouette(df_c_km$cluster, dist(df_c))
fviz_silhouette(sil)

fviz_cluster(df_c_km, data = df_c, geom = "text")+
  theme(legend.position = "none")

table(df_c_km$cluster)

sm <-df_c_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm




#통합데이터 만들기
df_c$cluster = df_c_km$cluster
df_c$기업 = rownames(df_c)

cl_out = data.frame(기업 = rownames(df_c), cluster_out = df_c$cluster)

stock = stocks %>% 
  filter(연도 == 2018)

stock = stock %>% 
  left_join(cl_out, by='기업')

stock = stock %>% 
  mutate(cluster_out = as.factor(cluster_out)) %>% 
  mutate(cluster_out = ifelse(is.na(cluster_out), 'Other', cluster_out))

#군집 해석
result = stock %>%
  group_by(cluster_out) %>% 
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

rownames(result)=result[,'cluster_out']

result %>% 
  select(-cluster_out) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)
 

#저장
stocks = stocks %>% 
  left_join(cl_out, by='기업')

stocks = stocks %>% 
  left_join(cl_plus, by='기업')

stocks = stocks %>% 
  left_join(cl_trans, by='기업')

write.csv(stocks, file = "재무제표_cl.csv", row.names = F)















