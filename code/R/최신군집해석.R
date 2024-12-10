#install.packages("NbClust")
#install.packages("caret")
#install.packages("factoextra")
#install.packages('kableExtra')

library(dbscan)
library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)


#변수변환_원변수해석
rm(list=ls())

stocks = read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/미래에셋 공모전/data/final_stock.csv')


#2022년도로 해서 일년뒤, 전년도대비 증감율 둘 다 봐보기
df = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
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

df_c = df

set.seed(0802)
db=dbscan(df_c,  eps = .73, MinPts = log(nrow(df)))


fviz_cluster(db, df_c, geom = "point",
             palette = "Set2", ggtheme = theme_minimal())

table(db$cluster)


#통합데이터 만들기
df_c$cluster = db$cluster
df_c$기업 = rownames(df_c)

cl_trans = data.frame(기업 = rownames(df_c), cluster_trans = df_c$cluster)

stock = stocks %>% 
  filter(연도 == 2022)

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
            전년도대비증감율=mean(전년도대비증감율, na.rm = T),
            군집수 = n()) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_trans']

result %>% 
  select(-cluster_trans) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)

###############################################################################
#변수변환 군집변수해석
rm(list=ls())

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

colSums(is.na(df))


################################################################################
#dbscan
library(dbscan)

df_c = df

set.seed(0802)
db=dbscan(df_c,  eps = .73 , MinPts = log(nrow(df)))


fviz_cluster(db, df_c, geom = "point",
             palette = "Set2", ggtheme = theme_minimal())
table(db$cluster)

#통합데이터 만들기
cl_df = data.frame(기업 = rownames(df_c), cluster_trans = db$cluster)




interpret = stocks %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
  select(기업,연도,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A, 일년뒤증감율, 전년도대비증감율) %>% 
  na.omit()


interpret = interpret %>%
  filter((유동비율 > quantile(유동비율, 0.01) & 유동비율 < quantile(유동비율, 0.99)) &
           (EPS > quantile(EPS, 0.01) & EPS < quantile(EPS, 0.99)) &
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

summary(interpret)

interpret = interpret %>% 
  left_join(cl_df, by='기업')



interpret = interpret %>% 
  mutate(cluster_trans = as.factor(cluster_trans)) %>% 
  mutate(cluster_trans = ifelse(is.na(cluster_trans), 'big_Outlier',
                                ifelse(cluster_trans == '0', 'Outlier', cluster_trans)))


interpret %>% 
  filter(연도 == 2022) %>% 
  select(cluster_trans) %>% 
  table(useNA = 'ifany')

table(interpret$cluster_trans)



#군집 해석
result = interpret %>% 
  filter(연도 == 2022) %>% 
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
            전년도대비증감율=mean(전년도대비증감율, na.rm = T),
            군집수 = n()) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_trans']

result %>% 
  select(-cluster_trans) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)

###############################################################################
#지표양수군집화
rm(list=ls())

stocks = read.csv('final_stock.csv')


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
df_c_km = kmeans(df_c, centers = 9)
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

cl_plus = data.frame(기업 = rownames(df_c), cluster_plus = df_c$cluster)

stock = stocks %>% 
  filter(연도 == 2022)

stock = stock %>% 
  left_join(cl_plus, by='기업')

stock = stock %>% 
  mutate(cluster_plus = as.factor(cluster_plus)) %>% 
  mutate(cluster_plus = ifelse(is.na(cluster_plus), 'Other', cluster_plus))

#군집 해석
result = stock %>%
  group_by(cluster_plus) %>% 
  summarise(유동비율=mean(유동비율),
            EPS=mean(EPS, na.rm = T),
            PER=mean(PER, na.rm = T),
            PBR=mean(PBR, na.rm = T),
            PCR=mean(PCR, na.rm = T),
            POR=mean(POR, na.rm = T),
            PSR=mean(PSR, na.rm = T),
            GP.A=mean(GP.A, na.rm = T),
            ROE=mean(ROE, na.rm = T),
            전년도대비증감율=mean(전년도대비증감율, na.rm = T),
            군집수 = n()) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_plus']

result %>% 
  select(-cluster_plus) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)

###############################################################################
#지표음수군집화
rm(list=ls())

stocks = read.csv('final_stock.csv')


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
  filter(연도 == 2022)

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
            전년도대비증감율=mean(전년도대비증감율, na.rm = T),
            군집수 = n()) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_out']

result %>% 
  select(-cluster_out) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)






 






































