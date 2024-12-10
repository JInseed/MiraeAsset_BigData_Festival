install.packages("NbClust")
install.packages("caret")
install.packages("factoextra")
install.packages('kableExtra')

library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)




rm(list=ls())

df=read.csv('final_stock.csv')

df = df %>% 
  filter(매출액 >= 0 & 매출총이익 != 0 & 영업이익 != 0 & 당기순이익 != 0 & 영업현금흐름 != 0 & 자본총계 != 0 & 자산총계 != 0) %>% 
  filter(연도 == 2022) %>% 
  select(기업,유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A) %>% 
  na.omit()

rownames(df)=df[[1]]

df = df %>% 
  select(-기업)

summary(df)


df = df %>% 
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

###############################################################################
#kmeans

# NbClust 함수로 최적의 군집 수 찾기

df_c = df

nc <- NbClust(df_c, min.nc = 2, max.nc = 15, method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(df_c, kmeans, method = "wss", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(df_c, kmeans, method = "silhouette", k.max = 10) +
  theme_minimal() + 
  ggtitle("Silhouette Method")

df_c_km = kmeans(df_c, centers = 9)
sil = silhouette(df_c_km$cluster, dist(df_c))
fviz_silhouette(sil)

fviz_cluster(df_c_km, data = df_c, geom = "text")+
  theme(legend.position = "none")

df_c_km$centers

sm <-df_c_km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm

df_c_km$cluster
df_c_km$cluster[df_c_km$cluster==1]
df_c_km$centers


df_c$cluster=km$cluster
df_c2=as.data.frame(df_c$지역명,km$cluster)
view(df_c)
df_c=df_c[,c('지역명','cluster')]




################################################################################
#dbscan
install.packages('dbscan')
library(dbscan)

db=dbscan(df,  eps = 0.3 , MinPts = log(nrow(df)))


df$cluster=db$cluster
table(df$cluster)

fviz_cluster(db, df,geom = "point")


result=df %>% 
  group_by(cluster) %>% 
  summarise(유동비율=mean(유동비율),
            EPS=mean(EPS),
            PER=mean(PER),
            PBR=mean(PBR),
            PCR=mean(PCR),
            POR=mean(POR),
            PSR=mean(PSR),
            GP.A=mean(GP.A),
            ROE=mean(ROE)) %>% 
  as.data.frame()

rownames(result)=result[,'cluster']

result %>% 
  select(-cluster) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)


colnames(df)













