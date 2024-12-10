#install.packages("NbClust")
#install.packages("caret")
#install.packages("factoextra")
#install.packages('kableExtra')

options("scipen"=10)

library(tidyverse)
library(dbscan)
library(NbClust)
library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)




#2차 군집화: 투자성향 파악
#kmeans

rm(list=ls())
cs_df=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/미래에셋 공모전/data/cluster_cs.csv')

cs_df$ID = rownames(cs_df)

rownames(cs_df) = cs_df$ID


#2차 군집화: 투자성향 파악
cl_tend = cs_df %>% 
  filter(자산규모 == '1만 < < 100만') %>% 
  select(DAY_TR_RATIO, SWING_TR_RATIO, MONTHS_TR_RATIO, MID_TR_RATIO, HLD_TR_RATIO,
         YEARS_TR_RATIO, CASH_AST_M4, DMST_AST_EVAL_M4, 체결횟수_M4, 매수매도종목수, DMST_ITM_CNT_M4)


#표준화
for(i in 1:ncol(cl_tend)){
  
  cl_tend[,i] = scale(cl_tend[,i], center = T, scale = T)
  
}
#Elbow Method로 찾기
## find the optimal number of clusters using Total within-cluster sum of squares
fviz_nbclust(cl_tend, kmeans, method = "wss", k.max = 20) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


#실루엣 이용
fviz_nbclust(cl_tend, kmeans, method = "silhouette", k.max = 20) +
  theme_minimal() + 
  ggtitle("Silhouette Method")

set.seed(0802)
km = kmeans(cl_tend, centers = 10)
sil = silhouette(km$cluster, dist(cl_tend))
fviz_silhouette(sil)

fviz_cluster(km, data = cl_tend, geom = "text")+
  theme(legend.position = "none")

table(km$cluster)

sm <-km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)
sm



#군집해석
cl_df = data.frame(ID = rownames(cl_tend), cluster_tend = km$cluster)

cs_df = cs_df %>% 
  left_join(cl_df, by = 'ID')

cs_df = cs_df %>% 
  mutate(cluster_tend = as.factor(cluster_tend)) %>% 
  mutate(cluster_tend = ifelse(is.na(cluster_tend), 'Other',
                              ifelse(cluster_tend == '0', 'Outlier',cluster_tend)))

#군집 해석
result = cs_df %>%
  filter(평균이율 != Inf & 이율_M4 != Inf) %>% 
  group_by(cluster_tend) %>% 
  summarise(DAY_TR_RATIO = mean(DAY_TR_RATIO, na.rm = T),
            SWING_TR_RATIO = mean(SWING_TR_RATIO, na.rm = T),
            MONTHS_TR_RATIO = mean(MONTHS_TR_RATIO, na.rm = T),
            MID_TR_RATIO = mean(MID_TR_RATIO, na.rm = T),
            HLD_TR_RATIO = mean(HLD_TR_RATIO, na.rm = T),
            YEARS_TR_RATIO = mean(YEARS_TR_RATIO, na.rm = T),
            CASH_AST_M4 = mean(CASH_AST_M4, na.rm = T),
            DMST_AST_EVAL_M4 = mean(DMST_AST_EVAL_M4, na.rm = T),
            MONTHS_TR_RATIO = mean(MONTHS_TR_RATIO, na.rm = T),
            체결횟수_M4 = mean(체결횟수_M4, na.rm = T),
            매수매도종목수 = mean(매수매도종목수, na.rm = T),
            DMST_ITM_CNT_M4 = mean(DMST_ITM_CNT_M4, na.rm = T),
            평균이율 = mean(평균이율, na.rm = T),
            이율_M4 = mean(이율_M4, na.rm = T),
            고객수 = n()
  ) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_tend']

result %>% 
  select(-cluster_tend) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)


write.csv(cs_df, file = "자산규모낮은군집.csv", row.names = F)



