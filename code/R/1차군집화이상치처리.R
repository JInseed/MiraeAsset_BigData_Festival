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

rm(list=ls())
cs_df=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/미래에셋 공모전/data/cluster_cs.csv')

cs_df$ID = rownames(cs_df)

rownames(cs_df) = cs_df$ID

#1차 군집화: 이상치 처리
cl_out = cs_df %>% 
  filter(자산규모 == '1만 < < 100만') %>% 
  select(접속일수_M4, 총자산액_M4, 체결횟수_M4)


#표준화
for(i in 1:ncol(cl_out)){
  
  cl_out[,i] = scale(cl_out[,i], center = T, scale = T)
  
}

#dbscan
set.seed(0802)
db=dbscan(cl_out,  eps = .6, MinPts = log(nrow(cl_out)))


fviz_cluster(db, cl_out, geom = "point",
             palette = "Set2", ggtheme = theme_minimal())


#군집해석
cl_df = data.frame(ID = rownames(cl_out), cluster_out = db$cluster)

cs_df = cs_df %>% 
  left_join(cl_df, by = 'ID')

cs_df = cs_df %>% 
  mutate(cluster_out = as.factor(cluster_out)) %>% 
  mutate(cluster_out = ifelse(is.na(cluster_out), 'Other',
                        ifelse(cluster_out == '0', 'Outlier',cluster_out)))

#군집 해석
result = cs_df %>%
  filter(평균이율 != Inf & 이율_M4 != Inf) %>% 
  group_by(cluster_out) %>% 
  summarise(접속일수_M4 = mean(접속일수_M4, na.rm = T),
            총자산액_M4 = mean(총자산액_M4, na.rm = T),
            체결횟수_M4 = mean(체결횟수_M4, na.rm = T),
            평균이율 = mean(평균이율, na.rm = T),
            이율_M4 = mean(이율_M4, na.rm = T),
            고객수 = n()
  ) %>% 
  as.data.frame()

rownames(result)=result[,'cluster_out']

result %>% 
  select(-cluster_out) %>% 
  t()  %>%  
  kable("pandoc", digits = 3)





