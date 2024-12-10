install.packages('gridExtra')
library(gridExtra)



df=read.csv('final_stock.csv')

test = df %>% 
  select(유동비율,EPS,PER,PBR,PCR,POR,PSR,GP.A,일년뒤증감율,전년도대비증감율, 기업, 연도, 종가)

colSums(is.na(test))

test %>% 
  filter(!is.na(종가)) %>% 
  group_by(기업) %>% 
  mutate(n = n()) %>% 
  filter(n == 6)
view(one)

 

#past
p1 = past %>% 
  ggplot() +
  geom_point(aes(유동비율, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p2 = past %>% 
  ggplot() +
  geom_point(aes(EPS, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p3 = past %>% 
  ggplot() +
  geom_point(aes(PER, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p4 = past %>% 
  ggplot() +
  geom_point(aes(PBR, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p5 = past %>% 
  ggplot() +
  geom_point(aes(PCR, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p6 = past %>% 
  ggplot() +
  geom_point(aes(POR, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p7 = past %>% 
  ggplot() +
  geom_point(aes(PSR, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p8 = past %>% 
  ggplot() +
  geom_point(aes(GP.A, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p9 = past %>% 
  ggplot() +
  geom_point(aes(ROE, 전년도대비증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 2)



#one
p1 = one %>% 
  ggplot() +
  geom_point(aes(유동비율, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p2 = one %>% 
  ggplot() +
  geom_point(aes(EPS, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p3 = one %>% 
  ggplot() +
  geom_point(aes(PER, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p4 = one %>% 
  ggplot() +
  geom_point(aes(PBR, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p5 = one %>% 
  ggplot() +
  geom_point(aes(PCR, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p6 = one %>% 
  ggplot() +
  geom_point(aes(POR, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p7 = one %>% 
  ggplot() +
  geom_point(aes(PSR, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p8 = one %>% 
  ggplot() +
  geom_point(aes(GP.A, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)
p9 = one %>% 
  ggplot() +
  geom_point(aes(ROE, 일년뒤증감율),
             color = 'royalblue',
             alpha = .3,
             cex = 1.5)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 2)
















