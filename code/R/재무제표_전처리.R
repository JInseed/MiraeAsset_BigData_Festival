install.packages('tidyverse')


jm=read.csv('재무제표.csv')

test = jm %>% 
  distinct() %>%
  group_by(기업) %>% 
  mutate(n = n()) %>%
  filter(n == 6) %>% 
  select(-n) %>%

 
write.csv(test, file = "재무제표_전처리.csv", row.names = F)


colSums(is.na(test))  













