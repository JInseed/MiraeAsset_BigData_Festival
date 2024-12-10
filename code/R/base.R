library(tidyverse)

df=read.csv('cs_data.csv')

colSums(is.na(df))

colnames(df)

#테이블 분리
snap = df[,1:24]
asset = df[,25:148]
record = df[,149:520]

#1.자산내역 파생변수 생성
#1-(1)총 자산액(월 말일 기준)
asset = asset %>% 
  mutate(AST_M1 = CASH_AST_M1 + DMST_AST_EVAL_M1 + OVST_AST_EVAL_M1,
         AST_M2 = CASH_AST_M2 + DMST_AST_EVAL_M2 + OVST_AST_EVAL_M2,
         AST_M3 = CASH_AST_M3 + DMST_AST_EVAL_M3 + OVST_AST_EVAL_M3,
         AST_M4 = CASH_AST_M4 + DMST_AST_EVAL_M4 + OVST_AST_EVAL_M4)

#1-(2) 국내 및 해외주식 이율, 총 주식 이율(월 말일 기준)
asset %>% 
  mutate(DMST_rate_M1 = ((DMST_AST_EVAL_M1 - DMST_AST_PCHS_M1) / DMST_AST_PCHS_M1)*100,
         DMST_rate_M2 = ((DMST_AST_EVAL_M2 - DMST_AST_PCHS_M2) / DMST_AST_PCHS_M2)*100,
         DMST_rate_M3 = ((DMST_AST_EVAL_M3 - DMST_AST_PCHS_M3) / DMST_AST_PCHS_M3)*100,
         DMST_rate_M4 = ((DMST_AST_EVAL_M4 - DMST_AST_PCHS_M4) / DMST_AST_PCHS_M4)*100,
         
         OVST_rate_M1 = ((OVST_AST_EVAL_M1 - OVST_AST_PCHS_M1) / OVST_AST_PCHS_M1)*100,
         OVST_rate_M2 = ((OVST_AST_EVAL_M2 - OVST_AST_PCHS_M2) / OVST_AST_PCHS_M2)*100,
         OVST_rate_M3 = ((OVST_AST_EVAL_M3 - OVST_AST_PCHS_M3) / OVST_AST_PCHS_M3)*100,
         OVST_rate_M4 = ((OVST_AST_EVAL_M4 - OVST_AST_PCHS_M4) / OVST_AST_PCHS_M4)*100,
         
         rate_M1 = ((DMST_AST_EVAL_M1 + OVST_AST_EVAL_M1 - DMST_AST_PCHS_M1 - OVST_AST_PCHS_M1) / (DMST_AST_PCHS_M1 + OVST_AST_PCHS_M1))*100,
         rate_M2 = ((DMST_AST_EVAL_M2 + OVST_AST_EVAL_M2 - DMST_AST_PCHS_M2 - OVST_AST_PCHS_M2) / (DMST_AST_PCHS_M2 + OVST_AST_PCHS_M2))*100,
         rate_M3 = ((DMST_AST_EVAL_M3 + OVST_AST_EVAL_M3 - DMST_AST_PCHS_M3 - OVST_AST_PCHS_M3) / (DMST_AST_PCHS_M3 + OVST_AST_PCHS_M3))*100,
         rate_M4 = ((DMST_AST_EVAL_M4 + OVST_AST_EVAL_M4 - DMST_AST_PCHS_M4 - OVST_AST_PCHS_M4) / (DMST_AST_PCHS_M4 + OVST_AST_PCHS_M4))*100)





















