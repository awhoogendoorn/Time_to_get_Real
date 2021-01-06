## Prepare Postcode files

# libraries -----------------------------------------------------------------------------
library(dplyr)

# Leefbaarheidscores 
p<- read.csv2("./datasources/postcode/LBM4pc9812_0.2.0.csv")
postcode <- p %>% 
  filter(jaar==2006) %>%
  select(pc4, lbrmtr) %>%
  rename(zip=pc4)
rm(p)
save(postcode, file="./datasources/postcode/postcode.Rda")
