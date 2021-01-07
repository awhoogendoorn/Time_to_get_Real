## Prepare Postcode files

# libraries -----------------------------------------------------------------------------

library(dplyr)
library(openxlsx)

# CBS_PC4_2015_v2.xlsx
p <- read.xlsx("./datasources/postcode/CBS_PC4_2015_v2_edited.xlsx", na.strings = c("-99997"))
#View(p)
postcode <- p %>% 
  select(PC4,
         P_NL_ACHTG,
         P_WE_MIG_A,
         P_NW_MIG_A,
         P_HUURWON,
         P_KOOPWON,
         M_INKHH,
         P_LINK_HH,
         P_HINK_HH,
         OAD,
         STED
  ) %>%
  rename(zip=PC4,
         p_nl_achtg=P_NL_ACHTG,
         p_we_mig_a=P_WE_MIG_A,
         p_nw_mig_a=P_NW_MIG_A,
         p_huurwon=P_HUURWON,
         p_koopwon=P_KOOPWON,
         m_inkhh=M_INKHH,
         p_link_hh=P_LINK_HH,
         p_hink_hh=P_HINK_HH,
         oad=OAD,
         sted=STED
  ) %>%
  mutate(m_inkhh=as.factor(m_inkhh)) %>%
  mutate_if(is.numeric, list(~na_if(., -99997)))


# Leefbaarheidscores 
p<- read.csv2("./datasources/postcode/LBM4pc9812_0.2.0.csv")
p2 <-  p %>% 
  filter(jaar==2012) %>%
  select(pc4, lbrmtr) %>%
  rename(zip=pc4)
postcode <- postcode %>% 
  left_join(p2 , by = "zip")
rm(p, p2)

postcode %>% 
  summarise_if(is.numeric, min, na.rm = TRUE)

save(postcode, file="./datasources/postcode/postcode.Rda")

