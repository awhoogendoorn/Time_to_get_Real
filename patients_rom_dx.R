
rm(list = ls(all.names = TRUE)) 
library("dplyr")
library("tidyr")
library("GiG")
library("lubridate")

## Cluster trajectories creates clusters -------------------------------------------------
#  cluster_trajectories.R

load("data/patients.Rda")
rm("patients")
load("data/patient_months_interventions.Rda")
load("data/patients_cluster.Rda")
patients<-patients_cluster
rm("patients_cluster")

# read postcode info
load(file="./datasources/postcode/postcode.Rda")
# match zip code
p<- patients %>% 
   left_join(postcode, by = "zip") %>%
   select(-zip) %>%
   select(client_id, gender, year_of_birth, 
          p_nl_achtg, p_we_mig_a, p_nw_mig_a, p_huurwon,
          p_koopwon, m_inkhh, p_link_hh, p_hink_hh, oad, sted, lbrmtr,
          enroll_date, everything())

  
## ROM: create m_rom
bsi <- score_bsi(patients_GIG$rom_items$bsi, t_scores = TRUE) %>% 
  rename(score = gs) %>% 
  subset(!is.na(date)) %>%
  select(client_id, date, score) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  select(client_id, year, month, date, score) %>%
  mutate(rom="bsi")
sq48 <- score_sq48(patients_GIG$rom_items$sq48, t_scores = TRUE) %>% 
  rename(score = sq48) %>% 
  subset(!is.na(date)) %>%
  select(client_id, date, score) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  select(client_id, year, month, date, score) %>%
  mutate(rom="sq48")
oq45 <- score_oq45(patients_GIG$rom_items$oq45, t_scores = TRUE) %>% 
  rename(score = oq45) %>% 
  subset(!is.na(date)) %>%
  select(client_id, date, score) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  select(client_id, year, month, date, score) %>%
  mutate(rom="oq45")
honos <- score_honos(patients_GIG$rom_items$honos, t_scores = TRUE) %>% 
  rename(score = honos) %>% 
  subset(!is.na(date)) %>%
  select(client_id, date, score) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  select(client_id, year, month, date, score) %>%
  mutate(rom="honos")
rom <- rbind(bsi, sq48, oq45, honos) %>%
  arrange(client_id, year, month, rom, date)
# wide 
rom %>%
  group_by(client_id, year, month) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>% 
  group_by(idx) %>% 
  summarise(n=n())
# max 6 instances
patient_months_romscores <- rom %>%
  mutate(date = as.character(date)) %>%
  group_by(client_id, year, month) %>%
  mutate(.id = row_number()) %>%
  mutate(nrom = n()) %>%
  ungroup() %>%
  gather(var, val, score, rom, date) %>%
  unite(Var, var, .id) %>%
  spread(Var, val) %>%
  select(client_id, year, month, nrom, 
         date_1, rom_1, score_1, date_2, rom_2, score_2,
         date_3, rom_3, score_3, date_4, rom_4, score_4,
         date_5, rom_5, score_5, date_6, rom_6, score_6)
rm(bsi, sq48, oq45, honos, rom)

## m_diag: diagnoses per month for patients in R data file ------------------------------ 
patient_months_diagnoses <- subset(patients_GIG$diagnoses$dx, client_id %in% patients$client_id) %>%
  select(client_id, date, gaf1, gaf2, ax_1_1:ax_2_2, ax_3, ax_4) %>%
  mutate(date = as.character(date)) %>%
  filter_at(vars(gaf1, gaf2, ax_1_1:ax_2_2, ax_3, ax_4),
            any_vars(!is.na(.))) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  group_by(client_id, year, month) %>%
  mutate(.id = row_number()) %>%
  mutate(ndiag = n()) %>%
  ungroup() %>%
  gather(var, val, date, gaf1, gaf2, ax_1_1, ax_1_2, ax_1_3, ax_1_4, ax_2_1, ax_2_2, ax_3, ax_4) %>%
  unite(Var, var, .id) %>%
  spread(Var, val) %>%
  select(client_id, year, month, ndiag,
         date_1, ax_1_1_1, ax_1_2_1, ax_1_3_1, ax_1_4_1, ax_2_1_1, ax_2_2_1, ax_3_1, ax_4_1, gaf1_1, gaf2_1,
         date_2, ax_1_1_2, ax_1_2_2, ax_1_3_2, ax_1_4_2, ax_2_1_2, ax_2_2_2, ax_3_2, ax_4_2, gaf1_2, gaf2_2,
         date_3, ax_1_1_3, ax_1_2_3, ax_1_3_3, ax_1_4_3, ax_2_1_3, ax_2_2_3, ax_3_3, ax_4_3, gaf1_3, gaf2_3,
         date_4, ax_1_1_4, ax_1_2_4, ax_1_3_4, ax_1_4_4, ax_2_1_4, ax_2_2_4, ax_3_4, ax_4_4, gaf1_4, gaf2_4,
         date_5, ax_1_1_5, ax_1_2_5, ax_1_3_5, ax_1_4_5, ax_2_1_5, ax_2_2_5, ax_3_5, ax_4_5, gaf1_5, gaf2_5,
         date_6, ax_1_1_6, ax_1_2_6, ax_1_3_6, ax_1_4_6, ax_2_1_6, ax_2_2_6, ax_3_6, ax_4_6, gaf1_6, gaf2_6,
         date_7, ax_1_1_7, ax_1_2_7, ax_1_3_7, ax_1_4_7, ax_2_1_7, ax_2_2_7, ax_3_7, ax_4_7, gaf1_7, gaf2_7,
         date_8, ax_1_1_8, ax_1_2_8, ax_1_3_8, ax_1_4_8, ax_2_1_8, ax_2_2_8, ax_3_8, ax_4_8, gaf1_8, gaf2_8,
         date_9, ax_1_1_9, ax_1_2_9, ax_1_3_9, ax_1_4_9, ax_2_1_9, ax_2_2_9, ax_3_9, ax_4_9, gaf1_9, gaf2_9)

## ANONYMIZE
rm(patients_GIG)
load("./datasources/VAULT/clients_keytable.Rda")
patients <- patients %>% 
  left_join(clients_keytable,by="client_id") %>% 
  mutate(client_id=key) %>%
  select(-key) %>%
  arrange(client_id)
##
patient_months_diagnoses <- patient_months_diagnoses %>% 
  left_join(clients_keytable,by="client_id") %>% 
  mutate(client_id=key) %>%
  select(-key) %>%
  arrange(client_id, year, month)
##
patient_months_interventions <- patient_months_interventions %>% 
  left_join(clients_keytable,by="client_id") %>% 
  mutate(client_id=key) %>%
  select(-key) %>%
  arrange(client_id, year, month)
##
patient_months_romscores <- patient_months_romscores %>% 
  left_join(clients_keytable,by="client_id") %>% 
  mutate(client_id=key) %>%
  select(-key) %>%
  arrange(client_id, year, month)
##
rm(clients_keytable)

save(file = "data/patients_anonymised_time_to_get_real.Rda" , 
     patients, 
     patient_months_interventions,
     patient_months_diagnoses,
     patient_months_romscores)
