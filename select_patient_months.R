# Get Real 
#
# select target patient group -----------------------------------------------------------
# - patients with a primary diagnosis of "Depressieve stoornissen" 
# - who received the diagnosis in 2008 or later
# - log period: three years of treatment (after diagnosis)
# - discard patients with very little treatment service

# libraries -----------------------------------------------------------------------------
install.packages("datasources/GiG_0.18.2020.10.12.zip", repos = NULL, type = "win.binary")
library(GiG)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(openxlsx)


# patient query
#my_sys_date <- Sys.Date()  
my_sys_date <- as.Date("2020-10-12")
target_dx_major_nl <- "Depressieve stoornissen"
target_period <- 5 * 365 

# find target dx_codes
target_dx_codes <- dx_codes %>%
  # filter(major_nl == target_dx_major_nl) %>%
  filter(grepl("296.2|296.3", icd9cm_code)) %>%
  select(dx_code) %>%
  distinct(dx_code) %>%
  pull(dx_code)


# find patients
d <-
  # filter by target_dx_codes and dx date
  dx %>%
  filter(phase %in% c("admission", "intermediate")) %>%
  filter(year(date) >= 2008) %>%
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  slice(1) %>%
  filter(ax_1_1 %in% target_dx_codes) %>%
  select(client_id, ax_1_1, date) %>%
  rename(dx_date = date, dx = ax_1_1) %>%
  
  # filter by target_period
  left_join(
    clients %>%
      select(client_id, date, gender, year_of_birth) %>%
      rename(enroll_date = date),
    by = "client_id") %>%
  filter(my_sys_date - dx_date >= target_period) %>%
  select(client_id, gender, year_of_birth, enroll_date, everything()) %>%
  
  # filter patients with enrollment data > dx date
  filter(!is.na(enroll_date) & dx_date >= enroll_date) %>%
  
  # sort
  arrange(-as.numeric(enroll_date))


# add costs -----------------------------------------------------

# AH: selection on SGGZ should be included, I guess:
# ic %>% group_by(GGZ_type) %>% count(GGZ_type) 
# ic %>% subset(GGZ_type=="BGGZ")%>% group_by(year(date_start)) %>% count(GGZ_type) 
# ic <- subset(dbc, client_id %in% d$client_id & !is.na(date_stop)) 
ic <- subset(dbc, client_id %in% d$client_id & !is.na(date_stop)) %>%
 subset(GGZ_type=="SGGZ")
ic <- merge(ic, d[, c("client_id", "dx_date")])
setorder(ic, client_id, date_stop)
ic <- subset(ic, date_stop > dx_date)

ic <- ic %>% 
  group_by(client_id) %>%
  summarise(costs = sum(claimed, na.rm = TRUE))

d <- merge(d, ic)

# add treatment summary (before dx) -----------------------------------------------------
ib <- subset(interventions, client_id %in% d$client_id)

# merge dx_date
ib <- merge(ib, d[, c("client_id", "dx_date")])

setorder(ib, client_id, date)
ib[, days := as.numeric(dx_date - date), by = "client_id"]
ib <- subset(ib, date < dx_date)

ib_summary <- ib[ , 
                .(days.prev = max(days), 
                  n.prev = .N,
                  mins.prev = round(sum(mins_direct_staff + mins_indirect_staff), 0)), 
                by = "client_id"]

# merge treatment summary
# note: if NA, values are set to 0 (there was no previous treatment)
ib_summary$days.prev[is.na(ib_summary$days.prev)] <- 0
ib_summary$n.prev[is.na(ib_summary$n.prev)] <- 0
ib_summary$mins.prev[is.na(ib_summary$mins.prev)] <- 0



# treatment summary (after dx) ----------------------------------------------------------
i <- subset(interventions, client_id %in% d$client_id)

# merge dx_date
i <- merge(i, d[, c("client_id", "dx_date")])

setorder(i, client_id, date)
i[, day := as.numeric(date - dx_date), by = "client_id"]
i <- subset(i, day >= 0)
i <- subset(i, day <= target_period)

i_summary <- i[ , 
                .(days = max(day), 
                  n = .N,
                  mins = round(sum(mins_direct_staff + mins_indirect_staff), 0)), 
                by = "client_id"]

setorder(i, client_id, date)
i[, day := as.numeric(date - dx_date), by = "client_id"]



# Treatment activities (after dx): minutes & n  -----------------------------------------

# summarize treatment as (total) minutes per activity, one row per client (wide format)
# - use activity description instead of code, since multiple codes appear
# to be used for registering the same activity
# - use categorized descriptions to reduce the feature space

cat_intervention_types <- read.xlsx("datasources/intervention_types.xlsx")
cat_intervention_types$description <- iconv(cat_intervention_types$description, 
                                            "latin1", "ASCII", sub = " ")
cat_intervention_types$description <- gsub("  ", " ", 
                                           cat_intervention_types$description)

i <- merge(i, 
           cat_intervention_types[c(1, 4)], 
           by = "intervention_type_id", 
           all.x = TRUE)

# calculate n and minutes in each activity, per client
i_cat <- i[,
           .(
             n = .N,
             mins_direct = sum(mins_direct_staff, na.rm = TRUE),
             mins_indirect = sum(mins_indirect_staff, na.rm = TRUE)
           ),
           by = c(
             "client_id",
             "description_cat"
           )
]
i_cat <- i_cat[, mins := round(mins_direct + mins_indirect, 0)]
setorder(i_cat, client_id, description_cat)


# long to wide (one row per patient)
i_mins <- i_cat %>%
  select(client_id, description_cat, mins) %>%
  spread(description_cat, mins)

i_n <- i_cat %>%
  select(client_id, description_cat, n) %>%
  spread(description_cat, n)

# NA entries are zero-minute activities (i.e., the activity was not registered
# for a patient)
i_mins[is.na(i_mins)] <- 0
i_n[is.na(i_n)] <- 0

# lowercase activity labels, prepend with "n_" and "mins_"
n <- tolower(names(i_mins))[2:ncol(i_mins)]
names(i_mins)[2:ncol(i_mins)] <- paste0("mins_", n)

n <- tolower(names(i_n))[2:ncol(i_n)]
names(i_n)[2:ncol(i_n)] <- paste0("n_", n)

di_mins <- i_mins
di_n <- i_n

# At client X month level: again
# Treatment activities (after dx): minutes & n  -----------------------------------------

# summarize treatment as (total) minutes per activity, one row per client X month (wide format)
# - use activity description instead of code, since multiple codes appear
# to be used for registering the same activity
# - use categorized descriptions to reduce the feature space
#################################################################
i$year  <- year(i$date)
i$month <- month(i$date)

# calculate n and minutes in each activity, per client X month
m_i_cat <- i[,
        .(
          n = .N,
          mins_direct = sum(mins_direct_staff, na.rm = TRUE),
          mins_indirect = sum(mins_indirect_staff, na.rm = TRUE)
        ),
        by = c(
          "client_id",
          "year",
          "month",
          "description_cat"
        )
        ]
m_i_cat <- m_i_cat[, mins := round(mins_direct + mins_indirect, 0)]
setorder(m_i_cat, client_id, year, month, description_cat)

# tabelletje voor A&A 
m_i_cat %>% group_by(description_cat) %>% summarise(total=sum(mins)) %>% arrange(desc(total))


# long to wide (one row per patient-month)
m_i_mins <- m_i_cat %>%
  select(client_id, year, month, description_cat, mins) %>%
  spread(description_cat, mins)

m_i_n <- m_i_cat %>%
  select(client_id, year, month, description_cat, n) %>%
  spread(description_cat, n)

# NA entries are zero-minute activities (i.e., the activity was not registered
# for a patient)
m_i_mins[is.na(m_i_mins)] <- 0
m_i_n[is.na(m_i_n)] <- 0

# lowercase activity labels, prepend with "n_" and "mins_"
n <- tolower(names(m_i_mins))[4:ncol(m_i_mins)]
names(m_i_mins)[4:ncol(m_i_mins)] <- paste0("mins_", n)

n <- tolower(names(m_i_n))[4:ncol(m_i_n)]
names(m_i_n)[4:ncol(m_i_n)] <- paste0("n_", n)

patient_months_interventions <- merge(m_i_n, m_i_mins, by = c("client_id", "year", "month"))
rm(m_i_n, m_i_mins)

# add team info -------------------------------------------------------------------------

# calculate n, per team
i_team <- i[,
            .(
              n = .N,
              mins_direct = sum(mins_direct_staff, na.rm = TRUE),
              mins_indirect = sum(mins_indirect_staff, na.rm = TRUE)
            ),
            by = c(
              "client_id",
              "team_id"
            )
]
i_team <- i_team[, mins := round(mins_direct + mins_indirect, 0)]
setorder(i_team, client_id)

i_team <- merge(i_team, 
                teams, 
                by = "team_id", 
                all.x = TRUE, all.y = FALSE)

i_team <- subset(i_team, !is.na(team_id))

i_team <- i_team %>% 
  group_by(client_id) %>%
  mutate(max_mins = max(mins, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(max_mins == mins) %>%
  group_by(client_id) %>%
  slice(1) %>%
  select(client_id, team_id, mins, team, subdepartment, department, organisation) %>%
  rename(team.mins = mins)


# merge ---------------------------------------------------------------------------------

# previous treatment summary 
d <- merge(d, ib_summary, by = "client_id", all.x = TRUE)
# treatment summary 
# note; as a side-effect discards rows in d (i.e. patients) with no treatment information
d <- merge(d, i_summary, by = "client_id")
# dominant team
d <- merge(d, i_team, by = "client_id", all.x = TRUE)
# treatment activites
d <- merge(d, di_n, by = c("client_id"))
d <- merge(d, di_mins, by = c("client_id"))

# Final selection -----------------------------------------------------------------------
# discard patients who received very little treatment service after dx
d <- subset(d, days >= 5 & n >= 5 & mins >= 60)

## Get all GiG data of selected patients ------------------------------------------------
patients_GIG <- care_log(d$client_id)

## Save ---------------------------------------------------------------------------------
# save R data
patients <- d
save(file = "data/patients.Rda", patients_GIG, patients)
save(file = "data/patient_months_interventions.Rda" ,patient_months_interventions)

# Save GiG sample (for demo purposes)
# tmp <- care_log(
#   sample(patients$client_id, 10, replace = FALSE),
#   save_to_xls = TRUE
# )

## Clean tmp vars ----------------------------------
rm(
  cat_intervention_types,
  d,
  di_mins,
  di_n,
  tmp,
  i,
  ib,
  i_summary,
  ib_summary,
  i_mins,
  i_n,
  n,
  target_dx_codes,
  target_dx_major_nl,
  target_period
)


################################
