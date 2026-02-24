###############
# LOAD DEATHS #
###############

deaths.raw <- read.csv(here::here("out-data", "deaths-2019-2024.csv"))

##################
# LOAD EXPOSURES #
##################

exposures <- read.csv(here::here("out-data", "exposures-2019-2024.csv"))

############################
# LOAD STANDARD POPULATION #
############################

standard <- read.csv(here::here("out-data", "US-standard-2000.csv"))

####################
# DATA AGGREGATION #
####################

## generate mutually exclusive external-cause categories
## all external causes reported
external <-
  deaths.raw %>% 
  filter(substr(Cause, 1, 3) %in% c("U01", "U02", "U03") | substr(Cause, 1, 1) %in% c("V", "W", "X", "Y")) %>% 
  pull(Cause) %>% 
  unique()

## external deaths that are:
## 1) drug overdoses
drugs <-
  external[substr(external, 1, 3) %in% c(paste0("X", c(40:44, 60:64, 85)), paste0("Y", 10:14))]

## 2) alcohol poisonings
alcohol <- external[substr(external, 1, 3) %in% c("X45", "X65", "Y15")]

## 2) unintentional injuries
unintentional.injuries <-
  external[substr(external, 1, 1) %in% c("V", "W") | substr(external, 1, 3) %in% c(paste0("X0", 0:9), paste0("X", 10:59), "Y85", "Y86")]

## 3) suicides
suicide <-
  external[substr(external, 1, 3) %in% c("U03", paste0("X", 60:84)) | external == "Y870"]

## 4) no unintentional injuries / suicides
other.external <-
  external[!external %in% c(unintentional.injuries, suicide)]

## generate mutually exclusive categories
unintentional.injuries.drugs <-
  unintentional.injuries[unintentional.injuries %in% drugs]

suicide.drugs <-
  suicide[suicide %in% drugs]

other.external.drugs <-
  other.external[other.external %in% drugs]

unintentional.injuries.alcohol <-
  unintentional.injuries[unintentional.injuries %in% alcohol]

suicide.alcohol <-
  suicide[suicide %in% alcohol]

other.external.alcohol <-
  other.external[other.external %in% alcohol]

unintentional.injuries.no.substance <-
  unintentional.injuries[!unintentional.injuries %in% c(drugs, alcohol)]

suicide.no.substance <-
  suicide[!suicide %in% c(drugs, alcohol)]

other.external.no.substance <-
  other.external[!other.external %in% c(drugs, alcohol)]

## other drug-induced mortality
other.drugs <-
  c("D521",
    "D590",
    "D592",
    "D611",
    "D642",
    "E064",
    "E160",
    "E231",
    "E242",
    "E273",
    "E661",
    paste0("F11", c(1:5, 7:9)), 
    paste0("F12", c(1:5, 7:9)), 
    paste0("F13", c(1:5, 7:9)),     
    paste0("F14", c(1:5, 7:9)),
    paste0("F15", c(1:5, 7:9)),
    paste0("F16", c(1:5, 7:9)),
    paste0("F17", c(3:5, 7:9)),
    paste0("F18", c(1:5, 7:9)),
    paste0("F19", c(1:5, 7:9)),
    "G211", ## take out of 'neurodegenerative diseases'
    "G240",
    "G251",
    "G254",
    "G256",
    "G444",
    "G620",
    "G720",
    "I952",
    "J702",
    "J703",
    "J704",
    "K853",
    "L105",
    "L270",
    "L271",
    "M102",
    "M320",
    "M804",
    "M814",
    "M835",
    "M871",
    "R502",
    "R781",
    "R782",
    "R783",
    "R784",
    "R785")

## other alcohol-induced mortality
other.alcohol <-
  c("E244",
    paste0("F10", 0:9),
    "G312", ## take out of 'neurodegenerative diseases'
    "G621",
    "G721",
    "I426", ## take out of 'heart disease'
    "K292",
    paste0("K70", 0:9),
    "K852",
    "K860",
    "R780")

## aggregate deaths by broader age groups and cause categories
deaths.group <-
  deaths.raw %>%  
  ## age grouping
  mutate(Age = case_when(is.na(Age) ~ NA,
                         Age == 0 ~ 0,
                         Age %in% 1:4 ~ 1,
                         Age %in% 5:14 ~ 5,
                         Age %in% 15:24 ~ 15,
                         Age %in% 25:34 ~ 25,
                         Age %in% 35:44 ~ 35,
                         Age %in% 45:54 ~ 45,
                         Age %in% 55:64 ~ 55,
                         Age %in% 65:74 ~ 65,
                         Age %in% 75:84 ~ 75,
                         Age >= 85 ~ 85)) %>%
  ## cause grouping
  mutate(Cause = case_when(Cause %in% suicide.drugs ~ "Suicide, drugs only",
                           Cause %in% suicide.alcohol ~ "Suicide, alcohol only",
                           Cause %in% suicide.no.substance ~ "Suicide, excl. substances",
                           Cause %in% unintentional.injuries.drugs ~ "Unintentional injuries, drugs only",
                           Cause %in% unintentional.injuries.alcohol ~ "Unintentional injuries, alcohol only",
                           Cause %in% unintentional.injuries.no.substance ~ "Unintentional injuries, excl. substances",
                           Cause %in% other.external.drugs ~ "Other external, drugs only",
                           Cause %in% other.external.alcohol ~ "Other external, alcohol only",
                           Cause %in% other.external.no.substance ~ "Other external, excl. substances",
                           Cause %in% other.drugs ~ "Other drug-induced",
                           Cause %in% other.alcohol ~ "Other alcohol-induced",
                           substr(Cause, 1, 3) %in% paste0("A", 40:41) ~ "Septicemia",
                           substr(Cause, 1, 3) %in% c(paste0("C0", 0:9), paste0("C", 10:97)) ~ "Cancer",
                           substr(Cause, 1, 3) %in% paste0("E", 10:14) ~ "Diabetes",
                           substr(Cause, 1, 3) %in% c("F01", "F03", "G20", "G21", "G30", "G31") & !Cause %in% c("G211", "G312") ~ "Neurodegenerative diseases",
                           substr(Cause, 1, 3) %in% c(paste0("I0", 0:9), "I11", "I13", paste0("I", 20:51)) & Cause != "I426" ~ "Heart disease",
                           substr(Cause, 1, 3) %in% paste0("I", 60:69) ~ "Stroke",
                           substr(Cause, 1, 3) %in% c("I10", "I12", "I15", paste0("I", 70:78)) ~ "Hypertension and arterial diseases",
                           substr(Cause, 1, 3) %in% c("J09", paste0("J", c(10:18, 40:47))) ~ "Respiratory diseases",
                           substr(Cause, 1, 3) %in% paste0("K", c(73, 74)) ~ "Other chronic liver disease and cirrhosis",
                           substr(Cause, 1, 3) %in% c(paste0("N0", 0:7), paste0("N", c(17:19, 25:27))) ~ "Kidney disease",
                           Cause == "U071" ~ "COVID-19",
                           TRUE ~ "Residual")) 

## aggregation by age and cause
deaths.aux <- 
  deaths.group %>%
  summarize(Deaths = sum(Deaths), .by = c(Year, Sex, Age, Cause))

## add total death counts across sex
deaths <- 
  deaths.aux %>% 
  add_row(
    
    deaths.aux %>% 
      summarize(Deaths = sum(Deaths), .by = c(Year, Age, Cause)) %>% 
      mutate(Sex = "Total")
    
  ) 

## save grouped deaths
write.csv(deaths, here::here("out-data", "deaths-2019-2024-grouped.csv"), row.names = FALSE)

###################
# CALCULATE ASDRS #
###################

ASDRs <-
  deaths %>% 
  left_join(exposures) %>% 
  left_join(standard) %>% 
  summarize(Mx = sum(C * Deaths / Exposures, na.rm = TRUE) * 100000, .by = c(Year, Sex, Cause)) %>% 
  arrange(Year, Sex, Cause)

##############
# SAVE ASDRS #
##############

write.csv(ASDRs, here::here("out-data", "ASDRs-2019-2024.csv"), row.names = FALSE)
