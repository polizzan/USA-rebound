#######################
# LOAD 2019 EXPOSURES #
#######################

## total
exposures.2019.t <-
  read_excel(here::here("in-data", "nc-est2019-syasexn.xlsx"),
             range = c("M6:M106"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Total")

## male
exposures.2019.m <-
  read_excel(here::here("in-data", "nc-est2019-syasexn.xlsx"),
             range = c("M110:M210"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Male")

## female
exposures.2019.f <-
  read_excel(here::here("in-data", "nc-est2019-syasexn.xlsx"),
             range = c("M214:M314"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Female")  

## combine
exposures.2019 <-
  bind_rows(exposures.2019.t, 
            exposures.2019.m, 
            exposures.2019.f) %>% 
  mutate(Year = 2019)

#######################
# LOAD 2020 EXPOSURES #
#######################

exposures.2020 <-
  read.csv(here::here("in-data", "nc-est2020-agesex-res.csv")) %>% 
  ## exclude total exposures across all ages
  filter(AGE != 999) %>% 
  select(Sex = SEX, Age = AGE, Exposures = POPESTIMATE2020) %>% 
  ## recode sex variable
  mutate(Sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", "Total")),
         Year = 2020)

#######################
# LOAD 2021 EXPOSURES #
#######################

exposures.2021 <-
  read.csv(here::here("in-data", "nc-est2021-agesex-res.csv")) %>% 
  ## exclude total exposures across all ages
  filter(AGE != 999) %>% 
  select(Sex = SEX, Age = AGE, Exposures = POPESTIMATE2021) %>% 
  ## recode sex variable
  mutate(Sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", "Total")),
         Year = 2021)

#######################
# LOAD 2022 EXPOSURES #
#######################

exposures.2022 <-
  read.csv(here::here("in-data", "nc-est2022-agesex-res.csv")) %>% 
  ## exclude total exposures across all ages
  filter(AGE != 999) %>% 
  select(Sex = SEX, Age = AGE, Exposures = POPESTIMATE2022) %>% 
  ## recode sex variable
  mutate(Sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", "Total")),
         Year = 2022)

#######################
# LOAD 2023 EXPOSURES #
#######################

exposures.2023 <-
  read.csv(here::here("in-data", "nc-est2023-agesex-res.csv")) %>% 
  ## exclude total exposures across all ages
  filter(AGE != 999) %>% 
  select(Sex = SEX, Age = AGE, Exposures = POPESTIMATE2023) %>% 
  ## recode sex variable
  mutate(Sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", "Total")),
         Year = 2023)

#######################
# LOAD 2024 EXPOSURES #
#######################

## total
exposures.2024.t <-
  read_excel(here::here("in-data", "nc-est2024-syasexn.xlsx"),
             range = c("G6:G106"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Total")

## male
exposures.2024.m <-
  read_excel(here::here("in-data", "nc-est2024-syasexn.xlsx"),
             range = c("G110:G210"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Male")

## female
exposures.2024.f <-
  read_excel(here::here("in-data", "nc-est2024-syasexn.xlsx"),
             range = c("G214:G314"), 
             col_names = "Exposures") %>%
  mutate(Age = 0:100,
         Sex = "Female")  

## combine
exposures.2024 <-
  bind_rows(exposures.2024.t, 
            exposures.2024.m, 
            exposures.2024.f) %>% 
  mutate(Year = 2024)

#####################
# COMBINE EXPOSURES #
#####################

exposures <-
  bind_rows(exposures.2019,
            exposures.2020,
            exposures.2021,
            exposures.2022,
            exposures.2023,
            exposures.2024) %>% 
  ## aggregate exposures into broader age groups
  mutate(Age = cut(Age,
                   breaks = c(age.groups, Inf), 
                   labels = age.groups,
                   include.lowest = FALSE, 
                   right = FALSE)) %>%
  summarize(Exposures = sum(Exposures), .by = c(Year, Sex, Age)) %>% 
  mutate(Age = as.numeric(as.character(Age))) %>% 
  arrange(Year, Sex, Age)

##################
# SAVE EXPOSURES #
##################

write.csv(exposures, here::here("out-data", "exposures-2019-2024.csv"), row.names = FALSE)
