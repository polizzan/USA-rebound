############
# SET PATH #
############

here::i_am("scripts/00_main.R")

#################
# LOAD PACKAGES #
#################

library(LaF)
library(readxl)
library(tidyverse)

###########
# OBJECTS #
###########

age.groups <- c(0, 1, seq(5, 85, 10))

###############################
# COMBINE CAUSE OF DEATH DATA #
###############################

## the following files must already exist in the "in-data" folder
stopifnot(file.exists(here::here("in-data", "mort2019us", "VS19MORT.DUSMCPUB_r20210304")) &
            file.exists(here::here("in-data", "mort2020us", "VS20MORT.DUSMCPUB_r20220105")) &
            file.exists(here::here("in-data", "mort2021us", "VS21MORT.DUSMCPUB_r20230320.txt")) &
            file.exists(here::here("in-data", "mort2022us", "VS22MORT.DUSMCPUB_r20240307")) &
            file.exists(here::here("in-data", "mort2023us", "VS23MORT.DUSMCPUB_r20241030")) &
            file.exists(here::here("in-data", "mort2024us", "VS24MORT.DUSMCPUB_r20251208")))

## combine and save death files
source(here::here("scripts", "01_deaths.R"))
  
######################################
# DOWNLOAD AND COMBINE EXPOSURE DATA #
######################################

walk(2019:2024, function(x){
  
  if(x == 2019){
    
    url <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/national/asrh/nc-est2019-syasexn.xlsx"
    destfile <- here::here("in-data", "nc-est2019-syasexn.xlsx")    
    download.file(url, destfile, overwrite = TRUE, mode = "wb")
    
  }
  
  if(x == 2020){
    
    url <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/asrh/nc-est2020-agesex-res.csv"
    destfile <- here::here("in-data", "nc-est2020-agesex-res.csv")    
    download.file(url, destfile, overwrite = TRUE)
    
  }
  
  if(x %in% 2021:2023){
  
  url <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-", x, "/national/asrh/nc-est", x, "-agesex-res.csv")
  destfile <- here::here("in-data", paste0("nc-est", x, "-agesex-res.csv"))
  download.file(url, destfile, overwrite = TRUE)
  
  }
  
  if(x %in% 2024){
    
    url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2024/national/asrh/nc-est2024-syasexn.xlsx"
    destfile <- here::here("in-data", "nc-est2024-syasexn.xlsx")    
    download.file(url, destfile, overwrite = TRUE, mode = "wb")
    
  }
  
})

## combine and save exposure files
source(here::here("scripts", "02_exposures.R"))

############################################
# DOWNLOAD AND PREPARE STANDARD POPULATION #
############################################

## download standard population from SEER
download.file("https://seer.cancer.gov/stdpopulations/singleages.pops.tables.csv", 
              here::here("in-data", "seer-standard-population.csv"), 
              overwrite = TRUE)

## group standard population into ten-year age groups
US.standard.2000 <-
  read.csv(here::here("in-data", "seer-standard-population.csv"), skip = 1) %>% 
  slice_head(n = 86) %>% 
  mutate(Age = c(0, rep(1, 4), rep(seq(5, 75, 10), each = 10), 85),
         Standard = as.numeric(str_replace_all(Single.Ages.to.84, ",", ""))) %>% 
  summarize(Standard = sum(Standard), .by = Age) %>% 
  mutate(C = Standard / sum(Standard))

## save grouped standard population
write.csv(US.standard.2000, here::here("out-data", "US-standard-2000.csv"), row.names = FALSE)

####################################
# CALCULATE ASDRS AND PLOT RESULTS #
####################################

source(here::here("scripts", "03_ASDR.R"))
source(here::here("scripts", "04_plot.R"))
