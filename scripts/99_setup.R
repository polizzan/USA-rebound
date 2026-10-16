####################
# INSTALL PACKAGES #
####################

## list packages to be installed from cran
from.cran <- c("ggh4x", "here", "LaF", "plyr", "svglite", "tidyverse")

## check if installed, else install
for(i in from.cran){
  
  if(system.file(package = i) == ""){install.packages(i)}
  
}

############
# SET PATH #
############

here::i_am("scripts/99_setup.R")

######################
# CREATE DIRECTORIES #
######################

if(!dir.exists(here::here("in-data"))){dir.create(here::here("in-data"))}
if(!dir.exists(here::here("out-data"))){dir.create(here::here("out-data"))}
if(!dir.exists(here::here("out-figures"))){dir.create(here::here("out-figures"))}
