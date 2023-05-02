## Script to test the HydRochemistRy package
##
##

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/"

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx")) %>%
  filter(year == 2021)

# document adding data to package
usethis::use_data_raw("testdata")

# save data to be used by the package and available to the user
example_data <- data %>%
  filter(samplecode %in% c("GW001", "GW002", "GW003", "GW004", "GW005"))
usethis::use_data(example_data)

# save testdata for tests package
test_data <- data %>%
  filter(samplecode %in% c("GW050", "GW051", "GW052", "GW053", "GW054"))
save(test_data, file = "tests/testthat/testdata.Rda")

###############################################################################
# test
###############################################################################

# helper functions
check_columns(data)

# meql function
d <- data %>%
  group_by(samplecode, parameter) %>%
  summarise(Na_present = ifelse(parameter == "Na", 1, 0)) %>%
  ungroup()

data_meq <- meql(data, corr = F)
data_meq <- meql(data)

data_meq %>% filter(check == "error")

# Descriptive Statistics
dat <- DescriptiveStatistics(data)

# PiperPlot function
plt <- PiperPlot(data)








