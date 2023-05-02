## code to prepare `exampledata` and `testdata` dataset goes here

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/"

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx")) %>%
  dplyr::filter(year == 2021)

# write testdata to be used by the package and available to the user
example_data <- data %>%
  dplyr::filter(samplecode %in% c("GW001", "GW002", "GW003", "GW004", "GW005",
                                  "WW001", "SW001", "SR001", "TW001", "RW001", "SEA001"),
                parameter %in% c("EC", "Cl", "HCO3", "SO4", "NO3", "PO4", "Na", "Ca", "Mg", "K", "NH4", "Fe"))

# write testdata to be used by the tests and unavailable to the user
test_data <- data %>%
  dplyr::filter(samplecode %in% c("GW011", "GW012", "GW013", "GW015", "GW016",
                                  "GW017", "GW018", "GW019", "GW021", "GW022"),
                parameter %in% c("EC", "Cl", "HCO3", "SO4", "NO3", "PO4", "Na", "Ca", "Mg", "K", "NH4", "Fe")) %>%
  #change some aspects to produce errors whilst testing
  dplyr::mutate(parameter = dplyr::case_when(
    # parameter name wrong in all samples
    parameter == "EC" ~ "EC_uS",
    # parameter name wrong in 1 sample only
    parameter == "Cl" & samplecode == "GW011" ~ "cl",
    TRUE ~ parameter)) %>%
  dplyr::mutate(value = ifelse(samplecode == "GW012" & parameter %in% c("Na", "SO4"),
                               NA, value))

# save .RDS file for in the package to be used by users
usethis::use_data(example_data, overwrite = TRUE)

# save .RDS file for testing fucntions, unavailable to users
save(test_data, file = "tests/testthat/test_data.Rda")


# prepare_data <- function() {
#
#   # set data file location
#   input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/"
#
#   # load cleaned data
#   data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx")) %>%
#     dplyr::filter(year == 2021)
#
#   # write testdata to be used by the package and available to the user
#   example_data <- data %>%
#     dplyr::filter(samplecode %in% c("GW001", "GW002", "GW003", "GW004", "GW005",
#                                     "WW001", "SW001", "SR001", "TW001", "RW001", "SEA001"),
#                   parameter %in% c("EC", "Cl", "HCO3", "SO4", "NO3", "PO4", "Na", "Ca", "Mg", "K", "NH4", "Fe"))
#
#   # write testdata to be used by the tests and unavailable to the user
#   test_data <- data %>%
#     dplyr::filter(samplecode %in% c("GW011", "GW012", "GW013", "GW015", "GW016",
#                                     "GW017", "GW018", "GW019", "GW021", "GW022"),
#                   parameter %in% c("EC", "Cl", "HCO3", "SO4", "NO3", "PO4", "Na", "Ca", "Mg", "K", "NH4", "Fe")) %>%
#     #change some aspects to produce errors whilst testing
#     dplyr::mutate(parameter = dplyr::case_when(
#       # parameter name wrong in all samples
#       parameter == "EC" ~ "EC_uS",
#       # parameter name wrong in 1 sample only
#       parameter == "Cl" & samplecode == "GW011" ~ "cl",
#       TRUE ~ parameter)) %>%
#     dplyr::mutate(value = ifelse(samplecode == "GW012" & parameter %in% c("Na", "SO4"),
#                                  NA, value))
#
#   # save .RDS file for in the package to be used by users
#   usethis::use_data(example_data, overwrite = TRUE)
#
#   # save .RDS file for testing fucntions, unavailable to users
#   save(test_data, file = "tests/testthat/test_data.Rda")
#
# }
