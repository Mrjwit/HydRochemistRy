#' meql
#'
#' Function to convert hydrochemical dataset with concentrations in wide format to meq/L
#' and calculate Electrical Balance. Concentrations need to be in mg/L.
#'
#' @param d dataset of water quality in long format
#' @param corr correction option for converting alkalinity as CaCO3 to HCO3
#'
#' @return dataset of concentrations in meq/L in wide format with Electrical Balance check
#' @export
#'
#' @import dplyr
#'
meql <- function(d, corr = T) {

  # check columns from helper function in utils.R
  check_columns(d)
  print("columns are okay")

  # check parameters
  if(length(setdiff(valid_parameters(), unique(d$parameter))) > 0) {
    stop("Some major ions are missing: ", paste(valid_parameters(), collapse = ", "))
  }

  d_meql <- d %>%
    # only select main anions + cations
    dplyr::filter(parameter %in% c("Cl", "HCO3", "SO4", "NO3",
                                   "Na", "Ca", "Mg", "K", "NH4", "EC")) %>%
    # put all units in mg/L if not already
    dplyr::mutate(value = dplyr::case_when(
      units == "ug/l" ~ value * 1000, # or | "ug/L"
      TRUE ~ value )) %>%
    # calculate meq/l
    dplyr::mutate(meql = dplyr::case_when(
      parameter == "Cl" ~ value / 35.453,
      parameter == "HCO3" ~ value / 61.0168,
      parameter == "NO3" ~ value / 62.0049,
      parameter == "PO4" ~ value / 94.9714 * 3,
      parameter == "SO4" ~ value / 96.06 * 2,
      parameter == "Na" ~ value / 22.989769,
      parameter == "Ca" ~ value / 40.078 * 2,
      parameter == "Mg" ~ value / 24.305 * 2,
      #parameter == "Fe" ~ value / 55.845 * 2,
      parameter == "K" ~ value / 39.0983,
      parameter == "NH4" & limit_symbol != "<" ~ value / 18.04,
      parameter == "EC" ~ value,
      TRUE ~ NA_real_ )) %>%
    # select relevant columns
    dplyr::select(samplecode, parameter, meql) %>%
    # put from long to wide format
    tidyr::pivot_wider(names_from = parameter,
                       values_from = meql) %>%
    dplyr::filter(!is.na(Cl))

  # correct Alkalinity titration expressed as CaCO3 to HCO3
  if(corr == T) {
    d_meql$HCO3 <- d_meql$HCO3 * 1.22
  }

  d_meql <- d_meql %>%
    # calculate electrical balance
    dplyr::mutate(sum_an = rowSums(dplyr::select(., Cl, HCO3, NO3, SO4), na.rm = T),
                  sum_cat = rowSums(dplyr::select(., Na, Ca, Mg, K, NH4), na.rm = T),
                  `EC/100` = EC / 100) %>% # this is only valid up to 1500 uS/cm !
    dplyr::mutate(EB = ((sum_cat - sum_an) / (sum_cat + sum_an)) * 100) %>%
    dplyr::mutate(check = ifelse(EB < -10 | EB > 10, "error", "good")) %>%
    # put columns in order with anions, cations, results
    dplyr::select(samplecode, EC, Cl, HCO3, SO4, NO3,
                  Na, Ca, Mg, K, NH4,
                  sum_an, sum_cat, `EC/100`, EB, check)

  print("Table with meq/l constructed")

  return(d_meql)

}
