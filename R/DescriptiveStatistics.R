#' DescriptiveStatistics
#'
#' Function to return descriptive statistics of hydrochemical data per parameter (Cl, Na, etc)
#'
#' @param d dataset of water quality in long format
#'
#' @return dataset showing descriptive statistics per parameter:
#' - number of measurements per parameter
#' - detection limit of parameter (if applicable)
#' - percentage of measurements per parameter that is below the detection limit
#' - mean, median, min, max and stdev of values per parameter
#' - skewness of measurements per parameter
#'
#' @export
#' @import moments
#'
DescriptiveStatistics <- function(d) {

  # check columns from helper function in utils.R
  check_columns(d)

  # check parameters
  if(length(setdiff(valid_parameters(), unique(d$parameter))) > 0) {
    warning("Some major ions are missing: ", paste(valid_parameters(), collapse = ", "))
  }

  # check NA values
  #

  dat <- d %>%
    dplyr::select(samplecode, parameter, value, limit_symbol, detection_limit, units) %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(units = unique(units),
              n = dplyr::n(),
              #dl = paste(unique(detection_limit, collapse = ", "),
              dl = ifelse(unique(is.na(detection_limit)), NA, unique(min(detection_limit, na.rm = T))),
              `% <dl` = round(length(value[limit_symbol == "<"]) / length(value) * 100, digits = 1),
              mean = round(mean(value, na.rm = T), digits = 1),
              median = round(median(value, na.rm = T), digits = 1),
              min = round(min(value, na.rm = T), digits = 1),
              max = round(max(value, na.rm = T), digits = 1),
              sd = round(sd(value, na.rm = T), digits = 1),
              skewness = round(moments::skewness(value, na.rm = T), digits = 1))

  return(dat)
}
