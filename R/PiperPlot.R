#' PiperPlot
#'
#' Function to produce piper diagram that displays water types of hydrochemical data.
#' Input concentrations need to be in mg/L.
#'
#' Piper diagram functions adjusted from https://rstudio-pubs-static.s3.amazonaws.com/542159_ae160ba405044883a58ba3a53e4f7e6d.html
#' to include option for adding NO3 and water type legend to the ggpiper_plot object.
#'
#' Not fully functional and thoroughly tested!!
#'
#' @param d dataset of water quality in wide format including Cl, HCO3, SO4, Na, Ca, Mg, K
#' @param NO3 bolean (TRUE/FALSE) to indicate whether NO3 should be included in the percentage contribution calculation and the piper plot. Standard is False
#'
#' @return piper diagram ggplot object
#' @export
#'
PiperPlot <- function(d, NO3 = FALSE) {

  #### Piper diagram ####
  # functions for piper diagram https://rstudio-pubs-static.s3.amazonaws.com/542159_ae160ba405044883a58ba3a53e4f7e6d.html

  # Step 1 - takes as input a dataframe with cation and anion concentrations in meql and converts them to percentages
  percents <- toPercent(d, NO3 = FALSE)

  # Step 2 - takes percentage data and converts them to xy coordinates for plotting on Piper diagram
  piper.data <- transform_piper_data(percents)

  # Step 3 - Plot piper diagram
  p <- ggplot_piper(piper.data, output = "ggplot")

  return(p)
}
