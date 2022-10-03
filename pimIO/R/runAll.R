#' Run all.
#'
#' Run all series through the PIM.
#'
#' @section Data Specification:
#' The .data input data.frame should consist of one row per Sector/Industry/Asset
#' with a nested "data" column containing the data for that series, and a nested
#' "config" column containing the output from pimir::pimConfig for that series.
#' For example, have the following structure:
#'
#' \tabular{rrrrrr}{
#' Sector \tab Industry \tab Asset \tab data \tab config \tab refYear\cr
#' S.11001 \tab 02 \tab OTHER.BUILDINGS \tab <data.frame> \tab <list> \tab Y2014 \cr
#' S.11001 \tab 02 \tab OTHER.STRUCTURES \tab <data.frame> \tab <list> \tab Y2014 \cr
#' ... \tab ... \tab ... \tab ... \tab ... \tab ... \cr
#' }
#'
#' The nested "data" column should have the following structure:
#'
#' \tabular{lrrrrrrrrrrrrrrrr}{
#' Vintage \tab gfcfCP \tab PriceIndex \tab Average \tab CoV \tab Max \tab Min \tab K1CP \tab K3CP \tab K4CP \tab K5CP \tab K61CP \tab K62CP \tab CPI \tab TaxAdjustmentFactor \tab UTIL  \cr
#' Y1948Q1 \tab 10 \tab 0.050 \tab 80 \tab 0.15 \tab 128 \tab 4 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0.04 \tab 1.04 \tab 1 \cr
#' Y1948Q2 \tab 12 \tab 0.057 \tab 80 \tab 0.15 \tab 128 \tab 4 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0.04 \tab 1.04 \tab 1 \cr
#' ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \cr
#' }

#'
#' The data.frame should also contain a "refYear" column used in the calculation of
#' Capital Services.
#'
#' @param .data A data.frame with one row per Sector/Asset/Industry with a "data"
#' list-column containing GFCF, a "config" list-column, and a refYear column. See details below.
#' @param parallelise Set to FALSE to turn off parallel processing.
#' @return The input data.frame with an addional "result" list-column containing the PIM results.
#' @import doSNOW parallel
#' @importFrom foreach foreach "%dopar%" "%do%"
#' @export
runAll <- function(df, parallelise = TRUE) {

  # --- Check Inputs -----------------------------------------------------------
  # Check main data.frame contains expected columns
  expectedCols <- c("Sector", "Industry", "Asset", "data", "config", "refYear")
  unmatchedCols <- !(expectedCols %in% colnames(df))
  if (any(unmatchedCols)) {
    stop(paste("Incorrect column names. Could not find",
                 paste(expectedCols[unmatchedCols], collapse = ", ")))
  }

  # Check nested "data" is a data.frame

  # Check nested "data" contains expected columns
  expectedCols <- c("Vintage", "gfcfCP", "PriceIndex", "Average", "CoV", "Max",
                    "Min", "K1CP", "K3CP", "K4CP", "K5CP", "K61CP", "K62CP",
                    "CPI", "TaxAdjustmentFactor", "UTIL")
  unmatchedCols <- !(expectedCols %in% colnames(as.data.frame(df[[1, "data"]])))
  if (any(unmatchedCols)) {
    stop(paste("Incorrect column names in nested \"data\"\ column. Could not find",
               paste(expectedCols[unmatchedCols], collapse = ", ")))
  }

  # --- Set up parallel processing ---------------------------------------------
  `%doOrDoPar%` <- `%do%`  # define default iterator to be sequential
  # Set up parallel processing for multiple cores (use all except one)
  if (parallelise) {
    `%doOrDoPar%` <- `%dopar%`
    availCores <- parallel::detectCores()
    nCores <- dplyr::case_when(
      is.na(availCores) ~ 1,
      availCores > 1 ~ availCores - 1,
      TRUE ~ 1)
    # Register the cluster
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    # Pass libPath to workers
    clusterCall(cl, function(x) .libPaths(x), .libPaths())
  }

  # PIM starting time

  print(paste0("PIM started running: " ,Sys.time()))

  # Run the PIM for each series (i)
  x <- dplyr::mutate(df, result = foreach(i = seq_len(nrow(df)),
                                          .packages = c("dplyr"),
                                          .errorhandling = "pass") %do%
                       pimIO::run(inputData = as.data.frame(df[[i, "data"]]),
                                  config = df[[i, "config"]][[1]],
                                  referenceYear = df[[i, "refYear"]]))

  if (parallelise) parallel::stopCluster(cl)

  print(paste0("PIM finished running: " ,Sys.time()))

  return(x)

}
