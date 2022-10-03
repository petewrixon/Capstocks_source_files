#' Pre-process Input Data.
#'
#' Pre-process the time series of gfcf and other changes in volume. This includes
#' calculating GFCF and K-Value adjustments in CVM using the provided Price Index.
#'
#' @param timeSeries a data.frame with a column for GFCF in CP (gfcfCP), a Price
#' Index (PriceIndex), K.1 in CP (K1CP), K.3 in CP (K3CP), K.4 in CP (K4CP),
#' K.5 in CP (K5CP), K.61 in CP (K61CP) and K.62 in CP (K62CP)
#'
#' @return the given data.frame with the addition of CVM variables
#' @export
preProcess <- function(timeSeries){

  result <- timeSeries %>%
    # Calculate CVM values using provided PriceIndex
    dplyr::mutate(gfcfCVM = gfcfCP/PriceIndex,
                  K1CVM = K1CP/PriceIndex,
                  K3CVM = K3CP/PriceIndex,
                  K4CVM = K4CP/PriceIndex,
                  K5CVM = K5CP/PriceIndex,
                  K61CVM = K61CP/PriceIndex,
                  K62CVM = K62CP/PriceIndex) %>%
    # Sum GFCF and other changes in volume in CVM
    dplyr::mutate(gfcf_ociv = gfcfCVM + K1CVM + K3CVM + K4CVM + K5CVM +
                    K61CVM + K62CVM)

  return(result)
}
