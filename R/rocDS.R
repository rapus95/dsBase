#'
#' @title Computes statistical mean of a vectores
#' @description Calculates the mean value.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned.
#' @param xvect a vector
#' @return a numeric, the statistical mean
#' @author Matthis A
#' @export
#'
rocDS <- function(prediction, reference, breaks){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  # thr <- listDisclosureSettingsDS()
  # nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  res <- roc(reference, prediction)

  out.specificities <- res$specificities
  out.sensitivities <- res$sensitivities

  out.obj <- list(Specificities=out.specificities,Sensitivities=out.sensitivities)
  return(out.obj)

}
#AGGREGATE FUNCTION
# rocDS
