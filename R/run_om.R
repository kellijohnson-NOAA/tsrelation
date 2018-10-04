#' Simulate multivariate time-series data
#'
#' todo: details go here
#'
#' @param ompars A dataframe of parameter values for the OM.
#' Each row specifies a single scenario.
#' @template dir
#'
#' @return todo: document this further.
#' @examples
#'
#' @author Kelli Faye Johnson, \email{kelli.johnson@@noaa.gov}
#'
run_om <- function(ompars = NULL, dir = getwd()) {

  omparsall <- data.frame(
    "iterations" = 1,
    "nt" = 100,
    "B_1_1" = 0.5,
    "B_1_2" = 0.0,
    "B_2_1" = 0.0,
    "B_2_2" = 0.5,
    "error_obs" = 0.0,
    "W_1_1" = 0.0,
    "W_1_2" = 0.0,
    "W_2_1" = 0.0,
    "W_2_2" = 0.0,
    "scale" = TRUE,
    "dir" = dir
    )

  if (is.null(ompars)) {
    ompars <- omparsall
  } else {
    ompars <- cbind(ompars, omparsall[, !names(omparsall) %in% names(ompars)])
    ompars <- ompars[, c(colnames(omparsall), colnames(ompars)[(!colnames(ompars) %in% colnames(omparsall))])]
  }

  run_om_iteration <- function(ininfo) {
    myname <- "tsrelation_om"
    omout <- file.path(ininfo["dir"], myname,
      length(dir(ininfo["dir"], pattern = myname)) + 1)
    dir.create(omout, recursive = TRUE, showWarnings = FALSE)
    Bnames <- grep("B_", names(ininfo))
    nn <- length(Bnames) / 2
    Bmatrix <- matrix(
      as.numeric(ininfo[Bnames]),
      nrow = nn, ncol = nn)
    Wmatrix <- matrix(
      as.numeric(ininfo[grep("W_", names(ininfo))]),
      nrow = nn, ncol = nn)
    out <- simTVVAR(TT = as.numeric(ininfo["nt"]),
      Bt = Bmatrix,
      QQ_XX = Wmatrix,
      var_QX = 0, cov_QX = 0, var_QB = 0,
      scalevariance = if(ininfo["scale"]) Wmatrix else NULL)
    save(out, file = file.path(omout, "tsrelation_om.RData"))
  }
  ignore <- apply(ompars, 1, run_om_iteration)
  return(NULL)
}
