#'
#'  Calculate conditional probability
#'
#'  This returns the conditional probability: Pr(succ | trial).
#'
#'  Without the `trials` argument, this becomes
#'  a simple probability calculation.
#'
#'  In any event, the output include lower and upper
#'  confidence bounds.
#'
#' @param succ Vector where `succ[i]` is `TRUE`
#'   if trial `i` succeeded (logical)
#' @param trials Mask indicating with elements of `succ`
#'   are the relevent trials; NULL means all elements are relevent
#'   (logical)
#' @param ... Passed to `prop.test` function
#'   (e.g., `conf.level` or `alternative`)
#'
#' @return Returns a tibble with three columns
#' * `CondProb` - conditional probability
#' * `Low` - lower limit of confidence interval
#' * `High` - upper limit of confidence interval
#'
#' @export
#'
condProb = function(succ, trials = NULL, ...) {
  decl(succ, is.logical)
  decl(trials, is.null %or% is.logical)

  if (is.null(trials)) {
    nSucc = sum(succ, na.rm = TRUE)
    nTrials = length(na.omit(succ))
  } else {
    nSucc = sum(succ & trials, na.rm = TRUE)
    nTrials = sum(trials, na.rm = TRUE)
  }

  if (is.na(nTrials) || is.na(nSucc) || nTrials == 0) {
    return(tibble::tibble(CondProb = NA_real_, Low = NA_real_, High = NA_real_ ))
  }

  (prop.test(nSucc, nTrials, ...)
    |> broom::tidy()
    |> dplyr::transmute(CondProb = estimate,
                         Low = conf.low,
                         High = conf.high) )
}
