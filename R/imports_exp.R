#' Expression for Calculating Import Infection Probability
#'
#' @description
#' A quoted R expression that calculates the probability of importing an infected
#' animal from an infected herd, taking into account testing procedures and accuracy.
#'
#' @format A quoted R expression containing the following variables:
#' \describe{
#'   \item{w_prev}{Within-herd prevalence}
#'   \item{test_origin}{Probability of testing at origin}
#'   \item{test_sensi}{Test sensitivity}
#'   \item{infected}{Probability of animal being infected}
#'   \item{false_neg}{Probability of false negative test result}
#'   \item{no_test}{Probability of no testing}
#'   \item{no_detect}{Overall probability of non-detection}
#' }
"imports_exp" <- quote({
  # Probability that an animal in an infected herd is infected (a = an animal)
  infected <- w_prev

  # Probability an animal is tested and is a false negative (test specificity assumed to be 100%)
  false_neg <- infected * test_origin * (1 - test_sensi)

  # Probability an animal is not tested
  no_test <- infected * (1 - test_origin)

  # Probability an animal is not detected
  no_detect <- false_neg + no_test
})
