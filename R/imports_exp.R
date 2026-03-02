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
#'   \item{inf_a}{Probability of animal being infected}
#'   \item{false_neg_a}{Probability of false negative test result}
#'   \item{no_test_a}{Probability of no testing}
#'   \item{no_detect_a}{Overall probability of non-detection}
#' }
"imports_exp" <- quote({
  # Probability that an animal in an infected herd is infected (a = an animal)
  inf_a <- w_prev

  # Probability an animal is tested and is a false negative (test specificity assumed to be 100%)
  false_neg_a <- inf_a * test_origin * (1 - test_sensi)

  # Probability an animal is not tested
  no_test_a <- inf_a * (1 - test_origin)

  # Probability an animal is not detected
  no_detect_a <- false_neg_a + no_test_a
})
