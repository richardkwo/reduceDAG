#' The g-formula for identifying the counterfactual mean
#'
#' The g-formula (Robins, 1986) for the counterfactual mean of the outcome when
#' level \code{a} is imposed on the exposure.
#'
#' @param g A directed acyclic graph of class \code{dagitty}. A single vertex
#' should be labeled as \code{exposure} and another vertex should be labeled
#' as \code{outcome}. Use \code{\link[dagitty]{dagitty}} to create this object.
#'
#' @return Character string for the formula
#' @examples
#' data("g.example")
#' # compare:
#' gFormula(g.example)
#' gFormula(reduceDAG(g.example, verbose=FALSE))
#' @export
#' @references
#' James M. Robins. "A new approach to causal inference in mortality studies with
#' a sustained exposure period—application to control of the healthy worker
#' survivor effect." Mathematical modelling 7.9-12 (1986): 1393-1512.
#'
#' Thomas S. Richardson and James M. Robins. "Single world intervention graphs
#'  (SWIGs): A unification of the counterfactual and graphical approaches to
#'  causality." Center for the Statistics and the Social Sciences, University
#'  of Washington Series. Working Paper 128.30 (2013): 2013.
gFormula <- function(g) {
  g <- project.out.N.and.I(g)
  V <- get.Vset(g)
  A <- dagitty::exposures(g)
  Y <- dagitty::outcomes(g)
  V.no.A <- setdiff(V, A)
  n <- length(V)
  factors <- matrix(rep(0, (n-1)^2), nrow=n-1)
  row.names(factors) <- V.no.A
  colnames(factors) <- V.no.A
  for (v in V.no.A) {
    factors[v, c(v, setdiff(dagitty::parents(g, v), A))] <- 1
  }
  f <- ""
  for (i in 1:length(V.no.A)) {
    v <- V.no.A[i]
    .pa <- dagitty::parents(g, v)
    .pa[.pa == A] <- "A=a"
    if (length(.pa) > 0) {
      .term <- sprintf("%s | %s", v, paste0(.pa, collapse = ","))
    } else {
      .term <- sprintf("%s", v)
    }
    if (v == Y) {
      .term <- sprintf("%s P[%s]", v, .term)
    } else {
      .term <- sprintf("P(%s)", .term)
    }
    if (is.vector(factors)) {
      .to.sum <- names(factors)
    } else {
      factors <- factors[-1,]
      if (!is.null(nrow(factors))) {
        .to.sum <- colnames(factors)[colSums(factors) == 0]
      } else {
        .to.sum <- names(factors)[factors == 0]
      }
    }
    if (length(.to.sum) > 0) {
      .term <- sprintf("\u2211_{%s} %s", paste0(.to.sum, collapse = ","), .term)
      if (is.vector(factors)) {
        factors <- factors[-match(.to.sum, names(factors))]
      } else {
        factors <- factors[, -match(.to.sum, colnames(factors))]
      }
    }
    f <- paste(.term, f, sep=" ")
  }
  return(f)
}
