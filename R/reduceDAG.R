#' reduceDAG: Reduced DAG and g-formula for efficient estimation of a causal effect
#'
#' Consider estimating a counterfactual mean of a point intervention
#' (or the associated average treatment effect) when the causal structure is known and
#' described by a directed acyclic graph (DAG). Suppose there is no unobserved
#' confounder. It turns out that not every variable in the graph is informative for
#' optimally estimating the counterfactual mean. This package identifies and
#' projects out all the uninformative variables from the DAG. Based on the reduced
#' DAG, an efficient estimator achieves the semiparametric efficiency bound with
#' respect to the original DAG. In particular, the g-formula (Robins, 1986)
#' prescribed by the reduced formula, when estimated with empirical plugin, is
#' efficient.
#'
#' Use \code{\link[dagitty]{dagitty}} to represent a DAG, of which the
#' \code{exposure} vertex and the \code{outcome} vertex must be labeled.
#'
#' Use \code{\link{reduceDAG}} to reduce a DAG.
#'
#' Use \code{\link{gFormula}} to display the g-formula associated with a DAG.
#'
#' Use \code{\link{getUninformativeVariables}} to list all the uninformative
#' variables that can be eliminated from a DAG.
#'
#' @docType package
#' @name reduceDAG
NULL
