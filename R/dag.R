# topologically sort a vertex set -------
topo.sorted <- function(g, S) {
  if (length(S) == 0) {
    return(S)
  }
  .topo <- dagitty::topologicalOrdering(g)[S]
  .topo <- .topo[order(unlist(.topo))]
  return(names(.topo))
}

# get the vertex set --------
get.Vset <- function(g) {
  topo.sorted(g, names(g))
}

# get the set of instruments -------
get.Iset <- function(g) {
  stopifnot(length(dagitty::exposures(g)) == 1, length(dagitty::outcomes(g)) == 1)
  .I <- intersect(dagitty::ancestors(g, dagitty::exposures(g)), dagitty::ancestors(g, dagitty::outcomes(g)))
  .I <- setdiff(.I, dagitty::exposures(g))
  intersected.by.A <- unlist(lapply(.I, function(u) !any(dagitty::paths(g, u, dagitty::outcomes(g), Z = dagitty::exposures(g), directed = TRUE)$open)))
  .I <- .I[intersected.by.A]
  return(topo.sorted(g, .I))
}

# get the W-set (covariates) ----
get.Wset <- function(g) {
  stopifnot(length(dagitty::exposures(g)) == 1, length(dagitty::outcomes(g)) == 1)
  .W <- setdiff(dagitty::ancestors(g, dagitty::outcomes(g)), dagitty::descendants(g, dagitty::exposures(g)))
  .W <- setdiff(.W, get.Iset(g))
  return(topo.sorted(g, .W))
}

# get the M-set (mediators) ----
get.Mset <- function(g) {
  stopifnot(length(dagitty::exposures(g)) == 1, length(dagitty::outcomes(g)) == 1)
  .M <- intersect(dagitty::ancestors(g, dagitty::outcomes(g)), dagitty::descendants(g, dagitty::exposures(g)))
  .M <- setdiff(.M, dagitty::exposures(g))
  return(topo.sorted(g, .M))
}

# get the N-set (non-ancestors) ----
get.Nset <- function(g) {
  stopifnot(length(dagitty::exposures(g)) == 1, length(dagitty::outcomes(g)) == 1)
  .N <- setdiff(get.Vset(g), dagitty::ancestors(g, dagitty::outcomes(g)))
  return(topo.sorted(g, .N))
}

# get the O-set (optimal adjustment) ----
get.Oset <- function(g) {
  stopifnot(length(dagitty::exposures(g)) == 1, length(dagitty::outcomes(g)) == 1)
  M <- get.Mset(g)
  Pa.M <- suppressWarnings(dagitty::parents(g, M))
  De.M <- suppressWarnings(dagitty::descendants(g, M))
  .O <- setdiff(Pa.M, union(dagitty::exposures(g), De.M))
  return(topo.sorted(g, .O))
}

# get the O-min (subset of O that predicts A) ----
get.Omin <- function(g) {
  O <- get.Oset(g)
  if (length(O) == 0) return(O)
  for (size in 0:length(O)) {
    .subsets <- utils::combn(O, size)
    for (i in 1:ncol(.subsets)) {
      .Omin <- .subsets[,i]
      if (check.d.sep(g, dagitty::exposures(g), setdiff(O, .Omin), .Omin)) {
        .Omin <- O[sort(match(.Omin, O))]
        return(.Omin)
      }
    }
  }
}

# check d-separation ----
check.d.sep <- function(g, X, Y, Z=c()) {
  X <- setdiff(X, Z)
  Y <- setdiff(Y, Z)
  if (length(X) == 0 | length(Y) == 0){
    return(TRUE)
  }
  return(dagitty::dseparated(g, X, Y, Z))
}

# check W-criterion of uninformativeness for W vertex  ----
check.W.criterion <- function(g, w) {
  stopifnot(length(w) == 1)
  O <- get.Oset(g)
  W <- get.Wset(g)
  stopifnot(w %in% W)
  if (w %in% O) return(FALSE)
  w.ch <- W[sort(match(dagitty::children(g, w), W))]
  w.last.ch <- w.ch[length(w.ch)]
  Z.1 <- setdiff(union(dagitty::parents(g, w.last.ch), w.last.ch), w)
  if (!check.d.sep(g, w, O, Z.1)) {
    return(FALSE)
  } else {
    for (t in 1:length(w.ch)) {
      w.cur <- w.ch[t]
      if (t==1) {
        w.prev <- w
      } else {
        w.prev <- w.ch[t-1]
      }
      pa.cur <- dagitty::parents(g, w.cur)
      pa.prev <- dagitty::parents(g, w.prev)
      cond.i <- (w.prev %in% pa.cur)
      cond.ii <- all(pa.cur %in% union(pa.prev, w.prev))
      cond.iii <- check.d.sep(g, setdiff(pa.prev, pa.cur), O, pa.cur)
      if (!(cond.i & cond.ii & cond.iii)) return(FALSE)
    }
    return(TRUE)
  }
}

# check W-criterion of uninformativeness for M vertex  ----
check.M.criterion <- function(g, m) {
  stopifnot(length(m) == 1)
  Omin <- get.Omin(g)
  S <- union(dagitty::exposures(g), dagitty::outcomes(g))
  S <- union(S, Omin)
  M <- get.Mset(g)
  stopifnot(m %in% M)
  if (m == dagitty::outcomes(g)) return(FALSE)
  m.ch <- M[sort(match(dagitty::children(g, m), M))]
  m.last.ch <- m.ch[length(m.ch)]
  Z.1 <- setdiff(union(dagitty::parents(g, m.last.ch), m.last.ch), m)
  if (!check.d.sep(g, m, S, Z.1)) {
    return(FALSE)
  } else {
    for (t in 1:length(m.ch)) {
      m.cur <- m.ch[t]
      if (t==1) {
        m.prev <- m
      } else {
        m.prev <- m.ch[t-1]
      }
      pa.cur <- dagitty::parents(g, m.cur)
      pa.prev <- dagitty::parents(g, m.prev)
      cond.i <- (m.prev %in% pa.cur)
      cond.ii <- all(pa.cur %in% union(pa.prev, m.prev))
      cond.iii <- check.d.sep(g, setdiff(pa.prev, pa.cur), S, pa.cur)
      if (!(cond.i & cond.ii & cond.iii)) return(FALSE)
    }
    return(TRUE)
  }
}

#' List all the uninformative variables of a DAG
#'
#' This lists all the variables that are uninformative for estimating
#' the average effect of exposure on the outcome, given graph \code{g}.
#'
#' @param g A directed acyclic graph of class \code{dagitty}. A single vertex
#' should be labeled as \code{exposure} and another vertex should be labeled
#' as \code{outcome}.
#'
#' @seealso \code{\link[dagitty]{dagitty}}
#'
#' @return a character vector
#'
#' @examples
#' data("g.example")
#' getUninformativeVariables(g.example)
#' @export
getUninformativeVariables <- function(g) {
  N <- get.Nset(g)
  I <- get.Iset(g)
  .uninf <- union(N, I)
  for (w in get.Wset(g)) {
    if (check.W.criterion(g, w)) {
      .uninf <- union(.uninf, w)
    }
  }
  for (m in get.Mset(g)) {
    if (check.M.criterion(g, m)) {
      .uninf <- union(.uninf, m)
    }
  }
  return(.uninf)
}

#' List all the informative variables of a DAG
#'
#' This is the complement of \code{getUninformativeVariables}.
#'
#' @param g A directed acyclic graph of class \code{dagitty}. A single vertex
#' should be labeled as \code{exposure} and another vertex should be labeled
#' as \code{outcome}.
#'
#' @return a character vector
#'
#' @seealso \code{\link{getUninformativeVariables}}
#' @export
getInformativeVariables <- function(g) {
  return(setdiff(get.Vset(g), getUninformativeVariables(g)))
}

# project out the N-set and the I-set with latent projection ----
project.out.N.and.I <- function(g) {
  V <- get.Vset(g)
  N <- get.Nset(g)
  g <- get.induced.graph(g, setdiff(V, N))
  I <- get.Iset(g)
  while(length(I) > 0) {
    w <- I[length(I)]
    g <- project.out(g, w)
    I <- get.Iset(g)
  }
  return(g)
}

#' Reduce a causal directed acyclic graph (DAG)
#'
#' Reduce the causal DAG to a (potentially) smaller DAG, of which all the
#' variables are informative for estimating the effect of exposure on the
#' outcome.
#'
#' @param g A directed acyclic graph of class \code{dagitty}. A single vertex
#' should be labeled as \code{exposure} and another vertex should be labeled
#' as \code{outcome}. Use \code{\link[dagitty]{dagitty}} to create this object.
#'
#' @param verbose If \code{TRUE}, print the variables eliminated and the
#' reduced g-formula, also plot the reduced graph. (default: \code{TRUE})
#'
#' @return a DAG of class \code{daggity}
#'
#' @seealso \code{\link{gFormula}}, \code{\link{getUninformativeVariables}},
#' \code{\link{getInformativeVariables}}
#'
#' \code{\link[dagitty]{dagitty}}, \code{\link[dagitty]{exposures}},
#' \code{\link[dagitty]{outcomes}}
#'
#' @examples
#' data("g.example")
#' reduceDAG(g.example)
#' reduceDAG(g.example, verbose=FALSE)
#' data("g.long")
#' g <- reduceDAG(g.long, verbose=FALSE)
#' cat(gFormula(g))
#' plot(g)
#' @export
reduceDAG <- function(g, verbose=TRUE) {
  if (verbose) {
    .uninfo <- getUninformativeVariables(g)
    if (length(.uninfo) == 0) {
      message("All variables are informative.\n")
      return(g)
    } else {
      message("Uninformative variables {",
              paste0(.uninfo, collapse=", "),
              "} are eliminated.\n")
    }
  }
  g <- project.out.N.and.I(g)
  V.uninfo <- getUninformativeVariables(g)
  a <- dagitty::exposures(g)
  for (v in V.uninfo) {
    V <- get.Vset(g)
    pa <- dagitty::parents(g, v)
    ch <- topo.sorted(g, dagitty::children(g, v))
    if (a %in% ch) {
      ch <- c(setdiff(ch, a), a)
    }
    adj.mat <- to.adj.mat(g)
    adj.mat[pa, ch] <- 1
    if (length(ch) > 1){
      for (i in 1:length(ch)) {
        for (j in 1:length(ch)) {
          if (j > i) {
            adj.mat[ch[i], ch[j]] <- 1
          }
        }
      }
    }
    g <- to.dagitty(adj.mat, dagitty::coordinates(g), setdiff(V, v))
  }
  if (verbose) {
    plot(g)
    message("Reduced g-formula:")
    message(gFormula(g), "\n")
  }
  return(g)
}
