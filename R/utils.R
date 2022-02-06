# convert dagitty to adjacency matrix -----
to.adj.mat <- function(g) {
  V <- get.Vset(g)
  n <- length(V)
  amat <- matrix(rep(0, n^2), nrow=n)
  .edges <- dagitty::edges(g)
  row.names(amat) <- colnames(amat) <- V
  if (nrow(.edges) > 0) {
    .edges$v <- match(.edges$v, V)
    .edges$w <- match(.edges$w, V)
    for (i in 1:nrow(.edges)) {
      amat[.edges$v[i], .edges$w[i]] <- 1
    }
  }
  return(amat)
}

# convert adjacency matrix to dagitty ------
to.dagitty <- function(adj.mat, coord=NULL, subset=NULL) {
  V <- row.names(adj.mat)
  if (!is.null(subset)) {
    V <- intersect(V, subset)
    adj.mat <- adj.mat[V, V]
  }
  n <- length(V)
  .s <- "dag {\n"
  if (!is.null(coord)) {
    for (v in V) {
      if (v == "A") {
        .s <- paste0(.s, sprintf('A [pos="%g,%g", exposure]\n', coord$x[v], coord$y[v]))
      } else if (v == "Y") {
        .s <- paste0(.s, sprintf('Y [pos="%g,%g", outcome]\n', coord$x[v], coord$y[v]))
      } else {
        .s <- paste0(.s, sprintf('%s [pos="%g,%g"]\n', v, coord$x[v], coord$y[v]))
      }
    }
  }
  for (i in 1:n) {
    for (j in 1:n) {
      if(adj.mat[i,j] == 1) {
        .s <- paste0(.s, V[i], "->", V[j], "\n")
      }
    }
  }
  .s <- paste0(.s, "}")
  return(dagitty::dagitty(.s))
}

# get induced subgraph ------
get.induced.graph <- function(g, subset) {
  to.dagitty(to.adj.mat(g), dagitty::coordinates(g), subset=subset)
}

# project out a variable with 0 or 1 child -----
project.out <- function(g, v) {
  V <- get.Vset(g)
  adj.mat <- to.adj.mat(g)
  pa <- dagitty::parents(g, v)
  ch <- dagitty::children(g, v)
  stopifnot(length(ch) < 2)
  if (length(ch) == 1 & length(pa) > 0) {
    adj.mat[pa, ch] <- 1
  }
  to.dagitty(adj.mat, dagitty::coordinates(g), subset=setdiff(V, v))
}
