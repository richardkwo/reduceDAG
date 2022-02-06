test_that("uninformative",{
  data("g.example")
  expect_true(setequal(getUninformativeVariables(g.example),
                       c("I", "W4", "W1")))
  data("g.3M")
  expect_true(setequal(getUninformativeVariables(g.3M),
                       c("I", "M2", "M3")))
  data("g.double")
  expect_true(setequal(getUninformativeVariables(g.double),
                       c("W4", "W1")))
  data("g.long")
  expect_true(setequal(getUninformativeVariables(g.long),
                       c("I", "W6", "W2", "W1")))
  data("g.strange")
  expect_true(setequal(getUninformativeVariables(g.strange),
                       c("M")))
  data("g.strange.modified")
  expect_true(setequal(getUninformativeVariables(g.strange.modified),
                       c()))
})

test_that("consistency", {
  data("g.long")
  f1 <- gFormula(g.long)
  g2 <- reduceDAG(g.long, verbose = FALSE)
  f2 <- gFormula(g2)
  expect_true(nchar(f2) <  nchar(f1))
  expect_true(setequal(reduceDAG:::get.Vset(g2), getInformativeVariables(g.long)))
})
