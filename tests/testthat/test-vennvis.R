context("Diagram creation")

test_that("Two-variable calculations output properly", {
  load("obj/vv2.Rdata")

  set.seed(147289)
  x <- rnorm(100)
  y <- 0.5*x+rnorm(100, 0.25)
  vv2 <- vennvis(x, y, plot = FALSE)

  expect_equal(vv2, vv2_test)

  # vv2_test <- vv2
  # save(vv2_test, file = "obj/vv2.Rdata")
})

test_that("Three-variable calculations output properly", {
  load("obj/vv3.Rdata")

  set.seed(147289)
  x <- rnorm(100)
  y <- 0.5*x+rnorm(100, 0.25)
  z <- 0.3*x+0.4*y+rnorm(100, 0.15)
  vv3 <- vennvis(x, y, z, plot = FALSE)

  expect_equal(vv3, vv3_test)

  # vv3_test <- vv3
  # save(vv3_test, file = "obj/vv3.Rdata")
})

test_that("Plots output properly", {
  set.seed(147289)
  x <- rnorm(100)
  y <- 0.5*x+rnorm(100, 0.25)
  z <- 0.3*x+0.4*y+rnorm(100, 0.15)
  vv2 <- vennvis(x, y, plot = FALSE)
  vv3 <- vennvis(x, y, z, plot = FALSE)

  png(filename = "figs/vv2.png", width = 400, height = 300)
  expect_silent(plot(vv2))
  dev.off()

  png(filename = "figs/vv3.png", width = 400, height = 300)
  expect_silent(plot(vv3))
  dev.off()
})
