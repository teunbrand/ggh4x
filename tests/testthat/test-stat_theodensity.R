# Distribution classifier -------------------------------------------------

test_that("class_distri classifies discrete distributions from the stats package", {
  distis <- c("pois", "nbinom", "binom", "geom", "hyper", "signrank", "multinom")
  classes <- character(0)
  for (i in distis) {
    classes <- c(classes, class_distri(i))
  }
  expect_true(all(classes == "discrete"))
})

test_that("class_distri classifies continuous distributions from the stats package", {
  distis <- c("beta", "cauchy", "chisq", "exp", "f", "gamma",
              "lnorm", "norm", "t", "unif", "weibull", "logis")
  classes <- character(0)
  for (i in distis) {
    classes <- c(classes, class_distri(i))
  }
  expect_true(all(classes == "continuous"))
})



test_that("class_distri identifies unknown distributions correctly", {

  rxyz <- function(n, mean = 0, sd = 1) {rnorm(n, mean, sd)}
  rzyx <- function(n, lambda) {rpois(n, lambda)}
  
  test <- class_distri("xyz")
  expect_equal(test, "continuous")
  
  test <- class_distri("zyx")
  expect_equal(test, "discrete")
})

# Warning tester ----------------------------------------------------------

basic <- ggplot(iris, aes(x = Sepal.Length, colour = Species)) +
  geom_density()

test_that("stat_theodensity warns about hypergeometric and multinomial distributions", {
  expect_error(basic + stat_theodensity(distri = "hyper"))
  expect_error(basic + stat_theodensity(distri = "multinomial"))
})

test_that("stat_theodensity warns about discrete distributions with continuous data", {
  plot <- basic + stat_theodensity(distri = "binom")
  expect_error(print(plot))
})

test_that("stat_theodensity skips groups with less than two datapoints", {
  # Only include 1 setosa observation
  plot <- ggplot(iris[c(1,51:150),], aes(x = Sepal.Length, colour = Species)) +
    stat_theodensity()
  dat <- expect_warning(layer_data(plot))
  tab <- as.numeric(table(dat$group))
  expect_equal(tab, c(1, 512, 512))
  expect_true(all(c(is.na(dat$y[1]),
                    is.na(dat$x[1]),
                    is.na(dat$density[1]),
                    is.na(dat$count[1]),
                    is.na(dat$n[1]),
                    is.na(dat$scaled[1]))))
  expect_false(any(c(is.na(dat$y[-1]),
                    is.na(dat$x[-1]),
                    is.na(dat$density[-1]),
                    is.na(dat$count[-1]),
                    is.na(dat$n[-1]),
                    is.na(dat$scaled[-1]))))
})


# Discrete distributions --------------------------------------------------

dat <- faithful$waiting

dat_by_distri <- function(distri) {
  g <- ggplot(faithful, aes(x = waiting)) +
    stat_theodensity(distri = distri)
  layer_data(g)
}

test_that("stat_theodensity fits poisson", {
  df <- dat_by_distri("pois")
  expect_equal(df$x, seq(min(dat), max(dat), by = 1))
  thismax <- df$x[which.max(df$y)]
  expect_lt(abs(mean(dat) - thismax), 10)
})

test_that("stat_theodensity fits geometric", {
  df <- dat_by_distri("geom")
  expect_equal(df$x, seq(min(dat), max(dat), by = 1))
})

test_that("stat_theodensity fits negative binomial", {
  df <- dat_by_distri("nbinom")
  expect_equal(df$x, seq(min(dat), max(dat), by = 1))
  thismax <- df$x[which.max(df$y)]
  expect_lt(abs(mean(dat) - thismax), 10)
})

test_that("stat_theodensity fits binomial", {
  df <- expect_message(dat_by_distri("binom"))
  expect_equal(df$x, seq(min(dat), max(dat), by = 1))
  thismax <- df$x[which.max(df$y)]
  expect_lt(abs(mean(dat) - thismax), 10)
})

test_that("stat_theodensity fits sign rank", {
  df <- expect_error(dat_by_distri("signrank"))
})

test_that("stat_theodensity fits wilcoxon", {
  df <- expect_error(dat_by_distri("wilcox"))
})


# continuous distributions ------------------------------------------------

test_that("stat_theodensity fits beta", {
  set.seed(1)
  d <- data.frame(x = rbeta(500, 0.5, 0.5))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "beta")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)

  # Beta with 0.5, 0.5 shapes should have two maxima
  expect_true(all(diff(ld$y[1:200]) < 0))
  expect_true(all(diff(ld$y[313:512]) > 0))
})

test_that("stat_theodensity fits cauchy", {
  set.seed(1)
  d <- data.frame(x = rcauchy(500, 10, 1))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "cauchy", n = 1e4)
  ld <- layer_data(g)
  expect_equal(nrow(ld), 1e4)
  max <- ld$x[which.max(ld$y)]
  expect_lt(abs(10 - max), 0.5)
  expect_true(all(diff(ld$y[which.max(ld$y):1e4]) < 0))
  expect_true(all(diff(ld$y[1:which.max(ld$y)]) > 0))
})

test_that("stat_theodensity fits chi squared", {
  set.seed(1)
  k <- 3
  d <- data.frame(x = rchisq(500, k, 0))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "chisq")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  # Mode of chisq is max(k - 2, 0)
  expect_lt(abs(k - 2 - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits exponential", {
  set.seed(1)
  d <- data.frame(x = rexp(500, 1))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "exp")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  expect_true(all(diff(ld$y) < 0))
})

test_that("stat_theodensity fits F-distribution", {
  set.seed(1)
  d1 <- 10
  d2 <- 30
  d <- data.frame(x = rf(500, d1, d2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "f", start.arg = list(df1 = 4, df2 = 2))
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expmode <- ((d1 - 2)/d1) * (d2 / (d2 + 2))
  expect_lt(abs(median(d$x) - expmode), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits gamma", {
  set.seed(1)
  d <- data.frame(x = rgamma(500, 2, 2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "gamma")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(median(d$x) - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits lognormal", {
  set.seed(1)
  d <- data.frame(x = rlnorm(500, 2, 1))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "lnorm")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(mean(log(d$x)) - 2), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits normal", {
  set.seed(1)
  d <- data.frame(x = rnorm(500, 2, 2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "norm")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(median(d$x) - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits student t", {
  set.seed(1)
  d <- data.frame(x = rt(500, 1.5))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "t", start.arg = list(df = 20))
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(median(d$x) - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits uniform", {
  set.seed(1)
  d <- data.frame(x = rgamma(500, 2, 2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "unif")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  expect_true(all(diff(ld$y) == 0))
})

test_that("stat_theodensity fits weibull", {
  set.seed(1)
  d <- data.frame(x = rweibull(500, 2, 2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "weibull")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(median(d$x) - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})

test_that("stat_theodensity fits logistic", {
  set.seed(1)
  d <- data.frame(x = rlogis(500, 2, 2))
  g <- ggplot(d, aes(x = x)) +
    stat_theodensity(distri = "logis")
  ld <- layer_data(g)
  expect_equal(nrow(ld), 512)
  max <- which.max(ld$y)
  expect_lt(abs(median(d$x) - ld$x[max]), 0.5)
  expect_true(all(diff(ld$y[max:512]) < 0))
  expect_true(all(diff(ld$y[1:max]) > 0))
})
