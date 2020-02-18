# Setup test data ---------------------------------------------------------

set.seed(1)
df1 <- data.frame(x = rnorm(10), y = rnorm(10), a = rnorm(10))
df2 <- data.frame(x = rnorm(10), y = rnorm(10), b = LETTERS[c(1:5, 1:5)])
df3 <- data.frame(x = rnorm(10), y = rnorm(10), c = rnorm(10))
df4 <- data.frame(x = rnorm(10), y = rnorm(10), d = LETTERS[c(6:10, 6:10)])

base <- suppressWarnings(ggplot(mapping = aes(x, y)) +
  geom_point(data = df1, aes(a = a)) +
  geom_point(data = df2, aes(b = b)) +
  geom_point(data = df3, aes(c = c), shape = 21) +
  geom_point(data = df4, aes(d = d), shape = 21))

scalelist <- list(
  scale_colour_gradientn(colours = c("red", "green", "blue"), aesthetics = "a"),
  scale_colour_brewer(palette = "Set1", aesthetics = "b"),
  scale_fill_distiller(palette = "YlGnBu", aesthetics = "c"),
  scale_fill_viridis_d(aesthetics = "d")
)

replaces <- c("colour", "colour", "fill", "fill")


# Basic tests -------------------------------------------------------------

test_that("scale_listed returns list with MultiScale elements", {
  test <- scale_listed(scalelist, replaces)
  expect_is(test, "list")
  expect_equal(length(test), 2)
  expect_is(test[[1]], "MultiScale")
  expect_is(test[[2]], "MultiScale")
})

test_that("scale_listed adds scales to plot", {
  ctrl <- base
  test <- base + scale_listed(scalelist, replaces)
  expect_equal(length(ctrl$scales$scales), 0)
  expect_equal(length(test$scales$scales), 4)
})


# Mixed aesthetics --------------------------------------------------------

test_that("scale_listed can mix continuous colour and fill scales", {
  sl <- scale_listed(scalelist[c(1,3)], replaces[c(1,3)])
  test <- suppressWarnings(ggplot(mapping = aes(x, y)) +
                             geom_point(data = df1, aes(a = a)) +
                             geom_point(data = df3, aes(c = c), shape = 21))
  test <- test + sl
  gb <- ggplot_build(test)

  # Test column renaming
  colour <- gb$plot$layers[[1]]$geom$handle_na(gb$plot$layers[[1]]$data,
                                               gb$plot$layers[[1]]$geom_params)
  fill <- gb$plot$layers[[2]]$geom$handle_na(gb$plot$layers[[2]]$data,
                                             gb$plot$layers[[2]]$geom_params)
  expect_true("colour" %in% names(colour))
  expect_true("fill" %in% names(fill))

  # Test colours are continuous
  colour <- layer_data(test, 1)
  fill <- layer_data(test, 2)
  expect_equal(length(unique(colour$a)), 10)
  expect_equal(length(unique(fill$c)), 10)

  # Test gtable
  gt <- ggplotGrob(test)
  geoms <- gt$grobs[gt$layout$name == "panel"][[1]]$children
  geoms <- geoms[grepl("geom_point", names(geoms))]
  colour <- geoms[[1]]$gp
  fill <- geoms[[2]]$gp

  expect_equal(length(unique(colour$col)), 10)
  expect_equal(length(unique(colour$fill)), 1)
  expect_equal(length(unique(fill$fill)), 10)
  expect_equal(length(unique(fill$col)), 1)
})

test_that("scale_listed can mix discrete colour and fill scales", {
  sl <- scale_listed(scalelist[c(2,4)], replaces[c(2,4)])
  test <- suppressWarnings(ggplot(mapping = aes(x, y)) +
                             geom_point(data = df2, aes(b = b)) +
                             geom_point(data = df4, aes(d = d), shape = 21))
  test <- test + sl
  gb <- ggplot_build(test)

  # Test column renaming
  colour <- gb$plot$layers[[1]]$geom$handle_na(gb$plot$layers[[1]]$data,
                                               gb$plot$layers[[1]]$geom_params)
  fill <- gb$plot$layers[[2]]$geom$handle_na(gb$plot$layers[[2]]$data,
                                             gb$plot$layers[[2]]$geom_params)
  expect_true("colour" %in% names(colour))
  expect_true("fill" %in% names(fill))

  # Test colours are discrete
  colour <- layer_data(test, 1)
  fill <- layer_data(test, 2)
  expect_lte(length(unique(colour$b)), 5)
  expect_lte(length(unique(fill$d)), 5)

  # Test gtable
  gt <- ggplotGrob(test)
  geoms <- gt$grobs[gt$layout$name == "panel"][[1]]$children
  geoms <- geoms[grepl("geom_point", names(geoms))]
  colour <- geoms[[1]]$gp
  fill <- geoms[[2]]$gp

  expect_equal(length(unique(colour$col)), 5)
  expect_equal(length(unique(colour$fill)), 1)
  expect_equal(length(unique(fill$fill)), 5)
  expect_equal(length(unique(fill$col)), 1)
})

test_that("scale_listed can mix discrete and continuous colours", {
  sl <- scale_listed(scalelist[1:2], replaces[1:2])
  test <- suppressWarnings(ggplot(mapping = aes(x, y)) +
                             geom_point(data = df1, aes(a = a)) +
                             geom_point(data = df2, aes(b = b)))
  test <- test + sl

  # Test datapoints
  continuous <- layer_data(test, 1)
  discrete <- layer_data(test, 2)
  expect_equal(length(unique(continuous$a)), 10)
  expect_equal(length(unique(discrete$b)), 5)

  continuous <- col2rgb(continuous$a)
  discrete <- col2rgb(discrete$b)
  expect_is(continuous, "matrix")
  expect_is(discrete, "matrix")

  # Test guide
  gt <- ggplotGrob(test)
  gt <- gt$grobs[gt$layout$name == "guide-box"][[1]]
  gt <- gt$grobs[gt$layout$name == "guides"]
  is_bar <- which(sapply(gt, function(x){"bar" %in% x$layout$name}))
  is_key <- which(sapply(gt, function(x){any(grepl("key", x$layout$name))}))
  bar <- gt[[is_bar]]
  bar <- as.vector(bar$grobs[bar$layout$name == "bar"][[1]]$raster)
  keys <- gt[[is_key]]
  keys <- keys$grobs[grepl("key", keys$layout$name) & !endsWith(keys$layout$name, "bg")]
  keys <- sapply(keys, function(key){
    key$gp$col
  })

  nbin <- rev(seq_len(formals(guide_colourbar)$nbin))
  bar_should <- scales::gradient_n_pal(c("red","green","blue"))(scales::rescale(nbin))
  key_should <- c("#E41A1CFF", "#377EB8FF", "#4DAF4AFF", "#984EA3FF", "#FF7F00FF")

  expect_equal(bar, bar_should)
  expect_equal(keys, key_should)
})

test_that("scale_listed can mix discrete and continuous fills", {
  sl <- scale_listed(scalelist[3:4], replaces[3:4])
  test <- suppressWarnings(ggplot(mapping = aes(x, y)) +
                             geom_point(data = df3, aes(c = c), shape = 21) +
                             geom_point(data = df4, aes(d = d), shape = 21))
  test <- test + sl

  # Test datapoints
  continuous <- layer_data(test, 1)
  discrete <- layer_data(test, 2)
  expect_equal(length(unique(continuous$c)), 10)
  expect_equal(length(unique(discrete$d)), 5)

  continuous <- col2rgb(continuous$c)
  discrete <- col2rgb(discrete$d)
  expect_is(continuous, "matrix")
  expect_is(discrete, "matrix")

  # Test guide
  gt <- ggplotGrob(test)
  gt <- gt$grobs[gt$layout$name == "guide-box"][[1]]
  gt <- gt$grobs[gt$layout$name == "guides"]
  is_bar <- which(sapply(gt, function(x){"bar" %in% x$layout$name}))
  bar <- gt[[is_bar]]
  bar <- as.vector(bar$grobs[bar$layout$name == "bar"][[1]]$raster)
  is_key <- which(sapply(gt, function(x){any(grepl("key", x$layout$name))}))
  keys <- gt[[is_key]]
  keys <- keys$grobs[grepl("key", keys$layout$name) & !endsWith(keys$layout$name, "bg")]
  keys <- sapply(keys, function(key){
    key$gp$fill
  })

  nbin <- formals(guide_colourbar)$nbin
  
  key_should <- scales::viridis_pal()(5)
  
  expect_equal(keys, key_should)
  expect_equal(length(bar), nbin)
  
})

# Error tests -------------------------------------------------------------

test_that("scale_listed throws error if scalelist and replaces unequal length", {
  expect_error(scale_listed(scalelist[1:3], replaces),
               "argument parallel and")
})

test_that("scale_listed throws error when replaces has invalid aes", {
  expect_error(scale_listed(scalelist, c(replaces[1:3], "nonsense")),
               "recognised as valid aesthetics")
})

test_that("scale_listed throws error when non-scales are supplied as scalelist", {
  expect_error(scale_listed(c(scalelist[1:3], "nonsense"), replaces),
               "accepts only valid scale objects as list-elements")
})

test_that("scale_listed throws error when multiple aesthetics are supplied in a scale", {
  expect_error(scale_listed(c(scalelist[1:3], scale_fill_brewer(aesthetics = c("a", "b"))),
                            replaces),
               "only 1 aesthetic per scale")
})

test_that("scale_lsited throws error when empty aesthetics are supplied in a scale", {
  expect_error(scale_listed(c(scalelist[1:3], scale_fill_brewer(aesthetics = character(0))),
                            replaces),
               "make sure that the aesthetics of the scalelist are set")
})
