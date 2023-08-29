# Setup testing data ------------------------------------------------------

df <- list(data.frame(x = 1:100, y = 1, w = 1:100),
           data.frame(x = 1:100, y = 2, v = 1:100),
           data.frame(x = 1:100, y = 3, z = 1:100))

base <- suppressWarnings(ggplot(mapping = aes(x = x,  y = y)) +
                           geom_point(data = df[[2]], aes(colour1 = v)) +
                           geom_point(data = df[[1]], aes(colour2 = w)) +
                           geom_point(data = df[[3]], aes(colour3 = z)))


# basic tests -------------------------------------------------------------

test_that("scale_colour_multi adds multiple scales", {
  ctrl <- base
  test <- base + scale_colour_multi(
    aesthetics = c("colour1", "colour2", "colour3")
  )
  expect_equal(length(ctrl$scales$scales), 0)
  expect_equal(length(test$scales$scales), 3)
})

test_that("scale_colour_multi sets available aes in guide and scale", {
  g <- base + scale_colour_multi(
    aesthetics = c("colour1", "colour2", "colour3")
  )
  gb <- ggplot_build(g)
  guide_aes <- sapply(gb$plot$scales$scales[1:3], function(scale) {scale$guide$available_aes})
  scale_aes <- sapply(gb$plot$scales$scales[1:3], function(scale) {scale$aesthetics})
  expect_identical(guide_aes, c("colour1", "colour2", "colour3"))
  expect_identical(guide_aes, scale_aes)
})

test_that("scale_colour_multi nahandle update renames columnnames", {
  g <- base + scale_colour_multi(
    aesthetics = c("colour1", "colour2", "colour3")
  )
  layer_dat <- lapply(1:3, function(i) {layer_data(g, i)})
  update_dat <- lapply(1:3, function(i){
    g$layers[[i]]$geom$handle_na(layer_dat[[i]], g$layers[[1]]$geom_params)
  })

  old_colnames <- sapply(layer_dat, function(dat){colnames(dat)[1]})
  new_colnames <- sapply(update_dat, function(dat){colnames(dat)[1]})

  expect_true(all(new_colnames == "colour"))
  expect_identical(old_colnames, paste0("colour", 1:3))
})

test_that("scale_colour_multi can map multiple fill colours", {
  startcols <- c("white", "black", "grey50")
  endcols   <- c("red", "blue","green")
  g <- base + scale_colour_multi(
    aesthetics = c("colour1", "colour2", "colour3"),
    colours = list(c(startcols[1], endcols[1]),
                   c(startcols[2], endcols[2]),
                   c(startcols[3], endcols[3]))
  )
  layers <- lapply(1:3, function(i){layer_data(g, i)})
  starts <- sapply(layers, function(l){l[1,1]})
  ends   <- sapply(layers, function(l){tail(l, 1)[1,1]})
  hex_start <- apply(col2rgb(startcols), 2,
                     function(x){rgb(x[1], x[2], x[3], maxColorValue = 255)})
  hex_end <- apply(col2rgb(endcols), 2,
                   function(x){rgb(x[1], x[2], x[3], maxColorValue = 255)})
  expect_identical(starts, hex_start)
  expect_identical(ends, hex_end)
})

test_that("scale_colour_multi has appropriate legends", {
  startcols <- c("white", "black", "grey50")
  endcols   <- c("red", "blue","green")
  g <- base + scale_colour_multi(
    aesthetics = c("colour1", "colour2", "colour3"),
    colours = list(c(startcols[1], endcols[1]),
                   c(startcols[2], endcols[2]),
                   c(startcols[3], endcols[3]))
  )
  gt <- ggplotGrob(g)
  guidebox <- gt$grobs[gt$layout$name == "guide-box"][[1]]$grobs[1:3]
  guidenames <- vapply(guidebox, function(box) {
    box$grobs[box$layout$name == "title"][[1]]$children[[1]]$children[[1]]$label
  }, character(1))
  cols <- lapply(guidebox, function(leg){
    as.vector(leg$grobs[leg$layout$name == "bar"][[1]]$raster)
  })[order(guidenames)]
  starts <- unname(sapply(cols, tail, 1))
  starts <- col2rgb(starts)
  ends <- unname(sapply(cols, head, 1))
  ends <- col2rgb(ends)
  startcols <- col2rgb(startcols)
  endcols <- col2rgb(endcols)
  expect_identical(startcols, starts)
  expect_identical(endcols, ends)
})


# argument tests ----------------------------------------------------------

test_that("scale_colour_multi accepts independent positions", {
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = c("white", "red", "black"),
                               values = list(c(0, 0.50, 1),
                                             c(0, 0.75, 1),
                                             c(0, 0.25, 1)))

  cols <- lapply(1:3, function(i){col2rgb(layer_data(g, i)[,1])})
  redness <- lapply(cols, function(m){
    m["red",] - 0.5 * m["green",] - 0.5 * m["blue",]
  })
  maxred <- sapply(redness, which.max)
  expect_true(maxred[1] %in% 49:51)
  expect_true(maxred[2] %in% 74:76)
  expect_true(maxred[3] %in% 24:26)
})

test_that("scale_colour_multi accepts independent transformations", {
  g <- base  + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                                colours = c("white", "red", "black"),
                                trans = list("identity", "log10", "reverse"))
  # Test acceptance
  gb <- ggplot_build(g)
  tr <- sapply(gb$plot$scales$scales, function(scale) {scale$trans$name})[1:3]
  expect_equal(tr, c("identity", "log-10", "reverse"))

  # Test practical transformations
  cols <- lapply(1:3, function(i){col2rgb(layer_data(g, i)[,1])})
  redness <- lapply(cols, function(m){
    m["red",] - 0.5 * m["green",] - 0.5 * m["blue",]
  })
  expect_equal(cols[[1]], cols[[3]][,100:1])
  expect_equal(which.max(redness[[2]]), 10)
})

test_that("scale_colour_multi sets breaks independently", {
  breaks <- list(c(20, 50, 70),
                 c(10, 20, 80, 100),
                 c(seq(0, 100, by = 10)))
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = list(c("white", "red")),
                               breaks = breaks,
                               limits = c(0, 100))
  # Test theoretical breaks
  gb <- ggplot_build(g)
  br <- lapply(gb$plot$scales$scales, function(scale) {scale$get_breaks()})[1:3]
  expect_equal(breaks[[1]], br[[1]])
  expect_equal(breaks[[2]], br[[2]])
  expect_equal(breaks[[3]], br[[3]]) # 0 not in data, so is NA
})

test_that("scale_colour_multi sets limits independently", {
  limits <- list(c(20, 80), c(50, NA),c(NA, 25))
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = list(c("white", "red")),
                               limits = limits,
                               oob = scales::squish)
  # Test theoretical limits
  gb <- ggplot_build(g)
  lims <- lapply(gb$plot$scales$scales, function(scale){scale$get_limits()})[1:3]
  expect_equal(lims[[1]], limits[[1]])
  expect_equal(lims[[2]][1], limits[[2]][1])
  expect_equal(lims[[3]][2], limits[[3]][2])

  # Test practical breaks
  cols <- lapply(1:3, function(i){
    col2rgb(layer_data(g, i)[,1])
  })

  redness <- lapply(cols, function(m){
    m["red",] - 0.5 * m["green",] - 0.5 * m["blue",]
  })
  expect_true(all(redness[[1]][1:20] == 0))
  expect_true(all(redness[[1]][80:100] == 255))
  expect_true(all(redness[[2]][1:50] == 0))
  expect_true(all(redness[[3]][25:100] == 255))
})

test_that("scale_colour_multi sets labels independently", {
  labfuns = list(function(x){x/100},
                 function(x){paste0(x, " Nonsense")},
                 waiver())
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = list(c("white", "red")),
                               limits = c(0, 100),
                               labels = labfuns)
  # Test theoretical labels
  gb <- ggplot_build(g)
  labs <- lapply(gb$plot$scales$scales, function(scale) {scale$get_labels()})[1:3]
  expect_equal(labs[[1]], seq(0, 1, by = 0.25))
  expect_equal(labs[[2]], paste0(seq(0, 100, by = 25), " Nonsense"))
  expect_equal(labs[[3]], paste0(seq(0, 100, by = 25)))

  # Test practical labels
  gt <- ggplotGrob(g)
  guidebox <- gt$grobs[gt$layout$name == "guide-box"][[1]]$grobs[1:3]
  guidenames <- vapply(guidebox, function(box) {
    box$grobs[box$layout$name == "title"][[1]]$children[[1]]$children[[1]]$label
  }, character(1))
  i <- if (!new_guide_system) "label" else "labels"
  labs <- lapply(guidebox, function(tg){
    tg$grobs[tg$layout$name == i][[1]]$children[[1]]$label
  })[order(guidenames)]
  expect_equal(labs[[1]], paste0(seq(0, 1, by = 0.25)))
  expect_equal(labs[[2]], paste0(seq(0, 100, by = 25), " Nonsense"))
  expect_equal(labs[[3]], paste0(seq(0, 100, by = 25)))
})

test_that("scale_colour_multi sets titles independently", {
  titles <- list("White to Red", "Black to Blue", "Gray to Green")
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = list(c("white", "red"),
                                              c("black", "blue"),
                                              c("grey50", "green")),
                               name = titles)
  # Test theoretical titles
  gb <- ggplot_build(g)
  title <- lapply(gb$plot$scales$scales, function(scale) {scale$name})[1:3]
  expect_identical(titles, title)

  # Test practical titles
  gt <- ggplotGrob(g)
  gt <- gt$grobs[gt$layout$name == "guide-box"][[1]]$grobs[1:3]
  title <- lapply(gt, function(tg) {
    tg$grobs[tg$layout$name == "title"][[1]]$children[[1]]$children[[1]]$label
  })
  expect_true(all(title %in% titles))
})

test_that("scale_colour_multi handles discrete guides", {
  g <- base + scale_colour_multi(aesthetics = c("colour1", "colour2", "colour3"),
                               colours = list(c("white", "red"),
                                              c("black", "blue"),
                                              c("grey50", "green")),
                               guide = guide_legend())
  gt <- ggplotGrob(g)
  gt <- gt$grobs[gt$layout$name == "guide-box"][[1]]$grobs[1:3]
  keyvals <- lapply(gt, function(tg){
    key <- tg$grobs[grepl("key", tg$layout$name) & !endsWith(tg$layout$name, "bg")]
    cols <- sapply(key, function(k){k$gp$col})
  })
  keyvals <- do.call(c, keyvals)
  nunique <- length(unique(keyvals))
  expect_identical(nunique, 12L)
})

# Warnings ----------------------------------------------------------------

test_that("scale_colour_multi throws error when guide inappropriate", {
  expect_snapshot_error(
    base + scale_colour_multi(
      aesthetics = c("fill1", "fill2", "fill3"),
      colours = list(
        c("white", "red"),
        c("black", "blue"),
        c("grey50", "green")
      ),
      guide = "nonsense"
    )
  )
})

