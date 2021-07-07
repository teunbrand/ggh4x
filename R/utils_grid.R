# Splits a list of grobs and reports max height in cm per level
split_heights_cm <- function(grobs, split) {
  vals <- lapply(grobs, grobHeight)
  vals <- vapply(vals,  convertHeight, numeric(1),
                 unitTo = "cm", valueOnly = TRUE)
  vals <- unname(vapply(split(vals, split), max, numeric(1)))
  unit(vals, "cm")
}

# Splits a list of grobs and reports max width in cm per level
split_widths_cm <- function(grobs, split) {
  vals <- lapply(grobs, grobWidth)
  vals <- vapply(vals,  convertWidth, numeric(1),
                 unitTo = "cm", valueOnly = TRUE)
  vals <- unname(vapply(split(vals, split), max, numeric(1)))
  unit(vals, "cm")
}
