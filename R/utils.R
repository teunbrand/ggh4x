# Utilities ---------------------------------------------------------------

seq_range <- function(dat, ...) {
  seq.int(min(dat, na.rm = TRUE), max(dat, na.rm = TRUE), ...)
}

seq_nrow <- function(dat) {
  seq_len(NROW(dat))
}

seq_ncol <- function(dat) {
  seq_len(NCOL(dat))
}

width_cm <- function(x) {
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    cli::cli_abort("Unknown input: {.obj_type_friendly {x}}.")
  }
}

height_cm <- function(x) {
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    cli::cli_abort("Unknown input: {.obj_type_friendly {x}}.")
  }
}

fixup_docs <- function(x) {
  gsub("\\[=aes", "\\[ggplot2:aes", x)
}

# ggplot internals --------------------------------------------------------

data_frame0 <- function(...) {data_frame(..., .name_repair = "minimal")}

unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

get_transformation <- function(scale) {
  if (is_ggproto(scale$scale)) {
    scale <- scale$scale
  }
  if (is.function(scale$get_transformation)) {
    scale$get_transformation()
  } else {
    scale$trans %||% scale$transform
  }
}

new_guide_system <- NA
on_load(new_guide_system <- inherits(guide_none(), "Guide"))

.onLoad <- function(...) {
  ggh4x_theme_elements()
  run_on_load()
}
