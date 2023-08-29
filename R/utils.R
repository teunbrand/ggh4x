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

# ggplot internals --------------------------------------------------------

# Function for grabbing internal function of ggplot2 that are also used here.
# While most of these are now covered in the borrowed_ggplot2.R file, there
# are some functions that weren't so easy to copy, which remain below.
.grab_ggplot_internals <- function() {
  objects <- c(
    # too rabbithole-complex to manually copy
    "grid_as_facets_list",
    "wrap_as_facets_list"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.int <- .grab_ggplot_internals()

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

new_guide_system <- NA
on_load(new_guide_system <- inherits(guide_none(), "Guide"))

.onLoad <- function(...) {
  ggh4x_theme_elements()
  run_on_load()
}
