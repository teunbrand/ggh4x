# Utilities ---------------------------------------------------------------

# Like the try_require in ggplot2
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }
  # Use friendlier rlang install check message if available
  if (utils::packageVersion("rlang") >= package_version("0.4.10")) {
    rlang::check_installed(package, paste0("for `", fun, "`.\n"))
  } else {
    stop("Package `", package, "` required for `", fun, "`.\n",
         "Please install and try again.", call. = FALSE)
  }
}

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

.onLoad <- function(...) {
  ggh4x_theme_elements()
}
