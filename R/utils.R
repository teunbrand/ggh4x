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
# I'm sorry Thomasp85 for using these undocumented internal functions!
# I know it is discouraged, but blatantly copy-pasting the code for these
# functions feels wrong too (and is more work than checking that my code still
# works with the ggplot2 dev version every once in a while).
# Please forgive me!
.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics",
    "as_facets_list",
    "as_gg_data_frame",
    "add_margins",
    "axis_label_element_overrides",
    "check_aesthetics",
    "check_labeller",
    "check_subclass",
    "compact",
    "continuous_range",
    "convertInd",
    "df.grid",
    "draw_axis_labels",
    "reshape_add_margins",
    "new_data_frame",
    "defaults", "id",
    "empty",
    "eval_facets",
    "ggname",
    "rename_aes",
    "mapped_aesthetics",
    "make_labels",
    "grid_as_facets_list",
    "is.zero",
    "rbind_dfs",
    "sanitise_dim",
    "set_draw_key",
    "snake_class",
    "ulevels",
    "unique_combs",
    "var_list",
    "validate_mapping",
    "warn_for_guide_position",
    "weave_tables_col",
    "weave_tables_row",
    "wrap_as_facets_list",
    "join_keys",
    ".pt",
    "interleave",
    "justify_grobs",
    "compute_just",
    "axis_label_priority"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.int <- .grab_ggplot_internals()

.onLoad <- function(...) {
  ggh4x_theme_elements()
}
