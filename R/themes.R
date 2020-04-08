.onLoad <- function(...) {
  register_theme_elements(
    ggh4x.facet.nestline = element_line(),
    element_tree = list(
      ggh4x.facet.nestline = el_def("element_line", "line")
    )
  )
}
