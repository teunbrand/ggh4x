.onLoad <- function(...) {
  register_theme_elements(
    ggh4x.facet.nestline = element_line(),
    ggh4x.axis.nestline = element_line(),
    ggh4x.axis.nestline.x = element_line(),
    ggh4x.axis.nestline.y = element_line(),
    ggh4x.axis.nesttext.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    ggh4x.axis.nesttext.y = element_text(),
    element_tree = list(
      ggh4x.facet.nestline = el_def("element_line", "line"),
      ggh4x.axis.nestline = el_def("element_line", "axis.ticks"),
      ggh4x.axis.nestline.x = el_def("element_line", "ggh4x.axis.nestline"),
      ggh4x.axis.nestline.y = el_def("element_line", "ggh4x.axis.nestline"),
      ggh4x.axis.nesttext.x = el_def("element_text", "axis.text.x"),
      ggh4x.axis.nesttext.y = el_def("element_text", "axis.text.y")
    )
  )
}
