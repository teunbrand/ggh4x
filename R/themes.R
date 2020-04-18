ggh4x_theme_elements <- function() {
  register_theme_elements(
    ggh4x.facet.nestline = element_line(),
    ggh4x.axis.nestline = element_line(),
    ggh4x.axis.nestline.x = element_line(),
    ggh4x.axis.nestline.y = element_line(),
    ggh4x.axis.nesttext.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    ggh4x.axis.nesttext.y = element_text(),
    ggh4x.axis.ticks.length.minor = rel(2/3),
    ggh4x.axis.ticks.length.mini = rel(1/3),
    element_tree = list(
      ggh4x.facet.nestline = el_def("element_line", "line"),
      ggh4x.axis.nestline = el_def("element_line", "axis.ticks"),
      ggh4x.axis.nestline.x = el_def("element_line", "ggh4x.axis.nestline"),
      ggh4x.axis.nestline.y = el_def("element_line", "ggh4x.axis.nestline"),
      ggh4x.axis.nesttext.x = el_def("element_text", "axis.text.x"),
      ggh4x.axis.nesttext.y = el_def("element_text", "axis.text.y"),
      ggh4x.axis.ticks.length.minor = el_def(c("rel")),
      ggh4x.axis.ticks.length.mini = el_def(c("rel"))
    )
  )
}
