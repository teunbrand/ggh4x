# Main function -----------------------------------------------------------

#' Ribbon of panels with nested strips.
#'
#' \code{facet_nested_wrap()} wraps a sequence of panels onto a two-dimensional
#' layout, and nests grouped facets where possible.
#'
#' @inheritParams facet_wrap2
#' @inheritParams facet_nested
#'
#' @details This function inherits the capabilities of
#'   \code{\link[ggh4x]{facet_wrap2}()}.
#'
#'   This function only merges strips in the same row or column as they appear
#'   through regular \code{facet_wrap()} layout behaviour.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   \code{rows} or \code{cols}. The first variable is interpreted to be the
#'   outermost variable, while the last variable is interpreted to be the
#'   innermost variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. Strips are
#'   automatically grouped when they span a nested variable.
#'
#'   The \code{bleed} argument controls whether lower-level variables are allowed
#'   to be merged when higher-level are different, i.e. they can bleed over
#'   hierarchies. Suppose the \code{facet_wrap()} behaviour would be the
#'   following:
#'
#'   \code{[_1_][_2_][_2_]} \cr \code{[_3_][_3_][_4_]}
#'
#'   In such case, the default \code{bleed = FALSE} argument would result in the
#'   following:
#'
#'   \code{[_1_][___2____]} \cr \code{[_3_][_3_][_4_]}
#'
#'   Whereas \code{bleed = TRUE} would allow the following:
#'
#'   \code{[_1_][___2____]} \cr \code{[___3____][_4_]}
#'
#' @return A \code{FacetNestedWrap} ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#' @include facet_wrap2.R
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#' p + facet_nested_wrap(vars(cyl, drv))
#'
#' # Controlling the nest line
#' p + facet_nested_wrap(vars(cyl, drv), nest_line = TRUE) +
#'   theme(ggh4x.facet.nestline = element_line(linetype = 3))
#'
#' # Ignore nested hierarchies with the 'bleed' argument
#'  p + facet_nested_wrap(vars(drv, cyl), bleed = TRUE)
facet_nested_wrap <- function(
  facets, nrow = NULL, ncol = NULL,
  scales = "fixed", axes = "margins",
  remove_labels = "none",
  shrink = TRUE, labeller = "label_value",
  as.table = TRUE, drop = TRUE,
  dir = "h", strip.position = "top",
  nest_line = FALSE,
  resect = unit(0, "mm"),
  bleed = FALSE
) {

  strip.position <- match.arg(strip.position,
                              c("top", "bottom", "left",  "right"))
  labeller <- .int$check_labeller(labeller)
  facets <- .int$wrap_as_facets_list(facets)

  # Take care of direction
  dir <- match.arg(dir, c("h", "v"))
  if (identical(dir, "v")) {
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- .int$sanitise_dim(nrow_swap)
    ncol <- .int$sanitise_dim(ncol_swap)
  } else {
    nrow <- .int$sanitise_dim(nrow)
    ncol <- .int$sanitise_dim(ncol)
  }

  # Setup free scales
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # Setup axis positions
  axes <- match.arg(axes, c("margins", "rows", "cols", "full"))
  axes <- list(
    x = any(axes %in% c("rows", "full")),
    y = any(axes %in% c("cols", "full"))
  )

  rmlab <- match.arg(remove_labels, c("none", "rows", "cols", "all"))
  rmlab <- list(
    x = any(rmlab %in% c("rows", "all")),
    y = any(rmlab %in% c("cols", "all"))
  )

  ggproto(
    NULL, FacetNestedWrap,
    shrink = shrink,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      axes = axes,
      rmlab = rmlab,
      nest_line = nest_line,
      resect = resect,
      bleed = bleed
    )
  )
}


# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetNestedWrap <- ggproto(
  "FacetNestedWrap", FacetWrap2,
  finish_panels = function(self, panels, layout, params, theme) {
    merge_strips_wrap(
      panels, layout, params, theme,
      where = substr(params$strip.position, 1, 1)
    )
  }
)

# helpers -----------------------------------------------------------------

# TODO: Refactor to merge with the grid-version

merge_strips_wrap <- function(
  panel_table, layout, params, theme, where = "t"
) {
  nvar <- setdiff(colnames(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  if (length(nvar) <= 1) {
    # There are no strips to merge
    return(panel_table)
  }

  orient <- c("y", "x")[(where %in% c("t", "b")) + 1]
  mydim <- c("COL", "ROW")[(orient == "x") + 1]
  nlevels <- length(nvar)

  layout <- layout[order(layout[[mydim]], layout$PANEL), ]
  vars <- layout[, c(mydim, nvar), drop = FALSE]

  # Take just strips and promote them in the gtable
  template <- gtable_filter(panel_table, paste0("strip-", where), trim = FALSE)
  template <- elevate_strips(template, reverse = where == "b")

  if (params$bleed) {
    # Constrain merging by ROW/COL only
    vars[-1] <- lapply(vars[-1], function(x) {
      paste0(vars[[1]], x)
    })
  } else {
    # Constrain merging by parental categories
    vars[-1] <- lapply(seq_ncol(vars)[-1], function(i) {
      do.call(paste0, vars[, seq(i), drop = FALSE])
    })
  }

  merge <- c(FALSE, # Never merge over ROW/COL
             apply(vars[-1], 2, function(x) any(rle(x)$lengths > 1)))

  if (where == "r") {
    vars[-1]  <- rev(vars[-1])
    merge[-1] <- rev(merge[-1])
  }

  # Derive strip IDs from their names
  strip_ids <- strsplit(template$layout$name, "-", fixed = TRUE)
  strip_ids <- do.call(rbind, strip_ids)
  strip_ids <- strip_ids[, 3:ncol(strip_ids)]
  mode(strip_ids) <- "integer"

  # Reconstruct panel
  panels <- with(layout, matrix(0, nrow = max(ROW), ncol = max(COL)))
  panels[cbind(layout$ROW, layout$COL)] <- layout$PANEL

  # Setup expansion/deletion parameters
  remove <- rep(FALSE, nrow(strip_ids))
  aquire <- seq_nrow(strip_ids)
  i <- if (params$dir == "h") 2:1 else 1:2 # Probably transposed if dir == "v"
  panels <- paste(panels[strip_ids[, i]], strip_ids[, 3] + 1, sep = "_")

  for (level in seq_along(merge)) {
    if (!merge[level]) {
      next()
    }

    # Get item to merge as numeric
    item <- structure(unclass(as.factor(vars[, level])), levels = NULL)

    # Figure out what to merge
    rle <- rle(item)
    ends <- cumsum(rle$lengths)
    starts <- ends - rle$lengths + 1

    # Figure out what strips to remove
    delete_this <- paste(seq_along(item)[-starts], level, sep = "_")
    delete_this <- which(panels %in% delete_this)
    remove[delete_this] <- TRUE

    # Figure out what cells to expand
    expand <- rle$lengths > 1
    expand <- cbind(starts, ends)[expand, , drop = FALSE]
    expand <- structure(
      match(paste(expand, level, sep = "_"), panels),
      dim = dim(expand)
    )
    aquire[expand[, 1]] <- aquire[expand[, 2]]
  }

  # Do expansion
  if (orient == "x") {
    template$layout$r <- template$layout$r[aquire]
  } else {
    template$layout$b <- template$layout$b[aquire]
  }

  # Do deletion
  template$grobs  <- template$grobs[!remove]
  template$layout <- template$layout[!remove, , drop = FALSE]

  # Add nesting indicator
  template <- nesting_indicator(template, where, nlevels, params, theme)

  # Delete old strips
  panel_table <- gtable_filter(panel_table, paste0("strip-", where),
                               fixed = TRUE, trim = FALSE, invert = TRUE)
  # Add new strips
  panel_table <- with(template$layout, gtable_add_grob(
    panel_table,
    template$grobs,
    t = t, l = l, b = b, r = r,
    z = z, clip = clip, name = name
  ))

  panel_table
}

elevate_strips <- function(strips, reverse) {
  # Make empty template
  template <- strips
  template$grobs <- list()
  template$layout <- template$layout[0,]

  # Push sub-strips to top level in gtable
  for (id in seq_along(strips$grobs)) {
    strip <- strips$grobs[[id]]
    if (reverse) {
      # Reverse order of sub-strips
      strip$layout$t <- rev(strip$layout$t)
      strip$layout$b <- rev(strip$layout$b)
    }
    nsubstrips <- length(strip$grobs)

    # Expand layout
    layout <- strips$layout[rep(id, nsubstrips), ]
    # layout <- layout[rep(1, nsubstrips), ]
    rownames(layout) <- NULL

    strip <- lapply(seq_len(nsubstrips), function(j) {
      x <- strip
      x$grobs  <- x$grobs[j]
      x$layout <- x$layout[j, ]
      x
    })

    template <- with(layout, gtable_add_grob(
      template, strip,
      t = t, l = l, b = b, r = r,
      z = z, clip = clip, name = paste0(name, "-", seq_len(nsubstrips))
    ))
  }
  template
}

nesting_indicator <- function(template, where, nlevels, params, theme) {
  if (!params$nest_line) {
    return(template)
  }
  horizontal <- where %in% c("t", "b")
  secondary  <- where %in% c("b", "r")
  orient <- c("y", "x")[horizontal + 1]
  dim <- c("l", "t")[horizontal + 1]

  active  <- unit(c(0, 1), "npc") + c(-1, 1) * params$resect
  passive <- as.numeric(secondary)[c(1, 1)]
  # passive <- if (where %in% c("b", "r")) c(1, 1) else c(0, 0)
  indicator <- element_render(
    theme, "ggh4x.facet.nestline",
    x = switch(orient, x = active,  y = passive),
    y = switch(orient, x = passive, y = active)
  )
  longstrip <- which(with(template$layout, t != b | l != r))
  offset <- vapply(template$grobs, function(grob) {grob$layout[[dim]]},
                   numeric(1))
  offset <- if (xor(secondary, !horizontal)) offset else nlevels - offset
  template$grobs[longstrip] <- lapply(template$grobs[longstrip], function(grb) {
    grb <- with(grb$layout, gtable_add_grob(
      grb, indicator, t = t, l = l, r = r, b = b, z = z,
      name = "nester",
      clip = "off"
    ))
  })
  template$layout$z <- template$layout$z + offset
  template
}
