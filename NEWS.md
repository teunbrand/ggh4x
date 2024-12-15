# ggh4x 0.3.0

This is a new minor release, introducing a few small features, bug fixes and 
deprecating the position guides in favour of the legendry package.

## Improvements

* Added `at_panel()` for annotating specific panels in facets (#162).
* New `strip_tag()` strip function (#149).
* `guide_stringlegend()` has been modernised to use the ggplot2 3.5.0 guide
  system instead of the old S3 system.
* Facets allow take the `strip` as strings and functions too.
* `coord_axis_inside(labels_inside)` now supports independent `"x"` and `"y"` 
  (#167).
* `help_secondary()` has a new `na.rm` argument (#146, thanks @jrmuirhead!).

## Bug fixes

* Fixed bug in label remover (#158).
* Fixed bug in axis `check.overlap` setting (#165).
* Future-proofing of wrapped facets.
* Fixed a bug when using facets with extension elements (#151).
* Tweaked test for upcoming release of fitdistrplus (#176)

## Deprecations

* The following functions have been deprecated in favour of both base ggplot2 and
  the legendry package. They are scheduled to be removed in the next non-hotfix
  update.
    * `scale_x_dendrogram()` and `scale_y_dendrogram()` in favour of 
    `legendry::scale_x_dendro()` and `legendry::scale_y_dendro()`.
    * `guide_dendro()` in favour of `legendry::guide_axis_dendro()`.
    * `guide_axis_truncated()` in favour of `ggplot2::guide_axis(cap = TRUE)`.
    * `guide_axis_colour()` in favour of `ggplot2::guide_axis(theme = ...)`.
    * `guide_axis_minor()` in favour of 
    `ggplot2::guide_axis(minor.ticks = TRUE)`.
    * `guide_axis_logticks()` in favour of `ggplot2::guide_axis_logticks()`.
    * `guide_axis_nested()` in favour of `legendry::guide_axis_nested()`.
    * `guide_axis_manual()` in favour of `legendry::guide_axis_base(key = ...)`.
    * `guide_axis_scalebar()` in favour of `legendry::primitive_bracket()`.

# ggh4x 0.2.8

This is a small release fixing a NOTE by request. In addition, there are two
bugfixes.

* Fixed a bug in the `strip_vanilla(size)` argument (#143)
* Fixed a bug in axis layout in `facet_wrap2()` (#140)

# ggh4x 0.2.7

This is a small release for future compatibility with ggplot2, along with some
bugfixes.

* Anticipatory changes for ggplot2 3.5.0.
* `force_panelsizes()` can now take 'bigpts' units as `total_width` or `total_height` 
  arguments (#118)
* Fixed bug in `facet_nested()` when `margins = TRUE` (#121)
* Fix bug related to mixing standard scales and facetted scales (#137)

# ggh4x 0.2.6

Hot fix for release of ggplot2 3.4.3. There are no user-visible changes.

# ggh4x 0.2.5

This is a small release for future compatibility with ggplot2, along with some
minor improvements. Moreover, the life cycle of guides has been set to 
'questioning' because they might be migrated to a new package using an upcoming
overhaul of the guide system. The guides won't be deprecated until alternatives
are available.

## New features

* `facet_grid2()`, and by extension `facet_nested()` too, gains a `render_empty` 
  argument that can leave panels without data blank (#69).
* Added the `solo_line` argument to nested facets (#90).
* `force_panelsizes()` gains a `total_width` and `total_height` argument to
  set the size of the total panel area.

## Improvements

* Swapped inheritance of the `nest_line` in nested facets: it is now a line 
  element that inherits from blank elements, and the theme default is a blank 
  element. This makes it more responsive to the theme setting (#89).
* Anticipated compatibility with future ggplot2 > 3.4.2.
* `stat_difference()` is now more performant, particularly for larger data.
* `stat_difference()` no longer outputs a 0-level.
* `stat_rle()` now considers `NA`s as equivalent values, unlike `base::rle()`.
* Curb back overzealous whitespace removal in `facet_manual()` (#106).
* `scale_{x/y}_facet()` now uses quosures instead of expressions (#101).

# ggh4x 0.2.4

This is a small release for compatibility with ggplot2 3.4.2, along with some
minor improvements and bug fixes.

## New features

* `scale_{x/y}_manual()` is a semi-discrete scale that accepts categorical 
  input and maps this a continuous output (#94).

## Improvements

* `facet_manual()` can now not only omit axes when panels occupy 1 cell in
  the axes' direction, but also when all panels in the same rows/columns occupy
  the same range of cells, when scales are not free.
* `facet_manual()` now tries to omit white space introduced by axis placement 
  when scales are not free (#86)
* Added `inv` option to invert the label order from outer to inner in 
  `guide_axis_nested()`.
  
## Bug fixes

* Avoid spurious warning in `guide_axis_nested()` (#97).
* Compatibility with ggplot2 3.4.2 (#95).
* Improved error messages (#91).
* Fixed `annotate(geom = "pointpath")` (#83).
* Fixed bug in `geom_box()` where `radius` is now properly handed off to the
  grob.

# ggh4x 0.2.3

This is a small release updating some of the internals to better use the public
interface to ggplot2 and play well with the latest release of ggplot2 (3.4.0).

## New features

* New `scale_{x/y}_facet()` to provide a more convenient syntax for 
  `facetted_pos_scales()` (#76).
* New `geom_box()`, as a more flexible variant of `geom_rect()`/`geom_tile()`
  that can take any two of x/y, width/height, xmin/ymin or xmax/ymax, and
  supports rounded corners.

# ggh4x 0.2.2

This is a small release updating the documentation to be compatible with HTML5, 
compatibility for upcoming ggplot2 3.4.0, along with four small features.

## New features

* New experimental `strip_split()` strip style for facets. This function might
  still give unexpected results. Feel free to leave feedback!
* New `guide_axis_scalebar()` for drawing scale bars (#63).
* New `geom_outline_points()` to draw overlapping points with outlines.
* New `coord_axes_inside()` function that moves axes from outside the panel to
  inside the panel.

## Tweaks and bugfixes

* Documented `?theme_extensions` to collect information about extra theme 
  elements in a single place.
* Deprecated `ggsubset()`. Use `data = ~ subset(.x, ...)` instead.
* `geom_*()` and `stat_*()` functions now allow dynamic dots.
* Fixed bug wherein facet label purging didn't recognise rich text grobs as
  the axis text part of an axis (#67).
* To be more compatible with future ggplot2, the `linesize` aesthetic in 
  `geom_pointpath()` has been renamed to `linewidth`.

# ggh4x 0.2.1

This is a patch release fixing a few bugs and a broken unit test.

* Fixed misplacement of nest lines in vertical strips (#50).
* Fixed bug in setting aspect ratio in `facet_grid2()` and family (#56).

# ggh4x 0.2.0

## Facets

Facets have largely been refactored. Accordingly, the 
[vignette](https://teunbrand.github.io/ggh4x/articles/Facets.html) has been 
updated to reflect the latest changes.

### Wrap

The previously existing `facet_wrap2()` now has the `trim_blank` option to force
`nrow` and `ncol` argument if these exceed the number of rows or columns needed
to place all the panels. Thanks to @coolbutuseless for letting me steal the idea
from the [{facetious}](https://github.com/coolbutuseless/facetious) package. 
The `facet_wrap2()` function was already used to offer more options over axis 
drawing.

### Grid

In addition, the cousin `facet_grid2()` makes an entrance. It offers the same 
axis drawing extensions as `facet_wrap2()`, but for the grid layout. Moreover, 
it allows for independent scales across  rows and columns, which is a constraint 
in `ggplot2::facet_grid()`.

### Nested facets

Besides `facet_nested_wrap()` inheriting from `facet_wrap2()`, which was already
the case, now `facet_nested()` also inherits from `facet_grid2()` to make use
of the extended axis drawing options. Also, the `nest_line` argument now takes
a `element_line()` or `element_blank()` to draw the indicator instead of the 
`logical(1)` argument it took previously. The `bleed` argument has been moved
from the nested facet to the new `strip_nested()` function (see below).

### Manual

New in the family of facets is now `facet_manual()`: a facet that can take
a user specified design for a layout and populate the panels accordingly.

### Strips

The facet functions in ggh4x now have `strip` arguments, which can be used
with new `strip_*()` functions that control how the strips are drawn. For now,
they come in the following three variants:

* `strip_vanilla()` which draws normal strips, but allows you to control whether
  labels should be clipped and has the `size` argument that can let strips on
  different layers have different sizes. This can be convenient when the strip
  labels have very different sizes.
* `strip_themed()` does all of the above, but also let's you assign 
  `element_text()` and `element_rect()` to different strips, allowing you to
  have greater control of their styling. These elements can be assigned 
  independently for the horizontal and vertical strips and can be applied to 
  single strips or layers of strips.
* `strip_nested()` again does all of the above, but also merges the strips with
  the same labels if they are next to one another. This means that the core
  functionality of `facet_nested()` and `facet_nested()` wrap has been moved
  and is now powered by the `strip` argument. They can still be convenient to
  draw nesting indicators.

## Other changes

* Fixed spelling in README (#32, thanks @vikatti!)
* Added helper for secondary axes: help_secondary()
* Added stat_difference() for shading a ribbon based on the sign of difference.
* Included 'colour'/'color' arguments for axis guides for recolouring the
  whole axis in one go (instead of editing 3-5 theme elements).
* Fix bug in facetted_pos_scales() with date scales (#37)
* Fix bug in nested facets when `strip.text = element_blank`. Requires ggplot2
  >3.3.0, or the dev version at time of writing. (#35)
* Changed facet_wrap2()/facet_nested_wrap() 'free', 'axes' and 'remove_labels'
  arguments to be less ambiguous.
* Fix bug in truncated axes with discrete scales (#39).
* Fix bug in geom_pointpath() with 1-member groups (#43).
* New axis function `guide_axis_manual()`.

# ggh4x 0.1.2.1

* Updated functions for ggplot2 v3.3.0 (#1)
* Fixed compatibility issue for facet_nested() and patchwork (#4)
* Fixed bug in facet_nested(bleed = FALSE) (#7)
* Added scale_(x/y)_dendrogram() and associated guide guide_dendro() (#1)
* Added guide_axis_nested() and convenience function weave_factors() (#3)
* Added guide_axis_minor() for minor break tickmarks
* Added guide_axis_logticks for logarithmic tickmarks
* Added element_part_rect() for using rectangles with a subset of edges as theme
  element (#13).
* Added stat_rollingkernel() for different smoothing lines.
* Added stat_rle() for runlength calculation.
* Fixed bug in geom_pointpath() (#15)
* Fixed scale_(x/y)_dendrogram so that labels are passed from scale (#17)
* geom_pointpath() now curves with nonlinear coordinates (#15).
* Added stat_funxy(), stat_centroid() and stat_midpoint() (#16).
* Added facet_wrap2() with few extensions.
* Added facet_nested_wrap() for merging strips (#19)
* Alternative specification of facet scales in `facetted_pos_scales` through 
  formulas (#25).
* Added option to discard dendrogram labels (#23).
* Added coloured text legend (stringlegend, #31).
* Added truncated axis guide.
* Supported axis truncation in other position guides too.

# ggh4x 0.1.1

* Package should be in usable state
* Added facet_nested()
* Added facetted_pos_scales()
* Added geom_pointpath()
* Added geom_rectrug()
* Added position_disjoint_ranges()
* Added position_lineartrans()
* Added scale_listed()
* Added scale_(fill/colour)_multi()
* Added stat_theodensity()

# ggh4x 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Initialised repository skeleton
