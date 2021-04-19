# ggh4x 0.1.2.1.9 (dev version on github)

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
* Added facet_grid2() offering the same axis drawing extensions as facet_wrap2() 
  but for grid layouts. Also allows independent scales across rows and columns.
* facet_nested() now builds off facet_grid2() instead of facet_grid(),
  inheriting these extensions above.
* Fix bug in truncated axes with discrete scales (#39)

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
