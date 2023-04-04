## Test notes

Whilst checking the dev version on the win builder against R-devel, an error occurred that reads "Package required and available but unsuitable version: 'ggplot2'". At this point, I'm assuming the relevant binary of ggplot2 has not been build yet on R-devel which causes the error. I would expect this issue to resolve itself automatically.

## revdepcheck results

I checked 9 reverse dependencies (8 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

