# neighbouRhood
Adds an R implementation for the HistoCAT neightbourhood analysis that runs from CellProfiler output.

## Data requirements
This require an edge representation of a neightbourhood graph as well as a table that associates each object with a label.

The neightbourhood graph can be exported from object segmentation masks via the `Object relationships.csv` table.
To do this use the `MeasureObjectNeighbors` module and add an additional `ExportToSpreadsheet` module with the option `Export all measurement types -> No` and select `Object Relationships`.

Further it needs a table which associates each of the measured oject (with `ImageNumber` and `ObjectNumber`) with a `label` (eg a cluster).


## Installation
Either clone and install the repository or install  `devtools` to run:
`
devtools::install_github("BodenmillerGroup/neighbouRhood")
`

The current vignette (https://github.com/BodenmillerGroup/neighbouRhood/blob/master/vignettes/example_permutation_analysis.Rmd) gives hints how
to use the package, but is work in progress. However the individual functions are all documented, ie the documentation can be displayed using: `?neightbouRhood`.
