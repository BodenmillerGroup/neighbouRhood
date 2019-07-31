# neighbouRhood
Adds an R implementation for the HistoCAT neightbourhood analysis (https://www.nature.com/articles/nmeth.4391) that runs from 'CellProfiler output' or 'CellProfiler output'-like data.

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
## Documentation
Please follow the vignette https://github.com/BodenmillerGroup/neighbouRhood/blob/master/vignettes/example_permutation_analysis.md 
to see an explaination about the concept behind the package and how to use it.
Also all individual functions are documented, ie the documentation can be displayed using: `?neightbouRhood`.

## Support
In case you need help, *please* open a Github issue!

