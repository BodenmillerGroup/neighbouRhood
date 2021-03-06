% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighbouRhood.R
\name{aggregate_classic_patch}
\alias{aggregate_classic_patch}
\title{Calculates patch detection neightbourhood statistics}
\usage{
aggregate_classic_patch(dat_nb, patch_size)
}
\arguments{
\item{dat_nb}{a neightbourhood table with the labels applied with 'apply_labels'}

\item{patch_size}{minimum number of neighbours to be considered a patch}
}
\value{
a statistics with columns group, Firstlabel, Secondlabel, ct
}
\description{
Calculates: What fraction of cells of type A have at least a given number of neighbours of type B?
}
\details{
This was first proposed in Schulz D, Zanotelli VRT, Fischer JR, Schapiro D, Engler S, Lun XK, Jackson HW, Bodenmiller B.
Simultaneous Multiplexed Imaging of mRNA and Proteins with Subcellular Resolution in Breast Cancer Tissue Samples by Mass Cytometry.
Cell Syst. 2018 Jan 24;6(1):25-36.e5. doi: 10.1016/j.cels.2017.12.001
}
