context('This test contains a full example of a simple graph with 3 nodes and 2 groups where classic and histo differ')
library(neighbouRhood)
library(data.table)
#' The graph looks as follows:
#'
#' A - B
#'
#' B - C
#' |
#' C
#'
#' Legend":
#' A, B, C: nodes
#' - interaction

nperm = 100

dat_rel = data.table(`First Object Number`=c(1,2,3,4,3,5), `Second Object Number`=c(2,1,4,3,5,3),
                     `First Image Number`=1,
                     `Second Image Number`=1,
                     `First Object Name`='cell', `Second Object Name`='cell',
                     Relationship='Neighbors',
                     check.names = F)

dat_obj = data.table( ObjectNumber=c(1,2,3,4,5),ImageNumber=1, label=c('A','B','B','C', 'C'))



#' run the analysis
d = prepare_tables(dat_obj, dat_rel)

#' check aggregate_histo
dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_histo()




set.seed(123)
dat_perm = rbindlist(lapply(1:nperm, function(x){
  dat_labels = shuffle_labels(d[[1]])
  apply_labels(dat_labels, d[[2]]) %>%
    aggregate_histo()
}
), idcol = 'run')


dat_p = calc_p_vals(dat_baseline, dat_perm, n_perm = nperm)



#' check aggregate_classic
dat_baseline2 = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_classic()



set.seed(123)
dat_perm = rbindlist(lapply(1:nperm, function(x){
  dat_labels = shuffle_labels(d[[1]])
  apply_labels(dat_labels, d[[2]]) %>%
    aggregate_classic()
}
), idcol = 'run')


dat_p_2 = calc_p_vals(dat_baseline2, dat_perm, n_perm = nperm)

#' check aggregate_classic_patch
dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_classic_patch(patch=2)

test_that("Baseline statistics are calculated correctly with aggregate_classic_patch", {
  expect_equal(dat_baseline[FirstLabel =='B' & SecondLabel == 'C', ct], 0.5)
  expect_equal(sum(dat_baseline[!(FirstLabel =='B' & SecondLabel == 'C'), ct]), 0)

})

d

