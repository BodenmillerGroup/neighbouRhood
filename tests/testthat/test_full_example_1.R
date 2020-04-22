context('This test contains a full example of a simple graph with 3 nodes and 2 groups')

#' The graph looks as follows:
#'
#' W = B
#'
#'   W
#'
#' Legend":
#' W=a white node
#' B=a black node
#' '=' a connection

nperm = 100

dat_rel = data.table(`First Object Number`=c(1,2), `Second Object Number`=c(2,1),
                     `First Image Number`=1,
                     `Second Image Number`=1,
                     `First Object Name`='cell', `Second Object Name`='cell',
                     Relationship='Neighbors',
                     check.names = F)

dat_obj = data.table( ObjectNumber=c(1,2,3),ImageNumber=1, label=c('white','black','white'))



#' run the analysis
d = prepare_tables(dat_obj, dat_rel)

#' check aggregate_histo
dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_histo()

test_that("Baseline statistics are calculated correctly  with aggregate_histo", {
  expect_equal(dat_baseline[FirstLabel =='black' & SecondLabel == 'white', ct], 1)
  expect_equal(dat_baseline[FirstLabel =='white' & SecondLabel == 'black', ct], 1)
  expect_equal(nrow(dat_baseline), 2)
})


set.seed(123)
dat_perm = rbindlist(lapply(1:nperm, function(x){
  dat_labels = shuffle_labels(d[[1]])
  apply_labels(dat_labels, d[[2]]) %>%
    aggregate_histo()
}
), idcol = 'run')


dat_p = calc_p_vals(dat_baseline, dat_perm, n_perm = nperm)
test_that("Permutation statistics are calculated correctly", {
  expect_equal(dat_p[FirstLabel =='black' & SecondLabel == 'white', p_lt], 1)
  expect_equal(dat_p[FirstLabel =='white' & SecondLabel == 'black', p_lt], 1)
  expect_equal(dat_p[FirstLabel =='white' & SecondLabel == 'white', p_gt], 1)
  p = dat_p[ , unique(p)]

  # In this example the probability should be allways the same for all the three groups
  expect_equal(length(p), 1)

  # These bounds are fairly arbitrary, for a lot of permutations the value should converge to 0.66
  expect_gt(p, 0.55)
  expect_lt(p, 0.75)

  # Check the directions are all correctly calculated
  expect_true(all(dat_p[, (p_gt < p_lt) == direction ]))

})

#' check aggregate_classic
dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_classic()

test_that("Baseline statistics are calculated correctly with aggregate_classic", {
  expect_equal(dat_baseline[FirstLabel =='black' & SecondLabel == 'white', ct], 1)
  expect_equal(dat_baseline[FirstLabel =='white' & SecondLabel == 'black', ct], 1)
  expect_equal(dat_baseline[FirstLabel =='white' & SecondLabel == 'white', ct], 0)
  expect_equal(dat_baseline[FirstLabel =='black' & SecondLabel == 'black', ct], 0)
  expect_equal(nrow(dat_baseline), 4)
})


set.seed(123)
dat_perm = rbindlist(lapply(1:nperm, function(x){
  dat_labels = shuffle_labels(d[[1]])
  apply_labels(dat_labels, d[[2]]) %>%
    aggregate_classic()
}
), idcol = 'run')


dat_p = calc_p_vals(dat_baseline, dat_perm, n_perm = nperm)
test_that("Permutation statistics are calculated correctly", {
  expect_equal(dat_p[FirstLabel =='black' & SecondLabel == 'white', p_lt], 1)
  expect_equal(dat_p[FirstLabel =='black' & SecondLabel == 'black', p_gt], 1)
  expect_equal(dat_p[FirstLabel =='black' & SecondLabel == 'black', p_lt], 1)
  expect_equal(dat_p[FirstLabel =='white' & SecondLabel == 'black', p_lt], 1)
  expect_equal(dat_p[FirstLabel =='white' & SecondLabel == 'white', p_gt], 1)

  p = dat_p[ p!= 1 , unique(p)]

  # In this example the probability should be allways the same for all the three groups
  expect_equal(length(p), 1)

  # These bounds are fairly arbitrary, for a lot of permutations the value should converge to 0.66
  expect_gt(p, 0.55)
  expect_lt(p, 0.75)

  # P values should be symmetric for the classic case:
  expect_equal(dat_p[FirstLabel =='black' & SecondLabel == 'white', p],
               dat_p[FirstLabel =='black' & SecondLabel == 'white', p])
  # Check the directions are all correctly calculated
  expect_true(all(dat_p[, (p_gt < p_lt) == direction ]))

})
