## ------------------------------------------------------------------------
library(neighbouRhood)
library(data.table)
library(dplyr)
library(magrittr)
library(dtplyr)
library(ggplot2)
library(parallel)

## ------------------------------------------------------------------------
fn_cell = '/mnt/bbvolume/server_homes/janaf/Data/2018/NeighborhoodVito/20180727_celldata.csv'
fn_rel = '/mnt/bbvolume/server_homes/janaf/Data/2018/NeighborhoodVito/20180727_reldata.csv'

## ------------------------------------------------------------------------
dat_cell = fread(fn_cell)

dat_relation = fread(fn_rel)

dat_relation[, `First Object Name` := 'cell']
dat_relation[, `Second Object Name` := 'cell']
dat_relation[, `Relationship` := 'Neighbors']

## ------------------------------------------------------------------------
d = prepare_tables(dat_cell, dat_relation, col_label = 'CellType', objname = 'cell', col_group = NULL)


dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_histo()

## ------------------------------------------------------------------------
nperm = 1000

## ------------------------------------------------------------------------
start_time <- Sys.time()

dat_perm = mclapply(1:nperm, function(x){
   shuffle_labels(d[[1]]) %>%
     apply_labels(d[[2]]) %>%
    aggregate_histo()
},mc.cores = 16, mc.cleanup=T
) %>%
  rbindlist(idcol = 'run') 
end_time <- Sys.time()
print(end_time - start_time)

## ------------------------------------------------------------------------
start_time <- Sys.time()
dat_p <- calc_p_vals(dat_baseline, dat_perm, n_perm = nperm) 
end_time <- Sys.time()
print(end_time - start_time)

## ----Read in Janas output for comparison---------------------------------
fn_enr = '/mnt/bbvolume/server_homes/janaf/Data/2018/NeighborhoodVito/PvalsNicolasData_enrichment.csv'
fn_avoid = '/mnt/bbvolume/server_homes/janaf/Data/2018/NeighborhoodVito/PvalsNicolasData_avoidence.csv'

dat_j_enr = fread(fn_enr)
dat_j_avoid = fread(fn_avoid)

## ------------------------------------------------------------------------
setnames(dat_j_enr, c('CellType2new', 'CellType1new', 'Pvalue'),  c( 'FirstLabel', 'SecondLabel', 'p_jenr'))
setnames(dat_j_avoid, c('CellType2new', 'CellType1new', 'Pvalue'),  c( 'FirstLabel', 'SecondLabel', 'p_javoid'))

## ------------------------------------------------------------------------

dat =dat_p %>%
  mutate(ImageNumber= group) %>%
  merge(dat_j_enr, all=T, by= c('ImageNumber','FirstLabel', 'SecondLabel')) %>%
  merge(dat_j_avoid, all=T,  by= c('ImageNumber','FirstLabel', 'SecondLabel'))

## ------------------------------------------------------------------------
dat[is.na(p_gt), p_gt:=1]
dat[is.na(p_lt), p_lt:=1]

## ------------------------------------------------------------------------
ggplot(dat, aes(x=(p_gt), y=(p_jenr)))+
  geom_point(alpha=0.1)

## ------------------------------------------------------------------------
ggplot(dat, aes(x=(p_gt), y=(p_jenr-p_gt)))+
  geom_point(alpha=0.1)+
  geom_smooth()

## ------------------------------------------------------------------------
ggplot(dat, aes(x=log10(p_gt), y=log10(p_jenr)-log10(p_gt)))+
  geom_point(alpha=0.1)+
  geom_smooth()

## ------------------------------------------------------------------------
lm( p_gt ~ p_jenr, dat)

## ------------------------------------------------------------------------
ggplot(dat, aes(x=log10(p_gt), y=log10(p_jenr)))+
  geom_point(alpha=0.1) +
  coord_equal()

