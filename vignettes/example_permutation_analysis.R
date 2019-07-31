## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(magrittr)
library(dtplyr)
library(ggplot2)
library(parallel)
library(neighbouRhood)
library(gplots)
library(RColorBrewer)

## ------------------------------------------------------------------------
# path to a (potentially modified) cellprofiller like object measurements file
fn_cells = system.file("extdata", "cell.csv", package = "neighbouRhood", mustWork = TRUE)
# path to the Object relationships
fn_relationship = system.file("extdata", "Object relationships.csv", package = "neighbouRhood", mustWork = TRUE)

n_perm = 1000

# Number of cores used for multicore:
ncores=10

## ------------------------------------------------------------------------
dat_cells = fread(fn_cells)
dat_relation = fread(fn_relationship)

## ------------------------------------------------------------------------
dat_cells[, label := sample.int(10, size=.N, replace = T)]
dat_cells[, group := ImageNumber]

## ------------------------------------------------------------------------
d = prepare_tables(dat_cells, dat_relation)

## ------------------------------------------------------------------------
dat_baseline = apply_labels(d[[1]], d[[2]]) %>%
  aggregate_histo()

## ------------------------------------------------------------------------
set.seed(12312)
dat_perm = rbindlist(mclapply(1:n_perm, function(x){
  dat_labels = shuffle_labels(d[[1]])
  apply_labels(dat_labels, d[[2]]) %>%
    aggregate_histo()
},mc.cores = ncores
), idcol = 'run') 


## ------------------------------------------------------------------------
ggplot(dat_perm %>% filter(group==1), aes(x=ct)) +
  facet_grid(FirstLabel~SecondLabel)+
  geom_histogram() +
  geom_vline(data=dat_baseline%>% filter(group==1),aes(xintercept=ct), color='red')

## ------------------------------------------------------------------------
dat_p <- calc_p_vals(dat_baseline, dat_perm, n_perm = 1000, p_tresh = 0.01) 


## ------------------------------------------------------------------------


pmat = dcast(dat_p, 'FirstLabel ~ SecondLabel', value.var = 'sigval', fun.aggregate = sum,
             fill=0, drop=F)

rname = pmat$FirstLabel

pmat = pmat %>%
  select(-c('FirstLabel')) %>%
  as.matrix()

row.names(pmat) <- rname

## ----fig.height=5, fig.width=5-------------------------------------------

cols = rev(brewer.pal(11,'Spectral'))
cmap = colorRampPalette(cols)


hr <- hclust(dist(pmat), method="ward.D")
heatmap.2(pmat,
          Colv = as.dendrogram(hr),
          Rowv = as.dendrogram(hr),
          trace = "none",
          col=cmap(75),
          density.info ='none'
          #comments = data.frame(names = row.names(tab_Prot))
)

