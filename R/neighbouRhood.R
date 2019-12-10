library(magrittr)
library(dplyr)
library(dtplyr)
library(data.table)
.datatable.aware = TRUE

#' @import magrittr
#' @import data.table
#' @importFrom  dplyr select filter mutate group_by summarise
#' @import dtplyr

IMGNR = 'ImageNumber'
OBJNR = 'ObjectNumber'
OBJNAME = 'ObjectName'
OBJID = 'ObjectID'
GROUP = 'group'
LABEL = 'label'

FIRSTOBJNAME = 'First Object Name'
FIRSTIMAGENUMBER = 'First Image Number'
FIRSTOBJNUMBER = 'First Object Number'
FIRSTOBJID = 'First Object ID'

FIRSTLABEL = 'FirstLabel'

SECONDOBJNAME = 'Second Object Name'
SECONDIMAGENUMBER = 'Second Image Number'
SECONDOBJNUMBER = 'Second Object Number'
SECONDOBJID = 'Second Object ID'
SECONDLABEL = 'SecondLabel'

RELATIONSHIP = 'Relationship'
DEFAULTRELATIONSHIP = 'Neighbors'
DEFAULTOBJNAME = 'cell'

COUNTVAR = 'ct'


#' prepare_tables
#' This data prepares CellProfiller style input tables into standardized tables
#' optimized for the neightbourhood analysis.
#'
#' @param dat_obj an object level data.table from cellprofiler
#' @param dat_rel a relationship table from cellprofiler
#' @param objname the objectname of the object table
#' @param relationship the relationship table
#' @param col_imnr the image number column
#' @param col_objnr the object number column
#' @param col_label the label/cluster number to do the permutation test on
#' @param col_group a grouping to do the permutation test with, default: do it per image
#' @param col_relationship the column name for the relationship column
#' @param col_objname the column for the object name column
#' @return returns the percentile of each value of x
#'      a list with elements:
#'
#'           [[1]] a obj table with columns:
#'              'ObjectID': a new, integer ID from 1:N
#'              'label': the group/cluster label of the cells
#'              'ImageNumber': the CP imagenumber
#'              'ObjectNumber': the CP objectnumber
#'              'Group': The group to do the permutations by
#'              'ObjectName': The object name
#'
#'           [[2]] a relationship table with columns:
#'            'First Object ID': Id of the first object
#'            'Second Object ID': Id of the second object
#'            'Group': the group to which the relationship belongs
#' @export
prepare_tables <- function(dat_obj, dat_rel, objname=DEFAULTOBJNAME,
                           relationship = DEFAULTRELATIONSHIP,
                           col_imnr=IMGNR,
                           col_objnr=OBJNR,
                           col_label=LABEL,
                           col_group=NULL,
                           col_relationship=RELATIONSHIP,
                           col_objname=OBJNAME
){

  cols = c(col_imnr, col_objnr, col_label)

  has_objname = F
  if (col_objname %in% colnames(dat_obj)){
    cols = c(cols, col_objname)
    has_objname = T
  }

  has_group = F
  if (!is.null(col_group)){
    cols = c(cols, col_group)
    has_group = T
  }

  # subset and copy the relevant part of the data
  dat_obj <- copy(dat_obj[,cols, with=F])

  # set the objectname if needed
  if (has_objname == F){
    dat_obj[,(col_objname) := objname]
  } else{
    dat_obj = dat_obj[get(col_objname) == objname]

  }

  # set the group if needed
  if (has_group == F){
    col_group = GROUP
    dat_obj[,(col_group):=get(col_imnr)]
  }

  # rename all the columns
  setnames(dat_obj,
      c(col_imnr, col_objnr, col_label, col_group,
        col_objname),
      c(IMGNR, OBJNR, LABEL, GROUP,
        OBJNAME)
    )

  dat_obj[, label:=as.factor(label)]

  # same for the relationship table

  dat_rel = copy(dat_rel[(get(RELATIONSHIP) == relationship) &
          (get(FIRSTOBJNAME) == objname)&
          (get(SECONDOBJNAME) == objname), ][, c(FIRSTOBJNAME, FIRSTIMAGENUMBER, FIRSTOBJNUMBER,
                                                 SECONDOBJNAME,SECONDIMAGENUMBER,  SECONDOBJNUMBER,
                                                 RELATIONSHIP), with=F])



  # give new ids
  dat_obj[, (OBJID) := 1:.N]

  dat_rel <- merge(dat_rel, dat_obj[, c(IMGNR, OBJNR, OBJID, GROUP), with=F],
          by.x=c(FIRSTIMAGENUMBER, FIRSTOBJNUMBER),
          by.y=c(IMGNR, OBJNR)
    )
  setnames(dat_rel, OBJID, FIRSTOBJID)
  dat_rel <- merge(dat_rel, dat_obj[, c(IMGNR, OBJNR, OBJID), with=F],
          by.x=c(SECONDIMAGENUMBER, SECONDOBJNUMBER),
          by.y=c(IMGNR, OBJNR)
    )

  setnames(dat_rel, OBJID, SECONDOBJID)
  dat_rel[, (COUNTVAR) := 1]
  dat_rel <- dat_rel[, c(GROUP, FIRSTOBJID, SECONDOBJID), with=F]


  return(list(dat_obj, dat_rel))
}

#' Shuffle_labels
#'
#' This shuffles the labels by group in a standardized dat_labels table
#' @param  dat_labels a labels table as formated by the prepare_tables function. Must have columns 'label', 'ObjectID', 'group'
#' @return returns a copy of dat_labels with shuffeled labels within the group
#' @export
shuffle_labels <- function(dat_labels){
  return(dat_labels[ , .(label=sample(label), ObjectID=ObjectID), by=group])
}

#' Applies the labels on a a relationship table
#' @param   dat_labels a labels table as formated by the prepare_tables function. Must have columns 'label', 'ObjectID', 'group'
#' @param dat_rel a relationship table as formated by the prepare_tables function. Must have columns "First Object ID", "Second Object ID"
#'
#' @return a copy of the dat_rel table with additional coluns FirstLabel and SecondLabel
#' @export
apply_labels <- function(dat_labels, dat_rel){
  labels = dat_labels[, get(LABEL)]
  objid = dat_labels[, get(OBJID)]
  labvec = rep(labels[1], max(objid))
  labvec[objid] = labels

  dat_rel[, (FIRSTLABEL) := labvec[get(FIRSTOBJID)]]
  dat_rel[, (SECONDLABEL) := labvec[get(SECONDOBJID)]]
  dat_rel
}


#' Calculates the the `average neightourhood`
#'
#' Calculates: How many neightbours of type B does a cell of type A have on average.
#' @param dat_nb a neightbourhood table with the labels applied with 'apply_labels'
#' @return a statistics with columns group, Firstlabel, Secondlabel, ct
#' @export
aggregate_classic<- function(dat_nb){
  dcast.data.table(dat_nb, paste0(GROUP, '+', FIRSTLABEL, '+`', FIRSTOBJID, '`~', SECONDLABEL),
                   value.var = COUNTVAR,
                   fun.aggregate=sum, fill=0) %>%
    melt.data.table(id.vars=c(GROUP, FIRSTLABEL, FIRSTOBJID),
                    variable.name=SECONDLABEL,
                    value.name=COUNTVAR) %>%
    dcast.data.table(paste0(GROUP, '+', FIRSTLABEL, '~', SECONDLABEL),
                     value.var = COUNTVAR,
                     fun.aggregate=mean, fill=0) %>%
    melt.data.table(id.vars=c(GROUP, FIRSTLABEL),
                    variable.name=SECONDLABEL,
                    value.name=COUNTVAR)
}



#' Calculates HistoCAT-style neightbourhood statistics
#'
#' Calculates: How many many neightbours of type B has a cell of type A on average, given it has at least one neigthbour of type B?
#' @param dat_nb a neightbourhood table with the labels applied with 'apply_labels'
#' @return a statistics with columns group, Firstlabel, Secondlabel, ct
#' @export
aggregate_histo <- function(dat_nb){
  dat_temp = dat_nb[, .(ct=.N), by=.(group, FirstLabel, SecondLabel, `First Object ID`)]
  dat_temp[, .(ct=mean(ct)), by=.(group, FirstLabel, SecondLabel)]
}

#' Calculates p-values from the permutation data as well as the baseline
#' @param dat_baseline a baseline statistics table calculated by running the aggregate_* function on the unpermuted data
#' @param dat_perm the permuatation data
#' @param n_perm number of permuations used to calculate dat_perm
#' @param p_tresh p value threshold to be used
#' @return a table containing the results of the permutation test.
#'         Columns:
#'             group: group id
#'             FirstLabel: first label
#'             SecondLabel: second label
#'             p_gt: fraction of permutations equal or bigger than baseline
#'             p_lt: fraction of permutations equal or less than baseline
#'             direction: it the p_lt smaller than p_gt?
#'             p: the smaller p value of p_gt and p_lt
#'             sig: it the p significant compared to the p_tresh?
#'             sigval: -1=if sig and direction==False, 0=if not sig, 1=if sig and direction==True
#'
#'
#' @export
calc_p_vals<- function(dat_baseline, dat_perm, n_perm, p_tresh=0.01){
  dat_perm <-
    merge(dat_perm, dat_baseline[, c(FIRSTLABEL, SECONDLABEL, GROUP, COUNTVAR), with=F], by=c(FIRSTLABEL, SECONDLABEL, GROUP),
          suffixes = c("_perm", "_obs"),all=T)
  dat_perm[, ':='(ct_perm=replace(ct_perm, is.na(ct_perm), 0),
                  ct_obs=replace(ct_obs, is.na(ct_obs), 0)
                  )]


  dat_stat = dat_perm[ , .(p_gt=ifelse(max(ct_obs)==0, 1,(sum(ct_perm>=ct_obs)+1)/(n_perm+1)),
                  p_lt=(n_perm-sum(ct_perm>ct_obs)+1)/(n_perm+1)) , by=.(group, FirstLabel, SecondLabel)]

  dat_stat[, direction := p_gt < p_lt]
  dat_stat[, p := p_gt * direction + p_lt * (direction == F)]
  dat_stat[, sig := p < p_tresh]
  dat_stat[, sigval := as.integer(sig)*sign((direction-0.5))]
  dat_stat
}
