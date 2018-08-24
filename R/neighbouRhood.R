library(data.table)
library(dplyr)
library(magrittr)
library(dtplyr)

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
  dat_obj %<>%
    select(cols) %>%
    copy()

  # set the objectname if needed
  if (has_objname == F){
    dat_obj %<>%
      dplyr::mutate(x = objname) %>%
      setnames('x',col_objname)
  } else{
    dat_obj %<>%
      filter(get(col_objname) == objname)
  }

  # set the group if needed
  if (has_group == F){
    col_group = GROUP
    dat_obj %<>%
      dplyr::mutate(x = get(col_imnr)) %>%
      setnames('x',col_group)

  }

  # rename all the columns
  dat_obj %<>%
    setnames(
      c(col_imnr, col_objnr, col_label, col_group,
        col_objname),
      c(IMGNR, OBJNR, LABEL, GROUP,
        OBJNAME)
    )

  dat_obj[, label:=as.factor(label)]

  # same for the relationship table

  dat_rel %<>%
    select(c(FIRSTOBJNAME, FIRSTIMAGENUMBER, FIRSTOBJNUMBER,
             SECONDOBJNAME,SECONDIMAGENUMBER,  SECONDOBJNUMBER,
             RELATIONSHIP)) %>%
    filter(get(RELATIONSHIP) == relationship,
           get(FIRSTOBJNAME) == objname,
           get(SECONDOBJNAME) == objname) %>%
    copy()

  # give new ids
  dat_obj %<>%
    mutate(x = 1:.N) %>%
    setnames('x', OBJID)

  dat_rel %<>%
    merge(dat_obj %>% select(c(IMGNR, OBJNR, OBJID, GROUP)),
          by.x=c(FIRSTIMAGENUMBER, FIRSTOBJNUMBER),
          by.y=c(IMGNR, OBJNR)
    ) %>%
    setnames(OBJID, FIRSTOBJID) %>%
    merge(dat_obj %>% select(c(IMGNR, OBJNR, OBJID)),
          by.x=c(SECONDIMAGENUMBER, SECONDOBJNUMBER),
          by.y=c(IMGNR, OBJNR)
    ) %>%
    setnames(OBJID, SECONDOBJID) %>%
    select(c(GROUP, FIRSTOBJID, SECONDOBJID)) %>%
    mutate(x=1) %>%
    setnames('x', COUNTVAR)


  return(list(dat_obj, dat_rel))
}

#' @export
shuffle_labels <- function(dat_labels){
  return(dat_labels[ , .(label=sample(label), ObjectID=ObjectID), by=group])
}

#' @export
apply_labels <- function(dat_labels, dat_rel){
  labels = dat_labels[, get(LABEL)]
  objid = dat_labels[, get(OBJID)]
  labvec = rep(labels[1], max(objid))
  labvec[objid] = labels

  dat_rel %>%
    mutate(fx = labvec[get(FIRSTOBJID)],
           sx = labvec[get(SECONDOBJID)]) %>%
    setnames(c('fx', 'sx'), c(FIRSTLABEL, SECONDLABEL))
}

#' @export
aggregate_classic_wrong <- function(dat_nb){
  dcast(dat_nb,paste0(GROUP, '+', FIRSTLABEL, '~', SECONDLABEL),
        value.var = COUNTVAR,
        fun.aggregate=length, fill=0) %>%
    melt(id.vars=c(GROUP, FIRSTLABEL),
         variable.name=SECONDLABEL,
         value.name=COUNTVAR)% >%
    dcast.data.table(paste0(GROUP, '+', SECONDLABEL, '~', FIRSTLABEL),
                     value.var = COUNTVAR,
                     fun.aggregate=mean, fill=0) %>%
    melt.data.table(id.vars=c(GROUP, SECONDLABEL),
                    variable.name=FIRSTLABEL,
                    value.name=COUNTVAR)
}

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

#' @export
aggregate_histo <- function(dat_nb){
  dat_temp = dat_nb[, .(ct=.N), by=.(group, FirstLabel, SecondLabel, `First Object ID`)]
  dat_temp[, .(ct=mean(ct)), by=.(group, FirstLabel, SecondLabel)]
}

#' @export
calc_p_vals<- function(dat_baseline, dat_perm, n_perm, p_tresh=0.01){
  dat_perm %>%
    merge(dat_baseline %>%
            select(c(FIRSTLABEL, SECONDLABEL, GROUP, COUNTVAR)), by=c(FIRSTLABEL, SECONDLABEL, GROUP),
          suffixes = c("_perm", "_obs"),all=T) %>%
    mutate(ct_perm=replace(ct_perm, is.na(ct_perm), 0),
           ct_obs=replace(ct_obs, is.na(ct_obs), 0)
    ) %>%
    group_by(group, FirstLabel, SecondLabel) %>%
    summarise(p_gt=ifelse(max(ct_obs)==0, 1,(sum(ct_perm>=ct_obs)+1)/(n_perm+1)),
              p_lt=(n_perm-sum(ct_perm>ct_obs)+1)/(n_perm+1)) %>%
    ungroup() %>%
    mutate(direction=p_gt < p_lt)%>%
    mutate(p = p_gt * direction + p_lt * (direction == F))%>%
    mutate(sig = p < p_tresh)%>%
    mutate(sigval = as.integer(sig)*sign((direction-0.5)))
}
