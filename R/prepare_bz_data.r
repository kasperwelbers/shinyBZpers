
set_default_params <- function(min_similarity=0.05, measure='overlap_pct', weight='norm_tfidf', hour_window=c(0,7*24), ngrams=1) {
  list(min_similarity=min_similarity, hour_window=hour_window, ngrams=ngrams, weight=weight, measure=measure)
}

#' Prepare similarity data
#' 
#' @param tc          a tCorpus
#' @param from_subset a subset expression for selecting 'from' which documents to compare
#' @param to_subset   a subset expression for selecting 'to' which documents to compare
#' @param date_col    The column in tc$meta that contains a date (has to be in POSIXct)
#' @param ...         Optionally, default values for min_similarity, hour_window, ngrams, measure and weight
prepare_data <- function(tc, from_subset, to_subset, date_col, ...) {
  params = set_default_params(...)
  params$from_subset = tc$eval_meta(substitute(from_subset), parent.frame())
  params$to_subset = tc$eval_meta(substitute(to_subset), parent.frame())
  params$date_col = date_col
  if (params$ngrams == 1) params$ngrams = NA
  
  comp = do.call(corpustools::compare_documents, args = c(list(tc=tc, return_igraph=F), params))
  
  
  default_sim = list(sim = comp$d, settings = params)
  data.table::setindexv(default_sim$sim, cols='from')
  
  list(tc=tc, default_sim=default_sim)
  #usethis::use_data(tc, default_sim, overwrite=T)
}

#' Run the Shiny Application
#'
#' @param conn         An amcatr connection
#' @param project      AmCAT project
#' @param pers_set     AmCAT articleset with BZ press articles
#' @param nieuws_set   AmCAT articleset with BZ news articles
#' @param ... 
#'
#' @export
create_bz_data <- function(conn, project=1916, pers_set=79431, nieuws_set=79457, ...) {
  pers = amcatr::amcat.hits(conn, queries='*', project=project, sets=pers_set, col = c('doc_id','date','medium','headline','text'))  
  nieuws = amcatr::amcat.hits(conn, queries='*', project=project, sets=nieuws_set, col = c('doc_id','date','medium','headline','text'))  
  
  pers$from = 1
  nieuws$from = 0
  d = rbind(pers, nieuws)
  d = d[order(d$id),]
  d$title = d$headline

  tc = corpustools::create_tcorpus(d, doc_col='id', text_columns = c('title','text'), remember_spaces=T)
  tc$meta$date = as.POSIXct(tc$meta$date)
  
  prepare_data(tc, 
               from_subset = from == 1,
               to_subset = from == 0,
               date_col='date',
               ...)
}
  

