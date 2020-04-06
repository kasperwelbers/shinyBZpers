#' @import data.table
highlight_text <- function(input, output, data, state) {
 
  if (length(state$doc_ids) == 0 || is.null(input$articlelist_rows_selected)) {
    output$txt_x = shiny::renderText('')
    output$txt_y = shiny::renderText('')
    return(NULL)
  } 
  
  if (input$pers_of_nieuws == 'pers') {
    x_docs = state$doc_ids[input$articlelist_rows_selected]
    y_matches = state$sim[list(from=x_docs), , on='from', nomatch=0]
    if (length(input$media) > 0) {
      y_matches = y_matches[list(medium=input$media),,on='medium', nomatch=0]
    }
    y_docs = y_matches$to
  } else {
    y_docs = state$doc_ids[input$articlelist_rows_selected]
    x_matches = state$sim[list(to=y_docs), , on='to', nomatch=0]
    x_docs = x_matches$from
  }
  
  
  tc = get_tc(data$db_file, doc_ids=c(x_docs,y_docs))
  #tc$preprocess('lemma', new_column = 'ngram1')
  tc$preprocess('lemma', new_column = 'ngram2', ngrams=2)
  tc$preprocess('lemma', new_column = 'ngram3', ngrams=3)
  tc$preprocess('lemma', new_column = 'ngram4', ngrams=4)
  
  x = droplevels(tc$get(doc_id = x_docs))
  x_meta = droplevels(tc$get_meta(doc_id = x_docs))

  y = droplevels(tc$get(doc_id = y_docs))
  y_meta = droplevels(tc$get_meta(doc_id = y_docs))
  #y_meta$similarity = round(y_sim, 2)

  ## workaround for bug in tokenbrowser (solved in 0.1.3, but not yet on cran)
  y$doc_id = match(y$doc_id, unique(y$doc_id))
  y_meta$doc_id = 1:nrow(y_meta)
  
  #ngrams = input$ngrams
  
  #x_feature = if (nrow(x) == 0) NULL else corpustools::preprocess_tokens(x$token, ngrams = ngrams, context = x$doc_id)
  #y_feature = if (nrow(y) == 0) NULL else corpustools::preprocess_tokens(y$token, ngrams = ngrams, context = y$doc_id)
  
  x$highlight = NA
  y$highlight = NA
  for (ng in 2:4) {
    x_feature = x[[paste0('ngram', ng)]]
    y_feature = y[[paste0('ngram', ng)]]
    
    x_match = which(x_feature %in% levels(y_feature))
    y_match = which(y_feature %in% levels(x_feature))
    if (ng > 1) {
      #x_match = unique(unlist(lapply(x_match, function(x) (x-ng+1):x)))
      #y_match = unique(unlist(lapply(y_match, function(x) (x-ng+1):x)))
      
      x_match = unique(unlist(lapply(x_match, function(i) {
        i_ngram = (i-ng+1):i
        i_ngram = i_ngram[i_ngram > 0]
        same_doc = x$doc_id[i] == x$doc_id[i_ngram]
        i_ngram[same_doc]
      })))
      y_match = unique(unlist(lapply(y_match, function(i) {
        i_ngram = (i-ng+1):i
        i_ngram = i_ngram[i_ngram > 0]
        same_doc = y$doc_id[i] == y$doc_id[i_ngram]
        i_ngram[same_doc]
      })))
      
      #x_match = x_match[x_match > 0]
      #y_match = y_match[y_match > 0]
    }
    
    #x$highlight[x_match] = 0.4 + (0.6*ng/4)
    #y$highlight[y_match] = 0.4 + (0.6*ng/4)
    x$highlight[x_match] = ng / 4
    y$highlight[y_match] = ng / 4
  }
  #x$token = tokenbrowser::highlight_tokens(x$token, x$highlight, col = 'lightyellow')
  #y$token = tokenbrowser::highlight_tokens(y$token, y$highlight, col = 'lightyellow')


  x$token = tokenbrowser::colorscale_tokens(x$token, x$highlight, alpha = 0.3, col_range = c('lightyellow','blue'), span_adjacent = T, doc_id=x$doc_id)
  y$token = tokenbrowser::colorscale_tokens(y$token, y$highlight, alpha = 0.3, col_range = c('lightyellow','blue'), span_adjacent = T, doc_id=y$doc_id)
  
    
  if ('space' %in% colnames(x)) {
    x$token = paste(x$token, x$space, sep='')
    y$token = paste(y$token, y$space, sep='')
  }
  
  xdoc = tokenbrowser::wrap_documents(x, subset(x_meta, select = c('doc_id','headline','date','medium')))
  ydoc = tokenbrowser::wrap_documents(y, subset(y_meta, select = c('doc_id','headline','date','medium')))
  if (length(xdoc) > 0) xdoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', xdoc)
  if (length(ydoc) > 0) ydoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', ydoc)
  

  ## max 2 consequtive hard enter
  xdoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', xdoc)
  ydoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', ydoc)
  
  ## remove \s (used in udpipe)
  xdoc = gsub('\\s', '', xdoc, fixed=T)
  ydoc = gsub('\\s', '', ydoc, fixed=T)
  
  output$txt_x = shiny::renderText(xdoc)
  output$txt_y = shiny::renderText(ydoc)
}