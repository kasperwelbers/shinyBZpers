#' @import data.table
highlight_text <- function(input, output, sim, from_table) {
  ## better approach: get x and y docs (can be multiple) then merge x tokens to y tokens (after preprocessing). use doc id to merge in case of multiple documents
  i = input$from_rows_selected
  
  x_doc = from_table$doc_id[i]
  y_docs = sim[list(from=x_doc), , on='from', nomatch=0]$to
    
  x = droplevels(tc$get(doc_id = x_doc))
  x_meta = droplevels(tc$get_meta(doc_id = x_doc))
  y = droplevels(tc$get(doc_id = y_docs))
  y_meta = droplevels(tc$get_meta(doc_id = y_docs))
  
  ngrams = input$ngrams
  ngrams = if (is.na(ngrams)) 1 else ngrams
  
  x_feature = if (nrow(x) == 0) NULL else corpustools::preprocess_tokens(x$token, ngrams = ngrams, context = x$doc_id)
  y_feature = if (nrow(y) == 0) NULL else corpustools::preprocess_tokens(y$token, ngrams = ngrams, context = y$doc_id)
  
  x_match = which(x_feature %in% levels(y_feature))
  y_match = which(y_feature %in% levels(x_feature))
  if (ngrams > 1) {
    x_match = unique(unlist(lapply(x_match, function(x) (x-ngrams+1):x)))
    y_match = unique(unlist(lapply(y_match, function(x) (x-ngrams+1):x)))
    x_match = x_match[x_match > 0]
    y_match = y_match[y_match > 0]
    
  }
  
  x$highlight = 0
  y$highlight = 0
  x$highlight[x_match] = 1
  y$highlight[y_match] = 1
  
  x$token = tokenbrowser::highlight_tokens(x$token, x$highlight, col = 'lightyellow')
  y$token = tokenbrowser::highlight_tokens(y$token, y$highlight, col = 'lightyellow')
  
  if ('space' %in% colnames(x)) {
    x$token = paste(x$token, x$space, sep='')
    y$token = paste(y$token, y$space, sep='')
  }
  
  xdoc = tokenbrowser::wrap_documents(x, subset(x_meta, select = c('doc_id','headline','date','medium')))
  ydoc = tokenbrowser::wrap_documents(y, subset(y_meta, select = c('doc_id','headline','date','medium')))
  if (length(xdoc) > 0) xdoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', xdoc)
  if (length(ydoc) > 0) ydoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', ydoc)
  
  #if (length(xdoc) > 0) {
  #  xdoc = gsub('<doc_id>.*</doc_id>', '<doc_id>%s</doc_id>', xdoc)
  #  xdoc = sprintf(xdoc, x_meta$headline)
  ###}
  #if (length(xdoc) > 0) {
  #  ydoc = gsub('<doc_id>.*</doc_id>', '<doc_id>%s</doc_id>', ydoc)
  #  ydoc = sprintf(ydoc, y_meta$headline)
  #}
  
  ## max 2 consequtive hard enter
  xdoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', xdoc)
  ydoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', ydoc)
  
  output$txt_x = shiny::renderText(xdoc)
  output$txt_y = shiny::renderText(ydoc)
}