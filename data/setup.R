library(knitr)
library(kableExtra)

# a prefix nulling hook.

# make sure to keep the default for normal processing.
default_output_hook <- knitr::knit_hooks$get('output')

# output hooks handle normal R console output.
knitr::knit_hooks$set(output = function(x, options) {
  
  # grab 'comment' setting
  comment <- knitr::opts_current$get('comment')
  
  # if NA, replace comment with ''
  if(is.na(comment)) comment <- ''
  
  # regex to logically detect ' *[#]' string in x (the output string)
  can_null <- grepl(paste0(comment, '\\s*\\[\\d+\\]'), 
                    x, perl = TRUE)
  
  # check if 'null_prefix' chunk option is set to TRUE
  do_null <- isTRUE(knitr::opts_current$get('null_prefix'))
  
  if(can_null && do_null) {
    
    # R print output aligns at the right brace, gather this value - 1
    align_index <- regexpr('\\]', x)[1] - 1
    
    # two cases: start or newline; can probably combine into one using refs...
    
    #start
    # start of string, any character matching align_index times followed by ]
    re <- paste0('^.{', align_index, '}\\]\\s?')
    rep <- comment
    x <- gsub(re, rep, x) # replace re with empty string in x
    
    # new line
    # new line, any character matching align_index times followed by ]
    re <- paste0('\\\n.{', align_index, '}\\]\\s?')
    rep <- paste0('\n', comment) # new line followed by comment
    x <- gsub(re, rep, x) # replace re with new line followed by comment
  }
  
  # still unsure what this does...
  default_output_hook(x, options)
  
})

#options(width = 105) # for HTML output only; too wide for pdf
knitr::opts_chunk$set(cache = TRUE,
                      comment = NA,
                      message = FALSE, 
                      warning = FALSE, 
                      error = FALSE,
                      fig.align = 'center',
                      null_prefix = TRUE)