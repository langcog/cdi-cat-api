#
# Plumber API for running a CAT-based CDI. 
# You can run the API by clicking the 'Run API' button above.
# George Kachergis  June 19, 2020

source("runCAT.R")

#* @apiTitle CDI-CAT

#' Return "hello world"
#' @get /hello
function(){
  "hello world"
}

#* Get start item index
#* @param age_mos Child's age in months (12-36)
#* @get /startItem
get_start_item <- function(age_mos) {
  if(age_mos<12 | age_mos>36) start_it = 'MI' # or indicate out of age range
  start_it = subset(age_startits, age==age_mos)$index 
  return(start_it)
}

#* Get next CAT item
#* @param items Sequence of items presented (e.g.: [35,10])
#* @param responses User responses (0/1) to each item (e.g., [1,0])
#* @get /nextItem
processCAT <- function(items, responses) {
  items = unlist(fromJSON(items))
  responses = unlist(fromJSON(responses))
  #items = unlist(lapply(strsplit(items, ","), as.integer))
  #responses = unlist(lapply(strsplit(responses, ","), as.integer))
  
  # initialize CAT
  catd <- mirtCAT(df, mod_2pl, design = preferred_design, criteria = 'MI',
                  method='ML', start_item = items[1], design_elements = T) 
  
  for(i in 1:length(items)) {
    catd <- updateCAT(catd, items[i], responses[i])
  }
  print(catd$person$thetas_history)
  nextItem = findNextItem(catd)
  return(list(nextItem=nextItem, curTheta=catd$person$thetas))
}


#pr <- plumb('plumber.R')
