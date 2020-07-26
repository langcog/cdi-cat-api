#
# Plumber API for running a CAT-based CDI. 
# You can run the API by clicking the 'Run API' button above.
# George Kachergis  June 19, 2020

source("API/runCAT.R")

#* @apiTitle CDI-CAT

#' Return "hello world"
#' @get /hello
function(){
  # return(colnames(coefs_2pl))
  "Hello from CDI-CAT API!"
}

#* Get start item index and definition
#* @param age_mos Child's age in months (12-36)
#* @serializer unboxedJSON
#* @get /startItem
get_start_item <- function(age_mos) {
  if(age_mos<12 | age_mos>36) start_it = 'MI' # or indicate out of age range
  start_it = subset(age_startits, age==age_mos)
  return(list(index = start_it$index[1],
              definition = start_it$definition[1]))
}

#* Get next CAT item
#* @param items Sequence of item IDs presented so far to a user (e.g.: [35,10])
#* @param responses User's responses (0/1) to each item (e.g., [1,0])
#* @serializer unboxedJSON
#* @get /nextItem
processCAT <- function(items, responses) {
  items = unlist(fromJSON(items))
  responses = unlist(fromJSON(responses))
  
  # initialize CAT
  catd <- mirtCAT(df, mod_2pl, design = preferred_design, criteria = 'MI',
                  method='ML', start_item = items[1], design_elements = T) 
  
  for(i in 1:length(items)) {
    catd <- updateCAT(catd, items[i], responses[i])
  }
  print(catd$person$thetas_history)
  nextItem = findNextItem(catd)
  return(list(index=nextItem, 
              definition=questions[nextItem], 
              curTheta=catd$person$thetas[1]))
}


#* Get item definition
#* @param itemID Given numeric ID (1-679) returns the item definition (e.g., )
#* @serializer unboxedJSON
#* @get /itemDefinition
get_item_definition <- function(itemID) {
  itemID = as.numeric(itemID)
  if(itemID>length(questions) | itemID<1)
    return(paste0("Error: itemID out of range: ",itemID))
  return(questions[itemID])
}

#* Get item definition
#* @get /itemDefinitions
get_item_definitions <- function() {
  return(questions)
}

#* Get easiest word
#* @param items Given list of numeric ID (1-679) returns the id with the lowest difficulty
#* @serializer unboxedJSON
#* @get /easiestWord
get_easiest_word <- function(items) {
  items = unlist(fromJSON(items))
  diffs <- length(items)
  for(i in 1:length(items)) {
    itemID = items[i]
    if(itemID>length(questions) | itemID<1)
      return(paste0("Error: itemID out of range: ",itemID))
    diffs[i] = difficulties[itemID]
  }
  itemID = index=items[which.min(diffs)]
  return(list(index=itemID, definition=questions[itemID]))
}

#* Get hardest word
#* @param items Given list of numeric ID (1-679) returns the id with the highest difficulty
#* @serializer unboxedJSON
#* @get /hardestWord
get_hardest_word <- function(items) {
  items = unlist(fromJSON(items))
  diffs <- length(items)
  for(i in 1:length(items)) {
    itemID = items[i]
    if(itemID>length(questions) | itemID<1)
      return(paste0("Error: itemID out of range: ",itemID))
    diffs[i] = difficulties[itemID]
  }
  itemID = index=items[which.max(diffs)]
  return(list(index=itemID, definition=questions[itemID]))
}

