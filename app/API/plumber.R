#
# Plumber API for running a CAT-based CDI. 
# You can run the API by clicking the 'Run API' button above.
# George Kachergis  June 19, 2020
# updated June 19, 2022 to handle multiple languages (now FR, SP, and EN)

source("runCAT.R")

#* @apiTitle CDI-CAT

#' Return "hello world" 
#' @get /hello
function(){
  # return(colnames(coefs_2pl))
  "Hello from CDI-CAT API - multi lingual version!"
}

#* Get start item index and definition
#* @param age_mos Child's age in months (12-36)
#* @serializer unboxedJSON
#* @get /startItem
get_start_item <- function(age_mos, language) {
  if(age_mos<12 | age_mos>36) start_it = 'MI' # or indicate out of age range
  start_it = subset(age_startits[[language]], age==age_mos)
  return(list(index = start_it$index[1],
              definition = start_it$definition[1]))
}

#* Get next CAT item
#* @param items Sequence of item IDs presented so far to a user (e.g.: [35,10])
#* @param responses User's responses (0/1) to each item (e.g., [1,0])
#* @serializer unboxedJSON
#* @get /nextItem
processCAT <- function(items, responses, language) {
  items = unlist(fromJSON(items))
  responses = unlist(fromJSON(responses))
  df <- getCATquestions(language)
  # initialize CAT
  catd <- mirtCAT(df, irt_models[[language]], design = preferred_design, criteria = 'MI',
                  method='ML', start_item = items[1], design_elements = T) 
  
  for(i in 1:length(items)) {
    catd <- updateCAT(catd, items[i], responses[i])
  }
  
  print(catd$person$thetas_history)
  nextItem = findNextItem(catd)
  return(list(index = nextItem, 
              definition = irt_coefs[[language]]$definition[nextItem], 
              curTheta = catd$person$thetas[1],
              stop = catd$design@stop_now))
}


#* Get item definition
#* @param itemID Given numeric ID (1-679) returns the item definition (e.g., )
#* @serializer unboxedJSON
#* @get /itemDefinition
get_item_definition <- function(itemID, language) {
  itemID = as.numeric(itemID)
  if(itemID>length(irt_coefs[[language]]$definition) | itemID<1)
    return(paste0("Error: itemID out of range: ",itemID))
  return(irt_coefs[[language]]$definition[itemID])
}

#* Get item definition
#* @get /itemDefinitions
get_item_definitions <- function(language) {
  return(irt_coefs[[language]]$definition)
}

#* Get easiest word
#* @param items Given list of numeric ID (1-679) returns the id with the lowest difficulty
#* @serializer unboxedJSON
#* @get /easiestWord
get_easiest_word <- function(items, language) {
  items = unlist(fromJSON(items))
  if(min(items)<1 | max(items)>nrow(irt_coefs[[language]])) 
    return(paste0("Error: itemID out of range: ",itemID))
  # item difficulties (actually 'easiness', since mirt reverses it)
  diffs <- irt_coefs[[language]]$d[items]
  index = items[which.max(diffs)] # find easiest 
  return(list(index = index, 
              definition = irt_coefs[[language]]$definition[index]))
}

#* Get hardest word
#* @param items Given list of numeric ID (1-679) returns the id with the highest difficulty
#* @serializer unboxedJSON
#* @get /hardestWord
get_hardest_word <- function(items, language) {
  items = unlist(fromJSON(items))
  if(min(items)<1 | max(items)>nrow(irt_coefs[[language]])) 
    return(paste0("Error: itemID out of range: ",itemID))
  # item difficulties (actually 'easiness', since mirt reverses it)
  diffs <- irt_coefs[[language]]$d[items]
  index = items[which.min(diffs)] # find hardest 
  return(list(index = index, 
              definition = irt_coefs[[language]]$definition[index]))
}

#* tests
# get_easiest_word(1:nrow(irt_coefs[["EN"]]), "EN") # "ball"
# get_easiest_word(1:nrow(irt_coefs[["FR"]]), "FR") # "yeux"
# get_hardest_word(1:nrow(irt_coefs[["EN"]]), "EN") # "would"
# get_hardest_word(1:nrow(irt_coefs[["FR"]]), "FR") # "au sommet de"