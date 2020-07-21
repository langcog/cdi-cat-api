# functions for running mirtCAT from python
library(mirtCAT)
library(tibble)
library(jsonlite)

load("eng_ws_wg_mod_2pl_nobad.Rds")

age_startits = read.csv(file="EN_production_start_items_by_age.csv")

set.seed(123)


choices <- matrix(c(0,1), nrow(coefs_2pl), 2, byrow = TRUE)
questions <- coefs_2pl$definition
df <- data.frame(Question=questions, 
                 Option = choices, 
                 Type = 'radio', stringsAsFactors = FALSE)

preferred_design = list(min_items = 25,
                        max_items = 50, 
                        min_SEM = 0.15)

# given an age (in months), returns CAT design with appropriate start item
initializeCAT <- function(age_mos) {
  if(age_mos<12 | age_mos>36) start_it = 'MI' # or indicate out of age range
  start_it = subset(age_startits, age==age_mos)$index 
  catd <- mirtCAT(df, mod_2pl, design = preferred_design, criteria = 'MI',
                      method='ML', start_item = start_it, design_elements = T) 
  return(catd)
}


updateCAT <- function(catd, item, response) { # pass in all items seen and all responses, or just the new one?
  done = F
  catd <- updateDesign(catd, item, response) # 0=not producing, 1=produces
  # update theta
  catd$design@Update.thetas(catd$design, catd$person, catd$test)
  print(catd$person$thetas) # shows just single updated theta estimate
  
  theta_SE = tail(catd$person$thetas_SE_history, n=1)
  num_responses = length(na.omit(catd$person$responses))
  
  if(theta_SE <= catd$design@min_SEM & num_responses >= catd$design@min_items) {
    done = T
  } else if(num_responses >= catd$design@max_items) {
    done = T
  }
  
  if(done) {
    catd$design@stop_now = T
    catd$person$terminated_sucessfully = T
  }
  return(catd)
}

get_CAT_summary <- function(catd) {
  items_answered = na.omit(catd$person$items_answered)
  responses = catd$person$responses[items_answered]
  end_ind = length(catd$person$thetas_history)
  
  dat = tibble(index = 1:length(items_answered),
              item_inds = items_answered,
              items = coefs_2pl$definition[items_answered],
              responses = responses,
              thetas = catd$person$thetas_history[2:end_ind],
              thetaSE = catd$person$thetas_SE_history[2:end_ind]) # omit starting theta (0)
  return(dat)
}


# test sequence
# input response (0="not produces" / 1="produces"), update theta, get new item
test <- function() {
  
  # initialize CAT for 24-month-old
  catd = initializeCAT(24)
  nextItem = findNextItem(catd) # the start item
  
  responses = rep(c(0,1), 30)
  for(i in 1:60) {
    if(!catd$design@stop_now) {
      catd <- updateCAT(catd, nextItem, responses[i])
      nextItem = findNextItem(catd)
    }
  }
  
  print(get_CAT_summary(catd))

}

# test()


