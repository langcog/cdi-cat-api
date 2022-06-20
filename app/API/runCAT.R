# functions for running mirtCAT from python
library(mirtCAT)
library(tibble)
library(jsonlite)

# load combined CAT parameters (for all languages, saved in 00-combine-and-save-CAT-parms.R)
load("API/combined_CAT_parms.Rdata")


age_startits = list(EN = read.csv(file="API/EN_production_start_items_by_age.csv"),
                    FR = read.csv(file="API/FR_production_start_items_by_age.csv"),
                    SP = read.csv(file="API/SP_production_start_items_by_age.csv"))

# find mod_2pl and coefs_2pl

set.seed(123)

preferred_design = list(min_items = 25,
                        max_items = 50, 
                        min_SEM = 0.15)

getCATquestions <- function(language) {
  choices <- matrix(c(0,1), nrow(irt_coefs[[language]]), 2, byrow = TRUE)
  questions <- irt_coefs[[language]]$definition
  #difficulties <- irt_coefs[[language]]$d
  #colnames(irt_coefs[[language]])
  df <- data.frame(Question=questions, 
                   Option = choices, 
                   Type = 'radio', stringsAsFactors = FALSE)
  return(df)
}

# given an age (in months), and a language, returns CAT design with appropriate start item
initializeCAT <- function(age_mos, language) {
  df <- getCATquestions(language)
  if(age_mos<12 | age_mos>36) start_it = 'MI' # or indicate out of age range
  start_it = subset(age_startits[[language]], age==age_mos)$index 
  catd <- mirtCAT(df, irt_models[[language]], design = preferred_design, criteria = 'MI',
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

get_CAT_summary <- function(catd, language) {
  its = catd$person$items_answered
  items_answered = its[which(!is.na(its))]
  responses = catd$person$responses[items_answered]
  end_ind = length(catd$person$thetas_history) # has duplicate entries?
  
  thetas_ind = seq(1,end_ind, 2)
  
  dat = tibble(index = 1:length(items_answered),
              item_inds = items_answered,
              items = irt_coefs[[language]]$definition[items_answered],
              responses = responses,
              thetas = catd$person$thetas_history[thetas_ind[-1]],
              thetaSE = catd$person$thetas_SE_history[thetas_ind[-1]]) # omit starting theta (0)
  return(dat)
}


# test sequence
# input response (0="not produces" / 1="produces"), update theta, get new item
test <- function() {
  
  # initialize CAT for 24-month-old
  catd = initializeCAT(24, "EN")
  nextItem = findNextItem(catd) # the start item
  print(nextItem)
  responses = rep(c(0,1), 30)
  for(i in 1:60) {
    if(!catd$design@stop_now) {
      catd <- updateCAT(catd, nextItem, responses[i])
      nextItem = findNextItem(catd)
    }
  }
  
  print(get_CAT_summary(catd, "EN"))

}

# test()


