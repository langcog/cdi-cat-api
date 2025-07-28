# functions for running mirtCAT from python
library(mirtCAT)
library(tibble)
library(jsonlite)
library(dplyr)

# load combined CAT parameters (for all languages, saved in 00-combine-and-save-CAT-parms.R)
load("combined_CAT_parms.Rdata")
#load("app/api/combined_CAT_parms.Rdata") # for local testing

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

# estimate WS vocabulary size given a theta 
estimate_vocab_size <- function(theta, language, nsim=1000) {
  p <- generate_pattern(irt_models[[language]], Theta=matrix(rep(theta,nsim),nsim))
  return(list(mean=mean(rowSums(p)), 
              sd=sd(rowSums(p))))
}
 
# test sequence generated given age, language, and theta (standardized language ability)
# input response (0="not produces" / 1="produces"), update theta, get new item
test <- function(age=24, language="EN", theta=1) {
  # get response full CDI response pattern for given theta
  responses <- generate_pattern(irt_models[[language]], Theta = theta)
  # initialize CAT for given age and language
  catd = initializeCAT(age, language)
  nextItem = findNextItem(catd) # the start item
  print(nextItem)
  #responses = rep(c(0,1), 30) # generated alternating responses
  while(!catd$design@stop_now) {
    catd <- updateCAT(catd, nextItem, responses[nextItem])
    nextItem = findNextItem(catd)
  }
  
  print(get_CAT_summary(catd, language))
}


# # generate
en24 <- test(24, "EN", theta=1)

sp12 <- test(12, "SP", theta=1)

fr18 <- test(18, "FR", theta=1)

jp18 <- test(18, "JP", theta=1)

jp24 <- test(24, "JP", theta=1)

nl13 <- test(13, "NL", theta=1)

# # test against a given sequence
test_given_sequence <- function(age=24, language="EN", resp_seq) {
  # initialize CAT for given age and language
  catd = initializeCAT(age, language)
  nextItem = findNextItem(catd) # the start item
  print(nextItem)
  #responses = rep(c(0,1), 30) # generated alternating responses
  for(i in 1:nrow(resp_seq)) {
    if(!catd$design@stop_now) {
      # compare given and found items:
      given_item_num = which(irt_coefs[[language]]$definition==resp_seq$item[i])
      catd <- updateCAT(catd, nextItem, resp_seq[i,]$response)
      nextItem = findNextItem(catd)
    }
  } 
  
  print(get_CAT_summary(catd, language))
}
# 
# English
en1 <- read.csv("test_sequences/en_CAT_test_theta0_ball.csv") # 14
en2 <- read.csv("test_sequences/en_CAT_test_theta1_leg.csv") # 25
en1_test <- test_given_sequence(age=14, language="EN", en1)
testthat::expect_identical(en1$item, en1_test$items) 
en2_test <- test_given_sequence(age=25, language="EN", en2)
testthat::expect_identical(en2$item, en2_test$items) 

# Spanish
sp1 <- read.csv("test_sequences/sp_CAT_test_theta0_agua.csv") # 12
sp2 <- read.csv("test_sequences/sp_CAT_test_theta1_cama.csv") # 22
sp1_test <- test_given_sequence(12, language="SP", sp1)
testthat::expect_identical(sp1$item, sp1_test$items) 
sp2_test <- test_given_sequence(22, language="SP", sp2)
testthat::expect_identical(sp2$item, sp2_test$items) 
 
# French
fr1 <- read.csv("test_sequences/fr_CAT_test_theta0_12mos.csv")
fr2 <- read.csv("test_sequences/fr_CAT_test_theta1_24mos.csv")
fr1_test <- test_given_sequence(12, language="FR", fr1)
testthat::expect_identical(fr1$item, fr1_test$items) 
fr2_test <- test_given_sequence(24, language="FR", fr2)
testthat::expect_identical(fr2$item, fr2_test$items) 
 
# Japanese
jp1 <- read.csv("test_sequences/jp_CAT_test_theta0_12mos.csv") %>%
  rename(item_id = item) %>% left_join(irt_coefs$JP %>% select(item_id, definition))
jp2 <- read.csv("test_sequences/jp_CAT_test_theta1_24mos.csv") %>%
  rename(item_id = item) %>% left_join(irt_coefs$JP %>% select(item_id, definition))
jp1_test <- test_given_sequence(12, language="JP", jp1)
testthat::expect_identical(jp1$definition, jp1_test$items)
jp2_test <- test_given_sequence(24, language="JP", jp2)
testthat::expect_identical(jp2$definition, jp2_test$items)

# Dutch
nl1 <- read.csv("test_sequences/nl_CAT_test_theta0.csv")
nl2 <- read.csv("test_sequences/nl_CAT_test_theta1.csv")
nl1_test <- test_given_sequence(13, language="NL", nl1)
testthat::expect_identical(nl1$item, nl1_test$items)
nl2_test <- test_given_sequence(28, language="NL", nl2)
testthat::expect_identical(nl2$item, nl2_test$items)
