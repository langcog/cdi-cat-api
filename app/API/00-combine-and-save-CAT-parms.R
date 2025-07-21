require(tidyverse)
require(mirt)

# load IRT parameters and models for multiple languages
irt_models = list()
irt_coefs = list()
# English production
load("API/eng_ws_wg_mod_2pl_nobad.Rds")
irt_models$EN = mod_2pl
irt_coefs$EN = coefs_2pl
# French production
load("API/fr_ws_wg_mod_2pl_nobad.Rds")
irt_models$FR = mod_2pl
irt_coefs$FR = coefs_2pl
# Spanish production
load("API/sp_ws_wg_mod_2pl_nobad.Rds")
irt_models$SP = mod_2pl
irt_coefs$SP = coefs_2pl
# Dutch production
model_list <- readRDS("API/nl_ws_wg_mod_2PL.Rds")
irt_models$NL = model_list$mod_2pl
irt_coefs$NL = model_list$coefs_2pl
# Japanese production
load("API/jp_ws_wg_mod_2pl_prior.Rds")
jp_pars <- read_csv("API/JP_production_2PL_params_slopeint.csv")
irt_models$JP = mod_2pl
irt_coefs$JP = coefs_2pl %>% # merge in actual definition
  rename(item_id = definition) %>% 
  left_join(jp_pars %>% select(item_id, definition))


rm(mod_2pl, coefs_2pl, fscores_2pl, jp_pars)

# get age-based starting items
age_startits = list(EN = read.csv(file="API/EN_production_start_items_by_age.csv"),
                    FR = read.csv(file="API/FR_production_start_items_by_age.csv"),
                    SP = read.csv(file="API/SP_production_start_items_by_age.csv"),
                    JP = read.csv(file="API/JP_production_start_items_by_age.csv"),
                    NL = read.csv(file="API/NL_production_start_items_by_age.csv"))



save(irt_models, irt_coefs, age_startits, 
     file="API/combined_CAT_parms.Rdata")