library(tibble)
library(jsonlite)
library(dplyr)


# we want to replace old definitions (coefs_2pl$definition) with defs$definition
# (Virginia added parenthetical explanations to disambiguate objects/actions/descriptions/locations)
new_english_defs <- function() {
  load("app/API/eng_ws_wg_mod_2pl_nobad.Rds")
  defs <- read.csv(file="app/API/CDI_items.csv")
  # how many are we changing?
  setdiff(defs$definition, defs$old_definition) # 40
  extra_item = setdiff(defs$old_definition, coefs_2pl$definition)
  defs <- defs %>% arrange(old_definition) %>% 
    filter(old_definition!=extra_item)
  # test: same order for all 679 items
  sum(defs$old_definition == coefs_2pl$definition) 
  # now replace and re-save
  coefs_2pl$old_definition = coefs_2pl$definition
  coefs_2pl$definition = defs$definition
  save(mod_2pl, coefs_2pl, file="app/API/eng_ws_wg_mod_2pl_nobad.Rds") # don't need the fscores...
  # ran once Oct 28, 2020
}

# replace old definitions (coefs_2pl$definition) with defs$dismbiguation where it is not empty
new_french_defs <- function() {
  # NEW params -- need corrected definitions
  load("app/API/fr_ws_wg_mod_2pl_nobad.Rds") # 
  coefs_2pl # 632
  defs <- read.csv("app/API/FR_production_2PL_params_slopeint.csv") # 632
  
  old_defs <- read.csv("app/API/OLD/FR_production_2PL_params_slopeint_disambig.csv")
  old_defs_sp <- read.csv("app/API/OLD/FR_production_2PL_params_slopeint_disambig_spaces.csv")
  
  old <- old_defs %>% select(-disambiguation) %>% 
    left_join(old_defs_sp %>% rename(definition_spaces = definition))
  
  length(intersect(defs$definition, old$definition)) # 616 (of 632)
  
  new <- defs %>% left_join(old %>% select(-a1, -d, -g, -u))
  
  new <- new %>% rename(old_definition = definition,
                        definition = definition_spaces)
  
  # need to double-check/correct these 16 defs:
  setdiff(defs$definition, old$definition)
  #  "miam.miam"    "ohoh"         "coucou"       "boisson"      "genou"        "nombril"     
  # "oncle"        "horloge"      "blessé"       "dire"         "dormir"       "tomber"      
  # "coca"         "cassette"     "collant.e"    "faire..verb."
  
  # only 3 need to be fixed:
  new$disambiguation[which(new$old_definition=="miam.miam")] = "miam miam"
  new$disambiguation[which(new$old_definition=="collant.e")] = "collant/collante"
  new$disambiguation[which(new$old_definition=="faire..verb.")] = "faire"
  
  # same order as 2pl coefs? 0=yes
  sum(new$old_definition != coefs_2pl$definition)
  
  disambig_inds = which(new$disambiguation!="")
  
  coefs_2pl$old_definition = coefs_2pl$definition
  coefs_2pl$definition = new$definition
  coefs_2pl$definition[disambig_inds] = new$disambiguation[disambig_inds]
  
  missing_inds = which(is.na(coefs_2pl$definition))
  coefs_2pl$definition[missing_inds] = coefs_2pl$old_definition[missing_inds]
  
  sum(which(is.na(coefs_2pl$definition)))
  # no NA defs
  sum(which(coefs_2pl$definition==""))
  # no empty defs
  
  save(mod_2pl, coefs_2pl, file="app/API/fr_ws_wg_mod_2pl_nobad.Rds") 
  # run once Apr 16, 2023
}
