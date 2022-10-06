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
  load("app/API/fr_ws_wg_mod_2pl_nobad.Rds")
  defs <- read.csv("app/API/FR_production_2PL_params_slopeint_disambig_spaces.csv")
  # same order?
  sum(defs$definition != coefs_2pl$definition) # 0 mismatches
  disambig_inds = which(defs$disambiguation!="")
  coefs_2pl$old_definition = coefs_2pl$definition
  coefs_2pl$definition[disambig_inds] = defs$disambiguation[disambig_inds]
  
  #setdiff(defs$disambiguation, coefs_2pl$definition) # all changed
  
  save(mod_2pl, coefs_2pl, file="app/API/fr_ws_wg_mod_2pl_nobad.Rds") 
  # ran once Sep 26, 2022
}
