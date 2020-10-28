library(mirtCAT)
library(tibble)
library(jsonlite)

load("API/eng_ws_wg_mod_2pl_nobad.Rds")
# we want to replace old definitions (coefs_2pl$definition) with defs$definition
# (Virginia added parenthetical explanations to disambiguate objects/actions/descriptions/locations)
new_defs <- function() {
  library(dplyr)
  defs <- read.csv(file="API/CDI_items.csv")
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
  save(mod_2pl, coefs_2pl, file="API/eng_ws_wg_mod_2pl_nobad.Rds") # don't need the fscores...
  # ran once Oct 28, 2020
}