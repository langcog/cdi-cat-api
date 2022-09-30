require(tibble)
require(mirt)

# load IRT parameters and models for multiple languages
irt_models = list()
irt_coefs = list()
load("app/API/eng_ws_wg_mod_2pl_nobad.Rds")
irt_models$EN = mod_2pl
irt_coefs$EN = coefs_2pl
load("app/API/fr_ws_wg_mod_2pl_nobad.Rds")
irt_models$FR = mod_2pl
irt_coefs$FR = coefs_2pl
load("app/API/sp_ws_wg_mod_2pl_nobad.Rds")
irt_models$SP = mod_2pl
irt_coefs$SP = coefs_2pl

rm(mod_2pl, coefs_2pl, fscores_2pl)

save(irt_models, irt_coefs, file="app/API/combined_CAT_parms.Rdata")