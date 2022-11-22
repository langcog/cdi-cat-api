
source("API/runCAT.R")

# output from WebCDI
d <- read_csv("../cdi_forms_catresponse.csv") %>%
  mutate(est_theta = NA, 
         thetaSE = NA,
         language = ifelse(id < 359, "EN", "FR")) %>% 
  filter(administered_words!="NULL") # remove 6 empty admins

# a few later English admins..
EN_admins = c(369,370,378,380,411,414)
d[which(is.element(d$id, EN_admins)),]$language = "EN"

initializeCATfromData <- function(language, start_it) {
  df <- getCATquestions(language)
  catd <- mirtCAT(df, irt_models[[language]], design = preferred_design, criteria = 'MI',
                  method='ML', start_item = start_it, design_elements = T) 
  return(catd)
}



for(i in 1:nrow(d)) {
  admin_words = strsplit(gsub("\\{|\\}", "", d[i,]$administered_words), ",")[[1]]
  # still has some extra " and escapes, e.g.: "\"next to\""
  admin_items = as.numeric(strsplit(gsub("\\{|\\}", "", d[i,]$administered_items), ",")[[1]])
  responses = as.logical(toupper(strsplit(gsub("\\{|\\}", "", d[i,]$administered_responses), ",")[[1]]))
  catd = initializeCATfromData(d[i,]$language, admin_items[1])
  for(j in 1:length(admin_items)) {
    catd <- updateCAT(catd, admin_items[j], responses[j])
  }
  
  d_ag <- get_CAT_summary(catd, d[i,]$language)
  
  d[i,]$est_theta = tail(d_ag, 1)$thetas
  d[i,]$thetaSE = tail(d_ag, 1)$thetaSE
}

write_csv(d, file="cdi_forms_catresponse_w_theta.csv")