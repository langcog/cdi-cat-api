# for deploying to Digital Ocean

remotes::install_github("meztez/plumber", "digital_ocean")
remotes::install_github("sckott/analogsea")
install.packages("ssh")

library(analogsea)
library(plumber)
# run once:
#droplet <- analogsea::droplet_create(tags = "plumber", image = "ubuntu-20-04-x64")
# Change droplet default ssh password through digitalocean online console
#droplet <- analogsea::droplet(id=droplet$id)


droplet <- analogsea::droplet(id=199997196)
plumber::do_provision(droplet)

install_r_package(droplet, "Rcpp")
install_r_package(droplet, "mirt")
install_r_package(droplet, "mirtCAT")
install_r_package(droplet, "tibble")
install_r_package(droplet, "plumber")

options(plumber.apiHost = "0.0.0.0")
do_deploy_api(droplet,
  path='cdicat', localPath='cdi-cat', 
  port=4000, forward = T, swagger = T)

do_deploy_api(droplet,
              path='test', localPath='test', 
              port=8001, forward = T, swagger = F)

# remove example API
#do_remove_api(droplet, path="hello", delete = FALSE)
