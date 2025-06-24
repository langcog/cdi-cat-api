FROM rstudio/plumber
MAINTAINER George Kachergis <george.kachergis@gmail.com>

# install packages and check installation success, install.packages itself does not report fails
RUN R -e "install.packages('mirtCAT', dependencies=T);    if (!library(Rcpp, logical.return=T)) quit(status=10)" \
 && R -e "install.packages('tibble');  if (!library(tibble, logical.return=T)) quit(status=10)" \
 && R -e "install.packages('tidyverse');"

COPY . .

CMD ["/API/plumber.R"]
