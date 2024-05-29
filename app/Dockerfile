FROM trestletech/plumber
MAINTAINER George Kachergis <george.kachergis@gmail.com>

# install packages and check installation success, install.packages itself does not report fails
RUN R -e "install.packages('Rcpp');    if (!library(Rcpp, logical.return=T)) quit(status=10)" \
 && R -e "install.packages('mirt');    if (!library(mirt, logical.return=T)) quit(status=10)" \
 && R -e "install.packages('mirtCAT'); if (!library(mirtCAT, logical.return=T)) quit(status=10)" \
 && R -e "install.packages('tibble');  if (!library(tibble, logical.return=T)) quit(status=10)"

COPY . .

CMD ["/API/plumber.R"]
