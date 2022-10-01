FROM trestletech/plumber
MAINTAINER George Kachergis <george.kachergis@gmail.com>

RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('mirt')"
RUN R -e "install.packages('mirtCAT')"
RUN R -e "install.packages('tibble')"

COPY . .

CMD ["/API/plumber.R"]
