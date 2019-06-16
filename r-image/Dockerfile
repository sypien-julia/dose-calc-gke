FROM r-base:latest

RUN Rscript -e "install.packages('deSolve',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('distr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('data.table',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('dplyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"

COPY ./R_dialog_testowy.R /R_dialog_testowy.R

WORKDIR /

CMD ["Rscript", "/R_dialog_testowy.R"]
