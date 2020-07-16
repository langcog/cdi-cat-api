#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('cdi-cat/plumber.R')
pr$run(port=4000, host="0.0.0.0")
# Setting the host option on a VM instance ensures the application can be accessed externally.
# (This may be only true for Linux users.)

# needs to be executable by the server: chmod 755 runAPI.R