# R version 3.4.4 (2018-03-15) -- "Someone to Lean On"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# R is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under certain conditions.
# Type 'license()' or 'licence()' for distribution details.

# R is a collaborative project with many contributors.
# Type 'contributors()' for more information and
# 'citation()' on how to cite R or R packages in publications.

# Type 'demo()' for some demos, 'help()' for on-line help, or
# 'help.start()' for an HTML browser interface to help.
# Type 'q()' to quit R.

# base
if (!require("foreign",character.only = TRUE)) install.packages("foreign", dependencies= TRUE)
library(foreign)
if (!require("sf",character.only = TRUE)) install.packages("sf", dependencies= TRUE)
library(sf)
if (!require("zoo",character.only = TRUE)) install.packages("zoo", dependencies= TRUE)
library(zoo)
if (!require("lubridate",character.only = TRUE)) install.packages("lubridate", dependencies= TRUE)
library(lubridate)

# plot
if (!require("ggplot2",character.only = TRUE)) install.packages("ggplot2", dependencies= TRUE)
library(ggplot2)
if (!require("plotly",character.only = TRUE)) install.packages("plotly", dependencies= TRUE)
library(plotly)
if (!require("gridExtra",character.only = TRUE)) install.packages("gridExtra", dependencies= TRUE)
library(gridExtra)

# statistics
if (!require("hydroTSM",character.only = TRUE)) install.packages("hydroTSM", dependencies= TRUE)
library(hydroTSM)
if (!require("dplyr",character.only = TRUE)) install.packages("dplyr", dependencies= TRUE)
library(dplyr)
if (!require("plyr",character.only = TRUE)) install.packages("plyr", dependencies= TRUE)
library(plyr)
if (!require("tidyr",character.only = TRUE)) install.packages("tidyr", dependencies= TRUE)
library(tidyr)


# interface
if (!require("Rcpp",character.only = TRUE)) install.packages("Rcpp", dependencies= TRUE)
library(Rcpp)

# utilities
if (!require("R.utils",character.only = TRUE)) install.packages("R.utils", dependencies= TRUE)
library(R.utils)

