### -----------------------------------------------------
##  Explore CAPFEWS Results
##    D. Gorelick (Nov 2022)
##  To be stored in degorelick/CAPFEWS repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/CAPFEWS/results') # set directory

# read output from hdf5 file
# to do this, run: install.packages("BiocManager")
# then run: BiocManager::install("rhdf5")
library(rhdf5)
CAPFEWS_colnames = c()
CAPFEWS_names = h5readAttributes("results.hdf5", name = "s", native = TRUE)
for (item in names(CAPFEWS_names)) {
  if (grepl("column", item, fixed = TRUE)) {
    CAPFEWS_colnames = c(CAPFEWS_colnames, CAPFEWS_names[[item]])
  }
}
h5closeAll()

CAPFEWS_output = H5Fopen("results.hdf5", native = TRUE)
CO = as.data.frame(CAPFEWS_output$s)
colnames(CO) = CAPFEWS_colnames
CO$month_of_simulation = c(1:nrow(CO))
h5closeAll() # DO THIS OR CAPFEWS WONT OUTPUT DATA ON NEXT RUN

# export as csv
write.table(CO, "results.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# export some plots 
#library(tidyverse)