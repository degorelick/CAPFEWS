### -----------------------------------------------------
##  Explore CAPFEWS Results
##    D. Gorelick (Nov 2022)
##  To be stored in degorelick/CAPFEWS repository on GitHub
### -----------------------------------------------------

rm(list=ls()) # clear memory
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/CAPFEWS/results') # set directory

### read output from hdf5 file ------------------------------------------------
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

### export some plots --------------------------------------------------------
# need results file, read in as "CO" dataframe, from previous section
library(tidyverse)
entitlement_totals = read.csv("C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data/user_entitlements.csv", header = TRUE)

subcontractor_names = sub("_dailydemand", "", colnames(CO)[grep("dailydemand", colnames(CO))])
subcontractor_codes = entitlement_totals$Code
subcontractor_proper_names = entitlement_totals$User

# collect and plot subcontractor demands
for (name in subcontractor_names) {
  data = CO[,grep(name, colnames(CO))]
  
  # organize delivery requests over actual deliveries (by priority class)
  colnames(data) = sub(paste(name, "_", sep = ""), "", colnames(data))
  colnames(data)[1] = "Demand (kAF)"; colnames(data)[3] = "Curtailment"
  data_to_plot = data %>% select(`Demand (kAF)`, PTR, MUI, FED, NIA, EXCESS, Curtailment) %>% mutate(Month = c(1:nrow(data)))
  request = data_to_plot %>% select(Month, `Demand (kAF)`)
  deliveries = data_to_plot %>% select(Month, PTR, MUI, FED, NIA, EXCESS, Curtailment) %>% 
    pivot_longer(cols = c(PTR, MUI, FED, NIA, EXCESS, Curtailment))
  
  # plot
  to_show = ggplot() + ggtitle(paste(subcontractor_proper_names[which(subcontractor_names == name)], "Demands (2013-2021)", sep = " ")) +
    geom_line(data = request, aes(x = Month, y = `Demand (kAF)`), color = "black", size = 1.5) +
    geom_bar(data = deliveries, aes(x = Month, y = value, fill = name), stat = "identity", position = "stack", color = NA) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))
  ggsave(plot = to_show, filename = paste(name, "demand.png", sep = "_"), units = "in", width = 8, height = 5, dpi = 600)
} 

#AMA_columns = CO[,grep("AMA", colnames(CO))]
#Demand_Target_columns = CO[,grep("dailydemand", colnames(CO))]
#Demand_Curtailment_columns = CO[,grep("request_curtailment", colnames(CO))]
#Demand_Target_columns = CO[,grep("dailydemand", colnames(CO))]

