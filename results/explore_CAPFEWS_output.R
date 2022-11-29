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
setwd('C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/CAPFEWS/results') # set directory
CO = read.csv("results.csv", header = TRUE)
entitlement_totals = read.csv("C:/Users/dgorelic/OneDrive - University of North Carolina at Chapel Hill/UNC/Research/IM3/CAP/Data/user_entitlements.csv", header = TRUE)

subcontractor_names = sub("_dailydemand", "", colnames(CO)[grep("dailydemand", colnames(CO))])
subcontractor_codes = entitlement_totals$Code
subcontractor_proper_names = entitlement_totals$User
subcontractor_sectors = c("Tribal", "M&I", "Recharge", "Agri.", "M&I", "Recharge", "Agri.", "M&I",
                          "M&I", "Tribal", "M&I", "M&I", "M&I", "Tribal", "Agri.", "Agri.",
                          "M&I", "Agri.", "Agri.", "M&I", "M&I", "M&I", "M&I", "Tribal",
                          "M&I", "Tribal", "M&I", "M&I", "Tribal", "M&I", "Tribal")

### collect and plot subcontractor demands ---------------------------------
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
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) + ylab("Delivery Request (kAF)")
  ggsave(plot = to_show, filename = paste(name, "demand.png", sep = "_"), units = "in", width = 8, height = 5, dpi = 600)
} 

### collect curtailment data for plotting alone ---------------------------------
Curtailment = CO[,grep("request_curtailment", colnames(CO))]
which_initial = colnames(Curtailment)[grep("initial", colnames(Curtailment))]
Curtailment = Curtailment %>% select(-which_initial) 
colnames(Curtailment) = subcontractor_proper_names
Curtailment = Curtailment %>% mutate(Month = c(1:nrow(CO))) %>% 
  mutate_all(~na_if(., 0)) %>% select_if(colSums(!is.na(.)) > 0) 
Curtailment = Curtailment %>% pivot_longer(-Month)
Curtailment$Sector = NA
for (name in unique(Curtailment$name)) {
  Curtailment$Sector[which(Curtailment$name == name)] = subcontractor_sectors[which(subcontractor_proper_names == name)]
}
to_show = ggplot() + ggtitle("Unmet Sub-Contractor Monthly Delivery Requests (2013-2021)") +
  geom_bar(data = Curtailment, aes(x = Month, y = value, fill = name), 
           stat = "identity", position = "stack", color = NA) +
  facet_grid(Sector~., scales = "free_x", space = "free_y") + ylab("Unmet Request (kAF)")
ggsave(plot = to_show, filename = "subcontractor_curtailments.png", units = "in", width = 12, height = 7, dpi = 600)


### plot AMA contributions --------------------------------------------------------
AMA = CO[,grep("AMA", colnames(CO))]
AMA = AMA %>% mutate(Month = c(1:nrow(CO))) %>% 
  mutate_all(~na_if(., 0)) %>% select_if(colSums(!is.na(.)) > 0) 
AMA_loc = unlist(lapply(strsplit(colnames(AMA), "\\."), function(x) {x[1]}))
AMA_user = unlist(lapply(strsplit(colnames(AMA), "_"), function(x) {x[2]}))
AMA = AMA %>% pivot_longer(-Month)

AMA$AMA = NA
AMA$Subcontractor = NA
for (combo_name in unique(AMA$name)) {
  AMA$Subcontractor[which(AMA$name == combo_name)] = AMA_user[which(AMA_user == strsplit(combo_name, "_")[[1]][2])][1]
  AMA$AMA[which(AMA$name == combo_name)] = AMA_loc[which(AMA_loc == strsplit(combo_name, "\\.")[[1]][1])][1]
  AMA$Subcontractor[which(AMA$name == combo_name)] = subcontractor_proper_names[which(subcontractor_codes == AMA$Subcontractor[which(AMA$name == combo_name)[1]])]
}
to_show = ggplot() + ggtitle("Sub-Contractor Monthly Deliveries to each Active Management Area (AMA) (2013-2021)") +
  geom_bar(data = AMA, aes(x = Month, y = value, fill = Subcontractor), 
           stat = "identity", position = "stack", color = NA) +
  facet_grid(AMA~., scales = "free_x", space = "free_y") + ylab("Deliveries (kAF)")
ggsave(plot = to_show, filename = "subcontractor_ama_deliveries.png", units = "in", width = 12, height = 7, dpi = 600)


### plot Lake Mead and Pleasant elevation, storage, and pumping -----------------------
Pleasant = CO[,grep("pleasant", colnames(CO))] # keep storage, CAP storage, net pumping
Mead = CO[,grep("mead", colnames(CO))] # keep elevation, cap diversion

Reservoirs = data_frame(Month = c(1:nrow(CO)))
Reservoirs$`CAP Diversion (kAF)` = Mead$mead_cap_diversion
Reservoirs$`Mead Allocation (kAF)` = Mead$mead_cap_allocation
Reservoirs$`Pleasant Storage (kAF)` = Pleasant$pleasant_S
Reservoirs$`Pleasant CAP Allocation (kAF)` = Pleasant$pleasant_cap_allocation
Reservoirs$`Pleasant Net Pumping (kAF)` = Pleasant$pleasant_net_pleasant_pumping

Reservoirs = Reservoirs %>% pivot_longer(-Month)
to_show = ggplot() + ggtitle("CAP System Reservoir Conditions (2013-2021)") +
  geom_line(data = Reservoirs, aes(x = Month, y = value, color = name), size = 2) +
  geom_area(data = Reservoirs, aes(x = Month, y = value, fill = name), alpha = 0.4, outline.type = "upper") +
  facet_wrap(.~name, scales = "free_y", ncol = 1) + ylab("") + guides(color = FALSE, fill = FALSE) +
  theme(strip.text = element_text(size = 15, face = "bold"))
ggsave(plot = to_show, filename = "reservoirs.png", units = "in", width = 9, height = 10, dpi = 600)

