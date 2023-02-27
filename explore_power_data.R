
rm(list=ls()) # clear memory
setwd('/Users/summerstarr/PycharmProjects/CAPFEWS/AllCAPData') # set directory
library(tidyverse)


### ----------------------------------------------------------
##  Read in final spreadsheet on power purchases for 2021/2022

# read data
CAP_power_data = 
  readxl::read_xlsx(path = "2022 Monthly L&R initial Tier 1 Shortage R1 pricing 2022 post Budget 12-2-21 rev 500+ reductions.xlsx", 
                    sheet = "2022 Transaction Totals", range = "B1:U195")

# organize data - get section names and collect data from each
# each section is 13 rows, including header rows, and we can
# reduce this to 11 rows for current data because last row is
# always empty. we can also only take the first 7 columns and
# the last three because the rest are summary columns.
# there also appear to be some typos that say the data is 2021
# when it should be all 2022...
section_names = CAP_power_data$...1[apply(CAP_power_data, MARGIN = 1, function(x) {grepl("ACES", x[1], fixed = TRUE)})]
upper_case_months = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "TOTAL")
for (section_number in c(1:length(section_names))) {
  section_start_row = which(CAP_power_data$...1 == section_names[section_number])
  section_end_row = section_start_row + 11
  
  # split into two sections of data to collect more easily
  market_colnames = as.character(CAP_power_data[2,1:7])
  section_data_market = CAP_power_data[(section_start_row+2):section_end_row, c(1:7)]
  section_data_resources = CAP_power_data[(section_start_row+1):section_end_row, c(18:20)]
  
  # remove empty rows and expand set with new columns
  section_data_market = section_data_market[which(as.numeric(section_data_market$...1) > 0),]
  colnames(section_data_market) = market_colnames
  section_data_market$Month = upper_case_months[section_number]
  section_data_market$Year = 2022
  
  # similar cleaning process for resources mix data
  # just capture the SRP, Solar, and Hoover options not
  # the overall total and net purchases
  colnames(section_data_resources) = c("MWh", "Unit Price", "Total Dollars")
  section_data_resources = section_data_resources[!is.na(section_data_resources$MWh),]
  for (r in 1:nrow(section_data_resources)) {
    if (section_data_resources$MWh[r] %in% c("SRP FLEET OPTION", "SOLAR", "HOOVER")) {
      resource_data = section_data_resources[r+1,]
      resource_data$Resource = section_data_resources$MWh[r]
      resource_data$Month = upper_case_months[section_number]
      resource_data$Year = 2022
    } else {
      next()
    }
    if (r == 1) {resource_data_section = resource_data} else {
      resource_data_section = rbind(resource_data_section, resource_data)
    }
  }
  
  
  # add to full set
  if (section_number == 1) {
    section_data_market_long = section_data_market
    resource_data_long = resource_data_section
  } else {
    section_data_market_long = rbind(section_data_market_long, section_data_market)
    resource_data_long = rbind(resource_data_long, resource_data_section)
  }
  
}

# combine data into single set for plotting
section_data_market_long = section_data_market_long %>% 
  select(-c(Commodity, Trader)) %>%
  rename("Use" = `Buy/Sell`, "MWH" = MWh, "Unit Price ($/MWH)" = `Unit Price`)
market_melted = reshape2::melt(section_data_market_long, id = c("Year", "Month", "Product", "Use"))

resource_data_long = resource_data_long %>% 
  rename("MWH" = MWh, "Unit Price ($/MWH)" = `Unit Price`, "Product" = Resource) %>%
  mutate(Use = "Buy")
resource_melted = reshape2::melt(resource_data_long, id = c("Year", "Month", "Product", "Use"))

all_data = rbind(market_melted, resource_melted)
write.table(file = paste("CAP_power_purchase_data_2022.csv", sep = ""), 
            x = all_data, sep = ",", row.names = FALSE)

# do some plotting!
all_data$Month = fct_relevel(all_data$Month, 
                             "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                             "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", 
                             "TOTAL")

plotter = all_data %>% filter(Month != "TOTAL") %>%
  filter(variable == "Unit Price ($/MWH)")
temp = ggplot(data = plotter) +
  geom_bar(aes(y = as.numeric(value), x = Month, fill = Use), stat = "identity", color = NA, position = "dodge") + 
  facet_grid(variable ~ Product, scales = "free_y") + ylab('Unit Price ($/MWH)') + xlab("Month") +
  theme(axis.text.x = element_text(angle = 90),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        legend.background = element_rect(fill = "grey95", color = "black"),
        legend.position = c(0.01,0.98),
        legend.justification = c(0,1),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, 'cm'),
        legend.direction = "vertical") +
  ggtitle("CAP: Unit Prices of Power, Projected for 2022")
ggsave(paste("visualization/CAP_power_purchases_2022_unitprices", ".png", sep = ""), 
       dpi = 400, units = "in", height = 3, width = 9)

plotter = all_data %>% filter(Month != "TOTAL") %>%
  mutate(value = ifelse(Use == "Sell", -1*as.numeric(value), as.numeric(value))) %>%
  filter(variable != "Unit Price ($/MWH)")
temp = ggplot(data = plotter) +
  geom_bar(aes(y = as.numeric(value), x = Month, fill = Product), stat = "identity", color = NA) + 
  facet_grid(variable ~ ., scales = "free_y") + ylab('') + xlab("Month") +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle(label = "CAP: Planned 2022 Power Resources, \nPurchases, and Sales")
ggsave(paste("visualization/CAP_power_purchases_2022_stacks", ".png", sep = ""), 
       dpi = 400, units = "in", height = 7, width = 7)

