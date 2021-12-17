library(tidyverse)
library(treemap)
setwd("~/Documents")

vpr <- read_csv("VPR_Budgets.csv")

vpr_grouped <- vpr %>%
  select(-Dept, -Delegates, - `Community Area`, -`Subdelegate Amount`, - Subdelegates) %>%
  group_by(Unit) %>%
  filter(Unit !="LA", 
         Unit != "Federal",
         Unit != "Illinois", 
         Unit != "Maryland",
         Unit != "State") %>%
  mutate(Program = `Funding Categories†`) %>%
  select(-`Funding Categories†`)

vpr_unit_total_spend <- vpr %>% group_by(Unit) %>%
  filter(Unit !="LA", 
         Unit != "Federal",
         Unit != "Illinois", 
         Unit != "Maryland",
         Unit != "State") %>%
  summarise(total_spend = sum(`Total Amount`, na.rm = T))

vpr_unit_total_spend %>% 
  ggplot(aes(x = Unit, y = total_spend)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title = "Total Spending on Violence Reduction and Prevention (self-defined)")+
  theme_minimal()

chart <- function(category) {
  png(str_c("chart", category, ".png", sep = ""))
  
  chart_data <- vpr_grouped %>%
    mutate(category_spend = ifelse(Category == category, `Total Amount`, 0)) %>%
    summarise(total_category_spend = sum(category_spend)) %>%
    mutate(Chi = ifelse(Unit == "Chicago", "yes", "no"))
  
  chart <- chart_data %>% 
    ggplot(aes(x = Unit, y = total_category_spend/1000, fill = Chi)) +
             geom_bar(stat = "identity") +
             theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
             theme_minimal() +
             labs(title = str_c("Total Spending on", category, sep = " "),
                  y = "Total Spending (000s)") +
             scale_fill_manual(values = c(yes = "#41B6E6", no = "#4D4D4D"), guide = F)
  
  print(chart)
  print(str_c("The", category, "Chart is Done", sep = " "))
  dev.off()
}

categories <- unique(vpr_grouped$Category)

categories %>% walk(chart)


