## QC FY20 in process dashboards
install.packages("tidyverse")
library(tidyverse)
install.packages("devtools")
devtools::install_github("ICPI/ICPIutilities")
library(ICPIutilities)

folder_path <- "C:/Users/dolee/Desktop/fy20_q1_inprocess"

#file <- list.files(folder_path, pattern = "*.zip", full.names = TRUE)
file <- dir(folder_path, pattern = "*.zip", full.names = TRUE)

df <- purrr::map_dfr(.x = file, 
                     .f = ~ICPIutilities::read_msd(.x))

#troubleshoot duplicate west africa region in group 1 and 2 zip files
df <- ICPIutilities::read_msd("C:/Users/dolee/Desktop/fy20_q1_inprocess/PEPFAR-Data-Genie-OUByIMs-2020-02-10-group1.zip")

#table
df %>%
  distinct(indicator, disaggregate) %>%
  arrange(indicator, disaggregate)

#ind list
unique(df$indicator) %>% sort() %>% paste0(collapse = ", ")

#disagg list
unique(df$disaggregate) %>% sort() %>% paste0(collapse = ", ")

#OU
unique(df$operatingunit) %>% sort() %>% paste0(collapse = ", ")

S
#re-create indicator "stoplight" table
# define which OU

indc <- c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NET_NEW", "TX_NEW")

#first stoplight 
df %>% filter(operatingunit == "Kenya",
              fiscal_year == 2020,
              indicator %in% indc,
              standardizeddisaggregate == "Total Numerator") %>%
  mutate(fundingagency = factor(fundingagency, c("USAID", "HHS/CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NET_NEW", "TX_NEW"))) %>% 
  group_by(fundingagency, indicator) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% arrange(fundingagency)

#second stoplight (by mech)
df %>% filter(operatingunit == "Kenya",
              fiscal_year == 2020,
              indicator %in% indc,
              standardizeddisaggregate == "Total Numerator",
              fundingagency == "USAID") %>%
  mutate(fundingagency = factor(fundingagency, c("USAID", "HHS/CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW"))) %>% 
  group_by(mech_name, indicator, fundingagency) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% arrange(fundingagency) %>% 
  print(n=Inf)








