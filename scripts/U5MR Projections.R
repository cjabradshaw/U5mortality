## under-five mortality rate projections
## International Futures, United Nations Population Division, IIASA WIC2023
## Sachita Gera & Corey Bradshaw
## June 2026

## libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(tidyr)

## functions
find_data_dir <- function() {
  candidates <- c("data", file.path("..", "data"))
  matches <- candidates[dir.exists(candidates)]

  if (length(matches) == 0) {
    stop("Could not locate the data directory. Run the script from the repository root or from scripts/.")
  }

  normalizePath(matches[[1]], winslash = "/", mustWork = TRUE)
}

data_dir <- find_data_dir()

parse_unpd_numeric <- function(x) {
  cleaned <- gsub("\\s+", "", trimws(as.character(x)))
  cleaned[cleaned == "..."] <- NA_character_

  invalid <- unique(cleaned[!is.na(cleaned) & !grepl("^-?[0-9.]+$", cleaned)])
  if (length(invalid) > 0) {
    stop(
      sprintf(
        "Unexpected non-numeric UNPD values: %s",
        paste(invalid, collapse = ", ")
      )
    )
  }

  as.numeric(cleaned)
}

## International Futures (IF) data 
if_data <- read.csv(file.path(data_dir, "Master Sheet U5MR Final.csv"))
if_births <- read_csv(file.path(data_dir, "IF_births.csv"))

## UNPD data 
UNPD_data <- read.csv(file.path(data_dir, "Master Sheet UNPD.csv")) # births, deaths in the 1000s

# IIASA data 
deaths <- read.csv(file.path(data_dir, "deaths.csv"))
births <- read.csv(file.path(data_dir, "births.csv"))

# IIASA U5 mortality rate calculations 
deaths2 <- deaths %>%
  select(-c(Ratio, Death.Ratio, Population, Age))

deaths2 <- deaths2 %>% 
  group_by(Scenario, Area, Period) %>% 
  summarise(deaths = sum(Deaths), .groups = "drop") 

births2 <- births %>%  
  select(-c(Rate, Population))

deaths2 <- deaths2 %>% 
  left_join(births2, by = c("Scenario", "Area", "Period")) %>% 
  mutate(deaths = deaths*1000) %>% 
  mutate(Births = Births/1000) %>% 
  mutate(u5mr = deaths/Births) %>% 
  mutate(Year = as.numeric(substr(Period, 1, 4))) %>% 
  rename("Region" = Area)

# IMAGE region classification
if_data$Region <- factor(if_data$Region,
                         levels = c("World", "SSA", "SAS", "EAP"))

SSA <- c("Zambia", "Zimbabwe", "Uganda", "Togo", "United Republic of Tanzania", "Sudan", "Sudan South", "South Sudan", "South Africa", "Somalia", "Sierra Leone", "Seychelles", "Senegal", "Sao Tome and Principe", "Rwanda", "Nigeria", "Niger", "Namibia", "Mozambique", "Mauritius",
         "Mauritania", "Malawi", "Madagascar", "Liberia", "Lesotho", "Kenya", "Angola", "Benin", "Botswana",
         "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Cape Verde", "Central African Republic", "Chad", "Comoros", 
         "Democratic Republic of the Congo", "Congo",
         "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Eswatini", "Swaziland", "Cote d'Ivoire", "Mali")

SAS <- c("Sri Lanka", "Pakistan", "Nepal", "Maldives", "India", "Bhutan", "Bangladesh", "Afghanistan")

EAP <- c("Brunei Darussalam", "Cambodia", "China", "China, Hong Kong SAR", "Hong Kong Special Administrative Region of China", "Indonesia", "Lao People's Democratic Republic", 
         "Malaysia", "Mongolia", "Myanmar", "Papua New Guinea",
         "Philippines", "Singapore", "China, Taiwan Province of China", "Taiwan Province of China", "Thailand", "Timor-Leste", "Viet Nam")

# UNPD regional U5 mortality rate calculation & classification 
UNPD_data <- UNPD_data %>%
  mutate(
    Births = parse_unpd_numeric(Births),
    Deaths = parse_unpd_numeric(Deaths)
  )
UNPD_data

unpd_region_u5mr <- UNPD_data %>%
  mutate(
    Region_group = case_when(
      Region %in% SSA ~ "SSA",
      Region %in% SAS ~ "SAS",
      Region %in% EAP ~ "EAP",
      TRUE ~ NA_character_     #
    )
  ) %>%
  filter(!is.na(Region_group)) %>%
  group_by(Region_group, Year) %>%
  summarise(
    total_births = sum(Births, na.rm = TRUE),
    total_deaths = sum(Deaths, na.rm = TRUE),
    U5MR = (total_deaths / total_births) * 1000,
    .groups = "drop"
  )

UNPD_EAP <- unpd_region_u5mr %>% 
  filter(Region_group == "EAP")

UNPD_SSA <- unpd_region_u5mr %>% 
  filter(Region_group == "SSA")

UNPD_SAS <- unpd_region_u5mr %>% 
  filter(Region_group == "SAS")

# IIASA region classification 
iiasa_region_u5mr <- deaths2 %>%
  mutate(
    Region_group = case_when(
      Region %in% SSA ~ "SSA",
      Region %in% SAS ~ "SAS",
      Region %in% EAP ~ "EAP",
      TRUE ~ NA_character_     #
    )) %>%
  filter(!is.na(Region_group)) %>%
  group_by(Region_group, Year, Scenario) %>%
  summarise(
    total_births = sum(Births, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    U5MR = (total_deaths / total_births),
    .groups = "drop"
  )

iiasa_EAP <- iiasa_region_u5mr %>% 
  filter(Region_group == "EAP")

iiasa_SSA <- iiasa_region_u5mr %>% 
  filter(Region_group == "SSA")

iiasa_SAS <- iiasa_region_u5mr %>% 
  filter(Region_group == "SAS")

iiasa_world <- deaths2 %>% 
  filter(Region == "World")

# UNPD world U5MR calculation 
UNPD_world <- read.csv(file.path(data_dir, "Master Sheet UNPD World.csv"))

UNPD_world <- UNPD_world %>% 
  mutate(
    Births = parse_unpd_numeric(Births),
    Deaths = parse_unpd_numeric(Deaths),
    U5MR = (Deaths / Births) * 1000
  )

# IF raw deaths
if_births <- if_births %>% 
  rename(Region = region) %>%
  pivot_longer(cols = `2020`:`2100`, names_to = "Year", values_to = "Births") %>% 
  mutate(Year = as.numeric(Year)) 

if_data <- if_data %>%
  inner_join(if_births, by = c("Year", "SSP", "Region")) %>%
  mutate(deaths = U5MR*1000*Births) %>% 
  filter(Year >= 2030)

# IF world U5MR with UNPD baseline 
if_world <- if_data %>% 
  filter(Region == "World")

if_world %>% 
  ggplot(aes(x = Year, y = U5MR, colour = SSP, group = SSP)) + 
  geom_line() + 
  geom_line(data = UNPD_world, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  scale_linetype_manual(name = "UNPD Medium Scenario", values = c("UNPD" = "solid"),  labels = c("UNPD" = "")) +
  labs(
    x = "Year", 
    y = "U5MR",
    colour = "SSP",
    title = "IF Global under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
out_dir <- "/Users/brad0317/Documents/GitHub/U5mortality/out"
write.csv(if_world, file.path(out_dir, "IF_world_U5MR.csv"), row.names = FALSE)
write.csv(UNPD_world, file.path(out_dir, "UNPD_MEDIUM_U5MR.csv"), row.names = FALSE)

# SSA IF U5MR with UNPD baseline 
if_SSA <- if_data %>% 
  filter(Region == "SSA")

if_SSA %>% 
  ggplot(aes(x = Year, y = U5MR, colour = SSP, group = SSP)) + 
  geom_line() + 
  geom_line(data = UNPD_SSA, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  scale_linetype_manual(name = "UNPD Medium Scenario", values = c("UNPD" = "solid"),  labels = c("UNPD" = "")) + 
  labs(
    x = "Year", 
    y = "U5MR",
    colour = "SSP",
    title = "IF SSA under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
write.csv(if_SSA, file.path(out_dir, "IF_SSA_U5MR.csv"), row.names = FALSE)
write.csv(UNPD_SSA, file.path(out_dir, "UNPD_SSA_U5MR.csv"), row.names = FALSE)

# SAS IF U5MR with UNPD baseline 
if_SAS <- if_data %>% 
  filter(Region == "SAS")

if_SAS %>% 
  ggplot(aes(x = Year, y = U5MR, colour = SSP, group = SSP)) + 
  geom_line() +  
  geom_line(data = UNPD_SAS, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  scale_linetype_manual(name = "UNPD Medium Scenario", values = c("UNPD" = "solid"),  labels = c("UNPD" = "")) +
  labs(
    x = "Year", 
    y = "U5MR",
    colour = "SSP",
    title = "IF SAS under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export data
write.csv(if_SAS, file.path(out_dir, "IF_SAS_U5MR.csv"), row.names = FALSE)
write.csv(UNPD_SAS, file.path(out_dir, "UNPD_SAS_U5MR.csv"), row.names = FALSE)

# EAP EAP U5MR with UNPD baseline 
if_EAP <- if_data %>% 
  filter(Region == "EAP")

if_EAP %>% 
  ggplot(aes(x = Year, y = U5MR, colour = SSP, group = SSP)) + 
  geom_line() + 
  geom_line(data = UNPD_EAP, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  scale_linetype_manual(name = "UNPD Medium Scenario", values = c("UNPD" = "solid"),  labels = c("UNPD" = "")) +
  labs(
    x = "Year", 
    y = "U5MR",
    colour = "SSP",
    title = "IF EAP under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
write.csv(if_EAP, file.path(out_dir, "IF_EAP_U5MR.csv"), row.names = FALSE)
write.csv(UNPD_EAP, file.path(out_dir, "UNPD_EAP_U5MR.csv"), row.names = FALSE)

# IIASA world U5MR
iiasa_world %>% 
  ggplot(aes(x = Year, y = u5mr, colour = Scenario, group = Scenario)) + 
  geom_line(aes(linetype = "IIASA")) +  
  geom_line(data = UNPD_world, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_linetype_manual(name   = "Source", values = c("IIASA" = "solid", "IF" = "longdash", "UNPD" = "solid")) +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  labs(
    x = "Period", 
    y = "U5MR",
    colour = "SSP",
    title = "IIASA Global under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
write.csv(iiasa_world, file.path(out_dir, "IIASA_world_U5MR.csv"), row.names = FALSE)

# SSA IIASA U5MR
iiasa_SSA %>% 
  ggplot(aes(x = Year, y = U5MR, colour = Scenario, group = Scenario)) + 
  geom_line(aes(linetype = "IIASA")) +  
  geom_line(data = UNPD_SSA, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_linetype_manual(name   = "Source", values = c("IIASA" = "solid", "IF" = "longdash", "UNPD" = "solid")) +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  labs(
    x = "Period", 
    y = "U5MR",
    colour = "SSP",
    title = "IIASA SSA under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

write.csv(iiasa_SSA, file.path(out_dir, "IIASA_SSA_U5MR.csv"), row.names = FALSE)

# SAS IIASA U5MR
iiasa_SAS %>% 
  ggplot(aes(x = Year, y = U5MR, colour = Scenario, group = Scenario)) + 
  geom_line(aes(linetype = "IIASA")) +  
  geom_line(data = UNPD_SAS, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_linetype_manual(name   = "Source", values = c("IIASA" = "solid", "IF" = "longdash", "UNPD" = "solid")) +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) + 
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  labs(
    x = "Period", 
    y = "U5MR",
    colour = "SSP",
    title = "IIASA SAS under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
write.csv(iiasa_SAS, file.path(out_dir, "IIASA_SAS_U5MR.csv"), row.names = FALSE)

# EAP IIASA U5MR
iiasa_EAP %>% 
  ggplot(aes(x = Year, y = U5MR, colour = Scenario, group = Scenario)) + 
  geom_line(aes(linetype = "IIASA")) +  
  geom_line(data = UNPD_EAP, aes(x = Year, y = U5MR, linetype = "UNPD"), inherit.aes = FALSE, colour = "black") +
  scale_linetype_manual(name   = "Source", values = c("IIASA" = "solid", "UNPD" = "solid")) +
  scale_x_continuous(limits = c(2030,2100),
                     breaks = seq(2030, 2100, by = 10)) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10)) +
  labs(
    x = "Period", 
    y = "U5MR",
    colour = "SSP",
    title = "IIASA EAP under-5 deaths per 1,000 live births across SSP1-5"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom", 
    panel.spacing = unit(1, "lines"))

## export
write.csv(iiasa_EAP, file.path(out_dir, "IIASA_EAP_U5MR.csv"), row.names = FALSE)

# excess under-5 deaths - IIASA 
years_of_interest <- c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)

# IIASA
excess_rr_world_iiasa <- iiasa_world %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_world %>% 
      filter(Year %in% years_of_interest) %>%
      select(Region, Year, deaths_unpd = Deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000), 
    by = c("Region", "Year")
  ) %>%
  mutate(
    excess_deaths = deaths - deaths_unpd,
    relative_risk = u5mr / u5mr_unpd
  ) %>%
  select(Scenario, Region, Year, deaths_iiasa = deaths, deaths_unpd, excess_deaths, u5mr_iiasa = u5mr, u5mr_unpd, relative_risk)

## export
setwd(out_dir)
write.csv(excess_rr_world_iiasa, "IIASA_excess_RR.csv", row.names = FALSE)

excess_rr_eap <- iiasa_EAP %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_EAP %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = total_deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(Scenario, Region_group, Year, deaths_iasa = total_deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

excess_rr_sas <- iiasa_SAS %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_SAS %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = total_deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(Scenario, Region_group, Year, deaths_iasa = total_deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

excess_rr_ssa <- iiasa_SSA %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_SSA %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = total_deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(Scenario, Region_group, Year, deaths_iasa = total_deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

# IF
excess_rr_world_if <- if_world %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_world %>%
      filter(Year %in% years_of_interest) %>%
      select(Region, Year, deaths_unpd = Deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region", "Year")
  ) %>%
  mutate(
    excess_deaths = deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(SSP, Region, Year, deaths_iasa = deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

# export
setwd(out_dir)
write.csv(excess_rr_world_if, "IF_excess_RR.csv", row.names = FALSE)

excess_rr_eap_if <- if_EAP %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_EAP %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(SSP, Region, Year, deaths_iasa = deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

excess_rr_ssa_if <- if_SSA %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_SSA %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(SSP, Region, Year, deaths_iasa = deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

# export
setwd(out_dir)
write.csv(excess_rr_ssa_if, "SSA_IF_excess_RR.csv", row.names = FALSE)

excess_rr_sas_if <- if_SAS %>%
  filter(Year %in% years_of_interest) %>%
  left_join(
    UNPD_SAS %>%
      filter(Year %in% years_of_interest) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, u5mr_unpd = U5MR) %>%
      mutate(deaths_unpd = deaths_unpd * 1000),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    excess_deaths = deaths - deaths_unpd,
    relative_risk = U5MR / u5mr_unpd
  ) %>%
  select(SSP, Region, Year, deaths_iasa = deaths, deaths_unpd, excess_deaths, u5mr_iasa = U5MR, u5mr_unpd, relative_risk)

# excess under-5 deaths - world/IF
cumul_rr_if <- if_world %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_world %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region, Year, deaths_unpd = Deaths, births_unpd = Births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region", "Year")
  ) %>%
  mutate(
    births_if = Births * 1000000,
    excess_deaths = deaths - deaths_unpd
  ) %>%
  group_by(SSP, Region) %>%
  summarise(
    total_excess_deaths  = sum(excess_deaths, na.rm = TRUE),
    total_if_deaths      = sum(deaths, na.rm = TRUE),
    total_unpd_deaths    = sum(deaths_unpd, na.rm = TRUE),
    total_if_births      = sum(births_if, na.rm = TRUE),
    total_unpd_births    = sum(births_unpd, na.rm = TRUE),
    cumulative_RR        = ((total_if_deaths / total_if_births) * 1000) / 
      ((total_unpd_deaths / total_unpd_births) * 1000),
    .groups = "drop"
  )

# export
setwd(out_dir)
write.csv(cumul_rr_if, "IF_cumulative_RR.csv", row.names = FALSE)


# excess under-5 deaths - SSA/IF
cumul_rr_if_ssa <- if_SSA %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_SSA %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    births_if = Births * 1000000,
    excess_deaths = deaths - deaths_unpd
  ) %>%
  group_by(SSP, Region) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_if_deaths     = sum(deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_if_births     = sum(births_if, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_if             = (total_if_deaths / total_if_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_if / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_if_ssa, file.path(out_dir, "IF_SSA_cumulative_RR.csv"), row.names = FALSE)

# excess under-5 deaths - SAS/IF
cumul_rr_if_sas <- if_SAS %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_SAS %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    births_if = Births * 1000000,
    excess_deaths = deaths - deaths_unpd
  ) %>%
  group_by(SSP, Region) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_if_deaths     = sum(deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_if_births     = sum(births_if, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_if             = (total_if_deaths / total_if_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_if / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_if_sas, file.path(out_dir, "IF_SAS_cumulative_RR.csv"), row.names = FALSE)

# excess under-5 deaths - EAP/IF
cumul_rr_if_eap <- if_EAP %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_EAP %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region" = "Region_group", "Year")
  ) %>%
  mutate(
    births_if = Births * 1000000,
    excess_deaths = deaths - deaths_unpd
  ) %>%
  group_by(SSP, Region) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_if_deaths     = sum(deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_if_births     = sum(births_if, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_if             = (total_if_deaths / total_if_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_if / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_if_eap, file.path(out_dir, "IF_EAP_cumulative_RR.csv"), row.names = FALSE)


# excess under-5 deaths - world/IIASA
cumul_rr_iiasa <- iiasa_world %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_world %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region, Year, deaths_unpd = Deaths, births_unpd = Births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region", "Year")
  ) %>%
  mutate(
    births_iiasa = Births * 1000000,
    excess_deaths = deaths - deaths_unpd
  ) %>%
  group_by(Scenario, Region) %>%
  summarise(
    total_excess_deaths  = sum(excess_deaths, na.rm = TRUE),
    total_iiasa_deaths   = sum(deaths, na.rm = TRUE),
    total_unpd_deaths    = sum(deaths_unpd, na.rm = TRUE),
    total_iiasa_births   = sum(births_iiasa, na.rm = TRUE),
    total_unpd_births    = sum(births_unpd, na.rm = TRUE),
    cumulative_RR        = ((total_iiasa_deaths / total_iiasa_births) * 1000) / 
      ((total_unpd_deaths / total_unpd_births) * 1000),
    .groups = "drop"
  )  

## export
setwd(out_dir)
write.csv(cumul_rr_iiasa, file.path(out_dir, "IIASA_world_cumulative_RR.csv"), row.names = FALSE)

# excess under-5 deaths - SSA/IIASA
cumul_rr_iiasa_ssa <- iiasa_SSA %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_SSA %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region", "Year")
  ) %>%
  mutate(
    births_iiasa = total_births * 1000000,
    excess_deaths = total_deaths - deaths_unpd
  ) %>%
  group_by(Scenario) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_iiasa_deaths     = sum(deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_iiasa_births     = sum(births_iiasa, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_iiasa             = (total_iiasa_deaths / total_iiasa_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_iiasa / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_iiasa_ssa, file.path(out_dir, "IIASA_SSA_cumulative_RR.csv"), row.names = FALSE)

## IIASA cumulative by region and compare to UNPD data (cumulative deaths & relative risk)
## SSA
cumul_rr_iiasa_ssa <- iiasa_SSA %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_SSA %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    births_iiasa = total_births * 1000000,
    excess_deaths = total_deaths - deaths_unpd
  ) %>%
  group_by(Scenario) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_iiasa_deaths     = sum(total_deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_iiasa_births     = sum(births_iiasa, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_iiasa             = (total_iiasa_deaths / total_iiasa_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_iiasa / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_iiasa_ssa, file.path(out_dir, "IIASA_SSA_cumulative_RR.csv"), row.names = FALSE)

## SAS
cumul_rr_iiasa_sas <- iiasa_SAS %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_SAS %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    births_iiasa = total_births * 1000000,
    excess_deaths = total_deaths - deaths_unpd
  ) %>%
  group_by(Scenario) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_iiasa_deaths     = sum(total_deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_iiasa_births     = sum(births_iiasa, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_iiasa             = (total_iiasa_deaths / total_iiasa_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_iiasa / u5mr_unpd,
    .groups = "drop"
  )

write.csv(cumul_rr_iiasa_sas, file.path(out_dir, "IIASA_SAS_cumulative_RR.csv"), row.names = FALSE)

## EAP
cumul_rr_iiasa_eap <- iiasa_EAP %>%
  filter(Year >= 2030 & Year <= 2100) %>%
  left_join(
    UNPD_EAP %>%
      filter(Year >= 2030 & Year <= 2100) %>%
      select(Region_group, Year, deaths_unpd = total_deaths, births_unpd = total_births) %>%
      mutate(
        deaths_unpd = deaths_unpd * 1000,
        births_unpd = births_unpd * 1000
      ),
    by = c("Region_group", "Year")
  ) %>%
  mutate(
    births_iiasa = total_births * 1000000,
    excess_deaths = total_deaths - deaths_unpd
  ) %>%
  group_by(Scenario) %>%
  summarise(
    total_excess_deaths = sum(excess_deaths, na.rm = TRUE),
    total_iiasa_deaths     = sum(total_deaths, na.rm = TRUE),
    total_unpd_deaths   = sum(deaths_unpd, na.rm = TRUE),
    total_iiasa_births     = sum(births_iiasa, na.rm = TRUE),
    total_unpd_births   = sum(births_unpd, na.rm = TRUE),
    u5mr_iiasa             = (total_iiasa_deaths / total_iiasa_births) * 1000,
    u5mr_unpd           = (total_unpd_deaths / total_unpd_births) * 1000,
    cumulative_RR       = u5mr_iiasa / u5mr_unpd,
    .groups = "drop"
  )
    
write.csv(cumul_rr_iiasa_eap, file.path(out_dir, "IIASA_EAP_cumulative_RR.csv"), row.names = FALSE)
    