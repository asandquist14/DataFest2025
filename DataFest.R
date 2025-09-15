
library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)
library(dplyr)
library(tigris)
library(stringr)

options(repr.plot.width = 12, repr.plot.height = 10)

leases <- read.csv("Leases.csv")

Miami_precip <- read.csv("Miami_precipitation.csv")

Tampa_precip <- read.csv("Tampa_precipitation.csv")

covid_df <- read.csv("COVID19SurveillanceDeathsByYear.csv")

hurricane_df <- read.csv("DisasterDeclarationsSummaries.csv")

florida_counties <- st_read("tl_2020_12_county20.shp")


FL_leases <- leases %>% 
  filter(state == "FL")

FL_leases %>% 
  filter(city == "Miami") %>% 
  pull(internal_submarket) %>% 
  unique()

Miami_leases <- FL_leases %>% 
  filter(city == "Miami")

Tampa_leases <- FL_leases %>% 
  filter(city == "Tampa")

Miami_precip_sums <-  Miami_precip %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(total_precip = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(YEAR,MONTH)

Tampa_precip_sums <- Tampa_precip %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(total_precip = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(YEAR, MONTH)

Miami_precip_sums <- Miami_precip_sums %>% 
  mutate(city = "Miami")

Tampa_precip_sums <- Tampa_precip_sums %>% 
  mutate(city = "Tampa")

Miami_precip_sums <-  Miami_precip %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(total_precip = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(YEAR, MONTH) %>% 
  mutate(city = "Miami",
         month_year = ymd(paste(YEAR, MONTH, "01", sep = "-")))

Tampa_precip_sums <- Tampa_precip %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(total_precip = sum(precipitation, na.rm = TRUE)) %>% 
  arrange(YEAR, MONTH) %>% 
  mutate(city = "Tampa",
         month_year = ymd(paste(YEAR, MONTH, "01", sep = "-")))

covid_Tampa <- covid_df %>% 
  filter(COVID.19.Surveillance.Deaths == "Hillsborough")

covid_Miami <- covid_df %>% 
  filter(COVID.19.Surveillance.Deaths == "Miami-Dade")

covid_Miami <- covid_Miami %>%
  pivot_longer(cols = starts_with("X"),  # Select all columns that start with "202"
               names_to = "year",         # Create a column for the years
               values_to = "deaths_count")

covid_Tampa <- covid_Tampa %>%
  pivot_longer(cols = starts_with("X"),  # Select all columns that start with "202"
               names_to = "year",         # Create a column for the years
               values_to = "deaths_count")


covid_Tampa <- covid_Tampa %>%
  mutate(years = 2020 + row_number() - 1)

covid_Miami <- covid_Miami %>%
  mutate(years = 2020 + row_number() - 1)

covid_Miami$years <- as.numeric(covid_Miami$years)
covid_Tampa$years <- as.numeric(covid_Tampa$years)

covid_Miami <- covid_Miami %>% select(-year)
covid_Tampa <- covid_Tampa %>% select(-year)

population_data <- data.frame(
  years = 2020:2024,
  Miami_population = c(2695729, 2669925, 2713415, 2774250, 2838461), 
  Tampa_population = c(1466320, 1484844, 1523839, 1557313, 1581426)) 

covid_Miami <- covid_Miami %>%
  slice(1:(n() - 1))

covid_Tampa <- covid_Tampa %>%
  slice(1:(n() - 1))

covid_Miami_combined <- covid_Miami %>%
  left_join(population_data, by = "years")

covid_Tampa_combined <- covid_Tampa %>%
  left_join(population_data, by = "years")

covid_Miami_combined <- covid_Miami_combined %>%
  mutate(Deaths_perctg = (deaths_count / Miami_population) * 100)

covid_Tampa_combined <- covid_Tampa_combined %>%
  mutate(Deaths_perct = (deaths_count / Tampa_population) * 100)

Florida_hurricane <- hurricane_df %>% 
  filter(state == "FL")

Florida_hurricane <- Florida_hurricane %>% 
  filter(incidentType == "Hurricane")

Florida_hurricane <- Florida_hurricane %>% 
  filter(fyDeclared > 2017)

hurricane_counts <- Florida_hurricane %>%
  mutate(county = str_remove(tolower(designatedArea), " \\(county\\)")) %>%
  group_by(county) %>%
  summarise(hurricane_count = n()) %>%
  ungroup()

options(tigris_use_cache = TRUE)
florida_counties <- counties(state = "FL", cb = TRUE, class = "sf")

florida_counties <- florida_counties %>%
  mutate(county = tolower(NAMELSAD))

florida_counties$county <- gsub(" county$", "", florida_counties$county)


florida_map_data <- left_join(florida_counties, hurricane_counts, by = "county")

# Plot
p <-  ggplot() + 
  geom_line(data = Miami_precip_sums, aes(x = month_year, y = total_precip, color = "Miami"), linewidth = 1) +
  geom_line(data = Tampa_precip_sums, aes(x = month_year, y = total_precip, color = "Tampa"), linewidth = 1) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(
    title = "Monthly Precipitation: Miami vs. Tampa",
    x = "Date",
    y = "Total Precipitation (in)",
    color = "City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  panel.grid = element_blank()

ggsave("precipitation_plot.png", plot = p, width = 20, height = 10)

c1 <-  ggplot() + 
  geom_line(data = covid_Miami_combined, aes(x = years, y = Deaths_perctg, color = "Miami"), linewidth = 1) +
  geom_line(data = covid_Tampa_combined, aes(x = years, y = Deaths_perct, color = "Tampa"), linewidth = 1) +
  scale_x_continuous(breaks = seq(min(covid_Miami$years), max(covid_Miami$years), 1)) +
  labs(
    title = "Covid Survelllience Deaths: Miami vs. Tampa",
    x = "Year",
    y = "Percentage of Deaths based on population",
    color = "City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("covid_deaths.png", plot = c1, width = 20, height = 10)


c2 <- ggplot() + 
  geom_bar(data = covid_Miami_combined, aes(x = years, y = Deaths_perctg, fill = "Miami"), stat = "identity", position = "dodge") +
  geom_bar(data = covid_Tampa_combined, aes(x = years, y = Deaths_perct, fill = "Tampa"), stat = "identity", position = "dodge") +
  labs(
    title = "Covid Surveillance Deaths: Miami vs. Tampa",
    x = "Year",
    y = "Number of Deaths",
    fill = "City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("covid_deaths_bar.png", plot = c2, width = 20, height = 10)


c3 <- ggplot() + 
  geom_bar(data = covid_Miami, aes(x = as.factor(years), y = deaths_count, fill = "Miami"), 
           stat = "identity", position = "dodge", width = 0.7) +
  geom_bar(data = covid_Tampa, aes(x = as.factor(years), y = deaths_count, fill = "Tampa"), 
           stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Covid Surveillance Deaths: Miami vs. Tampa",
    x = "Year",
    y = "Number of Deaths",
    fill = "City"
  ) +
  scale_fill_manual(values = c("Miami" = "blue", "Tampa" = "red")) +  # Optional: Customize colors for each city
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("covid_deaths_bar_inverse.png", plot = c3, width = 20, height = 10)



fl <- ggplot(florida_map_data) +
  geom_sf(aes(fill = hurricane_count), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Hurricane Count", na.value = "gray90") +
  theme_minimal() +
  labs(
    title = "Florida Hurricane Frequency by County",
    caption = "Source: FEMA / NHC"
  )

print(fl)  

ggsave("FL_heatmap.png", plot = fl, width = 20, height = 10)


