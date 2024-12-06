# getting started ---------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

trends <- read_excel('data/dof_arsrapport_punkttaellingsprogrammet_2020/Indeks_og_standarderror_1976-2023_begge_saesoner.xlsx')
head(trends)

long <- trends %>%
  pivot_longer(
    cols = c(-'euring',-'artnavn',-'latin',-'english',-'season',-'type'),
    names_to = 'year',
    values_to = 'value'
  )

long$year = as.numeric(long$year)

# Create separate columns for spring and summer
wider <- long %>%
  pivot_wider(
    names_from = type,   # index & se
    values_from = value         # Populate these columns with `value`
  )

sortspaette <- wider %>%
  filter(english == 'Black Woodpecker')

test <- sortspaette[sortspaette$season == 'SY',]

population_range <- c(116, 142)  # Population range
reference_years <- c(2014, 2017) # Reference years
  

reference_data <- test %>%
  filter(year >= reference_years[1] & year <= reference_years[2])

# Calculate mean indices and SE for reference years
mean_indices <- mean(reference_data$indices)
mean_indices_se <- sqrt(sum(reference_data$se_indices^2) / nrow(reference_data))  # Combine SEs

# Calculate midpoint and SE of the population range
population_midpoint <- mean(population_range)
population_se <- (diff(population_range) / 2) / 1.96  # Assuming 95% CI

# Scaling factor
scaling_factor <- population_midpoint / mean_indices

# Scale indices values and SEs
test <- test %>%
  mutate(
    abs_population = indices * scaling_factor,
    abs_population_se = se_indices * scaling_factor
  )

# Combine SEs
test <- test %>%
  mutate(
    combined_se = sqrt(population_se^2 + abs_population_se^2 + (mean_indices_se * scaling_factor)^2)
  )

# Add confidence intervals
test <- test %>%
  mutate(
    lower_ci = abs_population - 1.96 * combined_se,
    upper_ci = abs_population + 1.96 * combined_se
  )

# Calculate the population size range for the reference years
pop_range_text <- paste("Reference Years: 2014-2017\nPopulation Size: ", 
                        round(min(population_range), 0), " to ", round(max(population_range), 0))


ggplot(test, aes(x = year, y = abs_population)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(
    title = "Interpolated Population Trends (Multi-Year Reference)",
    x = "Year",
    y = "Estimated Population"
  ) +
  # Add a block showing the reference years and population range
  annotate("rect", 
           xmin = 2013, xmax = 2017, ymin = 116, ymax = 142, 
           alpha = 1) +   # Light blue rectangle behind the text
  theme_minimal() 

