# Lucas Onofre - Data Analysis - Gapminder Dataset (ID: 1678505)

# Install necessary packages (only first time)
# install.packages("patchwork") 

# Load required libraries
library(gapminder)  # for dataset
library(dplyr)      # for data manipulation
library(ggplot2)    # for data visualization
library(patchwork)  # for combining multiple plots

# Loading the Gapminder dataset
data("gapminder")
head(gapminder)

# 1. **Range of Years in the Dataset**
# Checking the range of years represented in the dataset
year_range <- range(gapminder$year)
year_range

# 2. **Filter Data for Most Up-to-Date Years (2000-2026)**
# Filtering data to include only years >= 2000
gapminder_recent <- gapminder %>%
  filter(year >= 2000)

# 3. **Summary Statistics for Oceania (2000-2026)**
# Get the mean life expectancy, max GDP per capita, and total population for Oceania
oceania_summary <- gapminder_recent %>%
  filter(continent == "Oceania") %>%
  summarise(
    avg_lifeExp = mean(lifeExp, na.rm = TRUE),
    max_gdpPercap = max(gdpPercap, na.rm = TRUE),
    total_pop = sum(pop, na.rm = TRUE)
  )

oceania_summary

# 4. **Top 4 Continents by Total Population**
# Identify the four continents with the highest total population in recent data (2000-2026)
top_continents <- gapminder_recent %>%
  group_by(continent) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE)) %>%
  arrange(desc(total_pop)) %>%
  slice_head(n = 4) %>%
  pull(continent)

top_continents

# 5. **Filter Data for Top 4 Continents**
# Select only the data for the top 4 continents by population
gapminder_top <- gapminder_recent %>%
  filter(continent %in% top_continents)

# 6. **Visualizations**
# Create individual scatter plots for each of the top 4 continents
plots <- lapply(top_continents, function(cont) {
  gapminder_top %>%
    filter(continent == cont) %>%
    ggplot(aes(x = gdpPercap, y = lifeExp, color = as.factor(year))) +
    geom_point(size = 2, alpha = 0.7) +
    scale_x_log10() +  # Use log scale for GDP per capita
    labs(
      title = paste("Life Expectancy vs GDP per Capita: ", cont),
      x = "GDP per Capita (log scale)",
      y = "Life Expectancy",
      color = "Year"
    ) +
    theme_minimal()
})

# Combine the 4 plots into a 2x2 grid
combined_plot <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plot_layout(ncol = 2)
combined_plot

# 7. **Faceted Plot for All Top 4 Continents**
# A single plot with faceting by continent for better comparison
gapminder_top %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = as.factor(year))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_log10() +  # Log scale for GDP
  labs(
    title = "Life Expectancy vs GDP per Capita: Top 4 Continents",
    x = "GDP per Capita (log scale)",
    y = "Life Expectancy",
    color = "Year"
  ) +
  facet_wrap(~continent) +  # Facet by continent
  theme_minimal()