library(tidyverse)

#' Simulate an Exploited Fish Population
#' 
#' @param years Number of years to run the simulation
#' @param K Carrying capacity (un-fished maximum biomass in metric tons)
#' @param r Intrinsic population growth rate
#' @param H Annual harvest rate (proportion of biomass caught each year)
#' @param env_rand Environmental randomness affecting growth
#' @param B0 Initial biomass relative to K (0.1 to 1.0)
simulate_fishery <- function(years = 50, K = 1000, r = 0.4, H = 0.15, env_rand = 0.05, B0 = 0.8) {
  
  # vectors to hold results
  biomass <- numeric(years)
  catch <- numeric(years)
  
  # initializing first year
  biomass[1] <- K * B0
  catch[1] <- biomass[1] * H
  
  # run simulation loop
  for (t in 2:years) {
    # calculate surplus production (logistic growth)
    production <- r * biomass[t-1] * (1 - (biomass[t-1] / K))
    
    # incorporate log-normal environmental stochasticity (process error)
    environmental_shock <- exp(rnorm(1, mean = 0, sd = env_rand))
    
    # calculate next year's biomass (previous + growth - catch) * shock
    biomass[t] <- (biomass[t-1] + production - catch[t-1]) * environmental_shock
    
    # ensure population doesn't go below zero
    biomass[t] <- max(biomass[t], 0)
    
    # calculate annual catch for the current year
    catch[t] <- biomass[t] * H
  }
  
  # return data as tibble
  tibble(
    Year = 1:years,
    Biomass = biomass,
    Catch = catch,
    Harvest_Rate = H
  )
}

# sustainable scenario
sustainable_df <- simulate_fishery(years = 50, H = 0.15) %>% 
  mutate(Scenario = "Sustainable Management (15% Harvest)")

# overfishing scenario
overfished_df <- simulate_fishery(years = 50, H = 0.35) %>% 
  mutate(Scenario = "Overexploitation (35% Harvest)")

# combine both scenarios for plotting
portfolio_data <- bind_rows(sustainable_df, overfished_df)

# "lengthen" data for ggplot facets
plot_data <- portfolio_data %>%
  pivot_longer(cols = c(Biomass, Catch), names_to = "Metric", values_to = "Value")

# make a pretty graph
ggplot(plot_data, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Schaefer Biomass Dynamic Simulation of Exploited Fishery",
    subtitle = "Comparing long-term impacts of sustainable harvest vs. overfishing with environmental stochasticity",
    x = "Year", 
    y = "Biomass / Catch (Metric Tons)",
    color = "Management Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# notes to self...
# Schaefer Biomass Dynamic Model - treating fish stock as one large biomass instead of tracking individual fish, age groups
