library(tidyverse)

#' Function to simulate an exploited fish population
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

# defining some base params for evaluating against reference points
K_val <- 1000       # Carrying Capacity
r_val <- 0.4        # Intrinsic growth rate

# calculate biological reference points
B_MSY <- K_val / 2                 # Biomass Target
H_MSY <- r_val / 2                 # Harvest Rate Target
MSY   <- (r_val * K_val) / 4       # Catch Target

# define limit reference points (the internet tells me the industry standard is "often 50% of B_MSY")
B_Limit <- 0.5 * B_MSY


# sustainable scenario
sustainable_df <- simulate_fishery(years = 50, K = K_val, r = r_val, H = 0.15) %>% 
  mutate(Scenario = "Sub-MSY Fishing (15% Harvest)")

# overfishing scenario
overfished_df <- simulate_fishery(years = 50, K = K_val, r = r_val, H = 0.35) %>% 
  mutate(Scenario = "Overexploitation (35% Harvest)")

# combine both scenarios for plotting
portfolio_data <- bind_rows(sustainable_df, overfished_df)

# create some reference data for ggplot

reference_lines <- tibble(
  Metric = c("Biomass", "Biomass", "Catch"),
  Value = c(B_MSY, B_Limit, MSY),
  Label = c("B[MSY] (Target)", "B[Limit] (Threshold)", "MSY (Catch)") # apparently if you make the label too long it complains, so "MSY (Catch)" will have to do for now
)

# "lengthen" data for ggplot facets
plot_data <- portfolio_data %>%
  pivot_longer(cols = c(Biomass, Catch), names_to = "Metric", values_to = "Value")

# make a pretty graph... even prettier and more advanced?
ggplot() +
  # Simulation lines
  geom_line(data = plot_data, aes(x = Year, y = Value, color = Scenario), size = 1.2) +
  # Biological Reference Point lines
  geom_hline(data = reference_lines, aes(yintercept = Value), linetype = "dashed", color = "grey40") +
  # Adding text annotations for the reference points
  geom_text(data = reference_lines, aes(x = 42, y = Value, label = Label), 
            vjust = -0.5, color = "grey30", size = 3.5, parse = TRUE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Fishery Dynamics Evaluated Against Biological Reference Points",
    subtitle = paste0("Calculated Targets: H_MSY = ", H_MSY, " | B_MSY = ", B_MSY, " mt | MSY = ", MSY, " mt"),
    x = "Year", 
    y = "Biomass / Catch (Metric Tons)",
    color = "Management Strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")