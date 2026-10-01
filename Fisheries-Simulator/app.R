library(shiny)
library(tidyverse)

# building the UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  titlePanel("Fisheries Management & Reference Point Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Population Parameters"),
      sliderInput("K", "Carrying Capacity (K in metric tons):", 
                  min = 500, max = 5000, value = 2000, step = 100),
      sliderInput("r", "Intrinsic Growth Rate (r):", 
                  min = 0.1, max = 1.0, value = 0.4, step = 0.05),
      sliderInput("sigma", "Process Error (Environmental Shocks):", 
                  min = 0.0, max = 0.2, value = 0.05, step = 0.01),
      
      hr(),
      h4("Management Strategy"),
      sliderInput("H", "Annual Harvest Rate (H):", 
                  min = 0.0, max = 0.5, value = 0.15, step = 0.01),
      
      hr(),
      p(tags$small("Note: This dashboard simulates a Schaefer Biomass Dynamic Model over a 50-year horizon."))
    ),
    
    mainPanel(
      # Dynamic KPI blocks displaying calculated BRPs
      fluidRow(
        uiOutput("kpi_boxes")
      ),
      br(),
      # The main visualization output
      plotOutput("fisheryPlot", height = "500px")
    )
  )
)

# server + server logic

server <- function(input, output, session) {
  
  # Reactive calculations for Biological Reference Points
  brps <- reactive({
    b_msy <- input$K / 2
    h_msy <- input$r / 2
    msy   <- (input$r * input$K) / 4
    b_lim <- 0.5 * b_msy
    
    list(B_MSY = b_msy, H_MSY = h_msy, MSY = msy, B_Limit = b_lim)
  })
  
  # Reactive simulation execution triggered by slider adjustment
  sim_data <- reactive({
    set.seed(42) # Keeps environmental shocks consistent across slider adjustments
    
    years <- 50
    biomass <- numeric(years)
    catch <- numeric(years)
    
    # Initialize stock at 80% of carrying capacity
    biomass[1] <- input$K * 0.8
    catch[1] <- biomass[1] * input$H
    
    for (t in 2:years) {
      production <- input$r * biomass[t-1] * (1 - (biomass[t-1] / input$K))
      environmental_shock <- exp(rnorm(1, mean = 0, sd = input$sigma))
      
      biomass[t] <- (biomass[t-1] + production - catch[t-1]) * environmental_shock
      biomass[t] <- max(biomass[t], 0)
      catch[t] <- biomass[t] * input$H
    }
    
    tibble(Year = 1:years, Biomass = biomass, Catch = catch)
  })
  
  # Render the calculated KPI dynamic boxes
  output$kpi_boxes <- renderUI({
    vals <- brps()
    fluidRow(
      column(4, wellPanel(h5("B[MSY] Target"), h3(paste0(vals$B_MSY, " mt")), style = "background: #e3f2fd; text-align: center;")),
      column(4, wellPanel(h5("H[MSY] Target"), h3(paste0(vals$H_MSY * 100, "%")), style = "background: #e8f5e9; text-align: center;")),
      column(4, wellPanel(h5("MSY Limit"), h3(paste0(vals$MSY, " mt/yr")), style = "background: #fff3e0; text-align: center;"))
    )
  })
  
  # Render the faceted time series plot
  output$fisheryPlot <- renderPlot({
    vals <- brps()
    
    # Structure long data formatting for ggplot faceting
    plot_df <- sim_data() %>%
      pivot_longer(cols = c(Biomass, Catch), names_to = "Metric", values_to = "Value")
    
    # Setup dataframe to overlay current reactive target lines
    ref_lines <- tibble(
      Metric = c("Biomass", "Biomass", "Catch"),
      Value = c(vals$B_MSY, vals$B_Limit, vals$MSY),
      Label = c("B[MSY]", "B[Limit]", "MSY")
    )
    
    ggplot() +
      geom_line(data = plot_df, aes(x = Year, y = Value), color = "#2c3e50", size = 1.3) +
      geom_hline(data = ref_lines, aes(yintercept = Value), linetype = "dashed", color = "tomato", size = 0.9) +
      geom_text(data = ref_lines, aes(x = 45, y = Value, label = Label), 
                vjust = -0.6, color = "tomato", size = 4, fontface = "bold") +
      facet_wrap(~Metric, scales = "free_y") +
      labs(
        x = "Year of Simulation",
        y = "Metric Tons (mt)",
        title = "Simulated Population Path vs Equilibrium Benchmarks",
        subtitle = paste0("Current Harvest Rate (", input$H * 100, "%) vs Sustainable Max Rate (", vals$H_MSY * 100, "%)")
      ) +
      theme_minimal(base_size = 15) +
      theme(
        strip.text = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
