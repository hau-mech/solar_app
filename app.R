# Load required libraries
library(shiny)
library(tidyr)
library(ggplot2)
library(tidyverse)

# PV Module DATA 
Isc_r <- 8.73
Voc_r <- 23.19
Vmpp <- 18.36
Impp <- 8.17
Pmpp <- 150
ai <- 0.037 / 100
av <- -0.34 / 100
Ns <- 36
n <- 1.3
Gr <- 1000
Tr <- 25 + 273.6
q <- 1.60217646e-19
K <- 1.3806503e-23

V <- seq(0, Voc_r, length.out = 101)

# Define the user interface
ui <- fluidPage(
  titlePanel("PV Panel Characterization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_curves", "Number of Curves", value = 1, min = 1, max = 10),
      uiOutput("temp_inputs"),  # Dynamic temperature inputs
      uiOutput("irradiance_inputs"),  # Dynamic irradiance inputs
      actionButton("plot_button", "Plot Curves")
    ),
    mainPanel(
      plotOutput("pv_curve_plot"),
      plotOutput("efficiency_curve_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Generate dynamic temperature input fields based on the selected number of curves
  output$temp_inputs <- renderUI({
    num_curves <- input$num_curves
    temp_inputs <- lapply(1:num_curves, function(curve_num) {
      numericInput(inputId = paste0("temp_", curve_num), 
                   label = paste("Temperature (°C) for Curve", curve_num), 
                   value = 25)
    })
    do.call(tagList, temp_inputs)
  })
  
  # Generate dynamic irradiance input fields based on the selected number of curves
  output$irradiance_inputs <- renderUI({
    num_curves <- input$num_curves
    irradiance_inputs <- lapply(1:num_curves, function(curve_num) {
      numericInput(inputId = paste0("irradiance_", curve_num), 
                   label = paste("Irradiance (W/m2) for Curve", curve_num), 
                   value = 1000)
    })
    do.call(tagList, irradiance_inputs)
  })
  
  observeEvent(input$plot_button, {
    
    num_curves <- input$num_curves
    
    # Create data frame for all curves
    all_curve_data <- data.frame(V)
    
    for (curve_num in 1:num_curves) {
      temp <- input[[paste0("temp_", curve_num)]]
      T <- temp + 273.6
      dT <- T - Tr
      Isc <- Isc_r + ai * dT
      Voc <- Voc_r + av * dT  # Calculate Voc based on the temperature
      
      Vt <- Ns * n * K * T / q
      Eg <- 1.12
      irradiance <- input[[paste0("irradiance_", curve_num)]]  # Get irradiance for this curve
      Iph <- Isc * (irradiance / Gr)  # Calculate Iph using the irradiance
      Iss <- Isc / (exp(Voc / Vt) - 1)
      Is <- Iss * ((T / Tr) ^ 3) * exp(((q * Eg) / (n * K)) * ((1 / Tr) - (1 / T)))
      Rs <- 0.2
      Rp <- 230
      
      Ipv <- numeric(length(V))
      for (n1 in 1:length(V)) {
        I <- Iph
        for (n2 in 1:20) {
          Vd <- (V[n1] + Rs * I)
          Id <- Is * (exp(Vd / Vt) - 1)
          Ip <- Vd / Rp
          f <- Iph - I - Id - Ip
          df <- -1 - (Is * Rs / Vt) * exp(Vd / Vt) - (Rs / Rp)
          I <- I - f / df
        }
        if (I < 0) {
          I <- 0
        }
        Ipv[n1] <- I
      }
      
      P <- Ipv * V
      
      # Add data for this curve to the overall data frame
      all_curve_data[[paste0("V_curve_", curve_num)]] <- V
      all_curve_data[[paste0("Ipv_curve_", curve_num)]] <- Ipv
      all_curve_data[[paste0("P_curve_", curve_num)]] <- P
      
    }
    
    # Reshape the data frame to long format for ggplot using tidyr
    all_curve_data_ipv_long <- all_curve_data |> 
      select(V, starts_with("Ipv_curve_")) |>
      pivot_longer(
        starts_with("Ipv_curve_"),
        names_to = "Curve",
        values_to = "Value")
    
    all_curve_data_p_long <- all_curve_data |> 
      select(V, starts_with("P_curve_")) |>
      pivot_longer(
        starts_with("P_curve_"),
        names_to = "Curve",
        values_to = "Value")
    
    # Plot PV Curve for all curves
    pv_curve_plot <- ggplot(data = all_curve_data_ipv_long,
                            aes(x = V, y = Value, color = Curve)) +
      geom_line() +
      labs(x = "U (volt)", y = "I (Ampere)", title = "PV Curves") +
      theme_bw()
    
    # Plot Efficiency Curve for all curves
    efficiency_curve_plot <- ggplot(data = all_curve_data_p_long,
                                    aes(x = V, y = Value, color = Curve)) +
      geom_line() +
      labs(x = "U (volt)", y = "P (Watt)", title = "Efficiency Curves") +
      theme_bw()
    
    output$pv_curve_plot <- renderPlot({
      print(pv_curve_plot)
    })
    
    output$efficiency_curve_plot <- renderPlot({
      print(efficiency_curve_plot)
    })
    
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)

