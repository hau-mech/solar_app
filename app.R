# Load required libraries
library(shiny)
library(ggplot2)

# Define the user interface
ui <- fluidPage(
  titlePanel("PV Panel Characterization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_curves", "Number of Curves", value = 1, min = 1, max = 10),
      numericInput("temp", "Temperature (°C)", value = 25),
      numericInput("irradiance", "Solar Irradiance (W/m2)", value = 1000),
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
  observeEvent(input$plot_button, {
    num_curves <- input$num_curves
    temp <- input$temp
    irradiance <- input$irradiance
    
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
    T <- temp + 273.6
    dT <- T - Tr
    Isc <- Isc_r + ai * dT
    Voc <- Voc_r + av * dT
    q <- 1.60217646e-19
    K <- 1.3806503e-23
    Vt <- Ns * n * K * T / q
    Eg <- 1.12
    Iph <- Isc * (irradiance / Gr)
    Iss <- Isc / (exp(Voc / Vt) - 1)
    Is <- Iss * ((T / Tr) ^ 3) * exp(((q * Eg) / (n * K)) * ((1 / Tr) - (1 / T)))
    Rs <- 0.2
    Rp <- 230
    
    V <- seq(0, Voc, length.out = 101)
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
    
    # Create PV and Efficiency curves
    pv_curve <- data.frame(V, Ipv, curve_num = factor(1:num_curves))
    efficiency_curve <- data.frame(V, P, curve_num = factor(1:num_curves))
    
    # Plot PV Curve
    pv_curve_plot <- ggplot(pv_curve, aes(x = V, y = Ipv, color = curve_num)) +
      geom_line() +
      labs(x = "Voltage (V)", y = "Current (Ampere)", title = "PV Curves") +
      theme_minimal()
    
    # Plot Efficiency Curve
    efficiency_curve_plot <- ggplot(efficiency_curve, aes(x = V, y = P, color = curve_num)) +
      geom_line() +
      labs(x = "Voltage (V)", y = "Power (Watt)", title = "Efficiency Curves") +
      theme_minimal()
    
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
