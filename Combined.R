library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Dynamic Option Strategy Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("legs", "Number of Legs", 1, min = 1, max = 10),
      uiOutput("legInputs"),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      plotOutput("profitLossPlot"),
      tableOutput("summary")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observe({
    req(input$legs)
    output$legInputs <- renderUI({
      lapply(1:input$legs, function(i) {
        fluidRow(
          column(2, selectInput(paste0("direction", i), paste("Client Direction ", i), choices = c("Client Buy", "Client Sell"))),
          column(2, selectInput(paste0("option", i), paste("Option Type ", i), choices = c("Call", "Put"))),
          column(2, numericInput(paste0("strike", i), paste("Strike Price ", i), 100)),
          column(2, numericInput(paste0("premium", i), paste("Premium ", i), value = 1, step = 0.01)),
          column(2, numericInput(paste0("quantity", i), paste("Quantity ", i), 1, min = 1)),
          column(2, numericInput(paste0("contract_size", i), paste("Contract Size ", i), 100, min = 1))
        )
      })
    })
  })
  
  observeEvent(input$calculate, {
    req(input$legs)
    
    directions <- sapply(1:input$legs, function(i) input[[paste0("direction", i)]])
    options <- sapply(1:input$legs, function(i) input[[paste0("option", i)]])
    strikes <- sapply(1:input$legs, function(i) input[[paste0("strike", i)]])
    premiums <- sapply(1:input$legs, function(i) input[[paste0("premium", i)]])
    quantities <- sapply(1:input$legs, function(i) input[[paste0("quantity", i)]])
    contract_sizes <- sapply(1:input$legs, function(i) input[[paste0("contract_size", i)]])
    
    net_premiums <- ifelse(directions == "Client Buy", -premiums, premiums)
    
    expiry_prices <- seq(min(strikes) - 50, max(strikes) + 50, by = 1)
    profit_loss <- rep(0, length(expiry_prices))
    
    leg_profit_loss <- function(price, direction, option, strike, premium, quantity, contract_size) {
      if (direction == "Client Buy" && option == "Call") {
        return((max(price - strike, 0) - premium) * quantity * contract_size)
      } else if (direction == "Client Sell" && option == "Call") {
        return((premium - max(price - strike, 0)) * quantity * contract_size)
      } else if (direction == "Client Buy" && option == "Put") {
        return((max(strike - price, 0) - premium) * quantity * contract_size)
      } else if (direction == "Client Sell" && option == "Put") {
        return((premium - max(strike - price, 0)) * quantity * contract_size)
      }
      return(0)
    }
    
    for (price in expiry_prices) {
      profit_loss[expiry_prices == price] <- sum(sapply(1:input$legs, function(i) {
        leg_profit_loss(price, directions[i], options[i], strikes[i], premiums[i], quantities[i], contract_sizes[i])
      }))
    }
    
    # Strategy identification and calculation of max gain and max loss
    strategy <- "Complex Strategy"
    max_gain <- NA
    max_loss <- NA
    
    if (input$legs == 1) {
      if (directions[1] == "Client Buy" && options[1] == "Call") {
        strategy <- "Long Call"
        max_gain <- "Unlimited"
        max_loss <- -premiums[1] * quantities[1] * contract_sizes[1]
      } else if (directions[1] == "Client Sell" && options[1] == "Call") {
        strategy <- "Short Call"
        max_gain <- premiums[1] * quantities[1] * contract_sizes[1]
        max_loss <- "Unlimited"
      } else if (directions[1] == "Client Buy" && options[1] == "Put") {
        strategy <- "Long Put"
        max_gain <- (strikes[1] * quantities[1] * contract_sizes[1]) - (premiums[1] * quantities[1] * contract_sizes[1])
        max_loss <- -premiums[1] * quantities[1] * contract_sizes[1]
      } else if (directions[1] == "Client Sell" && options[1] == "Put") {
        strategy <- "Short Put"
        max_gain <- premiums[1] * quantities[1] * contract_sizes[1]
        max_loss <- (strikes[1] * quantities[1] * contract_sizes[1]) - premiums[1] * quantities[1] * contract_sizes[1]
      }
    } else if (input$legs == 2) {
      if (all(directions == "Client Buy") && all(options == c("Call", "Put"))) {
        strategy <- "Long Strangle"
        max_gain <- "Unlimited"
        max_loss <- -sum(premiums * quantities * contract_sizes)
      } else if (all(directions == "Client Sell") && all(options == c("Call", "Put"))) {
        strategy <- "Short Strangle"
        max_gain <- sum(premiums * quantities * contract_sizes)
        max_loss <- "Unlimited"
      } else if (all(directions == c("Client Buy", "Client Sell")) && all(options == "Call")) {
        strategy <- "Bull Call Spread"
        max_gain <- (strikes[2] - strikes[1] - premiums[1] + premiums[2]) * quantities[1] * contract_sizes[1]
        max_loss <- -(premiums[1] - premiums[2]) * quantities[1] * contract_sizes[1]
      } else if (all(directions == c("Client Buy", "Client Sell")) && all(options == "Put")) {
        strategy <- "Bear Put Spread"
        max_gain <- (strikes[1] - strikes[2] - premiums[1] + premiums[2]) * quantities[1] * contract_sizes[1]
        max_loss <- -(premiums[1] - premiums[2]) * quantities[1] * contract_sizes[1]
      } else if (all(directions == "Client Buy") && all(options == c("Call", "Put"))) {
        strategy <- "Long Straddle"
        max_gain <- "Unlimited"
        max_loss <- -sum(premiums * quantities * contract_sizes)
      } else if (all(directions == "Client Sell") && all(options == c("Call", "Put"))) {
        strategy <- "Short Straddle"
        max_gain <- sum(premiums * quantities * contract_sizes)
        max_loss <- "Unlimited"
      }
    }
    
    # Update summary and plot
    output$summary <- renderTable({
      data.frame(
        Strategy = strategy,
        MaxGain = max_gain,
        MaxLoss = max_loss
      )
    })
    
    output$profitLossPlot <- renderPlot({
      plot_data <- data.frame(
        ExpiryPrice = expiry_prices,
        ProfitLoss = profit_loss
      )
      
      ggplot(plot_data, aes(x = ExpiryPrice, y = ProfitLoss)) +
        geom_line() +
        labs(title = "Profit/Loss Plot", x = "Stock Price at Expiry", y = "Profit/Loss")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





