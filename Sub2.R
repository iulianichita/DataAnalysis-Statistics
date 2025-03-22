install.packages("shiny")
install.packages("ggplot2")
install.packages("gganimate")
install.packages("plotly")
install.packages("gifski")


library(shiny)
library(ggplot2)
library(gganimate)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Distribuția Negativ Binomială"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parametri distribuției"),
      selectInput("param_form", "Alege parametrizarea:",
                  choices = c("Parametrizare clasică (r, p)", 
                              "Parametrizare cu μ și k",
                              "Parametrizare cu n și q",
                              "Parametrizare logit",
                              "Parametrizare cu disperție")),
      sliderInput("param1", "Primul parametru (e.g., r, μ):", 
                  min = 1, max = 50, value = 10, step = 1),
      sliderInput("param2", "Al doilea parametru (e.g., p, k):", 
                  min = 0.01, max = 0.99, value = 0.5, step = 0.01),
      sliderInput("x_range", "Intervalul pentru x:", 
                  min = 0, max = 100, value = c(0, 30)),
      actionButton("animate", "Pornește animația"),
      hr(),
      h4("Exemple de utilizare"),
      p("Distribuția Negativ Binomială este utilizată pentru a modela numărul de eșecuri până la un număr fix de succese (în parametrizarea clasică). Este relevantă în domenii precum epidemiologie, modele de supraviețuire și analiza numărului de incidente.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Funcția de Masă",
                 plotOutput("mass_function_plot")),
        tabPanel("Funcția de Repartiție",
                 plotOutput("cdf_function_plot")),
        tabPanel("Animație",
                 plotOutput("animation_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Funcția de masă
  output$mass_function_plot <- renderPlot({
    x_vals <- seq(input$x_range[1], input$x_range[2], by = 1)
    param1 <- input$param1
    param2 <- input$param2
    
    # Funcție de masă în funcție de parametrizare
    if (input$param_form == "Parametrizare clasică (r, p)") {
      y_vals <- dnbinom(x_vals, size = param1, prob = param2)
    } else if (input$param_form == "Parametrizare cu μ și k") {
      y_vals <- dnbinom(x_vals, size = param1, mu = param2)
    } else if (input$param_form == "Parametrizare cu n și q") {
      q <- 1 - param2
      y_vals <- dnbinom(x_vals, size = param1, prob = q)
    } else if (input$param_form == "Parametrizare logit") {
      logit_p <- 1 / (1 + exp(-param2))
      y_vals <- dnbinom(x_vals, size = param1, prob = logit_p)
    } else {
      # Disperție customizată
      mu <- param1
      k <- param2
      p <- k / (mu + k)
      y_vals <- dnbinom(x_vals, size = k, prob = p)
    }
    
    ggplot(data = data.frame(x = x_vals, y = y_vals), aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Funcția de masă a probabilității", x = "x", y = "Probabilitatea")
  })
  
  # Funcția de repartiție
  output$cdf_function_plot <- renderPlot({
    x_vals <- seq(input$x_range[1], input$x_range[2], by = 1)
    param1 <- input$param1
    param2 <- input$param2
    
    # Funcție de repartiție
    if (input$param_form == "Parametrizare clasică (r, p)") {
      y_vals <- pnbinom(x_vals, size = param1, prob = param2)
    } else if (input$param_form == "Parametrizare cu μ și k") {
      y_vals <- pnbinom(x_vals, size = param1, mu = param2)
    } else if (input$param_form == "Parametrizare cu n și q") {
      q <- 1 - param2
      y_vals <- pnbinom(x_vals, size = param1, prob = q)
    } else if (input$param_form == "Parametrizare logit") {
      logit_p <- 1 / (1 + exp(-param2))
      y_vals <- pnbinom(x_vals, size = param1, prob = logit_p)
    } else {
      # Disperție customizată
      mu <- param1
      k <- param2
      p <- k / (mu + k)
      y_vals <- pnbinom(x_vals, size = k, prob = p)
    }
    
    ggplot(data = data.frame(x = x_vals, y = y_vals), aes(x = x, y = y)) +
      geom_line(color = "darkblue") +
      labs(title = "Funcția de repartiție cumulativă", x = "x", y = "F(x)")
  })
  
  # Animație
  output$animation_plot <- renderPlot({
    x_vals <- seq(input$x_range[1], input$x_range[2], by = 1)
    param1 <- input$param1
    param2 <- seq(0.1, 0.9, by = 0.1)
    
    anim_data <- data.frame()
    for (p in param2) {
      y_vals <- dnbinom(x_vals, size = param1, prob = p)
      anim_data <- rbind(anim_data, data.frame(x = x_vals, y = y_vals, p = p))
    }
    
    p <- ggplot(anim_data, aes(x = x, y = y, frame = p)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Evoluția funcției de masă în funcție de p", x = "x", y = "Probabilitatea")
    
    ggplotly(p)
  })
}

# Aplicația
shinyApp(ui = ui, server = server)

