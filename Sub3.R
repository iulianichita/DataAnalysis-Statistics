library(shiny)

#interfata
ui <- fluidPage(
  titlePanel("Reprezentarea grafica a functiilor de repartitie"),
  sidebarLayout(
    sidebarPanel(
      selectInput("case", "Tipul repartitiei:", 
                  choices = list(
                    "Repartitia normala standard" = 1, 
                    "Repartitia normala generala" = 2, 
                    "Repartitia exponentiala" = 3, 
                    "Repartitia Poisson" = 4, 
                    "Repartitia binomiala" = 5
                  )),
      numericInput("x_min", "Limita inferioara pentru X:", value = -10),
      numericInput("x_max", "Limita superioara pentru X:", value = 10),
      
      conditionalPanel(
        condition = "input.case == 1",
        selectInput("var_caz1", "Variabila aleatoare:",
                    choices = c("X", "3 - 2X", "X^2", "sum X", "sum X^2"))
      ),
      conditionalPanel(
        condition = "input.case == 2",
        selectInput("var_caz2", "Variabila aleatoare:",
                    choices = c("X", "3 - 2X", "X^2", "sum X", "sum X^2")),
        numericInput("mu", "Media (μ):", value = 0),
        numericInput("sigma", "Devierea standard (σ):", value = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.case == 3",
        selectInput("var_caz3", "Variabila aleatoare:",
                    choices = c("X", "2 + 5X", "X^2", "sum Xi")),
        numericInput("lambda", "Lambda (λ):", value = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.case == 4",
        selectInput("var_caz4", "Variabila aleatoare:",
                    choices = c("X", "3X - 2", "X^2", "sum Xi")),
        numericInput("lambda_pois", "Lambda (λ):", value = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.case == 5",
        selectInput("var_caz5", "Variabila aleatoare:",
                    choices = c("X", "5X - 4", "X^3", "sum Xi")),
        numericInput("r", "Numarul de experimente (r):", value = 10, min = 1),
        numericInput("p", "Probabilitatea de succes (p):", value = 0.5, min = 0, max = 1)
      ),
      #toate au n, restul sunt conditionate de tipul repartitiei
      numericInput("n", "Numarul de observatii (n):", value = 10, min = 1) 
    ),
    mainPanel(
      plotOutput("plotCDF") #graficul in dreapta
    )
  )
)

#serverul
server <- function(input, output) {
  output$plotCDF <- renderPlot({
    n <- input$n
    x_min <- input$x_min
    x_max <- input$x_max
    x <- seq(x_min, x_max, length.out = 1000)
    
    #rep normala standard
    if (input$case == 1) {
      if (input$var_caz1 == "X") {
        F_x <- pnorm(x, mean = 0, sd = 1)
        plot(x, F_x, type = "l", col = "pink", lwd = 2, 
             main = "Repartitia normala standard (X ~ N(0, 1))", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz1 == "3 - 2X") {
        F_x <- pnorm((3 - x) / 2, mean = 0, sd = 1)
        plot(x, F_x, type = "l", col = "pink", lwd = 2, 
             main = "Repartitia lui 3 - 2X", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz1 == "X^2") {
        F_x <- sapply(x, function(q) integrate(function(z) dnorm(z) * (z^2 <= q), -Inf, Inf)$value)
        plot(x, F_x, type = "l", col = "pink", lwd = 2, 
             main = "Repartitia lui X^2", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz1 == "sum X") {
        F_x <- pnorm(x, mean = 0, sd = sqrt(n))
        plot(x, F_x, type = "l", col = "pink", lwd = 2, 
             main = sprintf("Suma a %d variabile (sum X)", n), xlab = "x", ylab = "F(x)")
      } else if (input$var_caz1 == "sum X^2") {
        F_x <- sapply(x, function(q) pchisq(q, df = n))
        plot(x, F_x, type = "l", col = "pink", lwd = 2, 
             main = sprintf("Suma patratelor (sum X^2), df = %d", n), xlab = "x", ylab = "F(x)")
      }
    }
    
    #rep normala generala
    if (input$case == 2) {
      mu <- input$mu
      sigma <- input$sigma
      if (input$var_caz2 == "X") {
        F_x <- pnorm(x, mean = mu, sd = sigma)
        plot(x, F_x, type = "l", col = "red", lwd = 2, 
             main = sprintf("Repartitia normala generala (X ~ N(%.2f, %.2f^2))", mu, sigma), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz2 == "3 - 2X") {
        F_x <- pnorm((3 - x) / 2, mean = mu, sd = sigma * 2)
        plot(x, F_x, type = "l", col = "red", lwd = 2, 
             main = sprintf("Repartitia lui 3 - 2X (X ~ N(%.2f, %.2f^2))", mu, sigma), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz2 == "X^2") {
        F_x <- sapply(x, function(q) integrate(function(z) dnorm(z, mean = mu, sd = sigma) * (z^2 <= q), -Inf, Inf)$value)
        plot(x, F_x, type = "l", col = "red", lwd = 2, 
             main = "Repartitia lui X^2", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz2 == "sum X") {
        F_x <- pnorm(x, mean = n * mu, sd = sqrt(n) * sigma)
        plot(x, F_x, type = "l", col = "red", lwd = 2, 
             main = sprintf("Suma lui X (X ~ N(%.2f, %.2f^2))", mu, sigma), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz2 == "sum X^2") {
        F_x <- sapply(x, function(q) pchisq(q, df = n))
        plot(x, F_x, type = "l", col = "red", lwd = 2, 
             main = sprintf("Suma patratelor X^2 (n=%d)", n), xlab = "x", ylab = "F(x)")
      }
    }
    
    #rep exponentiala
    if (input$case == 3) {
      lambda <- input$lambda
      if (input$var_caz3 == "X") {
        F_x <- pexp(x, rate = lambda)
        plot(x, F_x, type = "l", col = "orange", lwd = 2, 
             main = sprintf("Repartitia exponentiala (X ~ Exp(%.2f))", lambda), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz3 == "2 + 5X") {
        F_x <- pexp((x - 2) / 5, rate = lambda)
        plot(x, F_x, type = "l", col = "orange", lwd = 2, 
             main = sprintf("Repartitia lui 2 + 5X (X ~ Exp(%.2f))", lambda), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz3 == "X^2") {
        F_x <- sapply(x, function(q) integrate(function(z) dexp(z, rate = lambda) * (z^2 <= q), 0, Inf)$value)
        plot(x, F_x, type = "l", col = "orange", lwd = 2, 
             main = "Repartitia lui X^2", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz3 == "sum Xi") {
        F_x <- pgamma(x, shape = n, rate = lambda)
        plot(x, F_x, type = "l", col = "orange", lwd = 2, 
             main = sprintf("Suma Xi (X ~ Exp(%.2f))", lambda), xlab = "x", ylab = "F(x)")
      }
    }
    
    #rep Poisson
    if (input$case == 4) {
      lambda_pois <- input$lambda_pois
      if (input$var_caz4 == "X") {
        F_x <- ppois(x, lambda = lambda_pois)
        plot(x, F_x, type = "s", col = "blue", lwd = 2, 
             main = sprintf("Repartitia Poisson (X ~ Poisson(%.2f))", lambda_pois), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz4 == "3X - 2") {
        F_x <- ppois((x + 2) / 3, lambda = lambda_pois)
        plot(x, F_x, type = "s", col = "blue", lwd = 2, 
             main = sprintf("Repartitia lui 3X - 2 (X ~ Poisson(%.2f))", lambda_pois), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz4 == "X^2") {
        F_x <- sapply(x, function(q) sum(dpois(0:q, lambda = lambda_pois)))
        plot(x, F_x, type = "s", col = "blue", lwd = 2, 
             main = "Repartitia lui X^2", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz4 == "sum Xi") {
        F_x <- pgamma(x, shape = n * lambda_pois, rate = 1)
        plot(x, F_x, type = "l", col = "blue", lwd = 2, 
             main = sprintf("Suma Xi (X ~ Poisson(%.2f))", lambda_pois), xlab = "x", ylab = "F(x)")
      }
    }
    
    #rep binomiala
    if (input$case == 5) {
      r <- input$r
      p <- input$p
      if (input$var_caz5 == "X") {
        F_x <- pbinom(x, size = r, prob = p)
        plot(x, F_x, type = "s", col = "purple", lwd = 2, 
             main = sprintf("Repartitia binomiala (X ~ B(%d, %.2f))", r, p), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz5 == "5X - 4") {
        F_x <- pbinom((x + 4) / 5, size = r, prob = p)
        plot(x, F_x, type = "s", col = "purple", lwd = 2, 
             main = sprintf("Repartitia lui 5X - 4 (X ~ B(%d, %.2f))", r, p), 
             xlab = "x", ylab = "F(x)")
      } else if (input$var_caz5 == "X^3") {
        F_x <- sapply(x, function(q) sum(dbinom(0:q, size = r, prob = p)^3))
        plot(x, F_x, type = "s", col = "purple", lwd = 2, 
             main = "Repartitia lui X^3", xlab = "x", ylab = "F(x)")
      } else if (input$var_caz5 == "sum Xi") {
        F_x <- pnorm(x, mean = r * p, sd = sqrt(r * p * (1 - p)))
        plot(x, F_x, type = "l", col = "purple", lwd = 2, 
             main = sprintf("Suma Xi (X ~ B(%d, %.2f))", r, p), xlab = "x", ylab = "F(x)")
      }
    }
  })
}

#configurarea shiny
shinyApp(ui = ui, server = server)
