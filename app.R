# Changes made here won't impact the deployed app, which runs on a Shiny server.
library(shiny)
library(shinythemes)
library(ggplot2)
library(glue)
library(dplyr)
library(tidyr)
library(caret)
library(DT)
library(sandwich)
library(lmtest)
library(car)

# Read in the SGX data into R
github_url <- "https://raw.githubusercontent.com/tsu2000/sgx_ols_perf/main/output.csv"
sgx_data <- read.csv(github_url)

# Set nonreactive user-defined inputs
dep_vars <- c("ROA", "ROE", "operating_margin", "cash_flow_margin")
indep_vars <- c("debt_ratio_lag1", "debt_equity_ratio_lag1", "current_ratio_lag1", "interest_coverage_ratio_lag1", "gross_margin_lag1", "net_profit_margin_lag1", "asset_turnover_lag1", "log_assets_lag1")
fye_available <- unique(sgx_data$fy_ended)
sectors_available <- c("All", unique(sgx_data$Sector))

# Define UI
ui <- fluidPage(
  withMathJax(), # Use for LaTeX
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "https://raw.githubusercontent.com/tsu2000/sgx_ols_perf/main/images/logo.png"),
    tags$title("SGX OLS Analysis")
  ),
  navbarPage(
    title = div(
      style = "font-family: JetBrains Mono, Courier New, sans-serif; font-size: 20px; font-weight: bold; color: #FFC300; margin-top: -10px;",
      HTML("SGX Financial Statement <br/> Multiple Regression Analysis")
    ),
    theme = shinytheme("flatly"),
    tabPanel(
      "🔎 Analysis",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Profitability Drivers Among SGX Companies - A Multiple OLS Approach"),
          selectInput("y", "Select Y-axis (dependent) variable", choices = dep_vars, selected = "ROE"),
          selectizeInput("x", "Select X-axis (independent) variable(s)", choices = indep_vars, selected = c("net_profit_margin_lag1", "asset_turnover_lag1", "debt_equity_ratio_lag1", "gross_margin_lag1"), multiple = TRUE),
          selectInput("fyFilter", "Select a financial year-end to filter by", choices = fye_available, selected = max(fye_available)),
          selectInput("sectorFilter", "Select a sector to filter by", choices = sectors_available, selected = "All")
        ),
        mainPanel(
          tags$h4("OLS Regression Summary"),
          verbatimTextOutput("summary"),
          fluidRow(
            column(
              width = 6, 
              tags$h4("Residual Plot"),
              plotOutput("residual_plot")
            ),
            column(
              width = 6,
              tags$h4("Q-Q Plot"),
              plotOutput("qq_plot")
            )
          ),
          tags$h4("Breusch-Pagan Test for Heteroskedasticity"),
          verbatimTextOutput("breusch_pagan"),
          p(),
          tags$h4("Heteroskedasticity-Robust Standard Errors"),
          verbatimTextOutput("robust_errors"),
          p(),
          tags$h4("Joint Hypothesis Test"),
          verbatimTextOutput("joint_hyp_test")
        )
      )
    ),
    tabPanel(
      "🗂️ Data",
      downloadButton(outputId = "download_data", label = "Download all data"),
      h2(),
      fluidRow(DTOutput("data_table"))
    ),
    tabPanel(
      "❓About",
      tags$h2("What drives financial performance among SGX-listed firms?"),
      tags$hr(style = "border-top: 1px solid #000;"),
      tags$h3("📊 Data Collection & Methodology"),
      p("This analysis examines the financial performance drivers of Singapore Exchange (SGX)-listed companies using comprehensive financial data. The dataset was constructed through automated web scraping using Python's", code("yfinance"), "package in a Jupyter notebook environment ", a("here", href = "https://github.com/tsu2000/sgx_ols_perf/blob/main/scrape_sgx_financials.ipynb", target = "_blank"), "."),
      p(strong("Data Source & Filtering:")),
      tags$ul(
        tags$li("Initial dataset: 621 SGX-listed companies from the SGX Stock Screener"),
        tags$li("Final dataset: 473 companies with complete financial data (1,372 firm-year observations)"),
        tags$li("Time period: 2022-2024 (using lagged independent variables from 2021-2023)"),
        tags$li("Only companies with all required financial statement line items were included to ensure consistency")
      ),
      p(strong("Key Methodological Feature - Lagged Variables:")),
      p("To avoid simultaneous causality bias, all independent variables (financial ratios) are", strong("lagged by one year"), ". This means we use previous year's financial metrics to predict current year's performance, creating a more robust predictive model."),
      p(strong("Why use Multiple OLS instead of Fixed Effects?")),
      p("With only three years of data (2022–2024) per company, fixed effects regression would use up too many degrees of freedom and offer limited within-firm variation. Instead, pooled OLS with robust standard errors provides a more suitable and stable approach for identifying cross-sectional relationships."
      ),
      tags$h3("🧮 Multiple OLS Regression Analysis"),
      p("This app employs", strong("Ordinary Least Squares (OLS) multiple regression"), "to identify which past-year financial factors most significantly impact current-year company financial performance. The general model takes the form:"),
      withMathJax(
        "$$Y_{i,t} = \\beta_0 + \\beta_1X_{1,i,t-1} + \\beta_2X_{2,i,t-1} + ... + \\beta_kX_{k,i,t-1} + \\epsilon_{i,t}$$"
      ),
      withMathJax(
        HTML("
          <p><strong>Where:</strong></p>
          <ul>
            <li>\\( Y_{i,t} \\) = Performance measure for company \\( i \\) in year \\( t \\)</li>
            <li>\\( X_{k,i,t-1} \\) = Financial ratio \\( k \\) for company \\( i \\) in year \\( t-1 \\) (lagged)</li>
            <li>\\( \\beta_k \\) = Coefficient showing the impact of each financial ratio</li>
            <li>\\( \\epsilon_{i,t} \\) = Error term</li>
          </ul>
        ")
      ),
      tags$h3("📈 Dependent Variables (Performance Measures)"),
      tags$ul(
        tags$li(strong("Return on Assets (ROA):"), " Net Income ÷ Total Assets ", em("(Measures overall asset efficiency)")),
        tags$li(strong("Return on Equity (ROE):"), " Net Income ÷ Total Equity ", em("(Measures shareholder value creation)")),
        tags$li(strong("Operating Margin:"), " EBIT ÷ Total Revenue - ", em("(Measures core operational efficiency)")),
        tags$li(strong("Cash Flow Margin:"), " Operating Cash Flow ÷ Total Revenue", em("(Measures cash generation efficiency)"))
      ),
      tags$h3("📊 Independent Variables (Financial Ratios - Lagged by 1 financial year)"),
      p(strong("Solvency & Leverage:")),
      tags$ul(
        tags$li(strong("Debt Ratio:"), " Total Debt ÷ Total Assets"),
        tags$li(strong("Debt-to-Equity Ratio:"), " Total Debt ÷ Total Equity")
      ),
      p(strong("Liquidity:")),
      tags$ul(
        tags$li(strong("Current Ratio:"), " Current Assets ÷ Current Liabilities")
      ),
      p(strong("Profitability:")),
      tags$ul(
        tags$li(strong("Gross Margin:"), " Gross Profit ÷ Total Revenue"),
        tags$li(strong("Net Profit Margin:"), " Net Income ÷ Total Revenue")
      ),
      p(strong("Efficiency:")),
      tags$ul(
        tags$li(strong("Asset Turnover:"), " Total Revenue ÷ Total Assets"),
        tags$li(strong("Interest Coverage Ratio:"), " EBIT ÷ Interest Expense")
      ),
      p(strong("Size Control:")),
      tags$ul(
        tags$li(strong("Log Assets:"), " ln(Total Assets)")
      ),
      tags$h3("🔧 Application Features"),
      p("This interactive application allows you to:"),
      tags$ul(
        tags$li(strong("Select different dependent variables"), " (ROA, ROE, Operating Margin, Cash Flow Margin)"),
        tags$li(strong("Choose specific independent variables"), " for customized regression models"),
        tags$li(strong("Filter by financial year-end and sector"), " for targeted analysis"),
        tags$li(strong("View comprehensive regression results"), " including coefficients, significance levels, and model fit statistics"),
        tags$li(strong("Examine diagnostic plots"), " (residual plots and Q-Q plots) to assess model assumptions"),
        tags$li(strong("Access heteroskedasticity-robust standard errors"), " for more reliable inference"),
        tags$li(strong("Conduct joint hypothesis tests"), " to evaluate overall model significance"),
        tags$li(strong("Download the complete dataset"), " for further analysis")
      ),
      tags$h3("📊 Statistical Methods"),
      p("The analysis incorporates several essential econometric techniques:"),
      tags$ul(
        tags$li(strong("Heteroskedasticity-robust standard errors (HC1):"), " Accounts for potential non-constant variance in residuals"),
        tags$li(strong("Joint hypothesis testing:"), " F-tests to evaluate whether all coefficients are jointly significant"),
        tags$li(strong("Diagnostic plots:"), " Visual inspection of model assumptions through residual and Q-Q plots"),
        tags$li(strong("Lagged variable specification:"), " Reduces endogeneity concerns and simultaneity bias")
      ),
      tags$h3("⚠️ Limitations & Considerations"),
      tags$ul(
        tags$li(strong("Survivorship bias:"), " Analysis only includes companies with complete financial data"),
        tags$li(strong("Time period:"), " Limited to 2022-2024 data, may not capture longer-term trends"),
        tags$li(strong("Industry effects:"), " While sector filtering is available, cross-sectoral analysis may not fully account for industry-specific factors"),
        tags$li(strong("Causality:"), " While lagged variables reduce simultaneity bias, true causality cannot be definitively established"),
        tags$li(strong("Data quality:"), " Relies on accuracy of financial reporting and yfinance package data"),
        tags$li(strong("Missing variables:"), " Other important factors (governance, market conditions, macroeconomic variables) are not included"),
        tags$li(strong("Linear relationships:"), " OLS assumes linear relationships between variables")
      ),
      tags$h3("🔍 Interpretation Guidelines"),
      p("When interpreting results:"),
      tags$ul(
        tags$li("Focus on coefficients with", strong("statistical significance"), " (p < 0.05 or p < 0.01)"),
        tags$li("Consider", strong("economic significance"), " - large coefficients may not always be practically meaningful"),
        tags$li("Examine", strong("R-squared values"), " to assess model explanatory power"),
        tags$li("Review", strong("diagnostic plots"), " to ensure model assumptions are reasonably met"),
        tags$li("Use", strong("robust standard errors"), " for more reliable inference in the presence of heteroskedasticity")
      ),
      tags$h3("💻 Code & Reproducibility"),
      p("The complete code for data collection, processing, and analysis is available on GitHub:"),
      tags$a(
        "View Code Repository", 
        href = "https://github.com/tsu2000/sgx_ols_perf", 
        target = "_blank",
        class = "btn btn-primary"
      ),
      br(),br(),
      tags$hr(),
      tags$p("Built with Shiny for R | Data sourced from SGX and Yahoo Finance", 
             style = "text-align: center; color: #666; font-style: italic;")
    )
  )
)

# Define server functions
server <- function(input, output, session) {
  
  ### DATA PREPROCESSING FOR REACTIVE FUNCTIONS ###
  
  # Create formula for regression
  reactive_reg_eqn <- reactive({
    paste(input$y, "~", paste(input$x, collapse = " + "))
  })
  
  # Filter all SGX data based on user selection
  reactive_sgx_data <- reactive({
    data <- sgx_data %>% 
      filter(fy_ended == input$fyFilter)
    # Only filter if user did not select all sectors
    if (input$sectorFilter != "All") {
      data <- data %>% filter(Sector == input$sectorFilter)
    }
    data
  })
  
  # Reactive Linear Regression Model
  reactive_model <- reactive({
    req(input$x)
    model <- lm(reactive_reg_eqn(), data = reactive_sgx_data())
  })
  
  # Get model residuals
  reactive_residuals <- reactive({
    residuals(reactive_model())
  })
  
  # Get model fitted values
  reactive_fitted_vals <- reactive({
    fitted(reactive_model())
  })
  
  # Get model robust covariance matrix
  reactive_robust_vcov <- reactive({
    vcovHC(reactive_model(), type = "HC1")
  })
  
  ### 1. LINEAR REGRESSION ###
  
  # Show summary of linear regression results
  output$summary <- renderPrint({
    summary(reactive_model())
  })
  
  # Show plot of residuals
  output$residual_plot <- renderPlot({
    plot(
      reactive_fitted_vals(), reactive_residuals(), 
      xlab = "Fitted Values",
      ylab = "Residuals",
      main = "Residuals vs Fitted Values",
      pch = 19, 
      col = "#1f77b4"
    )
    abline(h = 0, col = "red", lty = 2)
  })
  
  # Show Q-Q Plot
  output$qq_plot <- renderPlot({
    qqnorm(reactive_residuals(), main = "Q-Q Plot of Residuals", pch = 1, col = "black")
    qqline(reactive_residuals(), col = "blue", lwd = 2)
  })
  
  # Show Breusch-Pagan Test
  output$breusch_pagan <- renderPrint({
    bptest(reactive_model(), studentize = TRUE)
  })
  
  # Show robust standard errors
  output$robust_errors <- renderPrint({
    coeftest(reactive_model(), vcov = reactive_robust_vcov())
  })
  
  # Run F-test for hypothesis (use robust SEs): H_0: All coefficients = 0, H_1: At least one β is nonzero
  output$joint_hyp_test <- renderPrint({
    linearHypothesis(reactive_model(), paste0(input$x, " = 0"), vcov = reactive_robust_vcov())
  })
  
  ### 2. ALL DATA ###
  
  # Create download handler for data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("output.csv", sep = "")
    },
    content = function(filename) {
      write.csv(sgx_data, filename, row.names = FALSE)
    } 
  )
  
  # Display data table
  output$data_table <- renderDT({
    datatable(
      sgx_data,
      options = list(
        autoWidth = TRUE,
        pageLength = 10,  # Optional: controls number of rows per page
        scrollX = TRUE    # Enables horizontal scrolling if needed
      )
    )
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)
