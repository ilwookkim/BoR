library(shiny)
library(dplyr)
library(DT)
options(scipen = 999)

##############
# Assumption #
##############
# Every 5 years tax brackets and personal allowance are updated resulting in +80 Euro net salary increasing.
# Annual indexation is fixed as 3.0% (rent etc)
# Annual wage_indexation is set bit high than inflation due to promotion etc. fixed 3.2%
# sp500 last 10 year growing was around 203%
# house price last 10 year growing was around 48.57% in Belgium
# initial rent is 1200 and following index
# initial monthly gross is following index
# house hold price is currently 2000 amd following index
# Child benefit is currently 188.89 and follwing index
# interest rate for mortagate is 1.5-3.5%
# loan to vlaue ratio is 75 - 85%.
# Property Taxes in Germany is 0.26% - 1%
# Property Taxes in Belgium is Approximately 1.25% of the indexed (2024:2.1763) cadastral value in Wallonia (2.5% Flanders, 2.25% Brussels)
# cadastral value is typically 20-30% of market value: fixed 20%
# Annual Property Taxes Belgium = House price * 0.20 * 2.1763( * Indexed ) * 0.0125
# Maintenance and Repairs is typically 1% - 2% of the property value: fixed 1%
# Annual Home Insurance 300 - 600 Euro: initial 300 and following indexation

profit_cal <- function(house_price = 350000, n_periods_years = 1, mortagate_rate = 0.015, salary = 5940, idx = 1.03, wage_idx = 1.032, percentage_increase_sp500_10y = 287, percentage_increase_house_10y = 35, rental_cost = 1200, kinder_geld = 188.89,  m_living_cost = 2000, ltv = 0.8, age_kids = "11, 9"){
  
  
  age_kids <- as.numeric(unlist(strsplit(age_kids, ",")))
  
  lv_cost <- m_living_cost
  
  calculate_child_benefit <- function(INITIAL_MONTHLY_BENEFIT, indexation, ages, years) {
    if (!all(ages >= 0)) {
      stop("Ages must be non-negative")
    }
    if (years <= 0) {
      stop("Time period must be positive")
    }
    MAX_AGE <- 18
    YEARLY_INCREASE <- indexation - 1
    total_months <- years * 12
    monthly_benefits <- numeric(total_months)
    for (month in 1:total_months) {
      current_ages <- ages + (month - 1)/12
      years_passed <- floor((month - 1) / 12)
      current_benefit <- INITIAL_MONTHLY_BENEFIT * (1 + YEARLY_INCREASE)^years_passed
      eligible_children <- sum(current_ages < MAX_AGE)
      monthly_benefits[month] <- eligible_children * current_benefit
    }
    return(monthly_benefits)
  }
  result <- calculate_child_benefit(INITIAL_MONTHLY_BENEFIT = kinder_geld, indexation = idx, ages = age_kids, years = n_periods_years)
  
  # Compound Monthly Growth Rate
  CMGR_cal <- function(value_increasing, years){
    # value_increasing is percentage
    CMGR <- ((1*(value_increasing/100)+1)/1)^(1 / (years * 12)) - 1
    # output is not %
    return(CMGR)
  }
  
  # Compound Annual Growth
  CAG_cal <- function(init_value, years, CMGR){
    CAG <- init_value*(1+CMGR)^(years*12)
    CAG
  }
  
  # Level debt calculator
  level_debt_cal <- function(loan_amount, annual_rate, n_years){
    monthly_rate <- annual_rate / 12
    # Total number of payments
    n_payments <- n_years * 12
    # Calculate monthly payment using the annuity formula
    monthly_payment <- loan_amount * (monthly_rate * (1 + monthly_rate)^n_payments) / ((1 + monthly_rate)^n_payments - 1)
    res <- list()
    res <- c(res, list(monthly_payment))
    res <- c(res,list(monthly_payment * n_payments))
    names(res) <- c("Monthly_payment", "Total_payment")
    res
  }
  # gross to net salary
  
  # For 2025
  # options(scipen = 999)
  # df <- read.csv("g_n.csv")
  # model <- lm( df$n ~ df$g)
  # res <- summary(model)
  # slope <- res$coefficients[2, "Estimate"]
  # intercept  <-  res$coefficients[1, "Estimate"]
  
  gross2net <- function(init_gross){
    init_net <- init_gross*0.4162447+1567.282
    init_net
  }
  
  ltv <- ltv
  
  sp500_CMGR <- CMGR_cal(value_increasing = percentage_increase_sp500_10y, years = 10)
  house_CMGR <- CMGR_cal(value_increasing = percentage_increase_house_10y, years = 10)
  indexation <- idx
  wage_indexation <- wage_idx
  # income
  init_gross <- salary
  
  # spend
  init_rent <- rental_cost
  init_house_hold <- lv_cost
  
  investing_sp500_rent <- 0
  
  rent_df <- data.frame(matrix(nrow = n_periods_years * 12, ncol = 7))
  colnames(rent_df) <- c("Month", "Gross", "Net" , "Kindergeld", "living_cost", "Rent", "sp500_rent")
  # When rent
  for(i in 1:(n_periods_years*12)){
    if(i %% 12 == 0){
      init_gross <- init_gross * wage_indexation
      if(i %% 12*5 == 0){
        init_gross <- init_gross + 80
      }
      init_rent <- init_rent * indexation
      init_house_hold <- init_house_hold * indexation
      
    }
    
    init_net <- gross2net(init_gross = init_gross)
    
    savable <- init_net + result[i] - init_rent - init_house_hold
    
    investing_sp500_rent <- (investing_sp500_rent+savable)
    investing_sp500_rent <- investing_sp500_rent * (1+sp500_CMGR)
    rent_df$Month[i] <- round(i, 2)
    rent_df$Gross[i] <- round(init_gross, 2)
    rent_df$Net[i] <- round(init_net, 2)
    rent_df$Kindergeld[i] <- round(result[i], 2)
    rent_df$Rent[i] <- round(init_rent, 2)
    rent_df$living_cost[i] <- round(init_house_hold, 2)
    rent_df$sp500_rent[i] <- round(investing_sp500_rent, 2)
  }
  
  # When buy, rest to sp500
  
  loans <- house_price*ltv/100
  level_debt <- level_debt_cal(loan_amount = loans, annual_rate = mortagate_rate, n_years = n_periods_years)
  
  # income
  init_gross <- salary
  
  # spend
  level_debt <- level_debt$Monthly_payment
  
  lv_cost2 <- m_living_cost
  init_house_hold <- lv_cost2
  home_insureance <- 300/12 
  investing_sp500_buy <- 0
  
  buy_df <- data.frame(matrix(nrow = n_periods_years * 12, ncol = 11))
  colnames(buy_df) <- c("Month", "Gross", "Net" , "Kindergeld", "living_cost",  "Mortagate",  "Property_tax", "Home_insureance", "Repair", "sp500_buy", "House_price")
  
  for(i in 1:(n_periods_years*12)){
    
    if(i %% 12 == 0){
      init_gross <- init_gross * wage_indexation
      if(i %% 12*5 == 0){
        init_gross <- init_gross + 80
      }
      init_house_hold <- init_house_hold * indexation
      home_insureance <- home_insureance * indexation
    }
    
    # if(i %% 12 == 0){
    #   init_gross <- init_gross * wage_indexation
    #   init_house_hold <- init_house_hold * indexation
    #   property_tax <- property_tax * indexation
    #   home_insureance <- home_insureance * indexation
    #   if(i < 109){
    #     init_kindergeld <- init_kindergeld * indexation
    #   } else {
    #     init_kindergeld <- init_kindergeld * 0
    #   }
    # }
    init_net <- gross2net(init_gross = init_gross)
    house_price <- house_price* (1+house_CMGR)
    repair_cost <- house_price * 0.005 / 12
    property_tax <- (house_price * 0.20 * 2.1763 * 0.0125)/12
    
    savable <- init_net + result[i] - init_rent - init_house_hold - repair_cost - property_tax - home_insureance
    
    investing_sp500_buy <- (investing_sp500_buy+savable)
    investing_sp500_buy <- investing_sp500_buy * (1+sp500_CMGR)
    
    buy_df$Month[i] <- round(i, 2)
    buy_df$Gross[i] <- round(init_gross, 2)
    buy_df$Net[i] <- round(init_net, 2)
    buy_df$Kindergeld[i] <- round(result[i], 2)
    buy_df$Mortagate[i] <- round(level_debt, 2)
    buy_df$living_cost[i] <- round(init_house_hold, 2)
    buy_df$Property_tax[i] <- round(property_tax, 2)
    buy_df$Home_insureance[i] <- round(home_insureance, 2)
    buy_df$Repair[i] <- round(repair_cost, 2)
    buy_df$sp500_buy[i] <- round(investing_sp500_buy, 2)
    buy_df$House_price[i] <- round(house_price, 2)
  }
  
  final_df <- cbind(rent_df, buy_df[,c( "Mortagate",  "Property_tax", "Home_insureance", "Repair", "sp500_buy", "House_price")])
  
  overall <- list()
  overall <- c(overall, list(final_df$sp500_rent[nrow(final_df)]))
  overall <- c(overall, list(final_df$House_price[nrow(final_df)] + final_df$sp500_buy[nrow(final_df)]))
  overall <- c(overall, list(final_df))
  
  
  names(overall) <- c("Total_profit_rent", "Overall_profit_buying_house", "final_df")
  return(overall)
}


ui <- fluidPage(
  titlePanel("But or Rent"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "house_price",
        label = "Enter Real estate Price:",
        value = 300000.00,
        step = 0.01
      ),
      numericInput(
        inputId = "n_periods_years",
        label = "Enter Loan Periods (Years)",
        value = 20.0,
        step = 0.1
      ),
      numericInput(
        inputId = "mortagate_rate",
        label = "Enter Mortagate Rate (example 0.02 : 2%)",
        value = 0.040,
        step = 0.001
      ),
      numericInput(
        inputId = "salary",
        label = "Enter Monthly Gross Income (Belgium Tax system)",
        value = 6000.00,
        step = 0.01
      ),
      numericInput(
        inputId = "idx",
        label = "Enter Annual Indexation (example 1.02 : 2%)",
        value = 1.02,
        step = 0.01
      ),
      numericInput(
        inputId = "wage_idx",
        label = "Enter Annual Wage Indexation (example 1.021 : 2.1%)",
        value = 1.021,
        step = 0.001
      ),
      numericInput(
        inputId = "percentage_increase_sp500_10y",
        label = "Enter Last 10 years Interesting market Growth (default(SP500) 215%)",
        value = 215.0,
        step = 0.1
      ),
      numericInput(
        inputId = "percentage_increase_house_10y",
        label = "Enter Last 10 years Real Estate Price Growth (default(Belgium) 48.57%)",
        value = 48.57,
        step = 0.01
      ),
      numericInput(
        inputId = "rental_cost",
        label = "Enter Retal apartment cost (default 1200 Euro)",
        value = 1200.00,
        step = 0.01
      ),
      numericInput(
        inputId = "kinder_geld",
        label = "Enter Child benefit per child (default 188.89 Euro (Belgium))",
        value = 188.89,
        step = 0.01
      ),
      numericInput(
        inputId = "m_living_cost",
        label = "Enter monthly household expense (default 2000 Euro)",
        value = 2000.00,
        step = 0.01
      ),
      numericInput(
        inputId = "ltv",
        label = "Enter Loan-to-Value (default 85%)",
        value = 85.00,
        step = 0.01
      ),
      textInput("age_kids",
                "Enter ages of children (example 11,9)",
                value = "11,9"),
      actionButton("submit", 
                   "Run Analysis",
                   class = "btn-primary",
                   style = "width: 100%")
    ),
    
    mainPanel(
      # Dynamic title
      uiOutput("resultTitle"),
      # Results table
      DTOutput("priceTable")
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    # Your calculation logic here
    res <- profit_cal(house_price = input$house_price, 
                      n_periods_years = input$n_periods_years, 
                      mortagate_rate = input$mortagate_rate, 
                      salary = input$salary, 
                      idx = input$idx, 
                      wage_idx = input$wage_idx, 
                      percentage_increase_sp500_10y = input$percentage_increase_sp500_10y,
                      percentage_increase_house_10y = input$percentage_increase_house_10y,
                      rental_cost = input$rental_cost, 
                      kinder_geld = input$kinder_geld, 
                      m_living_cost = input$m_living_cost, 
                      ltv = input$ltv,
                      age_kids = input$age_kids)
    # res_df <- res$final_df
    results(res)
  })
   
  # Dynamic title based on results
  output$resultTitle <- renderUI({
    req(results())
    # overall profit rent and invest sp500
    rent_sp500 <- format(results()$Total_profit_rent, big.mark = ",")
    # overall profit buy house and invest sp500
    buy_sp500 <- format(results()$Overall_profit_buying_house, big.mark = ",")

    if(results()$Total_profit_rent > results()$Overall_profit_buying_house){
      titletext <- paste0("Renting is better. Rent: ",rent_sp500," Euro , Buy: ", buy_sp500, " Euro")
    } else {
      titletext <- paste0("Buying is better. Buy: ",buy_sp500," Euro , Rent: ",rent_sp500 , " Euro")
    }

    h3(titletext,
       style = "color: #2c3e50;
                   text-align: center;
                   padding: 15px;
                   background-color: #f8f9fa;
                   border-radius: 5px;
                   margin-bottom: 20px;")
  })
  
  # Render results table
  output$priceTable <- renderDT({
    datatable(results()$final_df,
              options = list(
                pageLength = 600,
                dom = 't',
                ordering = FALSE
              ),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)