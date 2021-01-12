library(shiny)
library(dygraphs)
library(tidyverse)

source("../scripts/setValues4Params.R")

wregular  <- readRDS("../data/regular/wregular.rds")
wcrypto   <- readRDS("../data/crypto/wcrypto.rds")
last.date <- wcrypto %>% select(Date) %>% pull() %>% max()
mainCryptoNames  <- 
    readRDS("../data/crypto/crypto_topN.rds") %>% 
    filter(Date == last.date) %>% 
    arrange(desc(MarketCap)) %>% 
    head(10) %>% 
    select(Name) %>% 
    pull()
otherCryptoNames <- 
    readRDS("../data/crypto/crypto_topN.rds") %>% 
    select(Name) %>%
    pull() %>% 
    unique() %>%
    setdiff(mainCryptoNames) %>%
    sort()
    



shinyUI(fluidPage(
    headerPanel("Momentum and contrarian effects on the crytpocurrency market"),
    
    tabsetPanel(
        # DATA EXPLORATION tab =================================================
        tabPanel(
            title = "Data exploration",
            sidebarPanel(
                width = 3,
                p(strong("Last observation found in the data:")),
                textOutput("lastDate"), 
                br(),
                selectInput(
                    inputId = "plotType", 
                    label = "Select type of the plot", 
                    choices = c(
                        "Prices for the single asset" = 1,
                        "Normalized prices for multiple assets" = 2,
                        "Correlations" = 3,
                        "Returns analysis" = 4
                    ), 
                    selected = 1, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL
                ),
                conditionalPanel(
                    "input.plotType == 1",
                    selectInput(
                        "asset", 
                        label = "Select asset", 
                        c(mainCryptoNames,
                          wregular %>% select(-Date) %>% colnames(),
                          otherCryptoNames),
                        selected = "Bitcoin"
                    ),
                    radioButtons(
                        inputId = "plotScale", 
                        label = "Scale of Y asis", 
                        choices = c("Linear" = 1, "Log10" = 2), 
                        selected = 1, inline = FALSE, width = NULL),
                ),
                conditionalPanel(
                    "input.plotType == 2",
                    dateRangeInput("analysisRange", 
                                   label = "Analysis period", 
                                   start = "2014-06-30", 
                                   end   = as.character(last.date), 
                                   min   = "2014-06-30", 
                                   max   = as.character(last.date), 
                                   separator = " to "),
                    # helpText("Parameters of the plot"),
                    radioButtons(
                        inputId = "normalizedPlotScale", 
                        label = "Scale of Y asis", 
                        choices = c("Linear" = 1, "Log10" = 2), 
                        selected = 2, inline = FALSE, width = NULL),
                    # helpText("Assets selection"),
                    checkboxGroupInput(
                        inputId  = "whichAssets", 
                        label    = "Which main cryptos?", 
                        choices  = mainCryptoNames, 
                        selected = c("Bitcoin"), 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssets2", 
                        label    = "Which main equity indices?", 
                        choices  = wregular %>% select(-Date) %>% colnames(), 
                        selected = "S&P500", 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssets3", 
                        label    = "Which other cryptos?", 
                        choices  = otherCryptoNames, 
                        selected = NULL, 
                        inline   = FALSE, width = NULL)
                ),
                conditionalPanel(
                    "input.plotType == 3",
                    dateInput(inputId = "correlationDate", 
                              label   = "Last observation to include", 
                              value   = as.character(last.date), 
                              min = "2014-06-30", 
                              max = as.character(last.date),
                              format = "yyyy-mm-dd", startview = "month", 
                              weekstart = 1,
                              language = "en", width = NULL),
                    selectInput(
                        "cLB", 
                        "Lookback period", 
                        choices = c(
                            "30 days"   = 30,
                            "60 days"   = 60,
                            "90 days"   = 90,
                            "180 days"  = 180
                        )
                    ),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForCorrelation", 
                        label    = "Which main cryptos?", 
                        choices  = mainCryptoNames, 
                        selected = mainCryptoNames, 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForCorrelation2", 
                        label    = "Which main equity indices?", 
                        choices  = wregular %>% select(-Date) %>% colnames(), 
                        selected = "S&P500", 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForCorrelation3", 
                        label    = "Which other cryptos?", 
                        choices  = otherCryptoNames, 
                        selected = NULL, 
                        inline   = FALSE, width = NULL)
                ),
                conditionalPanel(
                    "input.plotType == 4",
                    dateInput(inputId = "returnsDate", 
                              label   = "Last observation to include", 
                              value   = as.character(last.date), 
                              min = "2014-06-30", 
                              max = as.character(last.date),
                              format = "yyyy-mm-dd", startview = "month", 
                              weekstart = 1,
                              language = "en", width = NULL),
                    selectInput(
                        "rLB", 
                        "Lookback period", 
                        choices = c(
                            "30 days"   = 30,
                            "60 days"   = 60,
                            "90 days"   = 90,
                            "180 days"  = 180
                        )
                    ),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForReturns", 
                        label    = "Which main cryptos?", 
                        choices  = mainCryptoNames, 
                        selected = mainCryptoNames[1:5], 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForReturns2", 
                        label    = "Which main equity indices?", 
                        choices  = wregular %>% select(-Date) %>% colnames(), 
                        selected = "S&P500", 
                        inline   = FALSE, width = NULL),
                    checkboxGroupInput(
                        inputId  = "whichAssetsForReturns3", 
                        label    = "Which other cryptos?", 
                        choices  = otherCryptoNames, 
                        selected = NULL, 
                        inline   = FALSE, width = NULL)
                )
            ),
            mainPanel(
                conditionalPanel(
                    "input.plotType == 1",
                    helpText(h3("Prices")),
                    dygraphOutput("pricesPlot")
                ),
                conditionalPanel(
                    "input.plotType == 2",
                    helpText(h3("Normalized prices")),
                    dygraphOutput("normalizedPricesPlot")
                ),
                conditionalPanel(
                    "input.plotType == 3",
                    tabsetPanel(
                        tabPanel(
                            "heatmap",
                            helpText(h3("Correlation between daily returns")),
                            plotOutput("correlationHeatMap")
                        ),
                        tabPanel(
                            "dynamics in time",
                            helpText(h3("not ready yet..."))
                        )
                    )
                ),
                conditionalPanel(
                    "input.plotType == 4",
                    helpText(h3("Histograms and scatter plots for returns")),
                    plotOutput("returnsAnalysis")
                )
            )
        ),
    
        # STRATEGIES tab =======================================================
        tabPanel(
            title = "Strategies",
            sidebarPanel(
                width = 3,
                helpText(
                    p("Parameters of the strategy")
                ),
                selectInput(
                    "sRB", 
                    "Rebalancing period", 
                    choices = values4Params[["values4RB"]], 
                    selected = "1W", multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                selectInput(
                    "sLB", 
                    "Lookback period", 
                    choices = values4Params[["values4LB"]], 
                    selected = "1W", multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                selectInput(
                    "topN", 
                    "top N contrarian/momentum crypto to invest in", 
                    choices = values4Params[["values4topN"]], 
                    selected = 25, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                dateRangeInput("datesRange", 
                               label = "Investment period", 
                               start = "2014-06-30", 
                               end   = as.character(last.date), 
                               min   = "2014-06-30", 
                               max   = as.character(last.date), 
                               separator = " to "),
                selectInput(
                    "DFL1", 
                    "DFL - Degree of Financial Leverage", 
                    choices = values4Params[["values4DFL"]], 
                    selected = 1, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                #sliderInput(
                #    "DFL1", 
                #    "DFL - Degree of Financial Leverage", 
                #    min = 0.1, max = 5, value = 1.0, step = 0.1),
                selectInput(
                    "costs", 
                    "transaction costs (%):", 
                    choices = values4Params[["values4TC"]], 
                    selected = 0.01, multiple = FALSE, 
                    selectize = TRUE, width = NULL, size = NULL),
                #sliderInput(
                #    "costs", 
                #    "transaction costs (%):", 
                #    min = 0.01, max = 0.5, value = 0.1, step = 0.01),
                helpText("Parameters of the plot"),
                radioButtons(
                    inputId  = "strategyPlotScale", 
                    label    = "Scale of Y asis", 
                    choices  = c("Linear" = 1, "Log10" = 2), 
                    selected = 1, inline = FALSE, width = NULL)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Equity lines",
                        helpText(h2("Equity lines")),
                        dygraphOutput("strategyPlot")),
                    tabPanel(
                        "Performance measures",
                        helpText(h2("Performance measures")),
                        htmlOutput("strategyPerformance"),
                        helpText(
                            p("abbrevations:"),
                            tags$ul(
                                tags$li(strong("aRC"),     " - Annualized Rate of Return (%), compounded daily"),
                                tags$li(strong("aSD"),     " - Annualized Standard Deviation of daily returns (%)"),
                                tags$li(strong("MD"),      " - Maximum Drawdown (%)"),
                                tags$li(strong("MLD"),     " - Maximum Loss Duration (in years)"),
                                tags$li(strong("IR"),      " - Information Ratio, aRC/aSD"),
                                tags$li(strong("IRMD"),    " - IR/MD"),
                                tags$li(strong("IRaRCMD"), " - IR * aRC/MD"),
                                tags$li(strong("IR2"),     " - aRC ^ 3 * sign(aRC) / (aSD * MD * MLD)"),
                                tags$li(strong("nObs"),    " - number of observations")
                            )
                        )
                        ),
                    tabPanel(
                        "Drawdowns",
                        helpText(h2("Drawdowns")),
                        # plotOutput("drawdownsPlot")
                        dygraphOutput("drawdownsPlot2")
                    ),
                    tabPanel(
                        "Correlations",
                        helpText(h3("Correlations between strategies")),
                        plotOutput("correlationHeatMap2")
                    ),
                    tabPanel(
                        "Correlations in time",
                        helpText(h3("Correlations between strategies in time")),
                        fluidRow(
                            column(4, 
                                   selectInput(
                                       "strategy4corr1", 
                                       "strategy #1", 
                                       choices = c(
                                           "Momentum",
                                           "Contrarian",
                                           "MarketCapWeighted",
                                           "EquallyWeighted",
                                           "`S&P500`",
                                           "Bitcoin"),
                                       selected = "Momentum"
                                   )
                            ),
                            column(4, 
                                   selectInput(
                                       "strategy4corr2", 
                                       "strategy #2", 
                                       choices = c(
                                           "Momentum",
                                           "Contrarian",
                                           "MarketCapWeighted",
                                           "EquallyWeighted",
                                           "`S&P500`",
                                           "Bitcoin"),
                                       selected = "Contrarian"
                                   )
                            ),
                            column(4, 
                                   sliderInput(
                                       "lambda", 
                                       "lambda for EWMA",
                                       min = 0.8, 
                                       max = 0.99, 
                                       value = 0.96, 
                                       step = 0.01
                                   )
                            )
                        ),
                        dygraphOutput("strategyCorrelationsInTime1", height = "300px"),
                        br(), br(), br(),
                        dygraphOutput("strategyCorrelationsInTime2", height = "300px")
                    )
                )
            )
        ),
        
        
        # ABOUT tab ============================================================
        tabPanel(
            title = "About",
            sidebarPanel(
                br(), br(),
                tags$img(src = "qfrg_new.png", width = "350px",
                         style = "display: block; margin-left: auto; 
                         margin-right: auto;"), 
                br(), br(),
                tags$img(src = "wne-eng2.png", width = "250px",
                         style = "display: block; margin-left: auto; 
                         margin-right: auto;"), 
                br(), br()
            ),
            mainPanel(
                helpText(
                    h3("Application"),
                    p("This application has been designed and implemented to
                      extend the results presented in Kość, Sakowski, Ślepaczuk
                      (2019) Momentum and contrarian effects on the cryptocurrency 
                      market, Physica A 523 (2019) 691-701. The authors of the study
                      are members of the Quantitative Finance Research Group at 
                      the University of Warsaw, Faculty of Economic Sciences."),
                    p("The main purpose is to allow the user investigation of the 
                      momentum and contrarian effects on cryptocurrency markets. 
                      The investigated investment strategies involve 100 (amongst 
                      over 1200 present as of Nov 2017 and 
                      over 5500 as of May 2020) cryptocurrencies with 
                      the largest market cap and average 14-day daily volume 
                      exceeding a given threshold value. Investment portfolios 
                      are constructed using different assumptions regarding the 
                      portfolio reallocation period, width of the ranking window, 
                      the number of cryptocurrencies in the portfolio, and the
                      percent transaction costs."),
                    p("The performance is benchmarked 
                      against: (1) equally weighted and (2) market-cap weighted 
                      investments in all of the ranked assets, as well as against
                      the buy and hold strategies based on (3) S&P500 index, 
                      and (4) Bitcoin price."),
                    p("The backtesting engine lets the user test strategies 
                      with different lookback periods, levels of rebalancing
                      frequency, degree of financial leverage (DFL) on 
                      historical data from the manually specified period.
                      Core calculations and web interface have been 
                      fully implemented in R using Shiny technology."),
                    h3("Data vendors"),
                    p("Daily OHLC prices of assets from regulated markets are
                       provided by ", 
                      a(href = "www.stooq.com", "stooq.com"), ",",
                      "while daily OHLC prices and market capitalizations of
                      cryptocurriencies are scraped from ",
                      a(href = "www.coinmarketcap.com", "coinmarketcap.com"),
                      "."),
                    h3("Disclaimer"),
                    p("No offer or solicitation to buy or sell securities or 
                      cryptocurrency products of any kind, or any type of 
                      investment or trading advice or strategy, is made, 
                      given or in any manner endorsed by Quantitative Finance 
                      Research Group at WNE UW or their
                      affiliates. Testimonials regarding past performance 
                      are no guarantee of future results and may not be 
                      representative of the experience of other customers."),
                    h3("Author"),
                    p("Paweł Sakowski, WNE UW, QFRG, ", 
                      a(href = "mailto:pawelsakowski@gmail.com", 
                        "pawelsakowski@gmail.com")
                    ),
                    h3("Credits"),
                    p("Special thanks to Przemysław Ryś from QFRG for his 
                       substantial help and contributions to the core functions 
                       deployed in the application.")
                )
            )
        )
    )
    
))
    
