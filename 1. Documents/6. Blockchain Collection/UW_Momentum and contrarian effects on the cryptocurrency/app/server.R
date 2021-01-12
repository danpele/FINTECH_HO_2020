library(shiny)
library(dygraphs)
library(tidyverse)
library(ggrepel)
library(kableExtra)
library(xts)
library(PerformanceAnalytics)

wregular            <- readRDS("../data/regular/wregular.rds")
crypto_topN         <- readRDS("../data/crypto/crypto_topN.rds")
wrets               <- readRDS("../data/wrets.rds")
wdata               <- readRDS("../data/wdata.rds")
ldata               <- readRDS("../data/ldata.rds")
crypto_topN_names   <- readRDS("../data/crypto/crypto_topN_names.rds")
wcrypto             <- readRDS("../data/crypto/wcrypto.rds")
eqLList             <- readRDS("../data/eqLList.rds")

source("../scripts/setValues4Params.R")
source("../scripts/createStrategyParamsTable.R")

source("../funs/fun-optimizePortfolio.R")
source("../funs/fun-drawEfficientFrontier.R")
source("../funs/fun-drawRandomPortfolio.R")
source("../funs/fun-extractWeightsFromEF.R")
source("../funs/fun-getEquityLine2.R")
source("../funs/fun-getPerformanceStats.R")
source("../funs/fun-findFirstNonNAValue.R")
source("../funs/fun-getSigmaEWMA.R")


shinyServer(function(input, output) {
    
    output$lastDate <- renderText({
        wdata %>% select(Date) %>% 
            tail(1) %>% pull() %>%
            format(., "%Y-%m-%d") 
    })
    
    output$pricesPlot <- renderDygraph({
        wdata %>%
            filter(Date >= "2014-06-30") %>%
            select(input$asset) %>%
            zoo::na.locf(na.rm = F) %>%
            xts::xts(., order.by = wdata %>%
                         filter(Date >= "2014-06-30") %>%
                         select(Date) %>% 
                         pull()) %>%
            dygraph(., 
                    main = paste0(input$asset, 
                                  ", daily closing prices")) %>%
            dyAxis("y", logscale = (input$plotScale == "2")) %>%
            dyRangeSelector(., height = 40)
    })
    
    output$normalizedPricesPlot <- renderDygraph({
        wdata %>%
            filter(Date >= input$analysisRange[[1]]) %>%
            filter(Date <= input$analysisRange[[2]]) %>%
            select(-Date) %>%
            select(input$whichAssets,
                   input$whichAssets2,
                   input$whichAssets3) %>%
            zoo::na.locf(na.rm = F) %>%
            mutate_all(., function(x) x / findFirstNonNAValue(x)) %>%
            xts::xts(., order.by = wdata %>%
                         filter(Date >= input$analysisRange[[1]]) %>%
                         filter(Date <= input$analysisRange[[2]]) %>%
                         select(Date) %>% 
                         pull()) %>%
            dygraph(.,
                    main = paste0(input$whichAssets %>% 
                                      paste(., collapse = ", "))) %>%
            dyLegend(width = 650) %>%
            dyAxis("y", logscale = (input$normalizedPlotScale == "2")) %>%
            dyRangeSelector(., height = 40)
    })
    
    plotData <- reactive({
        # load appropriate previous optimization results
    
        eqL_mom <- 
            strategyParamsTable %>%
            filter(type == "mom",
                   RB   == input$sRB,
                   LB   == input$sLB,
                   topN == input$topN,
                   TC   == input$costs,
                   DFL  == input$DFL1) %>% 
            select(obs) %>%
            pull() %>% 
            eqLList[[.]] %>%
            filter(Date >= input$datesRange[1])
        
        eqL_con <-
            strategyParamsTable %>%
            filter(type == "con",
                   RB   == input$sRB,
                   LB   == input$sLB,
                   topN == input$topN,
                   TC   == input$costs,
                   DFL  == input$DFL1) %>% 
            select(obs) %>%
            pull() %>% 
            eqLList[[.]] %>%
            filter(Date >= input$datesRange[1])
        
        eqL_eqw <-
            strategyParamsTable %>%
            filter(type == "eqw") %>% 
            select(obs) %>%
            pull() %>% 
            eqLList[[.]] %>%
            filter(Date >= input$datesRange[1])
        
        eqL_mcw <-
            strategyParamsTable %>%
            filter(type == "mcw") %>% 
            select(obs) %>%
            pull() %>% 
            eqLList[[.]] %>%
            filter(Date >= input$datesRange[1])
        
        eqL_spx <- 
            wdata %>% 
            select(Date, "S&P500") %>%
            rename(equityLine = "S&P500")
        
        eqL_btc <- 
            wdata %>% 
            select(Date, "Bitcoin") %>%
            rename(equityLine = "Bitcoin")
        
        plot.data <-
            eqL_mom %>% rename(Momentum = equityLine) %>%
            left_join(eqL_con) %>% rename(Contrarian = equityLine) %>%
            left_join(eqL_mcw) %>% rename(MarketCapWeighted = equityLine) %>%
            left_join(eqL_eqw) %>% rename(EquallyWeighted = equityLine) %>%
            left_join(eqL_spx) %>% rename(`S&P500` = equityLine) %>%
            left_join(eqL_btc) %>% rename(Bitcoin = equityLine) %>%
            filter(Date >= input$datesRange[[1]]) %>%
            filter(Date <= input$datesRange[[2]]) %>%
            mutate(Momentum = Momentum / Momentum[1],
                   Contrarian = Contrarian / Contrarian[1],
                   MarketCapWeighted = MarketCapWeighted / MarketCapWeighted[1],
                   EquallyWeighted = EquallyWeighted / EquallyWeighted[1],
                   `S&P500` = `S&P500`/`S&P500`[1],
                   Bitcoin = Bitcoin / Bitcoin[1]
                   )
        
        return(plot.data)
    })
    
    output$strategyPlot <- renderDygraph({
        
        plotData() %>% 
            select(-Date) %>%
            xts::xts(., order.by = plotData()$Date) %>%
            dygraph(.) %>%
            dyAxis("y", logscale = (input$strategyPlotScale == "2")) %>%
            dySeries("S&P500", color = "darkorange") %>%
            dyLegend(width = 750) %>%
            dyRangeSelector(., height = 40)
       
    })
    
    output$strategyPerformance <- renderText({
        
        results <- 
            plotData() %>% 
            select(-Date) %>% 
            lapply(., function(x) getPerformanceStats(x)) %>%
            lapply(., as.data.frame.list) %>%
            bind_rows() %>% 
            as_tibble() %>%
            mutate(IR = ifelse(IR > 0,
                               cell_spec(IR %>% round(1), bold = T),
                               cell_spec(IR %>% round(1), bold = T, color = "red")))
        
        rownames(results) <- names(plotData() %>% select(-Date))
        
        results %>%
            kableExtra::kable(
                caption = "based on daily intervals",
                row.names = T, digits = 2, escape = F
            ) %>%
            kableExtra::kable_styling(
                .,
                bootstrap_options = c("striped", 
                                      "hover", 
                                      #"condensed", 
                                      "responsive" ),
                full_width = T, font_size = 14
            )
        
    })
    
    output$correlationHeatMap <- renderPlot({
        wrets %>%
            filter(Date <= input$correlationDate ) %>%
            filter(Date >= (input$correlationDate - 
                                (input$cLB %>% as.numeric()))) %>%
            select(-Date) %>%
            select(input$whichAssetsForCorrelation,
                   input$whichAssetsForCorrelation2,
                   input$whichAssetsForCorrelation3) %>%
            cor(. , use = "pairwise.complete.obs") %>%
            round(2) %>%
            reshape2::melt() %>%
            ggplot(aes(x = Var1, y = Var2, fill = value)) + 
            geom_tile() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1)) +
            geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.title = element_blank()) +
            labs(title = "")
    })
    
    output$returnsAnalysis <- renderPlot({
        wrets %>%
            filter(Date <= input$returnsDate ) %>%
            filter(Date >= (input$returnsDate - 
                                (input$rLB %>% as.numeric()))) %>%
            select(input$whichAssetsForReturns,
                   input$whichAssetsForReturns2,
                   input$whichAssetsForReturns3) %>%
            psych::pairs.panels(method = "pearson",
                                hist.col = "#3746f8",
                                breaks = 70,
                                rug = FALSE,
                                stars = TRUE,
                                lm = TRUE,
                                density = FALSE)
    })
   
    output$drawdownsPlot <- renderPlot({
        tmp <-
            plotData() %>%
            select(-Date) %>%
            mutate_all(function(x) xts::diff.xts(x) / xts::lag.xts(x)) %>%
            xts::xts(., 
                     order.by =  plotData()$Date)
        
        chart.Drawdown(tmp, legend.loc = "bottomleft") 
        
    })
    
    output$drawdownsPlot2 <- renderDygraph({
        plotData() %>%
            select(-Date) %>%
            mutate_all(function(x) xts::diff.xts(x) / xts::lag.xts(x)) %>%
            xts::xts(., 
                     order.by =  plotData()$Date) %>%
            Drawdowns() %>%
            dygraph(.) %>%
            dyLegend(width = 750) %>%
            dyRangeSelector(., height = 40) %>%    
            dyAxis("y", valueRange = c(-1, 0.15)) %>%
            dySeries("S&P500", color = "darkorange")
    
    })
    
    output$correlationHeatMap2 <- renderPlot({
        plotData() %>%
            select(-Date) %>%
            cor(. , use = "pairwise.complete.obs") %>%
            round(2) %>%
            reshape2::melt() %>%
            ggplot(aes(x = Var1, y = Var2, fill = value)) + 
            geom_tile() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1)) +
            geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.title = element_blank()) +
            labs(title = "")
    })
  
    strategyCorrInTimeData <- reactive({
        mat <- 
            plotData() %>%
            select(-Date) %>%
            mutate_all(list(function (x) diff.xts(x)/lag.xts(x))) %>%
            select(input$strategy4corr1, 
                   input$strategy4corr2) %>%
            filter(row_number() > 1) %>%
            as.matrix()
        
        ewma.results <- getSigmaEWMA(mat, lambda = input$lambda)
        
        # EWMA estimate of correlation  
        ewma.corr <- ewma.results$Sigma.t[, 2] /
            (sqrt(ewma.results$Sigma.t[, 1] * ewma.results$Sigma.t[, 4]))
        ewma.corr <- c(NA, ewma.corr)
        
        plotData <-
            plotData() %>% 
            select(input$strategy4corr1, 
                   input$strategy4corr2, 
                   -Date) %>%
            mutate(correlation = ewma.corr) %>%
            xts::xts(., order.by = plotData()$Date)
        
        return(plotData)
    })
    
    output$strategyCorrelationsInTime1 <- renderDygraph({
        
        strategyCorrInTimeData()[, c(1:2)]%>%
        dygraph(., 
                group = strategyCorrInTimeData(),
                height = 250) %>%
            dyAxis("y", logscale = F,
                   label = "correlation", independentTicks = TRUE) %>%
            dyLegend(width = 750) %>%
            dyRangeSelector(., height = 40) %>%    
            dyAxis("y", logscale = (input$strategyPlotScale == "2"))
           
    })
    
    output$strategyCorrelationsInTime2 <- renderDygraph({
        
        strategyCorrInTimeData()[, "correlation"] %>%
        dygraph(., 
                group = strategyCorrInTimeData(),
                height = 250) %>%
            dySeries("correlation", color = "darkorange") %>%
            dyAxis("y", logscale = F,
                   label = "correlation", independentTicks = TRUE) %>%
            dyLegend(width = 750) %>%
            dyRangeSelector(., height = 40)
        
    })
    
})
