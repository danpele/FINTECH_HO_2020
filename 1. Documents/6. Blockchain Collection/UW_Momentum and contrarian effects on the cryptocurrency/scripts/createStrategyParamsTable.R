strategyParamsTable <-
  expand.grid(type = values4Params[["strategyTypes"]], 
              RB   = values4Params[["values4RB"]],
              LB   = values4Params[["values4LB"]],
              topN = values4Params[["values4topN"]],
              TC   = values4Params[["values4TC"]],
              DFL  = values4Params[["values4DFL"]],
              stringsAsFactors = F) %>%
  as_tibble() %>%
  bind_rows(expand.grid(type = c("eqw", "mcw"), 
                        RB   = "1W",
                        LB   = "1W",
                        topN = 0.25,
                        TC   = 0.01,
                        DFL  = 1,
                        stringsAsFactors = F) %>%
              as_tibble()
            ) %>%
  mutate(obs = 1:nrow(.)) %>%
  select(obs, everything())


  
