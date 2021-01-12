first_date <- readRDS("data/wrets.rds") %>% select(Date) %>% pull() %>% min() %>% as.Date()
last_date  <- readRDS("data/wrets.rds") %>% select(Date) %>% pull() %>% max() %>% as.Date()

cat("The first date in the data is ", first_date %>% as.character(), ", ",
    "while the last date is ", last_date %>% as.character(), ".\n\n",
    "If you want to download the new data, please run the script 'src/download_data.R'.\n\n",
    "To transform the downloaded raw data and to update the database used in the application,\n", 
    "please assign the appropraite date to the 'stop_date' variable and run the script 'src/transform_data.R'\n\n",
    "Otherwise, you may proceed with the script 'load_data.R'",
    sep = "")
