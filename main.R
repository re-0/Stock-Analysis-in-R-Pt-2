library(tidyquant)
library(forecast)

# Prepare stock symbols
stock.sym <- c("GOOG", "MSFT", "IBM",  "INTC", "AAPL",
               "T",    "VZ",   "CSCO", "TSM",  "CHL",
               "ORCL", "NVDA", "ADBE", "ACN",  "BIDU")

# Get Data
tech.stocks <- tq_get(stock.sym,
                      get = "stock.prices",
                      from = (today() - years(5)),
                      to = today()) %>% 
                      group_by(symbol)


# Compute Annual Returns
tech.annual.returns <- tech.stocks %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               type ="arithmetic")

# Get returns for a specific date
# tech.annual.returns %>% filter(date == "2018-03-29")

# Plot Annual Returns
tech.comp <- 
tech.annual.returns %>% 
  ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tech Stocks: Annual Returns",
       subtitle = "Stocks have been randomly selected",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 3) +
  theme_tq() + 
  coord_x_date(ylim = c(0, 1)) +
  scale_fill_grey()

# This piece of code returns the five top performers in the last year [i.e., today(); today - years(1)]
stocks.top5 <- (tech.annual.returns %>% 
       filter(date >= (today() - years(1)) & date <= today()) %>% 
       arrange(desc(yearly.returns)))[1:5,]

# Plot Line Chart of Top Five Stocks (adj. close) in one graph
# Prepare the data
tech.stocks.top5 <- tech.stocks %>% filter(symbol == stocks.top5$symbol)

# Chart the graph
tech.stocks.top5 %>% ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Five best perfoming stocks",
       subtitle = "NVDA, ADBE, AAPL, BIDU, TSM",
       x = "", y = "Price in US$ (adjusted)") +
  theme_tq() +
  scale_color_tq()
# Note: As of April 4, 2018

# Plot the Top 5 with Bollinger Bands and Candlesticks
tech.stocks.top5 %>%
  ggplot(aes(x     = date, y     = adjusted,
             open  = open, high  = high,
             low   = low,  close = adjusted,
             group = symbol)) +
  geom_line(size = 1) +
  geom_candlestick(size = 1) +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) +
  labs(title = "Daily Prices",
       x = "Date", y = "Price") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq()

### Compute Daily Returns and Betas
stock_returns.daily <- tech.stocks %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "arithmetic",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns)

sp500 <- tq_get("^GSPC", from = (today() - years(5)))
ret.sp500 <- sp500 %>% tq_transmute(select     = adjusted,
                                    mutate_fun = periodReturn,
                                    period     = "daily",
                                    type       = "arithmetic")
  
tnx <- tq_get("^TNX", from = (today() - years(5)))
ret.tnx <- tnx %>% tq_transmute(select     = adjusted,
                                mutate_fun = periodReturn,
                                period     = "daily",
                                type       = "arithmetic")
## There was data missing. hence:
## sp <- ret.sp500[,-2]
## tx <- ret.tnx[,-2]
## anti_join(sp, tx) # returns "2016-11-11"
ret.tnx <- add_row(ret.tnx, date = "2016-11-11", daily.returns = 0)
ret.tnx <- ret.tnx %>% arrange(date)

stock_returns.daily <- add_column(stock_returns.daily,
                                    SP500 = ret.sp500$daily.returns, TNX = ret.tnx$daily.returns)

excess.Returns <- ((stock_returns.daily.2[, 1:ncol(stock_returns.daily.2)-1]) - stock_returns.daily.2$TNX)[,-1]

# Returns a Vector with every beta
# Note: Will throw a warning message for SP500 ~ SP500 (obviously)
beta <- (sapply(colnames(excess.Returns),
                function(x){
                  summary(
                    lm(
                      excess.Returns[,x] ~ excess.Returns[, ncol(excess.Returns)])
                    )$coefficients[2,1]
                  }))

# Remove SP500 ~ SP500
beta <- beta[-length(beta)]

# Uncomment to plot line graph of AAPL over one year
# tq_get("AAPL") %>% 
#   ggplot(aes(x = date, y = close)) + 
#   geom_line() + 
#   geom_bbands(aes(high = high, low = low, close = close), 
#               ma_fun = SMA, n = 50) + 
#   coord_x_date(xlim = c(today() - years(1), today()), ylim = c(100, 170))

# Old way I computed beta
# temp <- tech.stocks
# tmp.aapl <- temp %>% filter(symbol == "AAPL" & date >= (today() - years(1)) & date <= today()) 
# ret.aapl <- unlist(diff(tmp.aapl$adjusted)/tmp.aapl[-nrow(tmp.aapl),   "adjusted"], use.names = FALSE)
## Getting S&P 500 & T-Bill returns
# xRet_AAPL   <- ret.aapl  - ret.tnx
# beta.AAPL <- summary(lm(xRet_AAPL   ~ xRet_SP.TNX))$coefficients[2,1]
# ...
