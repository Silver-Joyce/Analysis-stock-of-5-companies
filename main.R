# --------------------------------------- Part 1: Prepare data --------------------------------------- #
# The symbols of the financial time series 
symbols <- c("AAPL", "GOOG", "MSFT", "GLD", "AMZN")
getSymbols(symbols, from = "2010-01-01")


# --------------------------------------- Part 2: Process data --------------------------------------- #
# Price level
prices <- cbind(AAPL$AAPL.Close, GOOG$GOOG.Close, MSFT$MSFT.Close, GLD$GLD.Close, AMZN$AMZN.Close)

prices_df <- data.frame(prices)
colnames(prices_df) <- c("AAPL", "GOOG", "MSFT", "GLD", "AMZN")
prices_df$day <- rownames(prices_df)
prices_df$day <- as.Date(prices_df$day)
rownames(prices_df) <- NULL
# Reformat to plot
prices_long <- gather(prices_df, key = "Index", value = "Price", -day)

# Return level
# returns <- diff(log(prices))
returns <- diff(prices)
returns <- na.omit(returns) 
returns <- (returns / prices[-nrow(prices), ]) * 100

returns_df <- data.frame(returns)
colnames(returns_df) <- c("AAPL", "GOOG", "MSFT", "GLD", "AMZN")
returns_df$day <- rownames(returns_df)
returns_df$day <- as.Date(returns_df$day)
rownames(returns_df) <- NULL
# Reformat to plot
returns_long <- gather(returns_df, key = "Index", value = "Return", -day)


# --------------- Statistics --------------- #
# Price level
summary(prices_df)
price.std <- sapply(prices_df, sd)
price.std$day <- NULL
# Return level
summary(returns_df)
return.std <- sapply(returns_df, sd)
return.std$day <- NULL


# --------------------------------------- Part 3: Portfolio computing --------------------------------------- #
# Equally-weighted portfolio
weights_eq <- rep(1/5, 5) 
returns_eq <- returns %*% weights_eq
returns_eq <- data.frame(returns_eq)
returns_eq$day <- returns_df$day
colnames(returns_eq) <- c("Equally-weighted", "day")
rownames(returns_eq) <- NULL

# Personalised portfolio
per_weights <- c(2/5, 1/5, 1/5, 1/10, 1/10) # Personal weighted
returns_per <- returns %*% per_weights 
returns_per <- data.frame(returns_per)
returns_per$day <- returns_df$day
colnames(returns_per) <- c("Personal weigthed", "day")
rownames(returns_per) <- NULL

# --------------- Statistics of portfolio --------------- #
summary(returns_eq)
sd(returns_eq$`Equally-weighted`)

summary(returns_per)
sd(returns_per$`Personal weigthed`)

# Reformat to plot
portfolio_return <- merge(returns_eq, returns_per, by = "day")
portfolio_return <- gather(portfolio_return, key = "pfl", value = "Return", -day)

# --------------------------------------- Part 4: Plotting --------------------------------------- #
# --------------- Part 4.1: Plot the price and return --------------- #
# ---------- Part 4.1.1: Plot the price ---------- #
price_plt <- ggplot(data = prices_long, aes(x=day, y=Price, 
                                            color=Index))
price_plt + 
  geom_line() + 
  coord_cartesian(xlim = as.Date(c("2010-01-01", "2023-01-01"))) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 350, 50)) +
  xlab("Year") + # Title of x axis
  ylab("Price ($)") + # Title of y axis 
  ggtitle("Stock price of 5 companies") + # Plot title
  theme(plot.title = element_text(size = 40, color = "DarkBlue", family = "Courier", hjust = 0.5), 
        axis.title.x = element_text(size = 20, color = "DarkGreen"),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20, color = "DarkGreen"),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("LightGray", 1)),
        legend.key = element_rect(fill="LightGray"),
        panel.background = element_rect(fill=alpha("LightGray", 0)),
        panel.grid.major = element_line(color="Black", linewidth = 0.1),
        panel.grid.minor = element_line(linewidth = 0))
  

# ---------- Part 4.1.2: Plot the return ---------- #
returns_plt <- ggplot(data = returns_long, aes(x=day, y=Return, 
                                              color=Index))
returns_plt + 
  geom_line() + 
  coord_cartesian(xlim = as.Date(c("2010-01-01", "2023-01-01"))) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_grid(Index~.) + 
  xlab("Year") + # Title of x axis
  ylab("Return (%)") + # Title of y axis 
  ggtitle("Stock return of 5 companies") + # Plot title
  theme(plot.title = element_text(size = 40, color = "DarkBlue", family = "Courier", hjust = 0.5), 
        axis.title.x = element_text(size = 20, color = "DarkGreen"),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20, color = "DarkGreen"),
        axis.text.y = element_text(size = 15),
        legend.position = "none",
        panel.background = element_rect(fill=alpha("LightGray", 0)),
        panel.grid.major = element_line(color="Black", linewidth = 0.1),
        panel.grid.minor = element_line(linewidth = 0))

# --------------- Part 4.2: Plot the two portfolio --------------- #
plt <- ggplot(data = portfolio_return, aes(x=day, y=Return,
                                           color=pfl))
plt +
  geom_line() + 
  coord_cartesian(xlim = as.Date(c("2010-01-01", "2023-01-01"))) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(-10, 10)) +
  facet_grid(pfl~., labeller = labeller(group=c("A", "B"))) +
  xlab("Year") + # Title of x axis
  ylab("Return (%)") + # Title of y axis 
  ggtitle("Portfolio return") + # Plot title
  theme(plot.title = element_text(size = 40, color = "DarkBlue", family = "Courier", hjust = 0.5), 
        axis.title.x = element_text(size = 20, color = "DarkGreen"),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20, color = "DarkGreen"),
        axis.text.y = element_text(size = 15),
        legend.position = "none",
        panel.background = element_rect(fill=alpha("LightGray", 0)),
        panel.grid.major = element_line(color="Black", linewidth = 0.1),
        panel.grid.minor = element_line(linewidth = 0))



