library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)

# Get data
getSymbols(c("TLT", "SHY", "HYG", "LQD", "MBB"))

# Assign to dataframe
# Get adjusted prices
prices.data <- merge.zoo(TLT[,6], SHY[,6], HYG[,6], LQD[,6], MBB[,6])

# Calculate returns
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)

# Set names
colnames(returns.data) <- c("TLT", "SHY", "HYG", "LQD", "MBB")

# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)

# Start with the names of the assets
port <- portfolio.spec(assets = c("TLT", "SHY", "HYG", "LQD", "MBB"))

# Box
port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)

# Leverage
port <- add.constraint(portfolio = port, type = "full_investment")

# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 100000, rp_method = "sample")

# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")

# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")

# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate vector of returns
minret <- 0
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = nrow(rportfolios))

eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           Sharperatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  # eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
  #                            conc_aversion = 0.001)
  
  eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")
  
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  
  eff.frontier$Sharperatio[i] <- eff.frontier$Return[i] / eff.frontier$Risk[i]
  
  frontier.weights[i,] = eff.port$weights
  
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}

feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd



p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(data = eff.frontier, x = ~Risk, y = ~Return, mode = "markers", 
            type = "scattergl", showlegend = F, 
            marker = list(color = "#F7C873", size = 5)) %>% 
  
  layout(title = "Random Portfolios with Plotly",
         yaxis = list(title = "Mean Returns"),
         xaxis = list(title = "Standard Deviation"),
#         plot_bgcolor = "#434343",
#         paper_bgcolor = "#F8F8F8",
         annotations = list(
           list(x = 0.004, y = 0.000075, 
                ax = -30, ay = -30, 
                text = "Efficient frontier", 
                font = list(color = "#F6E7C1", size = 15),
                arrowcolor = "white")
         ))

print_app <- function(widget) {
  
  # Generate random file name
  temp <- paste(tempfile('plotly'), 'html', sep = '.')
  
  # Save. Note, leaving selfcontained=TRUE created files that froze my browser
  htmlwidgets::saveWidget(widget, temp, selfcontained = FALSE)
  
  # Launch with desired application
  system(sprintf("chromium-browser -app=file://%s", temp))
  
  # Return file name if it's needed for any other purpose
  temp
}

library(ggplot2)
ggplotly(p)
plotly_json(p)
# use plotly_build() to get at the plotly.js definition
# behind *any* plotly object
b <- plotly_build(p)

# Confirm there 8 traces
length(b$x$data)


# Extract the `name` of each trace. plotly.js uses `name` to 
# populate legend entries and tooltips
purrr::map_chr(b$x$data, "name")


# Every trace has a type of histogram
unique(purrr::map_chr(b$x$data, "type"))


frontier.weights.melt <- reshape2::melt(frontier.weights)

q <- plot_ly(frontier.weights.melt, x = ~Var1, y = ~value, split = ~Var2, type = "bar") %>%
  layout(title = "Portfolio weights across frontier", barmode = "stack",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weights(%)", tickformat = ".0%"))
q

