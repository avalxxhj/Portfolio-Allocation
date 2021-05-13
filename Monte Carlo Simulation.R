etf.price=as.matrix(Bond_ETF_2007_2021)
mc_rep = 500 # Number of Monte Carlo Simulations
training_days = 30

# This function returns the first differences of a t x q matrix of data
returns = function(Y){
  len = nrow(Y)
  yDif = Y[2:len, ] / Y[1:len-1, ] - 1
}

# Get the Stock Returns
etf.returns = returns(etf.price)

# or etf.returns=as.matrix(returns.data)

# Suppose we invest our money evenly among all three assets 
# We use today's Price 11/14/2018 to find the number of shares each stock 
# that we buy
portfolio_Weights = t(as.matrix(rep(1/ncol(etf.returns), ncol(etf.returns))))
print(portfolio_Weights)

# Get the Variance Covariance Matrix of Stock Returns
coVarMat = cov(etf.returns)
miu = colMeans(etf.returns)
# Extend the vector to a matrix
Miu = matrix(rep(miu, training_days), nrow = 5)

# Initializing simulated 100 day portfolio returns
portfolio_Returns_100_m = matrix(0, training_days, mc_rep)

set.seed(200)
for (i in 1:mc_rep) {
  Z = matrix ( rnorm( dim(etf.returns)[2] * training_days ), ncol = training_days )
  # Lower Triangular Matrix from our Choleski Factorization
  L = t( chol(coVarMat) )
  # Calculate stock returns for each day
  daily_Returns = Miu + L %*% Z  
  # Calculate portfolio returns for 30 days
  portfolio_Returns_100 = cumprod( portfolio_Weights %*% daily_Returns + 1 )
  # Add it to the monte-carlo matrix
  portfolio_Returns_100_m[,i] = portfolio_Returns_100;
}

# Visualising result
x_axis = rep(1:training_days, mc_rep)
y_axis = as.vector(portfolio_Returns_100_m-1)
plot_data = data.frame(x_axis, y_axis)
ggplot(data = plot_data, aes(x = x_axis, y = y_axis)) + geom_path(col = 'red', size = 0.1) +
  xlab('Days') + ylab('Portfolio Returns') + 
  ggtitle('Simulated Portfolio Returns in 30 days')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Porfolio Returns statistics on the 30th day.
Avg_Portfolio_Returns = mean(portfolio_Returns_100_m[30,]-1)
SD_Portfolio_Returns = sd(portfolio_Returns_100_m[30,]-1)
Median_Portfolio_Returns = median(portfolio_Returns_100_m[30,]-1)
print(c(Avg_Portfolio_Returns,SD_Portfolio_Returns,Median_Portfolio_Returns))