# plumber.R

library("qrmtools")
library("rugarch")

con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  username = "",
  password = "", 
  host = "", 
  port = 3306,
  dbname = "")


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}



#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}  



#* Fit ARMA-GARCH and get Prediction VaR
#* @param df The dataframe name
#* @param mu_plot The mu plot
#* @param mainplot The main plot
#* @post /column
function(df,mu_plot,mainplot) {
  df = dbReadTable(con, df)##### LOAD DATA ####################
  n = length(df$close)
  armaOrder <- c(1,1) # ARMA order
  garchOrder <- c(1,1) # GARCH order
  varModel <- list(model = "sGARCH", garchOrder = garchOrder)
  
  spec <- ugarchspec(varModel, mean.model = list(armaOrder = armaOrder),
                     distribution.model = "std") # without fixed parameters here
  X = df$close
  
  fit <- ugarchfit(spec, data = X) # fit
  
  ## Extract the resulting series
  mu. <- fitted(fit) # fitted hat{mu}_t (= hat{X}_t)
  sig. <- sigma(fit) # fitted hat{sigma}_t
  
  if (mu_plot == 1)
  {
    plot(X, type = "l", xlab = "t",
         ylab = expression("Data"~X[t]~"and fitted values"~hat(mu)[t]))
    lines(as.numeric(mu.), col = adjustcolor("blue", alpha.f = 0.5))
    legend("bottomright", bty = "n", lty = c(1,1),
           col = c("black", adjustcolor("blue", alpha.f = 0.5)),
           legend = c(expression(X[t]), expression(hat(mu)[t])))
  }
  
  
  alpha <- 0.99
  
  
  
  ## Extract fitted VaR_alpha
  VaR. <- as.numeric(quantile(fit, probs = alpha))
  
  ## Build manually and compare the two
  nu. <- fit@fit$coef[["shape"]] # extract (fitted) d.o.f. nu
  VaR.. <- as.numeric(mu. + sig. * sqrt((nu.-2)/nu.) * qt(alpha, df = nu.)) # VaR_alpha computed manually
  
  
  
  fspec <- getspec(fit) # specification of the fitted process
  setfixed(fspec) <- as.list(coef(fit)) # set the parameters to the fitted ones
  m <- ceiling(n / 100) # number of steps to forecast (roll/iterate m-1 times forward with frequency 1)
  pred <- ugarchforecast(fspec, data = X, n.ahead = 1, n.roll = m-1, out.sample = m) # predict from the fitted process
  
  ## Extract the resulting series
  mu.predict <- fitted(pred) # extract predicted X_t (= conditional mean mu_t; note: E[Z] = 0)
  sig.predict <- sigma(pred) # extract predicted sigma_t
  VaR.predict <- as.numeric(quantile(pred, probs = alpha)) # corresponding predicted VaR_alpha
  
  ## Build predicted VaR_alpha manually and compare the two
  nu. <- fit@fit$coef[["shape"]]
  VaR.predict. <- as.numeric(mu.predict + sig.predict * sqrt((nu.-2)/nu.) *
                               qt(alpha, df = nu.)) # VaR_alpha computed manually
  stopifnot(all.equal(VaR.predict., VaR.predict))
  
  
  ## Compute simulated VaR_alpha and corresponding (simulated) confidence intervals
  ## Note: Each series is now an (m, B) matrix (each column is one path of length m)
  B <- 1000
  set.seed(271)
  X.sim.obj <- ugarchpath(fspec, n.sim = m, m.sim = B) # simulate future paths
  
  X.sim <- fitted(X.sim.obj) # extract simulated X_t
  sig.sim <- sigma(X.sim.obj) # extract sigma_t
  eps.sim <- X.sim.obj@path$residSim # extract epsilon_t
  VaR.sim <- (X.sim - eps.sim) + sig.sim * sqrt((nu.-2)/nu.) * qt(alpha, df = nu.) # (m, B) matrix
  VaR.CI <- apply(VaR.sim, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
  
  if (mainplot == 1){
    yran <- range(X, # simulated path
                  mu., VaR., # fitted conditional mean and VaR_alpha
                  mu.predict, VaR.predict, VaR.CI) # predicted mean, VaR and CIs
    myran <- max(abs(yran))
    yran <- c(-myran, myran) # y-range for the plot
    xran <- c(1, length(X) + m) # x-range for the plot
    
    ## Simulated (original) data (X_t), fitted conditional mean mu_t and VaR_alpha
    plot(X, type = "l", xlim = xran, ylim = yran, xlab = "Time t", ylab = "",
         main = "Simulated ARMA-GARCH, fit, VaR, VaR predictions and CIs")
    lines(as.numeric(mu.), col = adjustcolor("darkblue", alpha.f = 0.5)) # hat{\mu}_t
    lines(VaR., col = "darkred") # estimated VaR_alpha
    mtext(paste0("Expected exceed.: ",btest$expected.exceed,"   ",
                 "Actual exceed.: ",btest$actual.exceed,"   ",
                 "Test: ", btest$cc.Decision),
          side = 4, adj = 0, line = 0.5, cex = 0.9) # label
    
    ## Predictions
    t. <- length(X) + seq_len(m) # future time points
    lines(t., mu.predict, col = "blue") # predicted process X_t (or mu_t)
    lines(t., VaR.predict, col = "red") # predicted VaR_alpha
    lines(t., VaR.CI[1,], col = "orange") # lower 95%-CI for VaR_alpha
    lines(t., VaR.CI[2,], col = "orange") # upper 95%-CI for VaR_alpha
    legend("bottomright", bty = "n", lty = rep(1, 6), lwd = 1.6,
           col = c("black", adjustcolor("darkblue", alpha.f = 0.5), "blue",
                   "darkred", "red", "orange"),
           legend = c(expression(X[t]), expression(hat(mu)[t]),
                      expression("Predicted"~mu[t]~"(or"~X[t]*")"),
                      substitute(widehat(VaR)[a], list(a = alpha)),
                      substitute("Predicted"~VaR[a], list(a = alpha)),
                      substitute("95%-CI for"~VaR[a], list(a = alpha))))
    
    
    
  }
  
  #return(c(mu.,sig.,X.sim,VaR.))
  VaR.predict
  
} 
 
#* Non-Parametric Risk Measure [VaR and Expected Shortfall]
#* @param df The name of the dataframe 
#* @post /Var&ES
function(df) {
  print("saafe")
  df = dbReadTable(con, df)
  L = df$close
  VaR_np(L, level = 0.99)
  ES_np(L, level = 0.99)
}

#* Expected Shortfall
#* @param df the name of the Dataframe
#* @param level the confidence level
#* @post /Expected_Shortfall
function(df,level = 0.95){
  
  df = dbReadTable(con, df)
  L = df$close
  ES_t(level, loc = 0, scale = 1, df = Inf)
  ES_GPD(level, shape, scale)
  ES_Par(level, shape, scale = 1)
  ES_GPDtail(level, threshold, p.exceed, shape, scale)
  
  
}


#* Geometric Value At Risk
#* @param df The name of the DataFrame
#* @post  /Geometric_Var
function(X){
  
  N <- 17 # number of angles (rather small here because of run time)
  phi <- seq(0, 2*pi, length.out = N) # angles
  r <- 0.98 # radius
  alpha <- r * cbind(alpha1 = cos(phi), alpha2 = sin(phi)) # vector of confidence levels
  ## Compute geometric value-at-risk
  system.time(res <- gVaR(X, level = alpha))
  gvar <- t(sapply(seq_len(nrow(alpha)), function(i) {
    x <- res[[i]]
    if(x[["convergence"]] != 0) # 0 = 'converged'
      warning("No convergence for alpha = (", alpha[i,1], ", ", alpha[i,2],
              ") (row ", i, ")")
    x[["par"]]
  })) # (N, 2)-ma
}

#* Empirical Distribution
#* @param df name of the dataframe
#* @post /Empirical_Distribution 
function(df){
  df = dbReadTable(con, df)
  x = df$close
  edf_plot(x)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  return(as.numeric(a) + as.numeric(b))
}

#* Random Number Generator 
#*@get /random
function(c){
  as.numeric(c)
}

library(plumber)
# 'plumber.R' is the location of the file shown above
pr("C:\\Users\\HA\\Documents\\Hexertech\\Trading\\trading\\Trade\\Risk.R") %>%
  pr_run(port=8200)