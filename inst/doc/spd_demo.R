## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE,message=FALSE----------------------------------------------
library(tsdistributions)
library(data.table)
library(knitr)
# simulate data
set.seed(200)
n <- 6000
sim <- rghst(n, mu = 0, sigma = 1, skew = -0.6, shape = 8.01)
spec <- spd_modelspec(sim, lower = 0.01, upper = 0.99, kernel_type = 'normal')
mod <- estimate(spec, method = "pwm")
print(summary(mod))

## ----fig.height=6,fig.width=7-------------------------------------------------
lower_treshold <- mod$gpd$lower$threshold
upper_treshold <- mod$gpd$upper$threshold
oldpar <- par(mfrow = c(1,1))
par(mfrow = c(3,1), mar = c(3,3,3,3))
hist(sim, breaks = 300, probability = TRUE, xlab = "r", 
     col = "steelblue", border = "whitesmoke",main = "PDF")
box()
abline(v = lower_treshold, col = 'mediumpurple3', lty = 2)
abline(v = upper_treshold, col = 'mediumpurple3', lty = 2)
lines(sort(sim), dspd(sort(sim), mod), col = "darkorange",lwd = 2)

plot(sort(sim), (1:length(sim)/length(sim)), 
     ylim = c(0, 1), pch = 19, 
     cex = 0.5, ylab = "p", xlab = "q", main = "CDF")
grid()
lines(sort(sim), pspd(sort(sim), mod), col = "darkorange",lwd = 2)

abline(v = lower_treshold, col = 'mediumpurple3', lty = 2)
abline(v = upper_treshold, col = 'mediumpurple3', lty = 2)

plot(sort(sim), qspd(ppoints(length(sim)), mod), ylab = "Model Simulated", xlab = "Observed", main = "Q-Q")
abline(0, 1, col = "darkorange")
grid()
abline(v = lower_treshold, col = 'mediumpurple3', lty = 2)
abline(v = upper_treshold, col = 'mediumpurple3', lty = 2)
abline(h = lower_treshold, col = 'mediumpurple3', lty = 2)
abline(h = upper_treshold, col = 'mediumpurple3', lty = 2)
par(oldpar)

## -----------------------------------------------------------------------------
f <- function(x) x * dspd(x, mod)
mu <- integrate(f, -50, 50)$value
f <- function(x) x^2 * dspd(x, mod)
variance <- integrate(f, -50, 50)$value
f <- function(x) (x - mu)^3 * dspd(x, mod)
skewness <- integrate(f, -50, 50)$value/(variance^(3/2))
f <- function(x) (x - mu)^4 * dspd(x, mod)
kurtosis <- integrate(f, -50, 50)$value/(variance^2)
value_at_risk <- qspd(0.01, mod)
f <- function(x) x * dspd(x, mod)
expected_shortfall <- integrate(f, -50, value_at_risk)$value/0.01

tab <- data.table(stat = c("mean","variance","skewness","kurtosis","VaR(1%)","ES(1%)"),
                  spd = c(mu, variance, skewness, kurtosis, value_at_risk, expected_shortfall),
                  observed = c(mean(sim), var(sim), mean((sim - mean(sim))^3)/(sd(sim)^3), 
                               mean((sim - mean(sim))^4)/(sd(sim)^4),quantile(sim, 0.01),
                               mean(sim[sim <= quantile(sim, 0.01)])))
kable(tab, digits = 2)

