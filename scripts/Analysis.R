### Analysis of data using poisson regression

# Import libraries
library(here)
library(MASS)

# Import data
df <- read.csv(here("data", "Eulaema.csv"))

# Create model
m <- glm(df$Eulaema_nigrita ~ df$Tseason + df$Pseason, family = "poisson")
m_nb <- glm.nb(df$Eulaema_nigrita ~ df$Tseason * df$Pseason)  # The data are overdispersed, use negative binomial model

# Plot
b <- m_nb$coefficients[1]  # Get y intercept
mT <- m_nb$coefficients[2]  # Get slope for Tseason
mP <-m_nb$coefficients[3]  # Get slope for Psearon
x_range <- seq(0, max(df$Eulaema_nigrita))  # Get evenly spaced range of counts
y_hat_Tseason <- exp(mT * x_range + b)  # Calculate outputs for Tseason model
# Calculate outputs for Pseason model
plot(df$Eulaema_nigrita, df$Tseason)  # Plot raw data
points(x_range, y_hat_Tseason, type = "l")  # Plot regression line
