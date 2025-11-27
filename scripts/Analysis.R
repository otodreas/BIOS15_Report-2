### Analysis of data using poisson regression

# Import libraries
library(here)
library(MASS)

# Import data
df <- read.csv(here("data", "Eulaema.csv"))

# Initial tests
c(head(unique(df$Eulaema_nigrita), 10), tail(unique(df$Eulaema_nigrita), 10))  # Show top and bottom 10 unique entries
hist(df$Eulaema_nigrita)  # Plot histogram of response variable
lm <- lm(
    df$Eulaema_nigrita
    ~ df$effort
    + df$altitude
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
    + df$forest.
    + df$lu_het
)  # Fit simple linear model for all numerical predictors
hist(lm$residuals)  # Show distribution of residuals

plm <- glm(
    df$Eulaema_nigrita
    ~ df$effort
    + df$altitude
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
    + df$forest.
    + df$lu_het
)  # Fit multiple poisson regression model
summary(plm)  # Check if data exhibit overdispersion

nblm <- glm.nb(
    df$Eulaema_nigrita
    ~ df$effort
    # + df$altitude
    # + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
    + df$forest.
    # + df$lu_het
)  # Fit multiple negative binomial regression to the data
# Check summary

# Drop terms that I do not expect have an important relationship to the response

# Check that residuals are normally distributed

# Run a multicollinearity check, drop correlated variables
vars <- list(
    df$effort, df$MAP, df$Tseason, df$Pseason, df$forest.
)  # Create list of variables
# par(mfrow = rep(length(vars), 2))  # Create plot grid
for (v_x in vars) {  # Loop through variables
  for (v_y in vars) {
    if (i > j) {
      
    } plot(vars[[i]], vars[[j]]) 
  }
}
# Plot each unique pair of variables

# Produce a table with parameter estimates, etc.

  # Produce plot(s)