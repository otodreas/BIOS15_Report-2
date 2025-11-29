### Analysis of data using poisson regression


## Import libraries
library(here)
library(MASS)


## Import data
df <- read.csv(here("data", "Eulaema.csv"))


## Initial tests and model fitting
c(head(
    unique(df$Eulaema_nigrita), 10), tail(unique(df$Eulaema_nigrita), 10
))  # Show top and bottom 10 unique entries

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
plm$deviance / plm$df.residual  # Check if data exhibit overdispersion

nblm <- glm.nb(
    df$Eulaema_nigrita ~
    df$effort
    + df$altitude
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
    + df$forest.
    + df$lu_het
)  # Fit multiple negative binomial regression to the data
summary(nblm)  # Check summary

# Compare AIC to see if the negative binomial model is problematic
print(paste0("Poisson AIC: ", round(plm$aic))); print(paste0("Neg. binom AIC: ", round(nblm$aic)))

nblm <- glm.nb(
    df$Eulaema_nigrita ~
    df$effort
    + df$altitude
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
    + df$forest.
    # + df$lu_het
)  # Drop terms that I do not expect have an important relationship to the response
plot(nblm)  # Check that residuals are normally distributed

# Note to self: e^slope for each parameter represents the % change in response for one step (+1) in the predictor
# You can't just plot one line for one set of vars (y=mx+b) you need to do the whole inear function (all the different "mx" terms). keep the other x's at their mean if you really want to.

## Multicollinearity check
vars <- list(
    df$effort, df$MAP, df$Tseason, df$Pseason, df$forest.
)  # Create list of variables
names(vars) <- c(
    "effort", "MAP", "Tseason", "Pseason", "forest."
)  # Assign names to list of variables

for (i in seq_along(vars)) {  # Loop through first variable
  for (j in seq_along(vars)) {  # Loop through second variable  
    if (i > j) {  # Ensure predictor combinations are evaluated once
      x <- lm(vars[[i]]~vars[[j]])  # Assign model to placeholder x
      vif <- 1 / (1 - summary(x)$r.squared)  # Assign VIF to quantify collinearity
      cat(paste0(
        "VIF ", names(vars)[i], "~", names(vars)[j], ": ", round(vif, 1), "\n"
      ))  # Return VIF to the console
    }
  }
}


## Produce summary table with parameter estimates, etc.


## Produce plots
