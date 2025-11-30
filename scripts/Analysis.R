### Analysis of data using poisson regression
## This file can be run line by line or all at once.
## I formatted my tables manually in the Latex document using the csv and txt files generated in this script
## I saved the plot manually in my IDE rather than coding a filewrite.


## Import libraries
library(here)
library(MASS)


## Import data
df <- read.csv(here("data", "Eulaema.csv"))


## Initial tests and model fitting
cat(c(head(
    unique(df$Eulaema_nigrita), 10), tail(unique(df$Eulaema_nigrita), 10
)))  # Show top and bottom 10 unique entries

hist(df$Eulaema_nigrita)  # Plot histogram of response variable

lm <- lm(
    df$Eulaema_nigrita
    ~ df$effort
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
)  # Fit simple linear model for all numerical predictors
hist(lm$residuals)  # Show distribution of residuals
par(mfrow = c(2, 2))
plot(lm)

plm <- glm(
    df$Eulaema_nigrita
    ~ df$effort
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
)  # Fit multiple poisson regression model
cat(plm$deviance / plm$df.residual)  # Check if data exhibit overdispersion

nblm <- glm.nb(
    df$Eulaema_nigrita ~
    df$effort
    + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
)  # Fit multiple negative binomial regression to the data
summary(nblm)  # Check summary

# Compare AIC to see if the negative binomial model is problematic
cat(paste0(
  "Poisson AIC: ", round(plm$aic), "\n", "Neg. binom AIC: ", round(nblm$aic)
))

# Drop MAT, seeing that its effect has a much larger p-value than the rest of the variables
nblm <- glm.nb(
    df$Eulaema_nigrita ~
    df$effort
    # + df$MAT
    + df$MAP
    + df$Tseason
    + df$Pseason
)
par(mfrow = c(2, 2))
plot(nblm)  # Check that residuals are normally distributed


## Multicollinearity check
vars <- list(
    df$effort, df$MAP, df$Tseason, df$Pseason
)  # Create list of variables
names(vars) <- c(
    "effort", "MAP", "Tseason", "Pseason"
)  # Assign names to list of variables

# Print multicollinearity check
for (i in seq_along(vars)) {  # Loop through first variable
  for (j in seq_along(vars)) {  # Loop through second variable  
    if (i > j) {  # Ensure predictor combinations are evaluated once
      x <- lm(vars[[i]]~vars[[j]])  # Assign model to placeholder x
      vif <- 1 / (1 - summary(x)$r.squared)  # Assign VIF to quantify collinearity
      cat(paste0(
        "VIF ", names(vars)[i], "~", names(vars)[j], ": ", round(vif, 3), "\n"
      ))  # Return VIF to the console
    }
  }
}


## Produce summary table with parameter estimates, etc.
tbl <- summary(nblm)$coefficients  # Assign coefficients table to tbl
tbl[,1] <- exp(tbl[,1])  # Backtransform parameter estimates
colnames(tbl)[1] <- "Backtransformed estimate"  # Rename estimate column
write.csv(signif(tbl, 3), here("outputs", "coefs.csv"))  # Write table
sink(here("outputs", "summary.txt")); summary(nblm); sink()  # Write model summary to txt


## Produce plots
coefs <- nblm$coefficients  # Assign model coefficients to coefs
icp <- coefs[1]  # Assign intercept to icp
slopes <- coefs[-1]  # Assign slopes to slopes
letters <- c("A", "B", "C", "D")  # Assign figure letters to letters 
par(
  mfrow = c(2,2), mar = c(4.4,4.5,1.5,0.5), oma = c(0,0,0,0)
)  # Build subplot grid with custom margins
means <- vars  # Make a copy of vars used above in which to store predictor values
for (i in 1:4) {
  means[[i]] <- rep(mean(vars[[i]]), length(vars[[i]]))  # Replace distinct variable values with the group mean
}
names(vars) <- c(
    "Sampling effort (log h)", "MAP (mm)", "Tseason (°C)", "Pseason"
)  # Update names of vars list for plotting

for (i in 1:4) {  # Generate plots iteratively
  # Generate predictions
  v <- vars[[i]]  # Assign unique variable values to v
  means_vars <- means  # Create a copy of means list to update for each plot
  x <- seq(
    min(v), max(v), length.out = length(v)
  )  # Generate vector of evenly spaced x intervals to predict the output
  means_vars[[i]] <- x  # Insert the evenly spaced inputs into means_vars
  y_hat <- exp(
    icp
    + slopes[1] * means_vars[[1]]
    + slopes[2] * means_vars[[2]]
    + slopes[3] * means_vars[[3]]
    + slopes[4] * means_vars[[4]]
  )  # Get predictions, holding every variable besides the one being plotted constant at its mean
  
  # Plot
  if (i %% 2 == 1) {  # Plot with y label if the current plot is on the left side of the figure
    plot(
      v, df$Eulaema_nigrita, xlab = names(vars)[i],
      ylab = expression(paste(italic("E. nigrita")," abundance")),
      col = rgb(0, 0, 0, alpha = 0.15), pch = 16
    )
  } else {  # Plot without y label if the current plot is on the left side of the figure
    plot(
      v, df$Eulaema_nigrita, xlab = names(vars)[i], ylab = "",
      col = rgb(0, 0, 0, alpha = 0.15), pch = 16
    )
  }
  points(x, y_hat, type = "l", col = "red")  # Plot regression line
  mtext(letters[i], side = 3, adj = 0, line = 0.5, font = 2)  # Plot figure subplot letters
}
