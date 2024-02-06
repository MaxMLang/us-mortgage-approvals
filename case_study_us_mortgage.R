# Libraries ----
set.seed(42)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(car)
library(rsq)
library(gbm)
library(boot)
library(xgboost)
library(margins)
library(ggeffects)
library(stargazer)
# Data read-in and data defintion ----
## Read and definition ----
mortg_raw <- read.csv("data/mortg.csv")
mortg <- read.csv("data/mortg.csv")
mortg$approved <- factor(mortg$approved,
                         levels = c("0", "1"),
                         labels = c("No", "Yes"))
mortg$mcs <- factor(mortg$mcs)
mortg$self <- factor(
  as.character(mortg$self),
  levels = c("1", "0"),
  labels = c("Yes", "No")
)
mortg$single <- factor(
  as.character(mortg$single),
  levels = c("1", "0"),
  labels = c("Yes", "No")
)
mortg$white <- factor(
  as.character(mortg$white),
  levels = c("1", "0"),
  labels = c("White", "Black")
)

##Scale ratios to percentage point scale for interpretability ----
mortg$hir <- mortg$hir * 100
mortg$odir <- mortg$odir * 100
mortg$lvr <- mortg$lvr * 100

# Exploratory Data Analysis ----
## Overview ----
summary(mortg)
table(mortg$approved, mortg$self)
table(mortg$approved, mortg$single)
table(mortg$approved, mortg$white)
prop.table(table(mortg$approved, mortg$self), 1)
prop.table(table(mortg$approved, mortg$single))
prop.table(table(mortg$approved, mortg$white), 2)
prop.table(table(mortg$white, mortg$self), 2)

## Bivarite Plots ----
### Boxplots (continous data) ----
create_vio_box_plot <-
  function(data,
           variable,
           title,
           xlab = "",
           limits = c(0, 100)) {
    ggplot(data, aes_string(x = variable, y = "approved")) +
      geom_violin(
        width = 1,
        color = "black",
        fill = "darkgrey",
        alpha = 0.2
      ) +
      geom_boxplot(width = 0.1,
                   outlier.size = 1.4,
                   outlier.shape = 1) +
      labs(title = title,
           x = xlab,
           y = "Mortgage Approval") +
      coord_flip() +
      theme_minimal(base_size = 15) +
      scale_x_continuous(labels = scales::percent_format(scale = 1),
                         limits = limits)
  }
create_scaled_stacked_bar_plot <-
  function(data,
           category,
           xlab = category,
           ylab = "Percentage",
           fill = c("#F05039", "#7CA1CC"),
           title =  paste("Mortgage Approval by", category, "Status")) {
    data %>%
      group_by(!!sym(category), approved) %>%
      summarise(Count = n()) %>%
      mutate(Total = sum(Count)) %>%
      mutate(Percent = (Count / Total) * 100) %>%
      ggplot(aes_string(x = category, 
                        y = "Percent", fill = "factor(approved)")) +
      geom_bar(stat = "identity", position = "fill") +
      labs(
        title = title,
        x = xlab,
        y = ylab,
        fill = "Approved"
      ) +
      scale_fill_manual(values = fill) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 15)
  }


plot_hir <-
  create_vio_box_plot(mortg,
                      "hir",
                      "HIR\nvs Mortgage Approval",
                      xlab = "Housing Expense-to-Income Ratio",
                      limits = c(0, 120))

plot_odir <-
  create_vio_box_plot(
    mortg,
    "odir",
    "ODIR\nvs Mortgage Approval",
    xlab = "Other Debt-to-Income Ratio",
    limits = c(0, 60)
  )
plot_lvr <-
  create_vio_box_plot(mortg,
                      "lvr",
                      "LVR\nvs Mortgage Approval",
                      xlab = "Loan-to-Value Ratio",
                      limits = c(0, 200))
plot_uria <-
  create_vio_box_plot(
    mortg,
    "uria",
    "URIA\nvs Mortgage Approval",
    xlab = "Unemployment Rate in Applicant's Industry",
    limits = c(0, 12)
  )
plot_self <-
  create_scaled_stacked_bar_plot(mortg,
                                 "self",
                                 xlab = "Self-Employed",
                                 ylab = "Proportion",
                       title = "Mortgage Approval by\nSelf-Employment Status")
plot_single <-
  create_scaled_stacked_bar_plot(mortg,
                                 "single",
                                 xlab = "Single",
                                 ylab = "Proportion",
                       title = "Mortgage Approval by\nMarital Status (Single)")
plot_white <-
  create_scaled_stacked_bar_plot(mortg,
                                 "white",
                                 xlab = "Ethnicity",
                                 ylab = "Proportion",
                                 title = "Mortgage Approval by\nEthnicity")
plot_mcs <-
  create_scaled_stacked_bar_plot(mortg,
                                 "mcs",
                                 xlab = "Mortgage Credit Score",
                                 ylab = "Proportion",
                         title = "Mortgage Approval by\nMortgage Credit Score")

grid.arrange(plot_hir, plot_odir, plot_lvr, plot_uria, ncol = 2)
grid.arrange(plot_self, plot_single, plot_white, plot_mcs, ncol = 2)

## Decile Plots ----
mortg_unscaled <- mortg_raw
plot_approval_rate_by_quantiles <-
  function(data,
     variable_name,
     xlab = paste(variable_name, "Deciles"),
     title = paste("Approval Rate by", toupper(variable_name), "Quantiles")) {
    # Compute the quantile bins for the variable
    data <- data %>%
      mutate(quantile_bin = findInterval(.data[[variable_name]],
                                         quantile(.data[[variable_name]], 
                                                  probs = seq(0.1, 1, 0.1)),
                                         left.open = TRUE) + 1)
    
    # Calculate the mean approval rate by quantile bin
    approval_rate <- data %>%
      group_by(quantile_bin) %>%
      summarise(approval_rate = mean(approved, na.rm = TRUE)) %>%
      ungroup()
    
    # Create the bar plot
    ggplot(approval_rate, aes(x = as.factor(quantile_bin), y = approval_rate)) +
      geom_bar(stat = "identity",
               colour = "grey",
               position = position_dodge()) +
      scale_x_discrete(limits = as.character(1:10), drop = FALSE) +
      labs(title = title,
           x = xlab,
           y = "Approval Rate") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 15)
  }



decile_approval_lvr <-
  plot_approval_rate_by_quantiles(mortg_raw, "lvr",
                              xlab = "Loan to value ratio Deciles")
decile_approval_hir <-
  plot_approval_rate_by_quantiles(mortg_raw, "hir",
                              xlab = "Housing Expenses to Income Ratio Deciles")
decile_approval_odir <-
  plot_approval_rate_by_quantiles(mortg_raw, "odir",
                              xlab = "Other Monthly Debt Ratio Deciles")
decile_approval_uria <-
  plot_approval_rate_by_quantiles(mortg_raw, "uria",
                              xlab = "Unemployment Rate Industry Deciles")

grid.arrange(
  decile_approval_lvr,
  decile_approval_hir,
  decile_approval_odir,
  decile_approval_uria,
  ncol = 2
)

# Modelling ----
## Set reference categories ----
mortg$mcs <-
  relevel(mortg$mcs, 2) # MCS 2 as reference category (rc)
mortg$self <- relevel(mortg$self, 2) # Not self-employed as rc
mortg$single <- relevel(mortg$single, 1) # Single Yes as rc
mortg$white <- relevel(mortg$white, 1) # white as rc

## Variable selection stepwise AIC ----
full_model <-
  glm(approved ~ . * self,
      family = binomial(link = "logit"),
      data = mortg)
summary(full_model)
full_model_summary <- summary(full_model)
reduced_model <- step(full_model, direction = "both")
summary(reduced_model)
reduced_model_summary <- summary(reduced_model)
## Model selection Analysis of Deviance ----
### LRT ----
D_full <- full_model$deviance
D_reduced <- reduced_model$deviance
p_full <- length(full_model$coefficients)
p_reduced <-
  length(reduced_model$coefficients)  # number of parameters in reduced model
deviance_change <-  D_reduced - D_full
pchisq(deviance_change,
       df = p_full - p_reduced,
       lower.tail = FALSE)
### via ANOVA function ----
anova(reduced_model, full_model, test = "Chisq")

# Model Diagnosis ----
# Cooks Distance ----
plot_cooks_distance <-
  function(model,
           model_name,
           alpha = 0.1,
           threshold = NULL) {
    # Calculate Cook's distance
    cooks_d <- cooks.distance(model)
    
    # If threshold is not set, use the default value
    if (is.null(threshold)) {
      threshold <- 4 / length(cooks_d)
    }
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Observation = 1:length(cooks_d),
      CooksDistance = cooks_d,
      AboveThreshold = cooks_d > threshold
    )
    
    # Create the plot using ggplot2
    ggplot(plot_data, aes(x = Observation, y = CooksDistance)) +
      geom_point(aes(color = AboveThreshold), alpha = alpha) + 
      geom_point(
        data = subset(plot_data, AboveThreshold),
        aes(color = AboveThreshold),
        alpha = 1
      ) + # Points above threshold with full opacity
      scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                         guide = FALSE) +
      ggtitle(paste("Cook's Distance for", model_name)) + # Title
      xlab('Observation') + # X-axis label
      ylab("Cook's Distance") + # Y-axis label
      geom_text(
        data = subset(plot_data, AboveThreshold),
        aes(label = Observation),
        vjust = -0.5,
        hjust = "inward",
        size = 3,
        angle = 45
      ) + # Label high points
      geom_hline(yintercept = threshold,
                 linetype = 'dotted',
                 col = 'black') +
      geom_hline(
        yintercept = 4 / length(cooks_d),
        linetype = 'dotted',
        col = 'pink'
      )
  }

plot_cooks_distance_barplot <-
  function(model,
           model_name,
           threshold = 4 / length(cooks.distance(model))) {
    # Calculate Cook's distance
    cooks_d <- cooks.distance(model)
    
    # Create a data frame for plotting
    plot_data <- data.frame(Observation = 1:length(cooks_d),
                            CooksDistance = cooks_d)
    
    # Create the bar plot using ggplot2
    ggplot(plot_data, aes(x = Observation, y = CooksDistance)) +
      geom_bar(stat = "identity",
               aes(fill = CooksDistance > threshold),
               width = 5) +
      scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
      ggtitle(paste("Cook's Distances for selected model")) +
      xlab("Observation") +
      ylab("Cook's Distance") +
      geom_text(
        data = subset(plot_data, CooksDistance > threshold),
        aes(label = Observation),
        vjust = -0.5,
        size = 3,
        angle = 45
      ) +
      geom_hline(aes(yintercept = threshold, linetype = "Soft threshold"),
                 color = 'black') +
      geom_hline(aes(
        yintercept = 8 / (length(cooks_d) - 2 * length(coef(reduced_model))),
        linetype = "Hard threshold"
      ),
      color = 'black') +
      scale_linetype_manual(
        name = "Threshold",
        values = c("Soft threshold" = "twodash", "Hard threshold" = "solid"),
        breaks = c("Soft threshold", "Hard threshold"),
        labels = c("Soft Threshold", "Hard Threshold (8/(n-2p))")
      ) +
      theme_minimal(base_size = 25) +
      theme(legend.position = "bottom")
  }

plot_cooks_distance(reduced_model, "Reduced", 0.1, 0.02)
plot_cooks_distance_barplot(reduced_model, "Reduced", 0.02)
cooks_d <- cooks.distance(reduced_model)


# Calibration Curve ----
marginalModelPlot(reduced_model,
                ylab = "Approved",
                cex.lab = 15)
title(main = "Predicted Probability of Mortgage Approval vs. Linear Predictor")
# Model results ----
# Model table ----
summary(reduced_model)

# Confidence Intervals ----
confint.default(reduced_model)
exp(confint.default(reduced_model))
## Multiple Confidence Intervals -----
calculate_interac_confint <-
  function(model,
           var1,
           var2 = "selfYes",
           sig.level = 0.05) {
    # Extract variance-covariance matrix from the model
    coefs_var <- vcov(model)
    # Extract the coefficients
    coefs <- coef(model)
    # Calculate the z-value based on the significance level
    z <- qnorm(1 - sig.level / 2)
    
    # Calculate the standard error for the interaction term
    # considering the covariance
    se <-
      sqrt(coefs_var[var1, var1] + coefs_var[var2, var2] 
           + 2 * coefs_var[var1, var2])
    ci_lower <- coefs[var1] + coefs[var2] - z * se
    ci_upper <- coefs[var1] + coefs[var2] + z * se
    # Return the confidence interval
    
    return(list(
      "confint" = c(lower = ci_lower, upper = ci_upper),
      "se" = se
    ))
  }

calculate_interac_pv <- function(model, var1, var2) {
  # Extract variance-covariance matrix from the model
  coefs_var <- vcov(model)
  # Extract the coefficients
  coefs <- coef(model)
  
  # Calculate the standard error for the interaction term
  # considering the covariance
  se <-
    sqrt(coefs_var[var1, var1] + coefs_var[var2, var2] 
         + 2 * coefs_var[var1, var2])
  
  p <- 2 * (1 - pnorm(abs((coefs[var1] + coefs[var2]) / se)))
  names(p) <- paste(var1, var2, sep = "+")
  # Return the confidence interval
  return(p)
}

### odir:selfYes ----
round(sum(coef(reduced_model)[c("odir", "odir:selfYes")]), 3)
calculate_interac_confint(
  model = reduced_model,
  var1 = "odir",
  var2 = "odir:selfYes",
  sig.level = 0.05
)
calculate_interac_pv(model = reduced_model, 
                     var1 = "odir", var2 = "odir:selfYes")
### selfYes:whiteBlack ----
round(sum(coef(reduced_model)[c("whiteBlack", "selfYes:whiteBlack")]), 3)
calculate_interac_confint(
  model = reduced_model,
  var1 = "whiteBlack",
  var2 = "selfYes:whiteBlack",
  sig.level = 0.05
)
calculate_interac_pv(model = reduced_model, 
                     var1 = "whiteBlack", var2 = "selfYes:whiteBlack")
### selfYes:uria ----
round(sum(coef(reduced_model)[c("uria", "selfYes:uria")]), 3)
calculate_interac_confint(
  model = reduced_model,
  var1 = "uria",
  var2 = "selfYes:uria",
  sig.level = 0.05
)
calculate_interac_pv(model = reduced_model, 
                     var1 = "uria", var2 = "selfYes:uria")



# Marginal Effects ----
marginal_effects <-
  margins(
    reduced_model,
    vce = "delta",
    vcov = vcov(reduced_model),
    type = "link"
  )

## Summary of marginal effects ----
summary(marginal_effects) %>% as.data.frame()


marginal_effects_df <- summary(marginal_effects) %>% as.data.frame()

marginal_effects_df <- marginal_effects_df %>%
  mutate(AME_exp = exp(AME))



marginal_effects_df <- marginal_effects_df %>%
  mutate(p = 2 * (1 - pnorm(abs(z))),
         p_rounded = round(2 * (1 - pnorm(abs(
           z
         ))), 4))

## AME Plot ----
ggplot(marginal_effects_df, aes(x = factor, y = AME)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +  # Flip coordinates to have factors on the y-axis
  labs(title = "Average Marginal Effects (AME) with Confidence Intervals",
       x = "Average Marginal Effect",
       y = "Factor") +
  theme_minimal()


## Effects on Prob Scale----
### Single effects ----
meff_self <- ggpredict(reduced_model, terms = c("self")) |>
  plot() +
  ylim(c(0, 1))
meff_white <- ggpredict(reduced_model, terms = c("white")) |>
  plot() +
  ylim(c(0, 1))
meff_single <- ggpredict(reduced_model, terms = c("single")) |>
  plot() +
  ylim(c(0, 1))
meff_mcs <- ggpredict(reduced_model, terms = c("mcs")) |>
  plot() +
  ylim(c(0, 1))
meff_odir <- ggpredict(reduced_model, terms = c("odir[all]")) |>
  plot() +
  ylim(c(0, 1))
meff_lvr <- ggpredict(reduced_model, terms = c("lvr[all]")) |>
  plot() +
  ylim(c(0, 1))
meff_hir <- ggpredict(reduced_model, terms = c("hir[all]")) |>
  plot() +
  ylim(c(0, 1))
meff_uria <- ggpredict(reduced_model, terms = c("uria[all]")) |>
  plot() +
  ylim(c(0, 1))

grid.arrange(
  meff_odir,
  meff_lvr,
  meff_hir,
  meff_uria,
  meff_self,
  meff_white,
  meff_single,
  meff_mcs,
  ncol = 4
)

## Interaction Plots ----
meff_odir_self <-
  ggeffect(reduced_model, terms = c("odir[all]", "self")) |>
  plot() +
  ylim(c(0, 1)) +
  geom_line() +
  xlab("ODIR") +
  ylab("Probability of Approval") +
  labs(colour = "Self-employment\nstatus") +
  ggtitle("Probability of Approval\nfor ODIR by\nSelf-Employment status") +
  theme_minimal(base_size = 15)


meff_uria_self <-
  ggeffect(reduced_model, terms = c("uria[all]", "self")) |>
  plot() +
  ylim(c(0, 1)) +
  xlab("URIA") +
  ylab("Probability of Approval") +
  labs(colour = "Self-employment\nstatus") +
  ggtitle("Probability of Approval\nfor URIA by\nSelf-Employment status") +
  theme_minimal(base_size = 15)


meff_white_self <-
  ggeffect(reduced_model, terms = c("white[all]", "self")) |>
  plot() +
  ylim(c(0, 1)) +
  xlab("Ethnicity") +
  ylab("Probability of Approval") +
  labs(colour = "Self-employment\nstatus") +
  ggtitle("Probability of Approval\nfor Ethnicity by\nSelf-Employment status") +
  theme_minimal(base_size = 15)

grid.arrange(meff_odir_self, meff_uria_self, meff_white_self, ncol = 3)

# Dispersion Parameter ----
E2 <- resid(reduced_model, type = "pearson")
N  <- nrow(mortg)
p  <- length(coef(reduced_model))
sum(E2 ^ 2) / (N - p)

check_overdispersion <- function(logit_model) {
  residual_df <- df.residual(logit_model)
  pearson_resid <- residuals(logit_model, type = "pearson")
  chi_squared <- sum(pearson_resid ^ 2)
  dispersion_ratio <- chi_squared / residual_df
  p_value <-
    pchisq(chi_squared, df = residual_df, lower.tail = FALSE)
  c(
    chi_sq = chi_squared,
    disp_ratio = dispersion_ratio,
    res_df = residual_df,
    p_val = p_value
  )
}

round(check_overdispersion(reduced_model), 5)

#Appendix ---
## Oversampling ----
# Splitting the data into majority and minority

minority_data <- mortg[mortg$approved == "No", ]
majority_data <- mortg[mortg$approved == "Yes", ]

# Oversampling minority class
oversampled_minority <-
  minority_data[sample(nrow(minority_data), nrow(majority_data), 
                       replace = TRUE), ]

# Combine back with majority class
balanced_data <- rbind(majority_data, oversampled_minority)
balanced_full_model <-
  glm(approved ~ . * self,
      family = binomial(link = 'logit'),
      data = balanced_data)
summary(balanced_full_model)
balanced_reduced_model <-
  glm(reduced_model$formula,
      family = binomial(link = 'logit'),
      data = balanced_data)
summary(balanced_reduced_model)

## Lasso Regression ----
model_lasso <- glmnet::glmnet(
  x = model.matrix( ~ . * self, data = mortg[, -1]),
  y = model.frame(mortg) |> model.response(),
  alpha = 1,
  family = "binomial"
)

lasso_cv <- glmnet::cv.glmnet(
  x = model.matrix( ~ . * self, data = mortg[, -1]),
  y = model.frame(mortg) |> model.response(),
  alpha = 1,
  nfolds = 20,
  family = "binomial"
)
plot(lasso_cv)
lasso_coef <-
  coef(model_lasso, s = lasso_cv$lambda.1se , digits = 0.3)
lasso_coef

lasso_coef <- coef(model_lasso, s = lasso_cv$lambda.1se)

coef_df <- as.data.frame(Matrix::as.matrix(lasso_coef))
names(coef_df) <- c("Coefficient")
coef_df$Feature <- row.names(coef_df)
coef_df <- coef_df[-c(1:2),]  # remove the intercept row

# Filter out zero coefficients for a cleaner plot
coef_df <- coef_df[coef_df$Coefficient != 0,]
row.names(coef_df) <- NULL

ggplot(coef_df, aes(x = Feature, y = Coefficient)) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  geom_point() +
  coord_flip() +  # Flip the axes to make it horizontal
  labs(x = "Features", y = "LASSO Coefficients", 
       title = "Non-zero LASSO Coefficients at lambda.1se") +
  theme_minimal()

## Weighted Logistic Regression ----
weights <- ifelse(
  mortg$approved == 1,
  nrow(mortg) / sum(mortg$approved == "Yes"),
  nrow(mortg) / sum(mortg$approved == "No")
)

weighted_full_model <-
  glm(
    approved ~ . * self,
    family = binomial(link = "logit"),
    data = mortg,
    weights = weights
  )
summary(weighted_full_model)
weighted_reduced_model <-
  glm(
    approved ~ hir + odir + lvr + mcs + self + single +
      odir * self + self * white + self * uria,
    family = binomial(link = "logit"),
    data = mortg,
    weights = weights
  )
summary(weighted_reduced_model)
