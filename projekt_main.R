library(quantmod)
library(lubridate)
library(dplyr)
library(MASS)
library(ggplot2)

source("data_extract.R")

# PLAN
# 1. Rens alle dataframes så du kun har closing price for hver måned i perioden. CHECK
# 2. Find return og variance for hver stock for hele perioden baseret på disse tal. CHECK
# 3. Udregn covariance-matrix ud fra dette. CHECK
# 4. Udregn MVP portfolio weights.CHECK
# 5. Eventuelt rens portfolio for meget små weights og rund op/ned (Mindst 2% før skalering). CHECK
# 6. Bruge denne disse weights til at udregne expected return for MVP portfolio og sammenlign
#    med den virkelige verdens OMXC25 index return for samme periode (1 år). CHECK

# UPDATED PLAN
# 1. Lav så datoerne automatisk assignes så første dag i måneden assignes for hver måned, for hele perioden. CHECK
# 2. Udregn MVP's expected return og variance. CHECK
# 3. Lav SML-linje.
#   DU ER NÅET TIL AT UDREGNE BETA. DU HAR PORTFØLJENS MÅNEDLIGE RETURN.
# 4. Lav CML-linje.
# 5. Kig på Correlation Coefficient Matrix og se på applications med distributions.


### EXTRACTING DATA ############################################################
C25_list_data <- get_data()

# Første trading-dag af hver måned.
ftdotm_dates <- data.frame(Date = index(C25_list_data[[1]]), C25_list_data[[1]]) %>%
  dplyr::select(Date) %>%
  dplyr::group_by(year(Date), month(Date)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date)


C25_list_after_data <- get_data_after()

# Første trading-dag af hver måned AFTER.
ftdotm_dates_after <- data.frame(Date = index(C25_list_after_data[[1]]), C25_list_after_data[[1]]) %>%
  dplyr::select(Date) %>%
  dplyr::group_by(year(Date), month(Date)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date)

### CALCULATING EXPECTED RETURN AND VARIANCE FOR LIST OF STOCKS##########################################
# Creating Lists and matrices.
ex_return <- list()
ex_return_squared <- list()
var_list <- list()
cov_matrix <- matrix(nrow = 25, ncol = 25)
one_vector <- c(rep(1, 25))
C25_after_return_list <- list()
C25_list <- list()
monthly_market_return <- numeric(nrow(ftdotm_dates) - 1)
C25_list_adjust <- list()
betas <- list()

for (i in seq_along(C25_list_data)) {
  C25_list[[i]] <- data.frame(Date = index(C25_list_data[[i]]), C25_list_data[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates$Date) %>%
    dplyr::select(Date, 5) %>%
    dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
    dplyr::mutate(Decimal_return_squared = Decimal_return^2)

  ex_return[[i]] <- data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return * (1 / nrow(ftdotm_dates))) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    dplyr::pull(ex_return)
  
  ex_return_squared[[i]] <-  data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return^2 * (1 / nrow(ftdotm_dates))) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    dplyr::pull(ex_return)
  
  var_list[[i]] <- ex_return_squared[[i]] - (ex_return[[i]]^2)
}
### Creating the SML #####################################################################
# We define the Sparinvest C25 Index weights
real_pf_weights <- list(0.0091, 0.0071, 0.0271, 0.051, 0.0637, 
                        0.0469, 0.0111, 0.1514, 0.0065, 0.085, 
                        0.0097, 0.0217, 0.0289, 0.0106, 0.0105, 
                        0.0113, 0.0059, 0.1662, 0.0355, 0.0622,
                        0.0298, 0.0124, 0.0078, 0.0261, 0.1025) %>%
  unlist()
# 1. First need to calculate the the expected return of the market pf
market_pf_ex_return <- sum(mapply(`*`, real_pf_weights, ex_return))

# 2. Now we need to find beta, which means that be have to find the monhtly return of the market pf (real_pf_weights)

for (i in seq_len(nrow(ftdotm_dates) - 1)) {
  total_sum <- 0
  for (j in seq_along(C25_list_data)) {
    C25_list_adjust[[j]] <- na.omit(C25_list[[j]])
    total_sum <- total_sum + (C25_list_adjust[[j]][i, 3] * real_pf_weights[j])
  }
  monthly_market_return[i] <- total_sum
}


# 3. We now find beta values
# We assume the risk free rate
risk_free_rate <- 0.02

for (i in seq_along(C25_list_data)) {
  # Extract the beta coefficient from the regression output
  betas[i] <- cov(na.omit(C25_list[[i]][, 3]), monthly_market_return) / var(monthly_market_return)
}

# 4. Plotting the SML model (DE LIGGER PÅ LINJEN VED AT E(R) KOMMER FRA CAPM-FORMLEN)
sml_data <- data.frame(betas = unlist(betas), expected_returns = unlist(ex_return))
# sml_data$expected_returns <- risk_free_rate + sml_data$betas * (market_pf_ex_return - risk_free_rate)

colors <- factor(rainbow(25))
names(colors) <- C25_names
ggplot(sml_data, aes(x = betas, y = expected_returns, color = unlist(C25_names), shape = unlist(C25_names))) + 
  geom_point() + 
  geom_abline(intercept = risk_free_rate, slope = market_pf_ex_return - risk_free_rate) + 
  xlab("Beta") + ylab("Expected Return") + 
  ggtitle("Security Market Line") +
  scale_color_manual(values = colors) + # use different colors for each asset
  scale_shape_manual(values = rep(c(15, 16, 17, 18, 3), 5)) + # use different shapes for each asset
  guides(color = guide_legend(title = "Assets"), shape = guide_legend(title = "Assets"))


### CALCULATING MVP WEIGHTS AND MVP EXPECTED RETURN/VARIANCE ################################################
# Filling cov-matrix entries 
for (i in seq_along(C25_list)) {
  for (j in seq_along(C25_list)) {
      cov_matrix[i, j] <- 1/nrow(ftdotm_dates) * sum(na.omit(C25_list[[i]])[, 3] * na.omit(C25_list[[j]])[, 3]) - ex_return[[i]]*ex_return[[j]]
}
}
# Calculating MVP weights
MVP_weights <- (ginv(cov_matrix) %*% one_vector) / diag(t(one_vector) %*% ginv(cov_matrix) %*% one_vector)
MVP_weights[MVP_weights < 0.02] <- 0
MVP_weights <- MVP_weights / sum(MVP_weights)
MVP_weights <- as.list(MVP_weights[, 1]) %>%
  unlist()


# Calculating the expected return and variance of the MVP
MVP_ex_return <- sum(mapply(`*`, MVP_weights, ex_return))
MVP_var <- t(MVP_weights) %*% cov_matrix %*% MVP_weights %>%
  `[`(1, 1) %>% 
  as.numeric()
MVP_sd <- sqrt(MVP_var)



### COMPARING MVP TO REAL DATA IN THE FOLLOWING YEAR ##################################
for (i in seq_along(C25_list_after_data)) {
  C25_after_return_list[[i]] <- data.frame(Date = index(C25_list_after_data[[i]]), C25_list_after_data[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates_after$Date) %>%
    dplyr::select(Date, 5) %>%
    dplyr::slice(c(1, n())) %>%
    dplyr::summarise(pct_return = (last(.[[2]]) - first(.[[2]])) / last(.[[2]])) %>%
    dplyr::pull(pct_return)
}


# Calculating the return of the MVP and the real pf for the following period.
actual_pf_return_after <- sum(mapply(`*`, real_pf_weights, C25_after_return_list))

actual_MVP_return_after <- sum(mapply(`*`, MVP_weights, C25_after_return_list))







