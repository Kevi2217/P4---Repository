library(quantmod)
library(lubridate)
library(dplyr)
library(MASS)
library(ggplot2)

source("data_extract.R")

# UPDATED PLAN
# 1. Lav så datoerne automatisk assignes så første dag i måneden assignes for hver måned, for hele perioden. CHECK
# 2. Udregn MVP's expected return og variance. CHECK
# 3. Lav SML-linje.
#   DU ER NÅET TIL AT UDREGNE BETA. DU HAR PORTFØLJENS MÅNEDLIGE RETURN.
# 4. Lav CML-linje.
# 5. Kig på Correlation Coefficient Matrix og se på applications med distributions.


### 0. EXTRACTING DATA ############################################################
C25_list_data <- get_data()

# Første trading-dag af hver måned.
ftdotm_dates <- data.frame(Date = index(C25_list_data[[1]]), C25_list_data[[1]]) %>%
  dplyr::select(Date) %>%
  dplyr::group_by(year(Date), month(Date)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date)


### 1. CALCULATING EXPECTED RETURN AND VARIANCE FOR LIST OF STOCKS##########################################
# Creating Lists and matrices.
ex_return <- list()
ex_return_squared <- list()
var_list <- list()
cov_matrix <- matrix(nrow = 25, ncol = 25)
EF_matrix <- matrix(nrow = 11, ncol = 3)
one_vector <- c(rep(1, 25))
C25_list <- list()
TP_monthly_return <- numeric(nrow(ftdotm_dates) - 1)
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


### 2. CALCULATING MVP WEIGHTS AND MVP EXPECTED RETURN/VARIANCE ################################################
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


### 3. Creating the tangency portfolio and CML #####################################################################
# We assume the risk free rate
risk_free_rate <- 0.02

# Calculating the weights of the tangency portfolio (and removing shorting and especially small weights)
v_ex_return <- as.numeric(ex_return)
TP_weights <- ginv(cov_matrix) %*% (v_ex_return - risk_free_rate) / sum(ginv(cov_matrix) %*% (v_ex_return - risk_free_rate))
TP_weights[TP_weights < 0.02] <- 0
TP_weights <- TP_weights / sum(TP_weights)
TP_weights <- as.list(TP_weights[, 1]) %>%
  unlist()

# Calculating the expected return and variance of the TP
TP_ex_return <- sum(mapply(`*`, TP_weights, ex_return))
TP_var <- t(TP_weights) %*% cov_matrix %*% TP_weights %>%
  `[`(1, 1) %>% 
  as.numeric()
TP_sd <- sqrt(TP_var)

# Creating the Efficient Portfolios (w_1 is the weight of the MVP and w_2 is the weigth of the TP)
# anden søjle er expected return og tredje søjle er standard deviation
for (i in 0:10) {
    w_1 <- i / 10
    w_2 <- (1-w_1)
    EF_matrix[i+1, 1] <- paste(w_1, "+", w_2)
    EF_matrix[i+1, 2] <- w_1*MVP_ex_return + w_2*TP_ex_return
    EF_matrix[i+1, 3] <- sqrt(t(w_1*MVP_weights + w_2*TP_weights) %*% cov_matrix %*% (w_1*MVP_weights + w_2*TP_weights) %>%
      `[`(1, 1) %>% 
      as.numeric())
}
EF_df <- as.data.frame(EF_matrix) %>%
  rename(weigths = V1, exreturn = V2, sd = V3) %>%
  mutate(exreturn = as.numeric(exreturn),
         sd = as.numeric(sd))

# Plotting the CML
ggplot(EF_df, aes(x = sd, y = exreturn)) + 
  geom_point(size = 0.8) +
  geom_abline(slope = ((TP_ex_return - risk_free_rate) / TP_sd), intercept = risk_free_rate, color = "orange") +
  xlim(0, max(EF_df$sd) + 0.001) +
  ylim(0, max(EF_df$exreturn) + 0.01) +
  xlab("Standard Deviation") +
  ylab("Expected Return") +
  annotate("text", x = EF_df$sd[which.min(EF_df$exreturn)], y = EF_df$exreturn[which.min(EF_df$exreturn)], label = "MVP", size = 2, color = "red", vjust = 1.6) +
  annotate("text", x = EF_df$sd[which.max(EF_df$exreturn)], y = EF_df$exreturn[which.max(EF_df$exreturn)], label = "TP", size = 2, color = "red", vjust = -1) +
  ggtitle("Capital Market Line")

### 4. Creating the SML #####################################################################
# 4.1. Now we need to find beta, which means that be have to find the monhthly return of the tangency-portfolio
for (i in seq_len(nrow(ftdotm_dates) - 1)) {
  total_sum <- 0
  for (j in seq_along(C25_list_data)) {
    C25_list_adjust[[j]] <- na.omit(C25_list[[j]])
    total_sum <- total_sum + (C25_list_adjust[[j]][i, 3] * TP_weights[j])
  }
  TP_monthly_return[i] <- total_sum
}

# 4.2. We now find beta values
for (i in seq_along(C25_list)) {
  betas[i] <- cov(na.omit(C25_list[[i]][, 3]), TP_monthly_return) / var(TP_monthly_return)
}

# 4.3. Plotting the SML model (DE LIGGER PÅ LINJEN VED AT E(R) KOMMER FRA CAPM-FORMLEN)
sml_data <- data.frame(betas = unlist(betas), expected_returns = unlist(ex_return))
sml_data$expected_returns <- risk_free_rate + sml_data$betas * (TP_ex_return - risk_free_rate)

colors <- factor(rainbow(25))
names(colors) <- C25_names
ggplot(sml_data, aes(x = betas, y = expected_returns, color = unlist(C25_names), shape = unlist(C25_names))) +
  geom_point() +
  geom_abline(intercept = risk_free_rate, slope = TP_ex_return - risk_free_rate) +
  xlab("Beta") + ylab("Expected Return") +
  ggtitle("Security Market Line") +
  scale_color_manual(values = colors) + # use different colors for each asset
  scale_shape_manual(values = rep(c(15, 16, 17, 18, 3), 5)) + # use different shapes for each asset
  guides(color = guide_legend(title = "Assets"), shape = guide_legend(title = "Assets"))




