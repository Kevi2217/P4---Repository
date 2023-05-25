library(quantmod)
library(lubridate)
library(dplyr)
library(MASS)
library(ggplot2)
library(ggthemes)

source("data_extract.R")

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
EF_matrix <- matrix(nrow = 21, ncol = 3)
one_vector <- c(rep(1, 25))
C25_list <- list()
TP_monthly_return <- numeric(nrow(ftdotm_dates) - 1)
betas <- list()

for (i in seq_along(C25_list_data)) {
  C25_list[[i]] <- data.frame(Date = index(C25_list_data[[i]]), C25_list_data[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates$Date) %>%
    dplyr::select(Date, 7) %>%
    dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
    dplyr::mutate(Decimal_return_squared = Decimal_return^2)

  ex_return[[i]] <- data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return * (1 / (nrow(ftdotm_dates)))) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    dplyr::pull(ex_return)
  
  ex_return_squared[[i]] <-  data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return^2 * (1 / (nrow(ftdotm_dates)))) %>%
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
MVP_weights <- as.numeric(ginv(one_vector %*% ginv(cov_matrix) %*% one_vector)) * as.vector(ginv(cov_matrix) %*% one_vector)


# Calculating the expected return and variance of the MVP
MVP_ex_return <- sum(mapply(`*`, MVP_weights, ex_return))
MVP_var <- t(MVP_weights) %*% cov_matrix %*% MVP_weights %>%
  `[`(1, 1) %>% 
  as.numeric()
MVP_sd <- sqrt(MVP_var)


### 3. Creating the tangency portfolio and CML #####################################################################
# We assume the risk free rate
risk_free_rate <- 0.01

# Calculating the weights of the tangency portfolio
v_ex_return <- as.numeric(ex_return)
# TP_weights <- as.vector(ginv(cov_matrix) %*% (v_ex_return - risk_free_rate * one_vector)) / as.numeric(one_vector %*% ginv(cov_matrix) %*% (v_ex_return - risk_free_rate * one_vector))
TP_weights <- as.vector(as.numeric(ginv(one_vector %*% ginv(cov_matrix) %*% (v_ex_return - risk_free_rate * one_vector))) * ginv(cov_matrix) %*% (v_ex_return - risk_free_rate * one_vector))
  
# Calculating the expected return and variance of the TP
TP_ex_return <- sum(mapply(`*`, TP_weights, ex_return))
TP_var <- t(TP_weights) %*% cov_matrix %*% TP_weights %>%
  `[`(1, 1) %>% 
  as.numeric()
TP_sd <- sqrt(TP_var)

# Creating the Efficient Portfolios (w_1 is the weight of the MVP and w_2 is the weigth of the TP)
# anden søjle er expected return og tredje søjle er standard deviation
for (i in -10:10) {
    w_1 <- i / 10
    w_2 <- (1-w_1)
    EF_matrix[i+11, 1] <- paste(w_1, "+", w_2)
    EF_matrix[i+11, 2] <- w_1*MVP_ex_return + w_2*TP_ex_return
    EF_matrix[i+11, 3] <- sqrt(t(w_1*MVP_weights + w_2*TP_weights) %*% cov_matrix %*% (w_1*MVP_weights + w_2*TP_weights) %>%
      `[`(1, 1) %>% 
      as.numeric())
}

EF_df <- as.data.frame(EF_matrix) %>%
  rename(weigths = V1, exreturn = V2, sd = V3) %>%
  mutate(exreturn = as.numeric(exreturn),
         sd = as.numeric(sd))

# Plotting the CML
CML <- ggplot(EF_df, aes(x = sd, y = exreturn)) + 
  geom_point(size = 0.8) +
  geom_abline(slope = ((TP_ex_return - risk_free_rate) / TP_sd), intercept = risk_free_rate, color = "orange") +
  xlim(0, max(EF_df$sd)) +
  ylim(0, max(EF_df$exreturn)) +
  xlab("\u03C3") +
  ylab("Expected Return") +
  annotate("text", x = EF_df[21, 3], y = EF_df[21, 2], label = "MVP", size = 2, color = "red", vjust = 1.6) +
  annotate("text", x = EF_df[11, 3], y = EF_df[11, 2], label = "TP", size = 2, color = "red", vjust = -1) +
  ggtitle("Capital Market Line")

### 4. Creating the SML #####################################################################
# 4.0 Calculating the market cap avg market protfolio
market_cap_avg <- c()
market_weights <- c()
market_monthly_return <- c()
outstanding_stocks <- c(234974389, 77873061, 131852496, 123657554, 198000000, 
                        862184621, 223939440, 219000000, 57650000, 65985932,
                        137193378, 10334436, 8372725, 185668226, 64272095,
                        3604803880, 50000000, 1717564000, 227256400, 420381080,
                        89000000, 50200000, 10753802, 634834980, 1009867260)

for (k in seq_along(C25_list)) {
  for (i in seq_along(C25_list)) {
    total_sum <- 0
    for (j in seq_along(nrow(ftdotm_dates) - 1)) {
      total_sum <- total_sum + (na.omit(C25_list[[i]])[j, 2] * outstanding_stocks[i]) / (nrow(ftdotm_dates) - 1)
    }
    market_cap_avg[i] <- total_sum
  }
  market_weights[k] <- market_cap_avg[k] / sum(market_cap_avg)
}
market_weights <- market_weights / sum(market_weights)

# Calculating market portfolio monthly return
for (i in seq_len(nrow(ftdotm_dates) - 1)) {
  total_sum <- 0
  for (j in seq_along(C25_list_data)) {
    total_sum <- total_sum + (na.omit(C25_list[[j]])[i, 3] * market_weights[j])
  }
  market_monthly_return[i] <- total_sum
}

market_ex_return <- sum(mapply(`*`, market_weights, ex_return))
market_var <- t(market_weights) %*% cov_matrix %*% market_weights %>%
  `[`(1, 1) %>% 
  as.numeric()
market_sd <- sqrt(market_var)


# 4.1 Calculating monthly porfolio return
for (i in seq_len(nrow(ftdotm_dates) - 1)) {
  total_sum <- 0
  for (j in seq_along(C25_list_data)) {
    total_sum <- total_sum + (na.omit(C25_list[[j]])[i, 3] * TP_weights[j])
  }
  TP_monthly_return[i] <- total_sum
}

# 4.2. We now find beta values
for (i in seq_along(C25_list)) {
  betas[i] <- cov(na.omit(C25_list[[i]][, 3]), market_monthly_return) / var(market_monthly_return)
}

# Binding betas and exreturn in a dataframe
sml_data <- data.frame(betas = unlist(betas), expected_returns = unlist(ex_return))

# CAPM/SML results show what expected value the stocks must have to following the market risk premium
CAPM_results <- risk_free_rate + sml_data$betas * (market_ex_return - risk_free_rate)

# 4.3. Plotting the SML model
colors <- factor(rainbow(25))
names(colors) <- C25_names
SML <- ggplot(sml_data, aes(x = betas, y = expected_returns, color = unlist(C25_names), shape = unlist(C25_names))) +
  geom_point() +
  geom_abline(intercept = risk_free_rate, slope = market_ex_return - risk_free_rate) +
  xlab("Beta") + ylab("Expected Return") +
  ggtitle("Security Market Line") +
  scale_color_manual(values = colors) + # use different colors for each asset
  scale_shape_manual(values = rep(c(15, 16, 17, 18, 3), 5)) + # use different shapes for each asset
  guides(color = guide_legend(title = "Assets"), shape = guide_legend(title = "Assets"))

### 5. Creating plotting normal distributions ########################################
# 5.1 Preprocessing of histogram
# Extracting data for larger period
C25_list_data_after <- get_data_after()
C25_list_after <- list()
ex_return_after <- list()
ex_return_squared_after <- list()
var_list_after <- list()
cov_matrix_after <- matrix(nrow = 25, ncol = 25)

# Creating monthly dates
 ftdotm_dates_after <- data.frame(Date = index(C25_list_data_after[[1]]), C25_list_data_after[[1]]) %>%
   dplyr::select(Date) %>%
   dplyr::group_by(year(Date), month(Date)) %>%
   dplyr::filter(row_number() == 1) %>%
   dplyr::ungroup() %>%
   dplyr::select(Date)

#finding C25_list_after like the main
 for (i in seq_along(C25_list_data_after)) {
   C25_list_after[[i]] <- data.frame(Date = index(C25_list_data_after[[i]]), C25_list_data_after[[i]]) %>%
     dplyr::filter(Date %in% ftdotm_dates_after$Date) %>%
     dplyr::select(Date, 7) %>%
     dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
     dplyr::mutate(Decimal_return_squared = Decimal_return^2)
   
   ex_return_after[[i]] <- data.frame(C25_list_after[[i]]) %>%
     dplyr::mutate(div_ex_return = Decimal_return * (1 / (nrow(ftdotm_dates_after)))) %>%
     na.omit() %>%
     dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
     dplyr::pull(ex_return)
   
   ex_return_squared_after[[i]] <-  data.frame(C25_list_after[[i]]) %>%
     dplyr::mutate(div_ex_return = Decimal_return^2 * (1 / (nrow(ftdotm_dates_after)))) %>%
     na.omit() %>%
     dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
     dplyr::pull(ex_return)
   
   var_list_after[[i]] <- ex_return_squared_after[[i]] - (ex_return_after[[i]]^2)
 }
 
# Covariance matrix for AFTER
for (i in seq_along(C25_list_after)) {
  for (j in seq_along(C25_list_after)) {
    cov_matrix_after[i, j] <- 1/nrow(ftdotm_dates_after) * sum(na.omit(C25_list_after[[i]])[, 3] * na.omit(C25_list_after[[j]])[, 3]) - ex_return_after[[i]]*ex_return_after[[j]]
  }
}
# Calculating the stocks' actual return in the period after and MAIN
C25_after_return <- list()
C25_after_actual_return <- c()

C25_after_return_main <- list()
C25_after_actual_return_main <- c()

for (i in seq_along(C25_list_data)) {
  C25_after_return_main[[i]] <- data.frame(Date = index(C25_list_data[[i]]), C25_list_data[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates$Date) %>%
    dplyr::select(7) %>%
    slice(c(1, n())) %>%
    dplyr::transmute(actual_return = (.[, 1] - lag(.[, 1]))/lag(.[, 1]))
  
  C25_after_actual_return_main <- c(C25_after_actual_return_main, C25_after_return_main[[i]][2, 1])
}
for (i in seq_along(C25_list_data)) {
   C25_after_return[[i]] <- data.frame(Date = index(C25_list_data_after[[i]]), C25_list_data_after[[i]]) %>%
     dplyr::filter(Date %in% ftdotm_dates_after$Date) %>%
     dplyr::select(7) %>%
     slice(c(1, n())) %>%
     dplyr::transmute(actual_return = (.[, 1] - lag(.[, 1]))/lag(.[, 1]))
   
   C25_after_actual_return <- c(C25_after_actual_return, C25_after_return[[i]][2, 1])
}
# Defining the sparinvest weights (DK0060442556)
sparinvest_weights <- c(0.0091, 0.0071, 0.0271, 0.051, 0.0637,
                            0.0469, 0.0111, 0.1514, 0.0065, 0.085,
                            0.0097, 0.0217, 0.0289, 0.0106, 0.0105,
                            0.0113, 0.0059, 0.1662, 0.0355, 0.0622,
                            0.0298, 0.0124, 0.0078, 0.0261, 0.1025)


# Calculating the actual actual return in the MAIN period
TP_actual_return_main <- sum(mapply(`*`, TP_weights, C25_after_actual_return_main))
#Dennes variance, er "TP_var"

sparinvest_actual_return_main <- sum(mapply(`*`, sparinvest_weights, C25_after_actual_return_main))
sparinvest_actual_var_main <- t(sparinvest_weights) %*% cov_matrix %*% sparinvest_weights %>%
  `[`(1, 1) %>% 
  as.numeric()

# Calculating the actual return AFTER of the TP and sparinvest's C25 index
TP_actual_return_after <- sum(mapply(`*`, TP_weights, C25_after_actual_return))
TP_var_after <- t(TP_weights) %*% cov_matrix_after %*% TP_weights %>%
  `[`(1, 1) %>% 
  as.numeric()

sparinvest_actual_return_after <- sum(mapply(`*`, sparinvest_weights, C25_after_actual_return))
sparinvest_actual_var_after <- t(sparinvest_weights) %*% cov_matrix_after %*% sparinvest_weights %>%
  `[`(1, 1) %>% 
  as.numeric()


# 5.2 Plotting histogram of returns for AMBU
# Making of histrogram
hist_vector <- as.numeric(na.omit(C25_list[[1]])$Decimal_return)



histogram_main <- hist(hist_vector, breaks = 10, col = "orange",
density = 99, xlab = "Monthly return ", ylim =c(0, 10) , main = "AMBU", border = "black")
curve(dnorm (x , mean = mean(hist_vector) ,sd= sd(hist_vector) ) ,col=" darkblue ", lwd =3,add=TRUE , yaxt ="n")



corr_matrix <- matrix(nrow = 25, ncol = 25)

# 6 Creating variance-covariance matrix
# Notice the nested for-loop take the covariance from the covariance matrix
for (i in seq_along(C25_list)) {
  for (j in seq_along(C25_list)) {
    corr_matrix[i, j] <- cov_matrix[i, j] / sqrt(var_list[[i]]*var_list[[j]])
  }
}


# Saving data and plots
write.csv(cbind(C25_list[[1]]$Date, round(C25_list[[1]][c("AMBU.B.CO.Adjusted", "Decimal_return", "Decimal_return_squared")], 4)), "AMBU_start_data.csv", row.names = FALSE)
write.csv(cbind(C25_names, round(as.numeric(ex_return), 4), round(as.numeric(var_list), 4)), "C25_exreturn_variance.csv", row.names = FALSE)
write.csv(as.data.frame(round(cov_matrix[1:5, 1:5], 5)), "cov_variance_matrix.csv", row.names = FALSE)
write.csv(cbind(C25_names, MVP_weights), "MVP_weights.csv", row.names = FALSE)
write.csv(cbind(C25_names, round(cbind(MVP_weights, TP_weights, market_weights, sparinvest_weights), 4)), "all_weights.csv", row.names = FALSE)
write.csv(cbind(C25_names, market_weights), "market_weights.csv", row.names = FALSE)
write.csv(cbind(EF_df$weigths, round(EF_df[c("exreturn", "sd")], 4)), "EF_df.csv", row.names = FALSE)
write.csv(cbind(betas), "beta_values.csv", row.names = FALSE)
ggsave(filename = "CML_model.png", plot = CML, width = 6, height = 4)
ggsave(filename = "SML_model.png", plot = SML, width = 6, height = 4)




