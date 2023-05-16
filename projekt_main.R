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
EF_matrix <- matrix(nrow = 21, ncol = 3)
one_vector <- c(rep(1, 25))
C25_list <- list()
market_monthly_return <- numeric(nrow(ftdotm_dates) - 1)
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
############################## INDSAT FRA SML ##############################################################
# 4.1. We define our market_portfolio (Sparinvest Index)
market_cap_avg <- c()
market_weights <- c()
outstanding_stocks <- c(234974389, 77873061, 131852496, 123657554, 198000000, 
                        862184621, 223939440, 219000000, 57650000, 65985932,
                        137193378, 10334436, 8372725, 185668226, 64272095,
                        3604803880, 50000000, 1717564000, 227256400, 420381080,
                        89000000, 50200000, 10753802, 634834980, 1009867260)
# "AMBU-B.CO", "BAVA.CO", "CHR.CO", "CARL-B.CO", "COLO-B.CO",
# "DANSKE.CO", "DEMANT.CO", "DSV.CO", "FLS.CO", "GMAB.CO",
# "GN.CO", "MAERSK-A.CO", "MAERSK-B.CO", "ISS.CO", "JYSK.CO",
# "NDA-DK.CO", "NETC.CO", "NOVO-B.CO", "NZYM-B.CO", "ORSTED.CO",
# "PNDORA.CO", "RBREW.CO", "ROCK-B.CO", "TRYG.CO", "VWS.CO")

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

# 4.2. Now we need to find beta, which means that be have to find the monhthly return of the market-portfolio
############################################################################################################
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
    print(w_1)
    print(w_2)
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
ggplot(EF_df, aes(x = sd, y = exreturn)) + 
  geom_point(size = 0.8) +
  geom_abline(slope = ((TP_ex_return - risk_free_rate) / TP_sd), intercept = risk_free_rate, color = "orange") +
  geom_abline(slope = ((market_ex_return - risk_free_rate) / market_sd), intercept = risk_free_rate, color = "lightblue") +
  xlim(0, market_sd) +
  ylim(0, max(EF_df$exreturn)) +
  xlab("Standard Deviation") +
  ylab("Expected Return") +
  annotate("text", x = EF_df[21, 3], y = EF_df[21, 2], label = "MVP", size = 2, color = "red", vjust = 1.6) +
  annotate("text", x = EF_df[11, 3], y = EF_df[11, 2], label = "TP", size = 2, color = "red", vjust = -1) +
  annotate("point", x = market_sd, y = market_ex_return, shape = 16, size = 3, color = "blue") +
  annotate("text", x = market_sd, y = market_ex_return, label = "Market Portfolio", size = 2, color = "blue", vjust = -1.5, hjust = 0.9) +
  ggtitle("Capital Market Line")

### 4. Creating the SML #####################################################################

# 4.3. We now find beta values
for (i in seq_along(C25_list)) {
  betas[i] <- cov(na.omit(C25_list[[i]][, 3]), market_monthly_return) / var(market_monthly_return)
}

# 4.4. Plotting the SML model (DE LIGGER PÅ LINJEN VED AT E[R] KOMMER FRA CAPM-FORMLEN)
sml_data <- data.frame(betas = unlist(betas), expected_returns = unlist(ex_return))
# CAPM results show what expected value the stocks must have to following the market risk premium
CAPM_result <- risk_free_rate + sml_data$betas * (market_ex_return - risk_free_rate)

colors <- factor(rainbow(25))
names(colors) <- C25_names
ggplot(sml_data, aes(x = betas, y = expected_returns, color = unlist(C25_names), shape = unlist(C25_names))) +
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
# C25_list_data_after <- get_data_after()
# 
# # Creating monthly dates
# ftdotm_dates_after <- data.frame(Date = index(C25_list_data_after[[1]]), C25_list_data_after[[1]]) %>%
#   dplyr::select(Date) %>%
#   dplyr::group_by(year(Date), month(Date)) %>%
#   dplyr::filter(row_number() == 1) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(Date)

# Calculating monthly returns

# C25_list_after <- list()
# 
# for (i in seq_along(C25_list_data)) {
#   C25_list_after[[i]] <- data.frame(Date = index(C25_list_data_after[[i]]), C25_list_data_after[[i]]) %>%
#     dplyr::filter(Date %in% ftdotm_dates_after$Date) %>%
#     dplyr::select(Date, 5) %>%
#     dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
#     dplyr::mutate(Decimal_return_squared = Decimal_return^2)
# }

# 5.2 Plotting histogram of returns for AMBU
# METODE 1
# h <- hist(na.omit(C25_list_after[[1]])$Decimal_return, breaks=10, col="red", xlab="Expected Value", main="dette er en titel")
# xfit <- seq(min(na.omit(C25_list_after[[1]])$Decimal_return), max(na.omit(C25_list_after[[1]])$Decimal_return, length=40))
# yfit <- dnorm(xfit, mean=mean(na.omit(C25_list_after[[1]])$Decimal_return), sd=sd(na.omit(C25_list_after[[1]])$Decimal_return))
# yfit <- yfit*diff(h$mids[1:2])*length(na.omit(C25_list_after[[1]])$Decimal_return)
# lines(xfit, yfit, col="blue", lwd=2)

# METODE 2
# ggplot(na.omit(C25_list_after[[1]]), aes(x = Decimal_return)) +
#   geom_histogram(bins = 20, fill = "orange", alpha = 0.8) +
#   xlab("Expected Value") +
#   ylab("Frequency") +
  # ggtitle("Histogram of Expected Values")

# METODE 3-4
# ggplot(na.omit(C25_list_after[[1]]), aes(x = Decimal_return)) +
#   geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black",) +
#   labs(x = "Expected Value", y = "Density", title = "Distribution of Values")
# hist (na.omit(C25_list_after[[1]])$Decimal_return, breaks =25, prob =TRUE , col = "orange", density = 60,
#               xlab ="Expected value ", ylim =c(0, 4) , main = "")



write.csv(cbind(C25_list[[1]]$Date, round(C25_list[[1]][c("AMBU.B.CO.Adjusted", "Decimal_return", "Decimal_return_squared")], 4)), "AMBU_start_data.csv", row.names = FALSE)

write.csv(cbind(C25_names, ex_return, var_list), "C25_exreturn_variance.csv", row.names = FALSE)

write.csv(as.data.frame(round(cov_matrix[1:5, 1:5], 5)), "cov_variance_matrix.csv", row.names = FALSE)

write.csv(cbind(MVP_weights), "MVP_weights.csv", row.names = FALSE)

write.csv(cbind(TP_weights), "TP_weights.csv", row.names = FALSE)

write.csv(cbind(EF_df$weigths, round(EF_df[c("exreturn", "sd")], 4)), "EF_df.csv", row.names = FALSE)

write.csv(cbind(betas), "beta_values.csv", row.names = FALSE)



# market_weights <- list(0.0091, 0.0071, 0.0271, 0.051, 0.0637, 
#                        0.0469, 0.0111, 0.1514, 0.0065, 0.085, 
#                        0.0097, 0.0217, 0.0289, 0.0106, 0.0105, 
#                        0.0113, 0.0059, 0.1662, 0.0355, 0.0622,
#                        0.0298, 0.0124, 0.0078, 0.0261, 0.1025) %>%
#   unlist()





