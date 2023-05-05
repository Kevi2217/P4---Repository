library(quantmod)
library(lubridate)
library(dplyr)
library(MASS)

source("data_extract.R")

# PLAN
# 1. Rens alle dataframes så du kun har closing price for hver måned i perioden. CHECK
# 2. Find return og variance for hver stock for hele perioden baseret på disse tal. CHECK
# 3. Udregn covariance-matrix ud fra dette. CHECK
# 4. Udregn MVP portfolio weights.CHECK
# 5. Eventuelt rens portfolio for meget små weights og rund op/ned (Mindst 1%). CHECK
# 6. Bruge denne disse weights til at udregne expected return for MVP portfolio og sammenlign
#    med den virkelige verdens OMXC25 index return for samme periode (1 år). CHECK

# UPDATED PLAN
# 1. Lav så datoerne automatisk assignes så første dag i måneden assignes for hver måned, for hele perioden. CHECK
# 2. Udregn MVP's expected return og variance.
# 3. Lav CML-linje.
# 4. Lav SML-linje.
# 5. Kig på Correlation Coefficient Matrix og se på applications med distributions.

C25_list <- get_data()

# Første trading-dag af hver måned.
ftdotm_dates <- data.frame(Date = index(C25_list[[1]]), C25_list[[1]]) %>%
  dplyr::select(Date) %>%
  dplyr::group_by(year(Date), month(Date)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date)


C25_list_after <- get_data_after()

# Første trading-dag af hver måned AFTER.
ftdotm_dates_after <- data.frame(Date = index(C25_list_after[[1]]), C25_list_after[[1]]) %>%
  dplyr::select(Date) %>%
  dplyr::group_by(year(Date), month(Date)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date)


# Creating Lists and matrices.
ex_return <- list()
ex_return_squared <- list()
var_list <- list()
cov_matrix <- matrix(nrow = 25, ncol = 25)
one_vector <- c(rep(1, 25))
C25_after_return_list <- list()

for (i in seq_along(C25_list)) {
  C25_list[[i]] <- data.frame(Date = index(C25_list[[i]]), C25_list[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates$Date) %>%
    dplyr::select(Date, 5) %>%
    dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
    dplyr::mutate(Decimal_return_squared = Decimal_return^2)

  ex_return[[i]] <- data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return * (1 / nrow(ftdotm_dates))) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    pull(ex_return)
  
  ex_return_squared[[i]] <-  data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return^2 * 1 / nrow(ftdotm_dates)) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    pull(ex_return)
  
  var_list[[i]] <- ex_return_squared[[i]] - (ex_return[[i]]^2)
}

# Filling cov-matrix entries ###################################################
for (i in seq_along(C25_list)) {
  for (j in seq_along(C25_list)) {
      cov_matrix[i, j] <- 1/12 * sum(na.omit(C25_list[[i]])[, 3] * na.omit(C25_list[[j]])[, 3]) - ex_return[[i]]*ex_return[[j]]
}
}
# Calculating MVP weights ######################################################
MVP_weights <- (ginv(cov_matrix) %*% one_vector) / diag(t(one_vector) %*% ginv(cov_matrix) %*% one_vector)
MVP_weights[MVP_weights < 0.01] <- 0
MVP_weights <- MVP_weights / sum(MVP_weights)
MVP_weights <- as.list(MVP_weights[, 1])




# COMPARE - Comparing MVP with real weights of index. ############
for (i in seq_along(C25_list_after)) {
  C25_after_return_list[[i]] <- data.frame(Date = index(C25_list_after[[i]]), C25_list_after[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates_after$Date) %>%
    dplyr::select(Date, 5) %>%
    dplyr::slice(c(1, n())) %>%
    dplyr::summarise(pct_return = (last(.[[2]]) - first(.[[2]])) / last(.[[2]])) %>%
    dplyr::pull(pct_return)
}


real_portfolio_weights <- list(0.0091, 0.0071, 0.0271, 0.051, 0.0637, 
                               0.0469, 0.0111, 0.1514, 0.0065, 0.085, 
                               0.0097, 0.0217, 0.0289, 0.0106, 0.0105, 
                               0.0113, 0.0059, 0.1662, 0.0355, 0.0622,
                               0.0298, 0.0124, 0.0078, 0.0261, 0.1025)

real_portfolio_return_after <- sum(mapply(`*`, real_portfolio_weights, C25_after_return_list))

MVP_return_after <- sum(mapply(`*`, MVP_weights, C25_after_return_list))






