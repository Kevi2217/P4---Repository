library(quantmod)
library(lubridate)
library(dplyr)


# PLAN
# 1. Rens alle dataframes så du kun har closing price for hver måned i perioden. CHECK
# 2. Find return og variance for hver stock for hele perioden baseret på disse tal. CHECK
# 3. Udregn covariance-matrix ud fra dette. CHECK
# 4. Udregn MVP portfolio weights.
# 5. Eventuelt rens portfolio for meget små weights og rund op/ned.
# 6. Bruge denne disse weights til at udregne expected return for MVP portfolio og sammenlign
#    med den virkelige verdens OMXC25 index return for samme periode (1 år).


get_data <- function() {
C25_list <- list("AMBU-B.CO", "BAVA.CO", "CHR.CO", "CARL-B.CO", "COLO-B.CO",
                "DANSKE.CO", "DEMANT.CO", "DSV.CO", "FLS.CO", "GMAB.CO",
                "GN.CO", "MAERSK-A.CO", "MAERSK-B.CO", "ISS.CO", "JYSK.CO",
                "NDA-DK.CO", "NETC.CO", "NOVO-B.CO", "NZYM-B.CO", "ORSTED.CO",
                "PNDORA.CO", "RBREW.CO", "ROCK-B.CO", "TRYG.CO", "VWS.CO")

as.data.frame(getSymbols(paste(C25_list), src = "yahoo", auto.assign = TRUE, from = "2019-12-01", to = "2021-01-20"))

C25_list <<- list(`AMBU-B.CO`, `BAVA.CO`, `CHR.CO`, `CARL-B.CO`, `COLO-B.CO`,
                 `DANSKE.CO`, `DEMANT.CO`, `DSV.CO`, `FLS.CO`, `GMAB.CO`,
                 `GN.CO`, `MAERSK-A.CO`, `MAERSK-B.CO`, `ISS.CO`, `JYSK.CO`,
                 `NDA-DK.CO`, `NETC.CO`, `NOVO-B.CO`, `NZYM-B.CO`, `ORSTED.CO`,
                 `PNDORA.CO`, `RBREW.CO`, `ROCK-B.CO`, `TRYG.CO`, `VWS.CO`)
}
get_data()


# Første trading-dag af hver måned.
ftdotm_dates <- as.Date(c("2019-12-02", "2020-01-02", "2020-02-03", "2020-03-02", "2020-04-01", "2020-05-01", "2020-06-02", "2020-07-01"
           , "2020-08-03", "2020-09-01", "2020-10-01", "2020-11-02", "2020-12-01"))

# as.data.frame(getSymbols("AMBU-B.CO", src = "yahoo", auto.assign = TRUE, from = "2019-12-01", to = "2021-01-20"))
# `AMBU-B.CO` <- data.frame(Date = index(`AMBU-B.CO`), `AMBU-B.CO`)
# `AMBU-B.CO` <- `AMBU-B.CO` %>%
#   dplyr::filter(Date %in% ftdotm_dates) %>%
#   dplyr::select(Date, 5) %>%
#   dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
#   dplyr::mutate(Decimal_return_squared = Decimal_return^2)

# Creating Lists and matrices.
ex_return <- list()
ex_return_squared <- list()
var_list <- list()
cov_matrix <- matrix(nrow = 25, ncol = 25)

for (i in seq_along(C25_list)) {
  C25_list[[i]] <- data.frame(Date = index(C25_list[[i]]), C25_list[[i]]) %>%
    dplyr::filter(Date %in% ftdotm_dates) %>%
    dplyr::select(Date, 5) %>%
    dplyr::mutate(Decimal_return = (.[, 2] - lag(.[, 2]))/lag(.[, 2])) %>%
    dplyr::mutate(Decimal_return_squared = Decimal_return^2)

  ex_return[[i]] <- data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return * 1/12) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    pull(ex_return)
  
  ex_return_squared[[i]] <-  data.frame(C25_list[[i]]) %>%
    dplyr::mutate(div_ex_return = Decimal_return^2 * 1/12) %>%
    na.omit() %>%
    dplyr::summarize(ex_return = sum(div_ex_return), na.rm = TRUE) %>%
    pull(ex_return)
  
  var_list[[i]] <- ex_return_squared[[i]] - (ex_return[[i]]^2)
}

for (i in seq_along(C25_list)) {
  for (j in seq_along(C25_list)) {
      cov_matrix[i, j] <- 1/12 * sum(na.omit(C25_list[[i]])[, 3] * na.omit(C25_list[[j]])[, 3]) - ex_return[[i]]*ex_return[[j]]
}
}















