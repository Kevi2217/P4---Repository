library(quantmod)
library(lubridate)
library(dplyr)

C25_list <- c("AMBU-B.CO", "BAVA.CO", "CHR.CO", "CARL-B.CO", "COLO-B.CO",
                "DANSKE.CO", "DEMANT.CO", "DSV.CO", "FLS.CO", "GMAB.CO",
                "GN.CO", "MAERSK-A.CO", "MAERSK-B.CO", "ISS.CO", "JYSK.CO",
                "NDA-DK.CO", "NETC.CO", "NOVO-B.CO", "NZYM-B.CO", "ORSTED.CO",
                "PNDORA.CO", "RBREW.CO", "ROCK-B.CO", "TRYG.CO", "VWS.CO")

as.data.frame(getSymbols(paste(C25_list), src = "yahoo", auto.assign = TRUE, from = "2020-01-01", to = "2021-01-20"))

# C25 <- getSymbols("^OMXC25", src = "yahoo", auto.assign = FALSE, from = "2020-01-01", to = "2021-01-01")

# PLAN
# 1. Rens alle dataframes så du kun har closing price for hver måned i perioden.
# 2. Find return og variance for hver stock for hele perioden baseret på disse tal.
# 3. Udregn covariance-matrix ud fra dette.
# 4. Udregn MVP portfolio weights.
# 5. Eventuelt rens portfolio for meget små weights og rund op/ned.
# 6. Bruge denne disse weights til at udregne expected return for MVP portfolio og sammenlign
#    med den virkelige verdens OMXC25 index return for samme periode (1 år).

C25_list_final <- list()

# Første trading-dag af hver måned.
dates <- c("2020-01-02", "2020-02-03", "2020-03-02", "2020-04-01", "2020-05-01", "2020-06-02", "2020-07-01"
           , "2020-08-03", "2020-09-01", "2020-10-01", "2020-11-02", "2020-12-01")




for (i in seq_along(C25_list)) {

  current_df <- C25_list[[i]]
  current_df <- as.data.frame(current_df)
  
  # All the dataframes are filtered for their first trading day of each month  
  current_df$date <- rownames(current_df)
  
  C25_list_final[[i]] <- current_df 
}












