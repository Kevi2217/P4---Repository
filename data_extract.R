# DATA EXTRACT 2020-2021 ##################################################
get_data <- function() {
  C25_list <- list("AMBU-B.CO", "BAVA.CO", "CHR.CO", "CARL-B.CO", "COLO-B.CO",
                   "DANSKE.CO", "DEMANT.CO", "DSV.CO", "FLS.CO", "GMAB.CO",
                   "GN.CO", "MAERSK-A.CO", "MAERSK-B.CO", "ISS.CO", "JYSK.CO",
                   "NDA-DK.CO", "NETC.CO", "NOVO-B.CO", "NZYM-B.CO", "ORSTED.CO",
                   "PNDORA.CO", "RBREW.CO", "ROCK-B.CO", "TRYG.CO", "VWS.CO")
  
as.data.frame(getSymbols(paste(C25_list), src = "yahoo", auto.assign = TRUE, from = "2019-12-01", to = "2021-12-05"))
  
  C25_list <- list(`AMBU-B.CO`, `BAVA.CO`, `CHR.CO`, `CARL-B.CO`, `COLO-B.CO`,
                    `DANSKE.CO`, `DEMANT.CO`, `DSV.CO`, `FLS.CO`, `GMAB.CO`,
                    `GN.CO`, `MAERSK-A.CO`, `MAERSK-B.CO`, `ISS.CO`, `JYSK.CO`,
                    `NDA-DK.CO`, `NETC.CO`, `NOVO-B.CO`, `NZYM-B.CO`, `ORSTED.CO`,
                    `PNDORA.CO`, `RBREW.CO`, `ROCK-B.CO`, `TRYG.CO`, `VWS.CO`)
return(C25_list)
}



# DATA EXTRACT 2021-2022 ##################################################
get_data_after <- function() {
  C25_list <- list("AMBU-B.CO", "BAVA.CO", "CHR.CO", "CARL-B.CO", "COLO-B.CO",
                         "DANSKE.CO", "DEMANT.CO", "DSV.CO", "FLS.CO", "GMAB.CO",
                         "GN.CO", "MAERSK-A.CO", "MAERSK-B.CO", "ISS.CO", "JYSK.CO",
                         "NDA-DK.CO", "NETC.CO", "NOVO-B.CO", "NZYM-B.CO", "ORSTED.CO",
                         "PNDORA.CO", "RBREW.CO", "ROCK-B.CO", "TRYG.CO", "VWS.CO")
  
  as.data.frame(getSymbols(paste(C25_list), src = "yahoo", auto.assign = TRUE, from = "2023-01-01", to = "2023-05-10"))
  
  C25_list_after <- list(`AMBU-B.CO`, `BAVA.CO`, `CHR.CO`, `CARL-B.CO`, `COLO-B.CO`,
                          `DANSKE.CO`, `DEMANT.CO`, `DSV.CO`, `FLS.CO`, `GMAB.CO`,
                          `GN.CO`, `MAERSK-A.CO`, `MAERSK-B.CO`, `ISS.CO`, `JYSK.CO`,
                          `NDA-DK.CO`, `NETC.CO`, `NOVO-B.CO`, `NZYM-B.CO`, `ORSTED.CO`,
                          `PNDORA.CO`, `RBREW.CO`, `ROCK-B.CO`, `TRYG.CO`, `VWS.CO`)
  return(C25_list_after)
}

C25_names <- list("AMBU", "BAVA", "CHR", "CARL-B", "COLO-B",
                  "DANSKE", "DEMANT", "DSV", "FLS", "GMAB",
                  "GN", "MAERSK-A", "MAERSK-B", "ISS", "JYSK",
                  "NDA", "NETC", "NOVO-B", "NZYM-B", "ORSTED",
                  "PNDORA", "RBREW", "ROCK-B", "TRYG", "VWS")