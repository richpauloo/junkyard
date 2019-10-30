library(bayesTFR)
sim.dir <- file.path(find.package("bayesTFR"), "ex-data", "bayesTFR.output")
m <- get.tfr.mcmc(sim.dir)
readr::write_lines(
  sort(
    paste0(
      country.names(m), 
      ", ", 
      country.names(m)
    )
  ), 
  path = "~/Desktop/temp.txt"
)