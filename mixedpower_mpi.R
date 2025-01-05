mixedpower_mpi<- 
  function (model, data, fixed_effects, simvar, steps, critical_value, 
            n_sim = 1000, SESOI = F, databased = T, maxCores = hosts_num(hosts="hosts"),
            chunkSize=floor(n_sim/(2*foreach::getDoParWorkers()))) 
  {
    R2 <- F
    data <- mixedpower:::check_input(model, data, fixed_effects, simvar, steps, 
                        critical_value, n_sim, SESOI, R2, R2var, R2level)
    confidence_level <- 0.68
    output <- list()
    i <- 1
    nCores <- maxCores
    
    if (databased == T) {
      databased_power_values <- power_simulation_mpi(model, data, 
                                                 simvar, fixed_effects, critical_value, steps, n_sim, 
                                                 confidence_level, safeguard = F, rnorm = F, R2 = F, 
                                                 R2var = 0, R2level = 0, nCores)
      databased_power_values["mode"] <- "databased"
      databased_power_values["effect"] <- row.names(databased_power_values)
      output[[i]] <- databased_power_values
      i <- i + 1
    }
    suppressWarnings(if (!is.logical(SESOI)) {
      model@beta <- SESOI
      SESOI_power_values <- power_simulation_mpi(model, data, simvar, 
                                             fixed_effects, critical_value, steps, n_sim, confidence_level, 
                                             safeguard = F, rnorm = F, R2 = F, R2var = 0, R2level = 0, 
                                             nCores)
      SESOI_power_values["mode"] <- "SESOI"
      SESOI_power_values["effect"] <- row.names(SESOI_power_values)
      output[[i]] <- SESOI_power_values
    })
    results <- data.frame()
    for (ii in 1:length(output)) {
      results <- rbind(results, output[[ii]])
    }
    save(results, file = "output_powersismulation.Rda")
    results
  }