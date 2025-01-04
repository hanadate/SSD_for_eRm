power_simularion_mpi <- 
  function (model, data, simvar, fixed_effects, critical_value, 
          steps, n_sim, confidence_level, safeguard = F, rnorm = F, 
          R2 = F, R2var, R2level, nCores, chunkSize) 
{
  depvar <- get_depvar(model)
  cl <- doMPI::startMPIcluster(count=nCores-1)
  doMPI::registerDoMPI(cl)
  print("Simulation running on:")
  print(cl)
  if (safeguard == T) {
    model_for_simulation <- prepare_safeguard_model(model, 
                                                    confidence_level, critical_value)
  }
  else {
    model_for_simulation <- model
  }
  n_row <- length(lme4::fixef(model)[-1])
  n_col <- length(steps)
  row_names <- row.names(summary(model)$coefficients)[-1]
  power_values_all <- data.frame(matrix(ncol = n_col, nrow = n_row), 
                                 row.names = row_names)
  names(power_values_all) <- steps
  index_n <- 0
  for (n in steps) {
    print("Estimating power for step:")
    print(n)
    index_n <- index_n + 1
    `%dopar%` <- foreach::`%dopar%`
    store_simulations <- suppressWarnings(foreach::foreach(iterators::icount(n_sim), 
                                                           .combine = "cbind", .export = ls(envir = globalenv()), 
                                                           .packages = c("lme4"), .errorhandling = "remove",
                                                           .options.mpi = list(chunkSize=chunkSize)) %dopar% 
                                            {
                                              if (rnorm == T) {
                                                model_for_simulation <- prepare_rnorm_model(model, 
                                                                                            data, simvar, critical_value)
                                              }
                                              simulated_data <- simulateDataset(n_want = n, 
                                                                                data = data, model = model_for_simulation, 
                                                                                simvar = simvar, fixed_effects = fixed_effects)
                                              final_dataset <- reset_contrasts(simulated_data, 
                                                                               data, model, fixed_effects)
                                              model_final <- update(model, data = final_dataset)
                                              if (R2 == T) {
                                                model_final@beta <- model_for_simulation@beta
                                                sim_data2 <- simulateDataset(n_want = R2level, 
                                                                             final_dataset, model_final, simvar = R2var, 
                                                                             fixed_effects = fixed_effects, use_u = T)
                                                sim_data2 <- reset_contrasts(sim_data2, data, 
                                                                             model, fixed_effects)
                                                model_R2_final <- update(model, data = sim_data2)
                                              }
                                              if (R2 == F) {
                                                to.store_simulations <- check_significance(model_final, 
                                                                                           critical_value)
                                              }
                                              else {
                                                to.store_simulations <- check_significance(model_R2_final, 
                                                                                           critical_value)
                                              }
                                            })
    print(paste("Simulations for step ", n, " are based on ", 
                length(store_simulations)/n_row, " successful single runs"))
    power_values_n <- apply(store_simulations, MARGIN = 1, 
                            FUN = mean, na.rm = T)
    column_name <- as.character(n)
    power_values_all[column_name] <- power_values_n
  }
  doMPI::closeCluster(cl)
  power_values_all
}