# happiness model

fit_z_ev_rpe <- function(data) {
  
  data <- data %>% arrange(userKey,TrialNumber)
  
  # Initialize lists to store matrices per participant
  evmtx_chosen_list <- list()
  rpemtx_chosen_list <- list()
  
  # Loop through each participant
  participants <- unique(data$userKey)
  
  for (participant in participants) {
    sub_data <- filter(data, userKey == !!participant)
    happyind <- which(!is.na(sub_data$zHappy))
    zhappy <- sub_data$zHappy[happyind]
    
    # Initialize matrix for participant
    mtx <- matrix(0, nrow = length(zhappy), ncol = nrow(sub_data))
    
    evmtx_chosen <- mtx
    rpemtx_chosen <- mtx
    
    # Loop over the happiness ratings
    for (m in 1:length(happyind)) {
      t <- sub_data[1:happyind[m], ]  # Subset all trials up to the happiness rating
      
      # Calculate EV and RPE for the chosen option
      tempev_chosen <- ifelse(t$Choice == 1, 
                              (t$SafeProb * t$SafeValue),   # Safe choice
                              (t$RiskyProb * t$RiskyValue)) # Risky choice
      
      temprpe_chosen <- t$Outcome - tempev_chosen  # RPE based on choice
      
      evmtx_chosen[m, 1:length(tempev_chosen)] <- rev(tempev_chosen)
      rpemtx_chosen[m, 1:length(temprpe_chosen)] <- rev(temprpe_chosen)
    }
    
    # Store matrices for each participant
    evmtx_chosen_list[[participant]] <- evmtx_chosen
    rpemtx_chosen_list[[participant]] <- rpemtx_chosen
  }
  
  result <- list(
    zhappy = zhappy,
    evmtx_chosen_list = evmtx_chosen_list,
    rpemtx_chosen_list = rpemtx_chosen_list,
    plays = data$TrialNumber
  )
  
  # Optimization settings
  result$options <- list(maxit = 1000, reltol = 1e-5)
  result$inx <- c(0, 0, 0.5, 0)  # ev, rpe, tau, const
  result$lb <- c(-100, -100, 0.2, -2)
  result$ub <- c(100, 100, 0.8, 2)
  result$blabel <- c("ev_chosen", "rpe_chosen", "tau", "const")
  
  # Fit model for each participant
  fitted_results <- lapply(1:length(participants), function(i) {
    fit <- optim(
      par = result$inx, 
      fn = function(x) happymodel_long(x, list(
        zhappy = result$zhappy,
        evmtx_chosen = evmtx_chosen_list[[i]],
        rpemtx_chosen = rpemtx_chosen_list[[i]]
      ))$sse,  # Only return SSE to optimize
      method = "L-BFGS-B", 
      lower = result$lb, 
      upper = result$ub, 
      control = result$options
    )
    
    sse_happypred_r2 <- happymodel_long(fit$par, list(
      zhappy = result$zhappy,
      evmtx_chosen = evmtx_chosen_list[[i]],
      rpemtx_chosen = rpemtx_chosen_list[[i]]
    ))
    
    return(list(b = fit$par, happypred = sse_happypred_r2$happypred, r2 = sse_happypred_r2$r2, sse = sse_happypred_r2$sse))
  })
  
  return(fitted_results)
}

# Happiness model function 
happymodel_long <- function(x, data) {
  ev_chosen_z <- x[1]
  rpe_chosen_z <- x[2]
  tau_z <- x[3]
  const_z <- x[4]
  
  decayvec <- tau_z ^ (0:(ncol(data$evmtx_chosen) - 1))
  
  happypred <- numeric(length(data$zhappy))
  
  for (i in 1:length(data$zhappy)) {
    happypred[i] <- ev_chosen_z * sum(data$evmtx_chosen[i, ] * decayvec) +
      rpe_chosen_z * sum(data$rpemtx_chosen[i, ] * decayvec) +
      const_z
  }
  
  sse <- sum((data$zhappy - happypred)^2)
  re <- sum((data$zhappy - mean(data$zhappy))^2)
  r2 <- 1 - sse / re
  
  return(list(sse = sse, happypred = happypred, r2 = r2))  # Return list with sse, happypred, and r2
}
  