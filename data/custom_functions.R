
# Friendly colours - https://sronpersonalpages.nl/~pault/
paul_tol <- c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB")

# mini helper function to restructure rating data and check number of unique responses
check_unique_values <- function(data,
                                col_map = list(p_id = "p_id",
                                               stim_id = "stim_id",
                                               rating = "rating")) {
  
  p_id <- col_map$p_id
  stim_id <- col_map$stim_id
  rating <- col_map$rating
  
  rating_matrix <- data |> 
    dplyr::select(all_of(c(p_id, stim_id, rating))) |> 
    tidyr::pivot_wider(names_from = all_of(p_id), values_from = all_of(rating)) |> 
    dplyr::select(-all_of(stim_id)) |> 
    as.matrix()
  
  apply(rating_matrix, 2, function(x) length(unique(x)))
}

#######################################################
#################### ---- ICC ---- ####################
#######################################################

### --- OBSERVED

# Compute ICC(2,1) and ICC(2,k) using psych package
# TODO; should I check here too for participants who only gave 2 different responses?

calc_icc <- function(data, group) {
  
  # Reshape data
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-c(trial_name)) |> 
    as.matrix()
  
  # Run ICC
  icc_result <- psych::ICC(rating_matrix)$results
  
  # Extract ICCs and corresponding CIs
  icc21_row <- icc_result |> filter(type == "ICC2")
  icc2k_row <- icc_result |> filter(type == "ICC2k")
  
  tibble(
    experiment = group$exp,
    
    `ICC(2,1)` = icc21_row$ICC,
    `ICC(2,1): 95% CI lower` = icc21_row$`lower bound`,
    `ICC(2,1): 95% CI upper` = icc21_row$`upper bound`,
    
    `ICC(2,k)` = icc2k_row$ICC,
    `ICC(2,k): 95% CI lower` = icc2k_row$`lower bound`,
    `ICC(2,k): 95% CI upper` = icc2k_row$`upper bound`,
    
    n_raters = ncol(rating_matrix),
    n_stimuli = nrow(rating_matrix)
  )
}


### --- RESAMPLING
# TODO; still clunky and slow

calc_icc_cos <- function(data, exp,
                   n_raters_seq = seq(10, 100, 10),
                   n_iter = 500) {
  
  # Reshape data
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-c(trial_name)) |> 
    as.matrix()
  
  # Drop raters with <3 unique responses
  unique_counts <- apply(rating_matrix, 2, function(x) length(unique(x)))
  var_ok <- unique_counts >= 3
  rating_matrix <- rating_matrix[, var_ok, drop = FALSE]
  
  available_raters <- colnames(rating_matrix)
  max_raters <- length(available_raters)
  
  # Adjust n_raters_seq to not exceed max_raters
  n_raters_seq <- n_raters_seq[n_raters_seq <= max_raters]
  
  # Run the resampling loop
  results <- purrr::map(n_raters_seq, function(n_raters_sampled) {
    
    iterations <- purrr::map(1:n_iter, function(iter) {
      
      sampled_raters <- sample(available_raters, n_raters_sampled, replace = FALSE)
      sampled_matrix <- rating_matrix[, sampled_raters, drop = FALSE]
      
      # Run ICC
      icc_result <- suppressWarnings(psych::ICC(sampled_matrix)$results)
      
      # Extract ICC(2,k)
      icc2k <- icc_result |> filter(type == "ICC2k")
      
      # Return result for this iteration
      tibble(
        experiment = exp,
        n_raters_sampled = n_raters_sampled,
        iter = iter,
        `ICC(2,k)` = icc2k$ICC,
        `ICC(2,k): 95% CI lower` = icc2k$`lower bound`,
        `ICC(2,k): 95% CI upper` = icc2k$`upper bound`,
      )
    })
    
    # Combine all iterations into one tibble
    list_rbind(iterations)
  })
  
  # Combine all raters_sampled groups into one tibble
  results <- list_rbind(results)
  
  return(results)
}

##############################################################
#### ---- Cronbach's alpha and McDonalds omega total ---- ####
##############################################################

### --- OBSERVED

calc_alpha_omega <- function(data, group) {
  
  # Prepare rating matrix
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-c(trial_name)) |> 
    as.matrix()
  
  # Rater names
  rater_names <- colnames(rating_matrix)
  
  # Count initial raters BEFORE filtering
  n_raters_initial <- ncol(rating_matrix)
  
  # Drop raters with <3 unique categories
  unique_counts <- apply(rating_matrix, 2, function(x) length(unique(x)))
  var_ok <- unique_counts >= 3
  dropped_raters_var <- rater_names[!var_ok]
  
  rating_matrix <- rating_matrix[, var_ok, drop = FALSE]
  
  n_raters_final <- ncol(rating_matrix)
  n_raters_dropped_variability <- n_raters_initial - n_raters_final
  
  # Compute alpha and omage if ≥2 raters left
  if (n_raters_final >= 2) {
    
    # Alpha
    alpha_result <- psych::alpha(rating_matrix)
    alpha_val <- alpha_result$total$raw_alpha
    
    # Omega total
    omega_result <- psych::omega(rating_matrix, nfactors = 1, plot = FALSE)
    omega_t <- omega_result$omega.tot
    
  } else {
    alpha_val <- NA
    omega_t <- NA
  }
  
  # Return summary
  tibble(
    experiment = group$exp,
    alpha = alpha_val,
    omega_t = omega_t,
    
    n_raters_initial = n_raters_initial,
    n_raters_final = n_raters_final,
    
    n_raters_dropped_variability = n_raters_dropped_variability,
    dropped_raters_var = paste(dropped_raters_var, collapse = "; "),
    
    n_stimuli = nrow(rating_matrix)
  )
}

### --- RESAMPLING

calc_alpha_omega_cos <- function(data,
                                 experiment_label,
                                 n_raters_seq = seq(10, 100, 10), # sequence of n_raters to test
                                 n_iter = 500) {
  
  # Prepare rating matrix
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-c(trial_name)) |> 
    as.matrix()
  
  # Drop raters with <3 unique categories
  unique_counts <- apply(rating_matrix, 2, function(x) length(unique(x)))
  var_ok <- unique_counts >= 3
  rating_matrix <- rating_matrix[, var_ok, drop = FALSE]
  
  # Available raters after filtering
  available_raters <- colnames(rating_matrix)
  max_raters <- length(available_raters)
  
  # Adjust n_raters_seq to not exceed max_raters
  n_raters_seq <- n_raters_seq[n_raters_seq <= max_raters]
  
  # Store results / run in parallel using furrr::future_map_dfr (instead of purr::map_dfr)
  results <- furrr::future_map_dfr(n_raters_seq, function(n_raters_sampled) {
    
    # Run n_iter subsamples for this n_raters_sampled
    furrr::future_map_dfr(1:n_iter, function(iter) {
      
      sampled_raters <- sample(available_raters, n_raters_sampled, replace = FALSE)
      sampled_matrix <- rating_matrix[, sampled_raters, drop = FALSE]
      
      # Compute alpha
      alpha_val <- suppressWarnings(
        psych::alpha(sampled_matrix)$total$raw_alpha
      )
      
      # Compute omega
      omega_t <- suppressWarnings(
        psych::omega(sampled_matrix, nfactors = 1, plot = FALSE)$omega.tot
      )
      
      # Return result for this iteration
      tibble(
        experiment = experiment_label,
        n_raters_sampled = n_raters_sampled,
        iter = iter,
        alpha = alpha_val,
        omega_t = omega_t
      )
    }, .options = furrr::furrr_options(seed = TRUE)) 
  },  .options = furrr::furrr_options(seed = TRUE))
  
  return(results)
}

#########################################################################################
##### ---- Hehman et al. (2018): Assessing point at which averages are stable ---- #####
#########################################################################################

# NOTE: Original code written by Gabe Nespoli
# https://github.com/gabenespoli/resampling
# Hehman, E., Xie, S. Y., Ofosu, E. K., & Nespoli, G. A. (2018). Assessing the point at which averages are stable: A tool illustrated in the context of person perception. https://doi.org/10.31234/osf.io/2n6jq

# Code downloaded from https://osf.io/82dsj/ on June 11, 2025 and slightly adapted by Iris.

# Goal is to calculate point of stability (POS): the point at which averages do no longer meaningfully change with the incorporation of additional observations.
# Hehman et al. (2018) randomly sampled with an increasing N of observations, and operationalised “point of stability” (POS) as N at which 95% of the averages were within the corridor of stability (COS) and did not again exceed the boundaries of the COS. For data collected on a 7-point Likert scale, Hehman et al. (2018) defined COS at +/- 1, +/- 0.5, and +/- 0.25 points.

# For each trait:
# 1)	Center each stimulus on its average rating, from all observations of that stimulus available in the dataset
# 2)	For each stimulus, sequentially sample 1 to N observations, at random and with replacement, in steps of 1 (in manuscript, N=100)
# 3)	At each N, calculated mean all randomly sampled observations
# 4)	Repeat this process for a total of 500 times
# 6)	For each N, across all stimuli, compute interval encompassing 95% of all means
# 7)	Record POS for each of the three COS thresholds, and plot.


##################################
##### ---- CI functions ---- #####

get_percentile <- function(x, interval) {
  boundary <- floor(length(x) * (1 - interval) / 2)
  x <- sort(x)
  ll <- x[boundary]
  ul <- x[length(x) - boundary]
  return(c(ll, ul))
}

get_normal_range <- function(x, interval) {
  z <- qnorm(interval + ((1 - interval) / 2))
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  error_margin <- z * (sd_x / sqrt(n))
  ll <- mean_x - error_margin
  ul <- mean_x + error_margin
  return(c(ll, ul))
}

get_ci <- function(x, interval = 0.95, method = "percentile") {
  if (method == "percentile") {
    ci <- get_percentile(x, interval)
  } else if (method == "normal") {
    ci <- get_normal_range(x, interval)
  } else {
    stop("Invalid method for getting CI")
  }
  return(ci)
}

#############################################
##### -------- The meaty stuff -------- #####

# New custom function
calc_stability_stats <- function(data = data,
                                 col_map = list(trait = "trait", # define what required columns are called in your data
                                                stim_id = "stim_id",
                                                rating = "rating"),
                                 N = 100, # max numbers of "raters" (number of obs) to be resampled, 100 in Hehman et al. (2018)
                                 iterations = 500, # how many times to resample, 500 in Hehman et al. (2018)
                                 ci_interval = 0.95, # CI to use to define COS and hence determine POS
                                 ci_method = "percentile", # other option: normal = 95% CIs based on normal distribution (rather than actual percentiles)
                                 cos_threshold = 0.5, # how much variability in mean is accepted as "stable" (default: 0.5 points on rating scale)
                                 save_means = FALSE) {
  trait <- col_map$trait
  stim_id <- col_map$stim_id
  rating <- col_map$rating
  
  # Center ratings for each trait
  data_centered <- data |>
    group_by(.data[[trait]], .data[[stim_id]]) |>
    mutate(rating_c = as.numeric(scale(.data[[rating]], scale = FALSE))) |>
    ungroup()
  
  # Resample ratings for each trait and stim_id (using resample_group())
  means_resampled <- data_centered |>
    group_by(.data[[trait]], .data[[stim_id]]) |>
    nest() |>
    mutate(resamples = map(data, ~ resample_group(.x$rating_c, N, iterations))) |>
    select(-data) |>
    unnest(resamples)
  
  # For each sample size of raters, calculate interval encompassing x% of values
  cis <- means_resampled |>
    ungroup() |>
    group_by(across(all_of(trait)), sample_size) |>
    summarise(
      ci = list(get_ci(mean_rating, interval = ci_interval, method = ci_method)),
      .groups = "drop") |>
    unnest_wider(ci, names_sep = "_") |>
    rename(ll = ci_1,
           ul = ci_2)
  
  
  # Calculate point of stability based on how averages behave relative to CIs
  pos <- calc_pos(cis = cis, threshold = cos_threshold, trait = trait)
  
  if (save_means) {
    list(means = means_resampled, cis = cis, pos = pos)
  } else {
    list(cis = cis, pos = pos)
  }
  
}

# RESAMPLING
# helper function: create grid based on desired number of N and iterations
# for each row, resample N values
resample_group <- function(ratings, N, iterations) {
  expand.grid(sample_size = 1:N,
              iteration = 1:iterations) |> 
    mutate(mean_rating = map_dbl(sample_size,
                                 ~ mean(sample(ratings, size = .x, replace = TRUE))))
}

# POINT OF STABILITY
# Based on CIs for each of respective sample sizes
# Threshold to be adjusted to whatever is desired (e.g., values used by Hehman et al., 2018)
# Note: Hehman et al. compare min CI to set threshold, but I think it would be more conservative to use max CI?
calc_pos <- function(cis, threshold, inarow = 1, trait = "trait") {
  
  threshold <- abs(threshold)

  cis  |> 
    group_by(across(all_of(trait))) |>
    mutate(max_ci = pmax(abs(ll), abs(ul)))  |> 
    summarise(
      pos = {
        stable <- max_ci < threshold
        if (length(stable) < inarow) {
          NA_integer_
        } else {
          runs <- map_lgl(
            1:(length(stable) - inarow + 1),
            ~ all(stable[.x:(.x + inarow - 1)])
          )
          match(TRUE, runs) %||% NA_integer_
        }
      },
      .groups = "drop"
    )
}