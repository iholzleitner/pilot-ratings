
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

# Function to create heatmap visualisations
heatmap <- function(data, exp_id, label) {
  data |>
    filter(exp %in% !!exp_id) |>
    count(lab_id, model_id, dv) |>
    ggplot(aes(x = dv, y = model_id, fill = n)) +
    geom_tile() +
    facet_wrap(~lab_id) +
    scale_fill_viridis_c() +
    labs(x = label, y = NULL, 
         title = paste(label, "Ratings")) +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90))
}

# Function to get mode, frequency, proportion
get_mode_info <- function(x) {
  x <- na.omit(x)
  tab <- table(x)
  mode_val <- names(tab)[which.max(tab)]
  freq <- max(tab)
  prop <- freq / length(x)
  
  list(age_mode = mode_val, age_prop = prop)
}

# T-test summary
ttest_summary <- function(x, y) {
  test <- t.test(x, y, paired = TRUE)
  p_val <- ifelse(test$p.value < 0.001, "<.001",
                  sub("^0", "", sprintf("%.3f", test$p.value)))
  tibble(M_std = round(mean(x, na.rm = TRUE), 2),
         M_unstd = round(mean(y, na.rm = TRUE), 2),
         M_diff = round(test$estimate, 2),
         CI_95_lower = round(test$conf.int[1], 2),
         CI_95_upper = round(test$conf.int[2], 2),
         t = round(test$statistic, 2),
         df = round(test$parameter, 2),
         p = p_val)
}

#######################################################
#################### ---- ICC ---- ####################
#######################################################

### --- OBSERVED

# Compute ICC(2,k) using psych package
calc_icc <- function(data,
                     group,
                     check_dropped_raters = FALSE) {
  
  # Reshape data
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    dplyr::select(-c(trial_name)) |> 
    as.matrix()
  
  # CHECK UNIQUE RESPONSES
  rater_names <- colnames(rating_matrix)
  n_raters_initial <- ncol(rating_matrix)
  
  # Drop raters with <3 unique categories
  unique_counts <- apply(rating_matrix, 2, function(x) length(unique(x)))
  var_ok <- unique_counts >= 3
  dropped_raters_var <- rater_names[!var_ok]
  
  rating_matrix <- rating_matrix[, var_ok, drop = FALSE]
  n_raters_final <- ncol(rating_matrix)
  
  # Compute ICCs if ≥2 raters left
  if (n_raters_final >= 2) {
    icc_result <- suppressWarnings(psych::ICC(rating_matrix)$results)
    
    icc2k_row <- icc_result |> filter(type == "ICC2k")

    icc_2k <- icc2k_row$ICC
    icc_2k_lower <- icc2k_row$`lower bound`
    icc_2k_upper <- icc2k_row$`upper bound`
    
  } else {

    icc_2k <- NA_real_
    icc_2k_lower <- NA_real_
    icc_2k_upper <- NA_real_
  }
  
  # Return summary
  if (check_dropped_raters) {
    tibble(experiment = group$exp,
           n_raters = n_raters_final,
           `ICC(2,k)` = icc_2k,
           `ICC(2,k): 2.5%` = icc_2k_lower,
           `ICC(2,k): 97.5%` = icc_2k_upper,
           dropped_n = n_raters_initial - n_raters_final,
           dropped_ids = paste(dropped_raters_var, collapse = "; "))
  } else {
    tibble(experiment = group$exp,
           n_raters = n_raters_final,
           `ICC(2,k)` = icc_2k,
           `ICC(2,k): 2.5%` = icc_2k_lower,
           `ICC(2,k): 97.5%` = icc_2k_upper)
  }
}


### --- RESAMPLING: Calculate "corridor of stability" (COS)
calc_icc_cos <- function(data, exp,
                         n_raters_seq = seq(10, 100, 10),
                         n_iter = 500) {

  # Reshape data
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-trial_name) |> 
    as.matrix()

  # Adjust n_raters_seq to not exceed max_raters
  available_raters <- colnames(rating_matrix)
  max_raters <- length(available_raters)
  n_raters_seq <- n_raters_seq[n_raters_seq <= max_raters]
  
  # Run the resampling loop
  all_results <- vector("list", length(n_raters_seq) * n_iter)
  counter <- 1
  
  for (n_raters_sampled in n_raters_seq) {
    
    for (iter in seq_len(n_iter)) {
      sampled_raters <- sample(available_raters, n_raters_sampled, replace = FALSE)
      sampled_matrix <- rating_matrix[, sampled_raters, drop = FALSE]
      # Run ICC
      icc_result <- suppressWarnings(psych::ICC(sampled_matrix)$results)
      # Extract ICC(2,k)
      icc2k <- icc_result |> filter(type == "ICC2k")
      # Return result for this iteration
      all_results[[counter]] <- tibble(experiment = exp,
                                       n_raters_sampled = n_raters_sampled,
                                       iter = iter,
                                       `ICC(2,k)` = icc2k$ICC,
                                       `ICC(2,k): 2.5%` = icc2k$`lower bound`,
                                       `ICC(2,k): 97.5% CI upper` = icc2k$`upper bound`)
      counter <- counter + 1
    }
  }
  # Combine all iterations
  list_rbind(all_results)
}

# CACHE INTERMEDIARY OUTPUT
# so next time script runs it will only recompute
# ICCs for experiments for which data has changed
run_or_load_icc <- function(exp_name, data,
                            cache_dir = "cache/icc",
                            n_raters_seq = seq(10, 100, 10),
                            n_iter = 50) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  data_hash <- digest::digest(data, algo = "xxhash64")
  cache_filename <- paste0("icc_", exp_name, "_", data_hash, ".rds")
  cache_path <- file.path(cache_dir, cache_filename)
  
  if (file.exists(cache_path)) {
    readr::read_rds(cache_path)
  } else {
    out <- calc_icc_cos(data = data,
                        exp = exp_name,
                        n_raters_seq = n_raters_seq,
                        n_iter = n_iter)
    
    readr::write_rds(out, cache_path)
    out
  }
}

##############################################################
#### ---- Cronbach's alpha and McDonalds omega total ---- ####
##############################################################

### --- OBSERVED

calc_alpha_omega <- function(data,
                             group,
                             check_dropped_raters = FALSE) {
  
  # Prepare rating matrix
  rating_matrix <- data |> 
    select(session_id, trial_name, dv) |> 
    pivot_wider(names_from = session_id, values_from = dv) |> 
    select(-c(trial_name)) |> 
    as.matrix()
  
  # CHECK UNIQUE RESPONSES
  rater_names <- colnames(rating_matrix)
  n_raters_initial <- ncol(rating_matrix)
  
  # Drop raters with <3 unique categories
  unique_counts <- apply(rating_matrix, 2, function(x) length(unique(x)))
  var_ok <- unique_counts >= 3
  dropped_raters_var <- rater_names[!var_ok]
  
  rating_matrix <- rating_matrix[, var_ok, drop = FALSE]
  n_raters_final <- ncol(rating_matrix)

  # Compute alpha and omega if ≥2 raters left
  if (n_raters_final >= 2) {
    alpha_val <- psych::alpha(rating_matrix)$total$raw_alpha
    omega_t <- psych::omega(rating_matrix, nfactors = 1, plot = FALSE)$omega.tot
  } else {
    alpha_val <- NA_real_
    omega_t <- NA_real_
  }
  
  # Return summary
  if (check_dropped_raters) {
    tibble(experiment = group$exp,
           n_raters = n_raters_final,
           alpha = alpha_val,
           omega_t = omega_t,
           dropped_n = n_raters_initial - n_raters_final,
           dropped_ids = paste(dropped_raters_var, collapse = "; "))
  } else {
    tibble(experiment = group$exp,
           n_raters = n_raters_final,
           alpha = alpha_val,
           omega_t = omega_t)
  }
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
resample_group <- function(ratings, N, iterations) {
  results <- vector("list", N)
  
  for (n in 1:N) {
    means <- numeric(iterations)
    for (i in 1:iterations) {
      means[i] <- mean(sample(ratings, size = n, replace = TRUE))
    }
    results[[n]] <- tibble(sample_size = n,
                           iteration = 1:iterations,
                           mean_rating = means)
  }
  bind_rows(results)
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