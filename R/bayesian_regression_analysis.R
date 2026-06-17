#The code I implementation was partially generated and refined with the assistance of Gemini for structure, debugging and clarification.”

library(mvtnorm)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1. Data Setup and Pre-processing ---

data_cake <- read.csv("data/manufacturing_quality_clean.csv", header = TRUE)


# Drop the unecessary 'rownames' column
if ("rownames" %in% names(data_cake)) {
  data_cake$rownames <- NULL 
}

# Create a UNIQUE Replicate identifier (e.g., A_1, A_2, ..., C_15)
data_cake$replicate_id <- factor(paste(data_cake$recipe, data_cake$replicate, sep = "_"))
data_cake$recipe <- factor(data_cake$recipe)

####################### Exploratory Data Analysis (EDA)#########################################
# check-1: Data Summary and Structure
cat("--- Data Structure and Dimensions ---\n")
print(str(data_cake))
cat("\nTotal number of rows (data points):", nrow(data_cake), "\n")
cat("Total number of columns:", ncol(data_cake), "\n")

#check-2: Identify unique levels for the grouping factors
unique_replicates <- data_cake %>%
  distinct(replicate, recipe) %>%
  group_by(recipe) %>%
  summarise(
    n_replicates = n(),
    replicate_list = paste(unique(replicate), collapse = ", ")
  )

cat("\n--- Grouping Factor Counts ---\n")
cat("Number of unique Recipe levels:", length(unique(data_cake$recipe)), "\n")
cat("Number of unique Replicate levels (total batches):", length(unique(data_cake$replicate)), "\n")
cat("Total unique combinations of Recipe and Replicate (total batches):", nrow(unique_replicates), "\n")

#check-3:Overall Descriptive Statistics (for Angle and Temperature)
overall_summary <- data_cake %>%
  summarise(
    N = n(),
    Angle_Mean = mean(angle),
    Angle_SD = sd(angle),
    Angle_Min = min(angle),
    Angle_Max = max(angle),
    Temp_Mean = mean(temperature),
    Temp_SD = sd(temperature),
    Temp_Min = min(temperature),
    Temp_Max = max(temperature)
  )

cat("\n--- Overall Descriptive Statistics ---\n")
print(as.data.frame(overall_summary))

#check-4:Grouped Descriptive Statistics (Angle by Recipe)
recipe_summary <- data_cake %>%
  group_by(recipe) %>%
  summarise(
    N = n(),
    Mean_Angle = mean(angle),
    SD_Angle = sd(angle),
    Min_Angle = min(angle),
    Max_Angle = max(angle),
    Min_Temp = min(temperature),
    Max_Temp = max(temperature),
    .groups = 'drop'
  )

cat("\n--- Descriptive Statistics for Angle, Grouped by Recipe ---\n")
print(as.data.frame(recipe_summary))


##################### --- Exploratory Data Analysis (EDA) through charts############################
#check-5: Distribution of Cake Breakage Angle (Y Variable)

plot_y_distribution <- ggplot(data_cake, aes(x = angle)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = "#0072B2", color = "black") +
  geom_density(linewidth = 1, color = "#D55E00") +
  labs(
    title = "Distribution of Cake Breakage Angle (Y Variable)",
    x = "Breakage Angle (Degrees)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

print(plot_y_distribution)

# check-6: Overall Linearity (Global Trend) Angle vs temperature

plot_global_linearity <- ggplot(data_cake, aes(x = temperature, y = angle)) +
  geom_point(alpha = 0.6, size = 2) +
  # Add a single regression line for the entire dataset
  geom_smooth(method = "lm", color = "#CC79A7", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Overall Linearity Check (All Recipes and Replicates)",
    subtitle = "Assessing the global linear trend between Temperature and Angle.",
    x = "Temperature",
    y = "Breakage Angle"
  ) +
  theme_minimal(base_size = 14)

print(plot_global_linearity)

#check-7: Recipe-Specific Linearity

plot_recipe_linearity <- ggplot(data_cake, aes(x = temperature, y = angle, color = recipe)) +
  geom_point(alpha = 0.6, size = 2) +
  # Add separate regression lines for each recipe
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Recipe-Specific Linearity and Variability Check",
    subtitle = "Trends grouped by Recipe (Level 3 Random Effect).",
    x = "Temperature",
    y = "Breakage Angle",
    color = "Recipe"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Set1")

print(plot_recipe_linearity)

#check-8: Replicate-Level Linearity

plot_replicate_linearity_faceted <- ggplot(data_cake, aes(x = temperature, y = angle)) +
  # Points are colored by recipe for context
  geom_point(aes(color = recipe), alpha = 0.8, size = 1.5) +
  # Add the linear fit line (lm) for each unique replicate
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  # --- FIX: Changed 'unique_replicate' to the correctly named column 'replicate_id' ---
  facet_wrap(~ replicate_id, scales = "fixed", ncol = 10) +
  labs(
    title = "Replicate-Level Variability: Individual Batch Trends",
    subtitle = "45 separate panels confirm the need for Level 2 (Replicate) Random Effects.",
    x = "Temperature",
    y = "Breakage Angle",
    color = "Recipe"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    strip.text = element_text(size = 6), # Small labels for facets
    axis.text = element_text(size = 5)  # Small axis text for detail
  ) +
  scale_color_brewer(palette = "Set1")

print(plot_replicate_linearity_faceted)


cat("--- Data Structure and Dimensions ---\n")
print(str(data_cake))

###################################################################
# Determine data dimensions
N_TOTAL <- nrow(data_cake)
N_RECIPE <- length(unique(data_cake$recipe)) # Should be 3 (A, B, C)
N_REPLICATE_TOTAL <- length(unique(data_cake$replicate_id)) # Should be 45 (3 * 15)
N_REPLICATE <- N_REPLICATE_TOTAL / N_RECIPE 
N_TEMP <- N_TOTAL / N_REPLICATE_TOTAL

print(paste("Data loaded successfully. N_TOTAL:", N_TOTAL, "N_RECIPE:", N_RECIPE, "N_REPLICATE (per recipe):", N_REPLICATE))

# Centered the Temperature variable for stable MCMC sampling.
data_mean_temp <- mean(data_cake$temperature)
data_cake$temp_c <- data_cake$temperature - data_mean_temp

# Drop the original 'temperature' column (It is now centered in 'temp_c')
data_cake$temperature <- NULL
data_cake$replicate <- NULL # Drop the non-unique ID

# Create index lists for efficient grouping, using the UNIQUE ID
replicates_list <- split(data_cake, data_cake$replicate_id)


# --- 2. MCMC Settings and Initial Values ---

N_ITER <- 5000  # Number of MCMC iterations
BURN_IN <- 1000 # Burn-in period

# Dimensionality
K <- 2 # Intercept and Slope

# Storage for posterior samples
chains <- list(
  gamma = matrix(0, nrow = N_ITER, ncol = K), 
  sigma2_y = numeric(N_ITER), 
  Sigma_rep = array(0, dim = c(K, K, N_ITER)), 
  Sigma_recipe = array(0, dim = c(K, K, N_ITER)), 
  beta_i = array(0, dim = c(K, N_RECIPE, N_ITER)),
  beta_ik = array(0, dim = c(K, N_REPLICATE_TOTAL, N_ITER)) # Stores 45 vectors
)

# Initial Values for Parameters
initial_gamma <- c(mean(data_cake$angle), 0)

current <- list(
  gamma = initial_gamma, 
  sigma2_y = var(data_cake$angle) / 4,
  Sigma_rep = diag(K) * 5, 
  Sigma_recipe = diag(K) * 1 
)

current$beta_ik <- replicate(N_REPLICATE_TOTAL, current$gamma) 
current$beta_i <- replicate(N_RECIPE, current$gamma)

# Priors (Weakly Informative)
prior_gamma_mean <- c(0, 0)
prior_gamma_precision <- diag(K) * 1e-4 

# Variance Priors
a0_y <- 0.001
b0_y <- 0.001
nu0_rep <- K + 1 
Psi0_rep <- diag(K) * 1 
nu0_recipe <- K + 1
Psi0_recipe <- diag(K) * 1 

# --- 3. Gibbs Sampler Implementation ---

# function to sample from Inverse-Wishart 
sample_inv_wishart <- function(nu, Psi) {
  Psi_inv <- solve(Psi)
  W <- rWishart(1, nu, Psi_inv)[,,1]
  return(solve(W))
}

print(paste("Starting Gibbs Sampler for", N_ITER, "iterations..."))

for (t in 1:N_ITER) {
  
  # --- BLOCK 1: Sample Replicate-Level Coefficients (beta_ik) ---
  for (rep_idx in 1:N_REPLICATE_TOTAL) {
    data_rep <- replicates_list[[rep_idx]]
    y_rep <- data_rep$angle
    X_rep <- cbind(1, data_rep$temp_c)
    
    recipe_name <- as.character(data_rep$recipe[1])
    recipe_idx <- which(levels(data_cake$recipe) == recipe_name)
    
    current_beta_i <- matrix(current$beta_i[, recipe_idx], ncol = 1) 
    
    V_ik_inv <- (1 / current$sigma2_y) * t(X_rep) %*% X_rep + solve(current$Sigma_rep)
    V_ik <- solve(V_ik_inv)
    
    v_ik <- (1 / current$sigma2_y) * t(X_rep) %*% y_rep + solve(current$Sigma_rep) %*% current_beta_i
    
    current$beta_ik[, rep_idx] <- t(rmvnorm(1, V_ik %*% v_ik, V_ik))
  }
  
  # --- BLOCK 2: Sample Recipe-Level Coefficients (beta_i) ---
  for (recipe_idx in 1:N_RECIPE) {
    # Isolate the columns of beta_ik corresponding to the current recipe
    rep_indices_start <- (recipe_idx - 1) * N_REPLICATE + 1
    rep_indices_end <- recipe_idx * N_REPLICATE
    recipe_replicates <- current$beta_ik[, rep_indices_start:rep_indices_end]
    
    V_i_inv <- N_REPLICATE * solve(current$Sigma_rep) + solve(current$Sigma_recipe)
    V_i <- solve(V_i_inv)
    
    sum_rep_prec <- solve(current$Sigma_rep) %*% rowSums(recipe_replicates)
    
    current_gamma_matrix <- matrix(current$gamma, ncol = 1)
    
    global_prec <- solve(current$Sigma_recipe) %*% current_gamma_matrix
    v_i <- sum_rep_prec + global_prec
    
    current$beta_i[, recipe_idx] <- t(rmvnorm(1, V_i %*% v_i, V_i))
  }
  
  # --- BLOCK 3: Sample Global-Level Parameters (gamma) ---
  V_gamma_inv <- N_RECIPE * solve(current$Sigma_recipe) + prior_gamma_precision
  V_gamma <- solve(V_gamma_inv)
  
  sum_recipe_prec <- solve(current$Sigma_recipe) %*% rowSums(current$beta_i)
  
  prior_gamma_mean_matrix <- matrix(prior_gamma_mean, ncol = 1)
  
  prior_prec <- prior_gamma_precision %*% prior_gamma_mean_matrix
  v_gamma <- sum_recipe_prec + prior_prec
  
  current$gamma <- t(rmvnorm(1, V_gamma %*% v_gamma, V_gamma))
  
  # --- BLOCK 4: Sample Variance and Covariance Components ---
  
  # 4a. Residual Variance (sigma^2_y)
  SSE <- 0
  for (idx in 1:N_TOTAL) {
    # CRITICAL FIX: Get the index (1 to 45) from the factor level of the unique ID
    rep_idx_char <- as.character(data_cake$replicate_id[idx])
    rep_idx <- which(levels(data_cake$replicate_id) == rep_idx_char)
    
    beta_ik <- current$beta_ik[, rep_idx]
    
    # Calculate predicted mean (mu_ijk)
    mu_ijk <- beta_ik[1] + beta_ik[2] * data_cake$temp_c[idx]
    
    SSE <- SSE + (data_cake$angle[idx] - mu_ijk)^2
  }
  
  a_star_y <- a0_y + N_TOTAL / 2
  b_star_y <- b0_y + SSE / 2
  current$sigma2_y <- 1 / rgamma(1, shape = a_star_y, rate = b_star_y) 
  
  # 4b. Replicate Covariance Matrix (Sigma_Rep)
  SOPD_rep <- matrix(0, K, K)
  for (rep_idx in 1:N_REPLICATE_TOTAL) {
    recipe_idx <- ceiling(rep_idx / N_REPLICATE)
    
    current_beta_i <- matrix(current$beta_i[, recipe_idx], ncol = 1)
    current_beta_ik <- matrix(current$beta_ik[, rep_idx], ncol = 1)
    
    deviation <- current_beta_ik - current_beta_i
    SOPD_rep <- SOPD_rep + deviation %*% t(deviation)
  }
  
  nu_star_rep <- nu0_rep + N_REPLICATE_TOTAL
  Psi_star_rep <- Psi0_rep + SOPD_rep
  current$Sigma_rep <- sample_inv_wishart(nu_star_rep, Psi_star_rep) 
  
  # 4c. Recipe Covariance Matrix (Sigma_Recipe)
  SOPD_recipe <- matrix(0, K, K)
  for (recipe_idx in 1:N_RECIPE) {
    current_beta_i_matrix <- matrix(current$beta_i[, recipe_idx], ncol = 1)
    current_gamma_matrix <- matrix(current$gamma, ncol = 1)
    
    deviation <- current_beta_i_matrix - current_gamma_matrix
    SOPD_recipe <- SOPD_recipe + deviation %*% t(deviation)
  }
  
  nu_star_recipe <- nu0_recipe + N_RECIPE
  Psi_star_recipe <- Psi0_recipe + SOPD_recipe
  current$Sigma_recipe <- sample_inv_wishart(nu_star_recipe, Psi_star_recipe) 
  
  # --- 5. Store Results ---
  chains$gamma[t,] <- current$gamma
  chains$sigma2_y[t] <- current$sigma2_y
  chains$Sigma_rep[,,t] <- current$Sigma_rep
  chains$Sigma_recipe[,,t] <- current$Sigma_recipe
  chains$beta_i[,,t] <- current$beta_i
  chains$beta_ik[,,t] <- current$beta_ik
}

# --- 4. Analysis and Output (Posterior Summaries) ---

# Discard burn-in samples
gamma_post <- chains$gamma[BURN_IN:N_ITER, ]
sigma2_y_post <- chains$sigma2_y[BURN_IN:N_ITER]
Sigma_rep_post <- chains$Sigma_rep[,,BURN_IN:N_ITER]
Sigma_recipe_post <- chains$Sigma_recipe[,,BURN_IN:N_ITER]
beta_i_post <- chains$beta_i[,,BURN_IN:N_ITER] 

# Calculate Posterior Means and 95% Credible Intervals (CIs)
gamma_mean <- colMeans(gamma_post)
gamma_ci <- apply(gamma_post, 2, quantile, probs = c(0.025, 0.975))

sigma2_y_mean <- mean(sigma2_y_post)
sigma2_y_ci <- quantile(sigma2_y_post, probs = c(0.025, 0.975))

# --- Replicate-Level Variance (Sigma_Rep) Summary ---
sigma2_rep_int_post <- Sigma_rep_post[1, 1, ]
sigma2_rep_slope_post <- Sigma_rep_post[2, 2, ]
rho_rep_post <- Sigma_rep_post[1, 2, ] / sqrt(sigma2_rep_int_post * sigma2_rep_slope_post)

sigma2_rep_int_mean <- mean(sigma2_rep_int_post)
sigma2_rep_int_ci <- quantile(sigma2_rep_int_post, probs = c(0.025, 0.975))

sigma2_rep_slope_mean <- mean(sigma2_rep_slope_post)
sigma2_rep_slope_ci <- quantile(sigma2_rep_slope_post, probs = c(0.025, 0.975))

rho_rep_mean <- mean(rho_rep_post)
rho_rep_ci <- quantile(rho_rep_post, probs = c(0.025, 0.975))

# --- Recipe-Level Variance (Sigma_Recipe) Summary ---
sigma2_recipe_int_post <- Sigma_recipe_post[1, 1, ]
sigma2_recipe_recipe_slope_post <- Sigma_recipe_post[2, 2, ] 
rho_recipe_post <- Sigma_recipe_post[1, 2, ] / sqrt(sigma2_recipe_int_post * sigma2_recipe_recipe_slope_post)

sigma2_recipe_int_mean <- mean(sigma2_recipe_int_post)
sigma2_recipe_int_ci <- quantile(sigma2_recipe_int_post, probs = c(0.025, 0.975))

sigma2_recipe_slope_mean <- mean(sigma2_recipe_recipe_slope_post)
sigma2_recipe_slope_ci <- quantile(sigma2_recipe_recipe_slope_post, probs = c(0.025, 0.975))

rho_recipe_mean <- mean(rho_recipe_post)
rho_recipe_ci <- quantile(rho_recipe_post, probs = c(0.025, 0.975))


# Print Results for Global Parameters
print("--- POSTERIOR MEAN & 95% CREDIBLE INTERVALS (GLOBAL) ---")
print(paste("Global Mean Intercept (gamma_00): Mean =", round(gamma_mean[1], 3), 
            "CI = [", round(gamma_ci[1, 1], 3), ",", round(gamma_ci[2, 1], 3), "]"))

slope_sig_check <- if (gamma_ci[1, 2] < 0 && gamma_ci[2, 2] > 0) " (CI includes 0, not significant)" else " (CI excludes 0, significant)"

print(paste("Global Mean Slope (gamma_10): Mean =", round(gamma_mean[2], 5), 
            "CI = [", round(gamma_ci[1, 2], 5), ",", round(gamma_ci[2, 2], 5), "]", slope_sig_check))
print("--------------------------------------------------")
print(paste("Residual Variance (sigma^2_y): Mean =", round(sigma2_y_mean, 3), 
            "CI = [", round(sigma2_y_ci[1], 3), ",", round(sigma2_y_ci[2], 3), "]"))
print("--------------------------------------------------")

# Print Replicate-Level Results
print("--- REPLICATE-LEVEL VARIANCE COMPONENTS (Sigma_rep) ---")
print(paste("Replicate-Level Variance (Intercept): Mean =", round(sigma2_rep_int_mean, 3), 
            "CI = [", round(sigma2_rep_int_ci[1], 3), ",", round(sigma2_rep_int_ci[2], 3), "]"))
print(paste("Replicate-Level Variance (Slope): Mean =", round(sigma2_rep_slope_mean, 5), 
            "CI = [", round(sigma2_rep_slope_ci[1], 5), ",", round(sigma2_rep_slope_ci[2], 5), "]"))
rho_rep_sig_check <- if (rho_rep_ci[1] < 0 && rho_rep_ci[2] > 0) " (CI includes 0, no correlation)" else " (CI excludes 0, significant correlation)"
print(paste("Replicate-Level Correlation (rho): Mean =", round(rho_rep_mean, 3), 
            "CI = [", round(rho_rep_ci[1], 3), ",", round(rho_rep_ci[2], 3), "]", rho_rep_sig_check))
print("--------------------------------------------------")

# Print Recipe-Level Results 
print("--- RECIPE-LEVEL VARIANCE COMPONENTS (Sigma_recipe) ---")
print(paste("Recipe-Level Variance (Intercept): Mean =", round(sigma2_recipe_int_mean, 3), 
            "CI = [", round(sigma2_recipe_int_ci[1], 3), ",", round(sigma2_recipe_int_ci[2], 3), "]"))

# Check if CI contains zero for the Recipe Slope Variance (CRITICAL CHECK for different temperature response)
slope_var_sig_check <- if (sigma2_recipe_slope_ci[1] <= 0 && sigma2_recipe_slope_ci[2] >= 0) {
  " (CI includes 0 or is negative. NO SIGNIFICANT VARIATION in recipe slopes.)" 
} else { 
  " (CI excludes 0. SIGNIFICANT VARIATION in recipe slopes.)"
}

print(paste("Recipe-Level Variance (Slope) sigma_u1^2: Mean =", round(sigma2_recipe_slope_mean, 5), 
            "CI = [", round(sigma2_recipe_slope_ci[1], 5), ",", round(sigma2_recipe_slope_ci[2], 5), "]", slope_var_sig_check))

rho_recipe_sig_check <- if (rho_recipe_ci[1] < 0 && rho_recipe_ci[2] > 0) " (CI includes 0, no correlation)" else " (CI excludes 0, significant correlation)"
print(paste("Recipe-Level Correlation (rho): Mean =", round(rho_recipe_mean, 3), 
            "CI = [", round(rho_recipe_ci[1], 3), ",", round(rho_recipe_ci[2], 3), "]", rho_recipe_sig_check))
print("--------------------------------------------------")


print(paste("NOTE: Global Intercept is the mean angle at the mean temperature, which is", round(data_mean_temp, 2)))


# --- Recipe-Specific Intercepts and Slopes ---
recipe_levels <- levels(data_cake$recipe)
print("--- RECIPE-SPECIFIC COEFFICIENTS (BETA_I) ---")
for (i in 1:N_RECIPE) {
  recipe_name <- recipe_levels[i]
  
  # Intercept (at T_c = 0)
  intercept_post <- beta_i_post[1, i, ]
  intercept_mean <- mean(intercept_post)
  intercept_ci <- quantile(intercept_post, probs = c(0.025, 0.975))
  
  # Slope (temperature sensitivity)
  slope_post <- beta_i_post[2, i, ]
  slope_mean <- mean(slope_post)
  slope_ci <- quantile(slope_post, probs = c(0.025, 0.975))
  
  print(paste("Recipe", recipe_name, "Intercept (Angle @ Mean Temp): Mean =", round(intercept_mean, 3), 
              "CI = [", round(intercept_ci[1], 3), ",", round(intercept_ci[2], 3), "]"))
  print(paste("Recipe", recipe_name, "Slope (Temp Sensitivity): Mean =", round(slope_mean, 5), 
              "CI = [", round(slope_ci[1], 5), ",", round(slope_ci[2], 5), "]"))
}
print("--------------------------------------------------")

# --- INTERCEPT COMPARISON (Determining Highest/Lowest Angle) ---
print("--- STATISTICAL COMPARISON OF RECIPE INTERCEPTS ---")

# Comparison 1: Recipe B vs. Recipe A (Intercept)
diff_B_A_post <- beta_i_post[1, 2, ] - beta_i_post[1, 1, ]
diff_B_A_mean <- mean(diff_B_A_post)
diff_B_A_ci <- quantile(diff_B_A_post, probs = c(0.025, 0.975))
B_A_sig_check <- if (diff_B_A_ci[1] < 0 && diff_B_A_ci[2] > 0) " (CI includes 0, NOT SIGNIFICANTLY DIFFERENT)" else " (CI excludes 0, SIGNIFICANTLY DIFFERENT)"
print(paste("Difference (B - A) Intercept: Mean =", round(diff_B_A_mean, 3), 
            "CI = [", round(diff_B_A_ci[1], 3), ",", round(diff_B_A_ci[2], 3), "]", B_A_sig_check))

# Comparison 2: Recipe C vs. Recipe A (Intercept)
diff_C_A_post <- beta_i_post[1, 3, ] - beta_i_post[1, 1, ]
diff_C_A_mean <- mean(diff_C_A_post)
diff_C_A_ci <- quantile(diff_C_A_post, probs = c(0.025, 0.975))
C_A_sig_check <- if (diff_C_A_ci[1] < 0 && diff_C_A_ci[2] > 0) " (CI includes 0, NOT SIGNIFICANTLY DIFFERENT)" else " (CI excludes 0, SIGNIFICANTLY DIFFERENT)"
print(paste("Difference (C - A) Intercept: Mean =", round(diff_C_A_mean, 3), 
            "CI = [", round(diff_C_A_ci[1], 3), ",", round(diff_C_A_ci[2], 3), "]", C_A_sig_check))

# Comparison 3: Recipe C vs. Recipe B (Intercept)
diff_C_B_post <- beta_i_post[1, 3, ] - beta_i_post[1, 2, ]
diff_C_B_mean <- mean(diff_C_B_post)
diff_C_B_ci <- quantile(diff_C_B_post, probs = c(0.025, 0.975))
C_B_sig_check <- if (diff_C_B_ci[1] < 0 && diff_C_B_ci[2] > 0) " (CI includes 0, NOT SIGNIFICANTLY DIFFERENT)" else " (CI excludes 0, SIGNIFICANTLY DIFFERENT)"
print(paste("Difference (C - B) Intercept: Mean =", round(diff_C_B_mean, 3), 
            "CI = [", round(diff_C_B_ci[1], 3), ",", round(diff_C_B_ci[2], 3), "]", C_B_sig_check))
print("--------------------------------------------------")

# Example Plot (Trace Plot for Convergence Check)
if (requireNamespace("coda", quietly = TRUE)) {
  # library(coda)
  # plot(coda::as.mcmc(gamma_post[, 1]), main="Trace Plot: Global Intercept (gamma_00)")
} else {
  # plot(gamma_post[, 1], type="l", main="Trace Plot: Global Intercept (gamma_00)")
}





# --- PART 5: REPLICATE-LEVEL POSTERIOR SUMMARY (The requested 'End Part') ---

# Replicate ID levels (used for indexing)
replicate_id_levels <- levels(data_cake$replicate_id)

# 1. Initialize a data frame for all replicates
replicate_summary <- data.frame(
  ReplicateID = replicate_id_levels,
  # Extract Recipe name (A, B, C) from the replicate ID (e.g., "A_1" -> "A")
  Recipe = factor(gsub("_.*", "", replicate_id_levels)) 
)

# 2. Extract post-burn-in beta_ik samples (already done in your code)

# 3. Pre-allocate vectors and loop through all replicates (i=1 to 45)
B0_means <- numeric(N_REPLICATE_TOTAL)
B0_lowers <- numeric(N_REPLICATE_TOTAL)
B0_uppers <- numeric(N_REPLICATE_TOTAL)
B1_means <- numeric(N_REPLICATE_TOTAL)
B1_lowers <- numeric(N_REPLICATE_TOTAL)
B1_uppers <- numeric(N_REPLICATE_TOTAL)

for (rep_idx in 1:N_REPLICATE_TOTAL) {
  # beta_ik_post is defined in section 4 of your main script
  B0_post <- chains$beta_ik[1, rep_idx, BURN_IN:N_ITER] 
  B1_post <- chains$beta_ik[2, rep_idx, BURN_IN:N_ITER] 
  
  # Intercept (B0) Summary
  B0_means[rep_idx] <- mean(B0_post)
  ci_b0 <- quantile(B0_post, probs = c(0.025, 0.975))
  B0_lowers[rep_idx] <- ci_b0[1]
  B0_uppers[rep_idx] <- ci_b0[2]
  
  # Slope (B1) Summary
  B1_means[rep_idx] <- mean(B1_post)
  ci_b1 <- quantile(B1_post, probs = c(0.025, 0.975))
  B1_lowers[rep_idx] <- ci_b1[1]
  B1_uppers[rep_idx] <- ci_b1[2]
}

# 4. Bind results back into the summary data frame
replicate_summary <- replicate_summary %>%
  dplyr::mutate(
    B0_Mean = B0_means, B0_Lower = B0_lowers, B0_Upper = B0_uppers,
    B1_Mean = B1_means, B1_Lower = B1_lowers, B1_Upper = B1_uppers
  )

# 5. Loop through recipes and display the results, sorted by Intercept (B0_Mean)
cat("\n\n######################################################\n")
cat("### REPLICATE-LEVEL COEFFICIENT SUMMARY (BETA_IK) ###\n")
cat("### (Sorted by Intercept Mean within each Recipe) ###\n")
cat("######################################################\n")

unique_recipes <- unique(replicate_summary$Recipe)

for (recipe_name in unique_recipes) {
  # Filter the summary table for the current recipe
  # FIX: Explicitly use dplyr::filter, dplyr::select, and dplyr::arrange
  recipe_table <- replicate_summary %>%
    dplyr::filter(Recipe == recipe_name) %>%
    dplyr::select(
      ReplicateID, 
      B0_Mean, B0_Lower, B0_Upper, # Intercept Data (Mean and 95% CI)
      B1_Mean, B1_Lower, B1_Upper  # Slope Data (Mean and 95% CI)
    ) %>%
    # Sort replicates by increasing Intercept mean (B0_Mean) as requested
    dplyr::arrange(B0_Mean) 
  
  # Print a clear header and the resulting table
  cat(paste0("\n======================================================\n"))
  cat(paste0("Recipe ", recipe_name, " Replicates: Sorted by Intercept Mean (B0)\n"))
  cat(paste0("======================================================\n"))
  print(recipe_table)
  cat(paste0("\n"))
}