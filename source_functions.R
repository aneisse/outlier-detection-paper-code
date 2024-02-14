# Outliers test via Mahalanobis
outlier_test_mah <- function(data, alpha) {
  #'Performs the Mahalanobis Outlier test
  #'
  #' @param data A dataframe with variables to be tested for outliers
  #' @param alpha the significance level for the test or acceptable
  #' probability for the Type I error
  #'
  #' @return A vector with TRUE for each outlier observation/line on the data
  
  # Obtaining Mahalanobis distances
  mah_dists <- mahalanobis(x = data,
                           center = colMeans(data),
                           cov = cov(data))
  
  # Obtaining the test statistic based on alpha
  test_stat <- qchisq(p = 1 - alpha,
                      df = ncol(data))
  
  # Performing the test
  outliers_boolean <- mah_dists > test_stat
  
  # Returning boolean vector
  return(outliers_boolean)
}

# Outliers test via the Robust Mahalanobis (mcd)
outlier_test_rmah <- function(data, alpha) {
  #'Performs the Robust Mahalanobis Outlier test
  #'
  #' @param data A dataframe with variables to be tested for outliers
  #' @param alpha the significance level for the test or acceptable
  #' probability for the Type I error
  #'
  #' @return A vector with TRUE for each outlier observation/line on the data
  
  # Obtaining center and scale measures
  mcd <- robustbase::covMcd(data)
  
  # Obtaining Robust Mahalanobis distances
  rmah_dists <- mahalanobis(x = data,
                            center = mcd$center,
                            cov = mcd$cov)
  
  # Obtaining the test statistic based on alpha
  test_stat <- qchisq(p = 1 - alpha,
                      df = ncol(data))
  
  # Performing the test
  outliers <- rmah_dists > test_stat
  
  # Returning boolean vector
  return(outliers)
}

# Outliers test via MVE
outlier_test_mve <- function(data, alpha) {
  #'Performs the Minimum Volume Ellipsoid (MVE) Outlier test
  #'
  #' @param data A dataframe with variables to be tested for outliers
  #' @param alpha the significance level for the test or acceptable
  #' probability for the Type I error
  #'
  #' @return A vector with TRUE for each outlier observation/line on the data
  
  # Obtaining center and scale measures
  mve <- MASS::cov.mve(data)
  
  # Obtaining MVE Mahalanobis distances
  mvemah_dists <- mahalanobis(x = data,
                              center = mve$center,
                              cov = mve$cov)
  
  # Obtaining the test statistic based on alpha
  test_stat <- qchisq(p = 1 - alpha,
                      df = ncol(data))
  
  # Performing the test
  outliers <- mvemah_dists > test_stat
  
  # Returning boolean vector
  return(outliers)
}

# Leverage Point test via Cooks' Distance
leverage_test_cooks <- function(data_x, data_y, alpha) {
  #'Performs the Cooks' Distance Leverage Point test
  #'
  #' @param data_x A dataframe with explanatory variables to be tested
  #' @param data_y A vector with the response variable
  #' @param alpha the significance level for the test or acceptable
  #' probability for the Type I error
  #'
  #' @return A vector with TRUE for each leverage point observation/line
  
  # Generating model data
  model_data <- as.data.frame(cbind(data_y, data_x))
  
  # Fitting the model
  model <- lm(formula = V1 ~ .,
              data = model_data)
  
  # Obtaning Cooks' Distance
  cooks_dists <- cooks.distance(model)
  
  # Obtaining test statistic based on degrees of freedom
  c <- ncol(data_x)
  n <- nrow(data_x)
  test_stat <- qf(p = 0.5,
                  df1 = c,
                  df2 = n - c)
  
  # Performing test
  leverage_points <- cooks_dists > test_stat
  
  # Returning boolean vector
  return(leverage_points)
}

# Leverage Point test via DFBeta
leverage_test_dfbeta <- function(data_x, data_y) {
  #'Performs the DFBETA Leverage Point test
  #'
  #' @param data_x A dataframe with explanatory variables to be tested
  #' @param data_y A vector with the response variable
  #'
  #' @return A vector with TRUE for each leverage point observation/line
  
  # Create dataset to fit the model
  model_data <- as.data.frame(cbind(data_y, data_x))
  
  # Fitting model
  model <- lm(formula = V1 ~ .,
              data = model_data)
  
  # Calculating DFBETA values
  dfbeta_values <- dfbeta(model)
  
  # Obtaining test statistic
  n <- nrow(model_data)
  test_stat <- 2 / sqrt(n)
  
  # Performing the test
  leverage_points <- abs(dfbeta_values) > test_stat
  
  # Summarises the leverage from all columns to one
  # Returns TRUE if at least one columns is TRUE (Leverage Point)
  leverage_summary <- apply(leverage_points, 1, max)
  
  # Returning boolean vector
  return(leverage_summary)
}

# Leverage Point test via  DFFits
leverage_test_dffits <- function(data_x, data_y) {
  #'Performs the DFBETA Leverage Point test
  #'
  #' @param data_x A dataframe with explanatory variables to be tested
  #' @param data_y A vector with the response variable
  #'
  #' @return A vector with TRUE for each leverage point observation/line
  
  # Create dataset to fit the model
  model_data <- as.data.frame(cbind(data_y, data_x))
  
  # Fitting model
  model <- lm(formula = V1 ~ .,
              data = model_data)
  
  # Calculating DFFITS values
  dffits_values <- dffits(model)
  
  # Obtaining test statistic
  p <- length(model$coefficients)
  n <- nrow(data_x)
  test_stat <- sqrt((2 * p) / n)
  
  # Performing the test
  leverage_points <- dffits_values > test_stat
  
  # Returning boolean vector
  return(leverage_points)
}

# Leverage Point test via Hat Values
leverage_test_hatval <- function(data_x, data_y) {
  #'Performs the DFBETA Leverage Point test
  #'
  #' @param data_x A dataframe with explanatory variables to be tested
  #' @param data_y A vector with the response variable
  #'
  #' @return A vector with TRUE for each leverage point observation/line
  
  # Create dataset to fit the model
  model_data <- as.data.frame(cbind(data_y, data_x))
  
  # Fitting model
  model <- lm(formula = V1 ~ .,
              data = model_data)
  
  # Calculating HAT Values
  hat_values <- hatvalues(model)
  
  # Obtaining test statistic
  p <- length(model$coefficients)
  n <- nrow(data_x)
  test_stat <- (2 * p) / n
  
  # Performing the test
  leverage_points <- hat_values > test_stat
  
  # Returning boolean vector
  return(leverage_points)
}

# Perform Outlier tests on a MNORM generated sample
simulation_outlier <- function(center, vcov, n_obs, n_out, betas) {
  #'Generates a length(center)-dimensional sample from the Multivariate Normal
  #'Distribution with the chosen number of outliers and perform ouliers tests
  #'
  #' @param center Vector with the NCOL samples for the generated data
  #' @param vcov Matrix with the NCOLxNCOL variances and covariances
  #' @param n_obs Number of observations for the generated sample
  #' @param n_out NUmber of outliers to be inserted at the sample
  #' @param betas Vector of NCOL betas to define the response variable
  #'
  #' @return A 1-line tibble identifying, for each test, the number of total
  #' outliers generated, the total number of outliers detected and the number
  #' of correctly detected outliers
  
  # Generating the Multivariate Normal sample
  sample <- lgarch::rmnorm(n = n_obs,
                           mean = center,
                           vcov = vcov)
  
  # Randomly chooses the variables to receive the outlier
  var_out <- sample(x = 1:ncol(sample),
                    size = n_out,
                    replace = TRUE)
  
  # Randomly chooses the observations to receive the outlier
  obs_out <- sample(x = 1:nrow(sample),
                    n_out,
                    replace = FALSE)
  
  # Creates a vector indicating the ouliers
  is_out <- rep(FALSE, nrow(sample))
  is_out[obs_out] <- TRUE
  
  # Inserting outliers in the dataset
  for (i in 1:n_out) {
    value <- sample[obs_out[i], var_out[i]]
    outlier <-
      (abs(value) + sqrt(vcov[var_out[i], var_out[i]]) * 3) * sign(value)
    sample[obs_out[i], var_out[i]] <- outlier
  }
  
  # Defining Response variable based on Betas
  Y <- sample %*% betas + rnorm(nrow(sample), 0, 1)
  
  # Defining the significance level (alpha) for the tests
  alpha <- 0.05
  
  # Performing the Oulier Tests
  mah_results <- outlier_test_mah(sample, alpha)
  rmah_results <- outlier_test_rmah(sample, alpha)
  mvemah_results <- outlier_test_mve(sample, alpha)
  
  # Creating summary dataset
  results <- dplyr::tibble(
    Outliers = sum(is_out),
    Classic_All = sum(mah_results),
    Classic_Correct = sum(mah_results & is_out),
    MCD_All = sum(rmah_results),
    MCD_Correct = sum(rmah_results & is_out),
    MVE_All = sum(mvemah_results),
    MVE_Correct = sum(mvemah_results & is_out)
  )
  
  # Returning a on-lined tibble with the results
  return(results)
}


# Função que gera amostra MNORM com outliers
simulation_leverage <- function(center, vcov, n_obs, n_lev, betas) {
  #'Generates a length(center)-dimensional sample from the Multivariate Normal
  #'Distribution with the chosen number of outliers and perform leverage tests
  #'
  #' @param center Vector with the NCOL samples for the generated data
  #' @param vcov Matrix with the NCOLxNCOL variances and covariances
  #' @param n_obs Number of observations for the generated sample
  #' @param n_lev NUmber of outliers to be inserted at the sample
  #' @param betas Vector of NCOL betas to define the response variable
  #'
  #' @return A 1-line tibble identifying, for each test, the number of total
  #' outliers generated, the total number of outliers detected and the number
  #' of correctly detected outliers
  
  # Generating the Multivariate Normal sample
  sample <- lgarch::rmnorm(n = n_obs,
                           mean = center,
                           vcov = vcov)
  
  # Randomly chooses the observations to receive the outlier
  obs_lev <- sample(x = 1:nrow(sample),
                    n_lev,
                    replace = FALSE)
  
  # Creates a vector indicating the leverage points
  is_lev <- rep(FALSE, nrow(sample))
  is_lev[obs_lev] <- TRUE
  
  # Inserting leverage points in the dataset
  for (i in 1:n_lev) {
    value <- sample[obs_lev[i],]
    leverage <- (abs(value) + sqrt(diag(vcov)) * 3) * sign(value)
    sample[obs_lev[i],] <- leverage
  }
  
  # Defining Response variable based on Betas
  Y <- sample %*% betas + rnorm(nrow(sample), 0, 1)
  
  # Performing the Leverage Point Tests
  cooks_results <- leverage_test_cooks(sample, Y)
  dfbeta_results <- leverage_test_dfbeta(sample, Y)
  dffits_results <- leverage_test_dffits(sample, Y)
  hatval_results <- leverage_test_hatval(sample, Y)
  
  # Creating summary dataset
  results <- dplyr::tibble(
    leverage_points = sum(is_lev),
    Cooks_All = sum(cooks_results),
    Cooks_Correct = sum(cooks_results & is_lev),
    DFBeta_All = sum(dfbeta_results),
    DFBeta_Correct = sum(dfbeta_results & is_lev),
    DFFits_All = sum(dffits_results),
    DFFits_Correct = sum(dffits_results & is_lev),
    HatVal_All = sum(hatval_results),
    HatVal_Correct = sum(hatval_results & is_lev)
  )
  
  return(results)
}
