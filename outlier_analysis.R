# Packages, parameters and source code ------------------------------------------------
library(tidyverse)
source(file = "source_functions.R",
       encoding = "UTF-8")

# Simulation of samples and test results  ---------------------------------

## Defines the Variance Covariance Matrix and Beta Parameters  ------------

### Defining the center (mean) for samples
var_centers <- rep(0, 10)

### Generating Variance/Covariance matrix
dim <- length(var_centers)
vcov_matrix <- diag(1, dim) # Creates diagonal matrix
vcov_matrix[lower.tri(vcov_matrix, diag = F)] <-
  runif(dim * (dim - 1) / 2,-5, 5) # Generates correlations
vcov_matrix <-
  Matrix::forceSymmetric(vcov_matrix, uplo = "L") # Mirrors matrix
var_scale <-
  as.matrix(Matrix::nearPD(vcov_matrix, corr = T)$mat) # Turns into positive definite matrix

### Define the beta parameters for the Response Variable calculation
beta_parameters <- c(2, 3, 1.5, 9, 3, 2, 1, 6, 4, 3.75)

## Simulate the test results  ---------------------------------------------

### Defines the simulation hyper parameters
NUMBER_SIMULATIONS = 1000
SAMPLE_SIZE = 100

tictoc::tic()
set.seed(62840)
### Simulation with 3 outliers
simulation_results1 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_outlier(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_out = 3
  )
)
### Simulation with 5 outliers
simulation_results2 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_outlier(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_out = 5
  )
)
### Simulation with 10 outliers
simulation_results3 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_outlier(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_out = 10
  )
)
### Simulation with 15 outliers
simulation_results4 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_outlier(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_out = 15
  )
)
### Simulation with 20 outliers
simulation_results5 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_outlier(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_out = 20
  )
)

tictoc::toc()

### Binding results together
simulation_results_all <- bind_rows(simulation_results1,
                                    simulation_results2,
                                    simulation_results3,
                                    simulation_results4,
                                    simulation_results5)

### Formatting results
simulation_results_norm <- simulation_results_all %>%
  pivot_longer(-Outliers,
               names_to = "Method",
               values_to = "Quantity") %>%
  separate(Method,
           into = c("Test", "Type"),
           sep = "_")

# Calculating proportions
simulation_results_prop <- as.data.frame(simulation_results_all / simulation_results_all$Outliers) %>%
  mutate(Outliers = simulation_results_all$Outliers) %>%
  pivot_longer(-Outliers,
               names_to = "Method",
               values_to = "Proportion") %>%
  separate(Method, into = c("Test", "Detection"), sep = "_")

# Graph build analysis -----------------------------------------------------


## Box plot - Outlier detection rate ---------------------------------------

### Mahalanobis graph
plot_mah <- simulation_results_prop %>% 
  filter(Test == "Classic") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(Outliers),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-2, 10)) +
  labs(title = " Classic Mahalanobis Distance",
       x = "Number of Outliers Simulated",
       y = "Proportion of detected outliers") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "cadetblue",
                               "All" = "grey"))


### Robust Mahalanobis graph - MCD
plot_mcd <- simulation_results_prop %>% 
  filter(Test == "MCD") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(Outliers),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-2, 10)) +
  labs(title = "MCD Mahalanobis Distance",
       x = "Number of Outliers Simulated",
       y = "Proportion of detected outliers") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "chartreuse3",
                               "All" = "grey"))


# Gráfico de MVE
plot_mve <- simulation_results_prop %>% 
  filter(Test == "MVE") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(Outliers),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-2, 10)) +
  labs(title = "MVE Mahalanobis Distance",
       x = "Number of Outliers Simulated",
       y = "Proportion of detected outliers") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "chocolate3",
                               "All" = "grey"))

### Creating the plot grid for the paper
grid_plot <- simulation_results_prop %>% 
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(Outliers),
    y = Proportion,
    fill = Detection)) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(title = NULL,
       x = "Number of outliers simulated",
       y = "Proportion of detected outliers") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "seagreen",
                               "All" = "dodgerblue")) +
  facet_grid(vars(Test), scales = "free") +
  theme(
    # Text size of X and Y axis' titles
    axis.title = element_text(size = 25),
    # Text size of X and Y axis text
    axis.text = element_text(size = 25),
    # Text size of plot title
    plot.title = element_text(size = 25),
    # Text size of legend title
    legend.title = element_text(size = 25),
    # Text size of legend text
    legend.text = element_text(size = 25),
    strip.text = element_text(size = 20))

ggsave(filename = "plot_outliers.png", 
       plot = grid_plot, 
       dpi = 320)

## Average detection graph analysis ----------------------------------------

### Creating average dataset for the results
average_result_prop <- simulation_results_prop %>%
  group_by(Outliers, Test, Detection) %>%
  summarise(Proportion = mean(Proportion))


### Average proportion of detections - All detections
average_result_prop %>%
  filter(Detection == "All",
         Test %in% c("Classic", "MCD", "MVE")) %>%
  ggplot() +
  geom_line(aes(
    x = as.factor(Outliers),
    y = Proportion,
    color = Test,
    group = Test
  ),
  size = 1.5) +
  geom_point(aes(
    x = as.factor(Outliers),
    y = Proportion,
    color = Test,
    group = Test
  ),
  size = 3) +
  labs(x = "Number of outliers simulated",
       y = "Proportion of all detected outliers") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Classic" = "cadetblue",
      "MCD" = "chartreuse3",
      "MVE" = "chocolate3"
    )
  ) + scale_y_continuous(limits = c(0, 5)) +
  geom_hline(yintercept = 1)+
  theme(
    # Text size of X and Y axis' titles
    axis.title = element_text(size = 28),
    # Text size of X and Y axis text
    axis.text = element_text(size = 28),
    # Text size of plot title
    plot.title = element_text(size = 28),
    # Text size of legend title
    legend.title = element_text(size = 28),
    # Text size of legend text
    legend.text = element_text(size = 28))
 


### Average proportion of detections - Correct detections
average_result_prop %>%
  filter(Detection == "Correct",
         Test %in% c("Classic", "MCD", "MVE")) %>%
  ggplot() +
  geom_line(aes(
    x = as.factor(Outliers),
    y = Proportion,
    color = Test,
    group = Test
  ),
  size = 1.5) +
  geom_point(aes(
    x = as.factor(Outliers),
    y = Proportion,
    color = Test,
    group = Test
  ),
  size = 3) +
  labs(x = "Number of outliers simulated",
       y = "Proportion of correctly detected outliers") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Classic" = "cadetblue",
      "MCD" = "chartreuse3",
      "MVE" = "chocolate3"))  + 
  scale_y_continuous(limits = c(0, 5)) +
  geom_hline(yintercept = 1) +
  theme(
    # Text size of X and Y axis' titles
    axis.title = element_text(size = 28),
    # Text size of X and Y axis text
    axis.text = element_text(size = 28),
    # Text size of plot title
    plot.title = element_text(size = 28),
    # Text size of legend title
    legend.title = element_text(size = 28),
    # Text size of legend text
    legend.text = element_text(size = 28))

