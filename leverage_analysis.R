# Packages, parameters and source code ------------------------------------------------
library("tidyverse")
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
  .f = ~ simulation_leverage(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_lev = 3
  )
)

### Simulation with 5 outliers
simulation_results2 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_leverage(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_lev = 5
  )
)

### Simulation with 10 outliers
simulation_results3 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_leverage(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_lev = 10
  )
)

### Simulation with 15 outliers
simulation_results4 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_leverage(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_lev = 15
  )
)

### Simulation with 20 outliers
simulation_results5 <- map_dfr(
  .x = 1:NUMBER_SIMULATIONS,
  .f = ~ simulation_leverage(
    center = var_centers,
    vcov = var_scale,
    betas = beta_parameters,
    n_obs = SAMPLE_SIZE,
    n_lev = 20
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
  pivot_longer(-leverage_points,
               names_to = "Method",
               values_to = "Quantity") %>%
  separate(Method, into = c("Test", "Detection"), sep = "_")

# Calculating proportions
simulation_results_prop <- as.data.frame(simulation_results_all / simulation_results_all$leverage_points) %>%
  mutate(leverage_points = simulation_results_all$leverage_points) %>%
  pivot_longer(-leverage_points,
               names_to = "Method",
               values_to = "Proportion") %>%
  separate(Method, into = c("Test", "Detection"), sep = "_")


# Graph build analysis -----------------------------------------------------

## Box plot - Outlier detection rate ---------------------------------------

### Cooks Distance Graph
simulation_results_prop %>% 
  filter(Test == "Cooks") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-1, 5)) +
  labs(title = "Cook's distance",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "blue1",
                               "Detected" = "grey")) +
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
          legend.text = element_text(size = 25))


### DFBeta Graph
simulation_results_prop %>% 
  filter(Test == "DFBeta") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
 # scale_y_continuous(limits = c(-5, 15)) +
  labs(title = "DFBETA",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "yellow",
                               "Detected" = "grey"))+
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
    legend.text = element_text(size = 25))



### DFFits Graph
simulation_results_prop %>% 
  filter(Test == "DFFits") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-1, 5)) +
  labs(title = "DFFIT",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "brown1",
                               "Detected" = "grey"))+
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
    legend.text = element_text(size = 25))



### Hat Values Graph
simulation_results_prop %>% 
  filter(Test == "HatVal") %>%
  ggplot() +
  geom_boxplot(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    fill = Detection
  )) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(-1, 5)) +
  labs(title = "Leverage - Hat value",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_fill_manual(values = c("Correct" = "darkgreen",
                               "Detected" = "grey"))+
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
    legend.text = element_text(size = 25))



## Average detection graph analysis ----------------------------------------

### Creating average dataset for the results
average_result_prop <- simulation_results_prop %>%
  group_by(leverage_points, Test, Detection) %>%
  summarise(Proportion = mean(Proportion))


### Average detection graph - Detected
average_result_prop %>%
  filter(Detection == "All",
         Test %in% c("HatVal", "DFFits", "DFBeta", "Cooks")) %>%
  ggplot() +
  geom_line(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    color = Test,
    group = Test
  )) +
  geom_point(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    color = Test,
    group = Test
  )) +
  labs(title = "Average proportion of all detected leverage points",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "HatVal" = "darkgreen",
      "DFFits" = "brown1",
      "DFBeta" = "yellow",
      "Cooks" = "blue1"
    )
  ) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(0,34))+
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

  


### Average detection graph - Correct
average_result_prop %>%
  filter(Detection == "Correct",
         Test %in% c("HatVal", "DFFits", "DFBeta", "Cooks")) %>%
  ggplot() +
  geom_line(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    color = Test,
    group = Test
  )) +
  geom_point(aes(
    x = as.factor(leverage_points),
    y = Proportion,
    color = Test,
    group = Test
  )) +
  labs(title = "Average proportion of correctly detected leverage points",
       x = "Number of leverage points simulated",
       y = "Proportion of detected leverage points") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "HatVal" = "darkgreen",
      "DFFits" = "brown1",
      "DFBeta" = "yellow",
      "Cooks" = "blue1"
    )
  ) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(limits = c(0, 2)) +
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



