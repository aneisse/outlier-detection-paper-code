# Evaluation and comparison of outliers detection methods: a simulation study

This repository contains the reproducible code for the paper "Evaluation and comparison of outliers detection methods: a simulation study".

## About the Paper

-   **Title:** Evaluation and comparison of outliers detection methods: a simulation study
-   **Authors:**
    -   [Jhennifer dos Santos Nascimento](http://lattes.cnpq.br/3797894381138113)
    -   [Jaqueline Akemi Suzuki Sediyama](http://lattes.cnpq.br/6051260154847913)
    -   [Anderson Cristiano Neisse](https://aneisse.com/)
    -   [Thaynara Aparecida de Souza Neto](http://lattes.cnpq.br/9038682869781380)
    -   [Paulo César Emiliano](http://lattes.cnpq.br/8618494924305058)
    -   [José Ivo Ribeiro Júnior](http://lattes.cnpq.br/2332963811685838)
    -   [Paulo Roberto Cecon](http://lattes.cnpq.br/4525265173613927)
-   **Journal:** Published on the *Australian Journal of Basic and Applied Sciences (AJBAS)*. [2, 4, 9, 14, 16, 18]

### Data availability

All data used in the paper is simulated using the code in this repository. The use of seeds ensures that anyone can reproduce the results.

## Repository Content

-   `source_functions.R`: Contains all the base functions for outlier/leverage detection and simulation.
-   `outlier_analysis.R`: Script to reproduce the outlier detection analysis and graphs from the paper.
-   `leverage_analysis.R`: Script to reproduce the leverage points analysis and graphs from the paper.

## System Requirements

To run the analysis, you will need R and the following packages. You can install them using the code below:

```r
install.packages(c("tidyverse", "lgarch", "robustbase", "MASS"))
```

## How to Reproduce the Results

1.  **Clone the repository:**
    ```sh
    git clone <repository-url>
    cd outlier-detection-paper-code
    ```
2.  **Run the analysis scripts in R or RStudio:**
    -   First, execute the `source_functions.R` script to load all necessary functions into your R session.
    -   To reproduce the outlier analysis, run `outlier_analysis.R`.
    -   To reproduce the leverage point analysis, run `leverage_analysis.R`.

The scripts will generate the results and graphs presented in the paper.

## Citation

If you use the code or findings from this study, please cite our paper:

> Nascimento, J. S., Sediyama, J. A. S., Neisse, A. C., Neto, T. A. S., Emiliano, P. C., Ribeiro Júnior, J. I., & Cecon, P. R. (2024). Evaluation and comparison of outliers detection methods: a simulation study. *Australian Journal of Basic and Applied Sciences*.

## License

This project is licensed under the MIT License - see the `LICENSE` file for details.
