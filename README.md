## Reproducible code for the paper entitled:

**Evaluation and comparison of outliers detection methods: a simulation study** 


Authored by:
  
  - [Jhennifer dos Santos Nascimento](http://lattes.cnpq.br/3797894381138113)
  - [Jaqueline Akemi Suzuki Sediyama](http://lattes.cnpq.br/6051260154847913)
  - [Anderson Cristiano Neisse](https://aneisse.com/)
  - [Thaynara Aparecida de Souza Neto](http://lattes.cnpq.br/9038682869781380)
  - [Paulo César Emiliano](http://lattes.cnpq.br/8618494924305058)
  - [José Ivo Ribeiro Júnior](http://lattes.cnpq.br/2332963811685838)
  - [Paulo Roberto Cecon](http://lattes.cnpq.br/4525265173613927)
  
Submitted to the *Statistical Methods in Medical Research*.

### Data availability

All the data used in the paper is simulated with the ode on this repository, so anyone should be capable of reproducing the results with the seeds provided within the code.

### How to reproduce the results

The script `source_functions.R` should be executed first, which contains the base functions for the analysis scripts to work. 

Each analysis has its own script, `outlier_analysis.R` and `leverage_analysis.R` both reproduce the results and graphs presented within the paper.

The only required R package is `tidyverse`.
