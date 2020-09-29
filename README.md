# Polyandry and senescence shape egg size variation in a precocial bird

**Lourenço Falcão Rodrigues, Anne G. Hertel, Medardo Cruz-López, Erick González-Medina, Julia Schroeder, Clemens Küpper and Luke J. Eberhart-Phillips**

In this repository you can find all the files used to conduct our statistical analysis. These files will enable you to follow and reproduce the analyses presented in our paper.

This repository is also available on the Open Science Framework at [DOI 10.17605/OSF.IO/TE76U](https://doi.org/10.17605/OSF.IO/TE76U)

**Repository contents**

`root/`:

- Open_me.Rproj: RStudio project file, which opens a new session of R with a working directory of the folder which it is found in. To replicate the analysis, we recommend cloning the whole repository locally and opening the .Rproj file, along with the script found in the [`script/`](script) folder.

- [index.md](https://lorenf95.github.io/Polyandry-and-senescence-shape-egg-size-variation-in-a-precocial-bird/): github pages html file containing the knitted output of our .Rmd file. Click on the link to view it on you internet browser.

`script/`:

- RCODE_SnowyPlover_senescence.Rmd and it's [knitted html output](https://lorenf95.github.io/Polyandry-and-senescence-shape-egg-size-variation-in-a-precocial-bird/) contains the commented code used for all analyses, which can be implemented after downloading the dataset provided in the [`data/`](data) folder.

`data/`:

- Ceuta_OPEN_v1-5.sqlite: file containing all data collected for the years 2006 to 2016 in [Bahía de Ceuta, Sinaloa](https://www.google.com/maps/@23.9197739,-106.9668912,2726m/data=!3m1!1e3). For more information about the CeutaOPEN project, or to access the data directly from the OSF directory click [here](https://osf.io/3k4fh/).

Because the raw data between 2017 and 2019 is not yet publically available, we provide the rest of the data used for out analysis in the following files. These files can be called from within our RCODE_SnowyPlover_senescence.Rmd file by running the `load()` functions in the code.

- BaSTA_checked_life_table_females.rds: BaSTA ready life table which contains all the observations of our individuals throughout the length of the study. This file contains the data necessary for the replication of Part 1 of our analysis.

- egg_volume_data_2006_2019.rds: dataframe containing egg volume information of our individuals throughout the length of the study. This file contains the data necessary for the replication of Part 2 and 3 of our analysis.

`R_objects/`:

This folder contains optional files which contain outputs of different stages of the analysis. By using these files, you can reduce the amount of time necessary to replicate our analysis. However, you do not need these files to replicate the analysis. You can run the analysis just with the data and reproduce it in its entirety by running the models shown in the RCODE_SnowyPlover_senescence.Rmd, instead of running the `load()` functions.

- multibasta_females_min_age_1.rds: output of the bayesian model run with the BaSTA package in Part 1 of our analysis.
- results_mod6.rds: output of the top model for our senescence modelling in Part 2 of our analysis.
- results_mod6_out_rm.rds: output of the model investigating the effects of two apparently outlying females in Part 3 of our analysis.
