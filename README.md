<!-- README.md is generated from README.Rmd. Please edit that file -->

# ENSO

This repository contains the code and data for the article

> Muenchow, J., Dieker, P., Böttcher, T., Brock, J., Didenko, G., Fremout, T., Jakubka, D., Jentsch, A., Nüst, D., Richter, M., Rodríguez, E.F., Rodríguez, R.A., Rollenbeck, R., Salazar Zarsosa, P., Schratz, P. and Brenning, A. (2020), **Monitoring and predictive mapping of floristic biodiversity along a climatic gradient in ENSO's terrestrial core region, NW Peru**. Ecography. doi: [10.1111/ecog.05091](https://doi.org/10.1111/ecog.05091)

## Reproduce locally

To install all required packages for this analysis, please execute

```{r environment, eval = FALSE}
renv::init()
renv::install()
```

from the repository root.
The _renv_ package is the successor of the _packrat_ package and can be [installed from CRAN](https://cran.r-project.org/package=renv).
This will create a project-based R library in a subdirectory `renv` based on the lockfile `renv.lock`, with R packages being kept at a specific version for better reproducibility.

## Contents

This repository contains the data, code, and text for the article and realises a [research compendium](https://research-compendium.science/).

### Text

The main body of the article can be found in `docs/enso_comp.Rmd`.
This [R Markdown](https://rmarkdown.rstudio.com/) source be rendered to a PDF document for easier reading, printing, etc.
The file `docs/supplementary_information.Rmd` contains Appendix 1-4 of the article.
The file `docs/title_page.Rmd` contains the authors, article metadata, and abstract.

### Data

Zenodo deposit

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3981436.svg)](https://doi.org/10.5281/zenodo.3981436)

### Code

The directory `code` contains script files for all analysis steps.

The directory `R` contains helper functions for the analysis and the visualisations.

### Run workflow

**Prerequisites**

Create an [Earthdata login](https://urs.earthdata.nasa.gov/) and add an `.Renviron` file with the following content (note this intentionally overwrites other files, e.g. `~/.Renviron`):

```
EARTHDATA_USER=<your username>
EARTHDATA_PASS=<your password>
```

Now restart R so the environment variables are picked up.

Next, run the data download script.

```{r data_download, eval=FALSE}
source(here::here("code/00_modis_download.R"))
```

```{r analysis_steps, eval=FALSE}
source(here("code/11_prep_data.R"))
source(here("code/12_study_area_ndvi.R"))
source(here("code/13_descriptive_stats.R"))
source(here("code/14_ordination.R"))
source(here("code/15_modeling.R"))
source(here("code/16_varpart.R"))
source(here("code/17_irrigation_nutrient_experiment.R"))
source(here("code/18_appendix.R"))
```

``` r
# render documents
TODO
```

