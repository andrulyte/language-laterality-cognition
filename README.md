# Language laterality and cognitive skills: does anatomy matter?

Code repository for the preprint:

**Andrulyte, I.**, Zago, L., Jobard, G., Lemaitre, H., Branzi, F. M., Rheault, F., Petit, L., Keller, S. S.  
*Language laterality and cognitive skills: does anatomy matter?*  
Preprint DOI: 10.1101/2025.09.06.674566

## Overview

This repository contains scripts used to:

1. Prepare tractography bundles (AF + CC) for tract subdivision.
2. Extract mean fractional anisotropy (FA) values from:
   - arcuate fasciculus (left/right),
   - corpus callosum (genu, body, splenium; central subdivision used in analyses).
3. Perform statistical analyses and generate figures relating white matter FA, language lateralisation, and cognition.

## Repository contents

### R scripts
- `01_cognitive_FA_analyses.R` — PCA, ANCOVA, regression analyses.
- `02_cognitive_FA_figures.R` — main and supplementary figures.

### Bash scripts
- `ComputeCentroids.sh` — computes tract centroids and splits bundles into five subdivisions.
- `GenerateFAvaluesAF.sh` — extracts FA values from arcuate fasciculus bundles.
- `GenerateFAvaluesCC.sh` — extracts FA values from corpus callosum bundles.

## Requirements

### Neuroimaging
- FSL
- scilpy

### R
- R ≥ 4.4
- tidyverse, ggplot2, patchwork, ggstatsplot, broom, MASS (see scripts for full list)

## Data availability

Raw neuroimaging and cognitive data are part of the **BIL&GIN** database and are **not publicly redistributed** in this repository.  
This repository contains **analysis code only**.

## Citation

If you use this code, please cite:

Andrulyte I et al.  
*Language laterality and cognitive skills: does anatomy matter?*  
Preprint, doi:10.1101/2025.09.06.674566

