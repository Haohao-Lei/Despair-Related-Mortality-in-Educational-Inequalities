# The Role of Despair-Related Mortality in Educational Inequalities in U.S. Mortality
**Insights from a Sequence Analysis-Based Measure**

## Overview

This repository contains all analytical scripts, documentation, and workflow files for the study “The Role of Despair-Related Mortality in Educational Inequalities in U.S. Mortality: Insights from a Sequence Analysis-Based Measure.”
The project introduces a new sequence analysis-based indicator to capture the temporal development of individual “despair trajectories” prior to death and examines educational inequalities in these trajectories within a competing-risks framework.

**Working paper notice:
This study is currently a working paper. The materials and results presented here are preliminary and subject to revision.
Please do not directly use, redistribute, or cite this repository or its outputs without the author’s explicit permission.**

Data note:
The project relies on restricted-access data from the Health and Retirement Study (HRS) and the RAND Harmonized HRS (1993–2020).
These data cannot be redistributed. Users must request them directly from the HRS repository and comply with all data-use agreements.
## Abstract
Recent declines in U.S. life expectancy and the rise in midlife mortality have been widely interpreted through the "deaths of despair" framework. A key limitation of existing research is its reliance on cause-specific mortality as a proxy for despair, without evidence that individuals were in despair prior to death.  
This study introduces a **sequence analysis-based measure** that reconstructs individuals’ despair states during the ten years before death using longitudinal data from the U.S. Health and Retirement Study (HRS). The method identifies distinct despair trajectories and examines educational differences in their incidence through competing risks models.  
Findings indicate that less-educated individuals are disproportionately represented in trajectories involving chronic pain and depression, highlighting how these despair-related deaths contribute to widening educational inequalities in mortality.

## Key Contributions
- Proposes a **new conceptual and empirical framework** for identifying despair-related deaths using individual-level longitudinal data.  
- Utilises **sequence analysis** to model temporal trajectories of pain and depression before death.  
- Applies **competing risk models** to decompose educational differences in various types of despair-related mortality.  
- Demonstrates that less-educated individuals face higher risks of chronic pain- and depression-related deaths, obscuring their competing risk of non-despair mortality.

## Data
- **Source:** RAND Harmonized HRS and G2aging HRS Version D (1993–2020).  
- **Population:** U.S. adults aged 50+, non-institutionalised.  
- **Sample size:** 40,246 individuals (2,883,253 person-years).  
- **Deaths observed:** 16,639.

Data are subject to restricted access. Users must obtain data directly from the Health and Retirement Study (HRS) under its data use agreement.

## Methodology
1. **Despair Measurement**
   - Despair defined as concurrent experiences of pain and/or depression (CESD-8 ≥ 4).  
   - Sequences constructed for each respondent’s despair states during the ten years preceding death.  
   - Optimal matching (OM) used to compute dissimilarity and derive typologies through hierarchical clustering.

2. **Despair Typologies**
   - Despair-free death  
   - Chronic pain-related death  
   - Dual despair death  
   - Unobserved despair development death  

3. **Statistical Framework**
   - Cause-specific hazard models  
   - Subdistribution proportional hazards models (Fine–Gray competing risks)
  
## Results
<img width="1099" height="561" alt="image" src="https://github.com/user-attachments/assets/1b382fe2-9225-4eb0-bb86-ae27cdcc2318" />

<img width="1120" height="633" alt="image" src="https://github.com/user-attachments/assets/fbdb0a87-299b-4c78-8ca9-b3fb96d609a8" />

<img width="1134" height="475" alt="image" src="https://github.com/user-attachments/assets/66546781-3f63-47b7-9668-6ac7e595e032" />


## Repository Structure

01_setup_and_load.R

Purpose:
Sets up the R environment, defines the working directory, and loads all required packages (tidyverse, TraMineR, survival, tidycmprsk, etc.).
Also defines global plotting themes, utility functions, and version control checks.
Outputs: A ready environment with all dependencies loaded.

⸻

02_merge_and_select_vars.R

Purpose:
Merges RAND Harmonized HRS and G2aging datasets by respondent ID, filters relevant waves, and selects analytic variables (demographics, education, health, pain, depression, and mortality).
Outputs: A cleaned, wide-format merged dataset (mortality or equivalent).

⸻

03_rename_and_cohort_age_entry.R

Purpose:
Renames variables for consistency across waves, generates birth-cohort labels, and constructs entry age/time origin variables for survival models.
Outputs: Cohort-coded, age-aligned dataset ready for longitudinal reshaping.

⸻

04_construct_pain_channel.R

Purpose:
Builds the pain channel — a longitudinal variable tracking whether respondents reported chronic pain at each wave.
Recodes raw items (e.g., HRS painfr_w1:painfr_w15), converts responses to binary states (P/N), handles missing data, and reshapes the dataset into long and wide formats by age.
Outputs: A respondent-level dataset of pain states across ages.

⸻

05_construct_cesd_channel.R

Purpose:
Builds the depression channel based on the CESD-8 scale.
Defines depression as CESD-8 ≥ 4, codes states (D/N), replaces non-responses with missing values, and produces a wave- and age-aligned dataset.
Outputs: A respondent-level dataset of depressive states across ages.

⸻

06_combine_channels_and_build_sequences.R

Purpose:
Merges the pain and CESD channels to create a combined despair-state variable for each year of age (e.g., no pain & no depression, pain only, depression only, pain + depression, missing).
Uses TraMineR::seqdef() to generate sequence objects representing each individual’s ten-year trajectory before death.
Outputs: Sequence objects for subsequent dissimilarity and clustering analyses.

⸻

07_sequence_clustering.R

Purpose:
Computes sequence dissimilarities using Optimal Matching (OM) and performs hierarchical clustering (Ward’s method via agnes()) to identify typologies of despair trajectories.
Plots representative sequence and index plots with custom colour schemes.
Outputs: Cluster membership labels (Type 1–4) and saved R objects (cl2.4fac, sequence).

⸻

08_attach_clusters_and_prep_competing_risk_data.R

Purpose:
Attaches the identified trajectory types to individual-level mortality records.
Constructs the event indicator (type_lcs) and defines competing outcomes (e.g., Despair-free death, Chronic-pain death, Dual despair death, Unobserved despair development death).
Outputs: Final analytic dataset linking cluster type, education, and survival times.

⸻

09_build_ipw_left_truncation.R

Purpose:
Builds inverse probability weights (IPW) to correct for left truncation and sample selection bias.
Calculates entry-by-age distributions, estimates probability of inclusion, and generates stabilised weights (ipw_LT).
Outputs: Weighted dataset ensuring unbiased cumulative-incidence estimation.

⸻

10_cif_and_competing_risk_models.R

Purpose:
Implements competing-risk survival analysis:
	•	Computes cumulative incidence functions (CIFs) for each trajectory type by education level;
	•	Fits cause-specific Cox models and Fine–Gray subdistribution models (tidycmprsk::crr()).
Applies IPW and generates summary tables and plots (CIF curves, hazard ratios).
Outputs: Model coefficients, CIF plots, and cumulative incidence tables.

⸻

11_visualizations_and_decompositions.R

Purpose:
Produces all visual outputs and decomposition figures used in the manuscript.
Includes age-standardised CIF plots, stacked area charts of despair typologies, educational-gap decomposition plots, and publication-ready graphs (via ggplot2 and ggsurvfit).
Also performs post-estimation decomposition of total educational inequalities into despair-related and despair-free components.
Outputs: Final figures in .png or .pdf format for inclusion in reports and manuscripts.

⸻

12_cox_hazard_and_stratified_analyses_and_descriptives.R

Purpose:
Conducts robustness and descriptive analyses:
	•	Fits standard Cox proportional-hazards models stratified by education, sex, and race;
	•	Provides baseline descriptive statistics (means, proportions, age distributions);
	•	Performs sensitivity checks on clustering solutions and model specifications.
Outputs: Stratified hazard models, descriptive tables (Table 1, Table 2), and supplementary analyses for appendices.

## Run all scripts sequentially:
source("code/01_setup_and_load.R")

source("code/02_merge_and_select_vars.R")

source("code/03_rename_and_cohort_age_entry.R")

source("code/04_construct_pain_channel.R")

source("code/05_construct_cesd_channel.R")

source("code/06_combine_channels_and_build_sequences.R")

source("code/07_sequence_clustering.R")

source("code/08_attach_clusters_and_prep_competing_risk_data.R")

source("code/09_build_ipw_left_truncation.R")

source("code/10_cif_and_competing_risk_models.R")

source("code/11_visualizations_and_decompositions.R")

source("code/12_cox_hazard_and_stratified_analyses_and_descriptives.R")

## Citation
If you use or refer to this repository, please cite as:

> Haohao Lei (2025). *The Role of Despair-Related Mortality in Educational Inequalities in U.S. Mortality: Insights from a Sequence Analysis-Based Measure.* [GitHub Repository]. https://github.com/[your-username]/despair-mortality-inequalities

## Contact
For questions or collaboration inquiries, please contact:  

**Haohao Lei**  

**Nuffield College, Oxford**

**Email**: [haohao.lei@nuffield.ox.ac.uk]
