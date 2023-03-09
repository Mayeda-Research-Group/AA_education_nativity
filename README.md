# AA_education_nativity

This repository contains code for the paper: Hayes-Larson E, Ikesu R, Fong J, Mobley TM, Gee GC, Brookmeyer R, Whitmer RA, Gilsanz P, Mayeda ER. Association of education and dementia incidence stratified by ethnicity and nativity in a cohort of older Asian Americans. JAMA Netw Open. 2023 Mar 1;6(3):e231661. doi: 10.1001/jamanetworkopen.2023.1661. https://pubmed.ncbi.nlm.nih.gov/36877520/

The contents are as follows:
1. "Data preparation" contains 3 scripts that conduct dataset construction, data management for us in an incidence rate macro, and multiple imputations for missing data.
2. "Main analysis" contains scripts for the analyses of multiplicative (Cox) and additive (Aalen) models of dementia incidence, and an ancillary function to assist with calculation of confidence intervals for interactions.
3. "Sensitivity analysis" contains Cox and Aalen models for sensitivity analyses controlling for addition variables, assessing education as a 3-level variable, and using time on study instead of age as the timescale.
4. "Figure" contains scripts that generate figures for the paper (main analysis and sensitivity with 3-level education).
5. "Summary table" contains scripts that generate tables with descriptive characteristics, including the main Table 1 in the paper, and versions stratified by education and nativity.
