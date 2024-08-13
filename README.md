# survival-research

**SurvivalResearchOverview** - a copy of Becky's Word doc explaining the survival research

**SurvivalAnalysisCode.r** - R code used for survival analysis as part of the survival R package. Phylogeny relationships used V.Phylomaker and Taxonstand packages. Cox models used coxme package

**FindSurvivalMoBot_20240306.r** - R code that takes raw data from LCMS and creates survival data, censors it, then merges it with variables of interest from LCMS

**FindPhylogenyForMBGSurvival20240319.r** - R code for matching species names with World Flora Online, take survival data from LCMS, and match names to phylogeny

**GBIFDataSurvivalAnalysisPrep20240320.r** - R code for examining and subsetting data to find species level names, taxon key, occurrences, remove bad coordinates, and export dataframe

**FindClimateVariables_20230320.r** - R code to match study species GBIF occurrences with climate variables, then calculate summary statistics for each species, along with one-sided climate variables needed for survival analysis
