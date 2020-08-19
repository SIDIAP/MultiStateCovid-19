# MultiStateCovid-19

[![DOI](https://zenodo.org/badge/279256918.svg)](https://zenodo.org/badge/latestdoi/279256918)

This study has been performed on data from the *Sistema d’Informació pel Desenvolupament de la Investigació a l’Atenció Primària* (SIDIAP) database, linked to COVID-19 PCR testing, hospitalisation, and mortality data, which was mapped to the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM). There are two distinct parts to the data analysis. First, creating cohort definitions for identifying particular comorbidities of interest. Second, bespoke R analysis code to execute the study. 

1. Cohort definitions  
diagCOVID is a 
[CohortDiagnostics-type package](https://github.com/OHDSI/CohortDiagnostics). This was used to assess cohort definitions, and insert these cohorts into the results schema of the CDM. The results from the diagnostics can be viewed at https://livedataoxford.shinyapps.io/MultiStateCovidCohorts/, with the cohort definitions available in inst/cohorts folder of the package. 

> Note, to view/edit/reuse the cohort definitions you can use OHDSI ATLAS, with a public version available [here](http://www.ohdsi.org/web/atlas/#/home). To load a specific cohort, copy the json file from inst/cohorts, navigate to Cohort Definitions in Atlas, create a new cohort, go to the JSON tab, paste the definition, and click reload. You can then inspect the cohort definition, edit, and reuse in the creation of subsequent study packages. Reuse does come though with the caveat that we strongly recommend you to first assess whether the definition is appropriate given your source data. 

2) R analysis files  
The first analysis file is "data preparation" which prepares the data for analysis (the end result being one wide, patient-per-row dataframe, and multiple dataframes created using the mstate package for considering each of the transitions of interest).  
Subssequent Rmakdown files were created for analysing the observed data, modelling the relationship between age and transitions of interest, modelling the relationship between gender and transitions of interest, and modelling the relationship between comorbidities and transitions of interest. Both the RMD file and the html file (where the uderlying code can also be seen, along with the output) are made available. 
