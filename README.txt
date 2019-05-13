International Trust Database (ITD) version 1.0
Marta Kołczyńska
20 April 2019

ITD v.1.0 contains data from the following survey projects:
1. EVS Round 5 (ZA7500_v1-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
2. EVS Rounds 1-4 (ZA4804_v3-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
3. ESS Rounds 1-8* (ESS1-8e01.zip): https://www.europeansocialsurvey.org/downloadwizard/
4. EQLS Rounds 1-4 (eqls_integrated_trend_2003-2016.sav): https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7348

* Note: The file doesn't include countries that occurred in ESS/1-8 only once (incl. Albania and Kosovo), and excludes country-specific variables.

Number of projects: 4
Number of project rounds: 21
Number of national surveys (project-round-country): 461
Number of respondents: 704,575
Number of countries/territories: 51** 

** Note that CY = Cyprus and CY-TCC = Northern Cyprus are treated as separate countries; in EVS GB-GBN = Great Britain and GB-NIR = Northern Ireland are separate samples.

Documentation:
ITD_code_v_1_0.Rmd - R code used to generate the data
ITD_code_v_1_0.nb.html - R Notebook
ITD_source_codebook_v_1_0.xlsx - selection of source variables
ITD_crosswalk_v_1_0.xlsx - mapping of source to target values for the selected variables
ITD_target_codebook_v_1_0.xlsx - codebook of the ITD data set
ITD_surveys_list_v_1_0.xlsx - list of surveys in ITD v.1.0
ITD_data_v_1_0.csv - CSV data file
ITD_data_v_1_0.sav - SAV data file

Note on the target variables on trust in institutions:
Originally, questions on trust in different institutions are accompanied by 4-point descending scales (EVS), 10-point ascending scales (EQLS) or 11-point ascending scales (ESS).
As shown in the Crosswalk, the target trust variables are recoded such that the source value corresponding to the lowest level of trust is assigned the target value 0, and consecutive source values are assigned integers 1, 2, 3, etc. In this way, the target trust variables in EVS range from 0 to 3, in EQLS 0-9, in ESS 0-10. The target variables are thus all ascending. No transformation onto a common range, e.g. 0-10 or 0-1, was performed intentionally, to avoid making an impression that the rescaled variables are in any way comparable. 
