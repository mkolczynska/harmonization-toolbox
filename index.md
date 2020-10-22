# Harmonization toolbox

This repository contains various resources for secondary analysis of cross-national survey data, including ex-post survey data harmonization, such as survey metadata tables, crosswalks, and workflow ideas.

## Survey data harmonization workflow

[Survey Data Harmonization: A Reproducible Workflow and Toolbox](https://github.com/mkolczynska/harmonization-toolbox/tree/master/paper)

Ex-post harmonization of survey data creates new opportunities for research by extending the geographical and/or time coverage of analyses. While increasingly researchers combine different survey projects to analyze them as a single data set, there are no standards for data processing and its documentation in a way that enables computational reproducibility. This paper describes a procedure and a set of simple tools for the exploration, recoding, and documentation of harmonization of survey data, relying on crosswalks and a combination of automation for improved reproducibility and efficiency, with human decision-making that allows for flexibility necessary in dealing with the variation and diverse standards observed in survey data sets. The presented sample tools rely on the programming language R and spreadsheets - both common software among social scientists. Harmonization of variables on trust in institutions from three cross-national survey projects serve as an illustration of the proposed workflow.


## [Survey metadata](https://github.com/mkolczynska/harmonization-toolbox/tree/master/survey-metadata)

### Eurobarometer variables list

There are actually two lists:

(1) `eb_var_labels_groups_all.*` is a list of variable labels and variable groups from [GESIS ZACAT](https://zacat.gesis.org/webview/) matched to archive IDs and wave numbers and dates. The variable labels need not be identical to those in the actual data. The list may be incomplete (may not include all variables that are in the data) and contains occasional duplicated labels (same label in two different variable groups within the same EB wave).

(2) `eb_var_names_labels.*` is a list of all variable names and labels taken from the data sets (metadata embedded in the Stata files, to be precise), as well as archive IDs, wave numbers and descriptions, and wave dates. The wave descriptions and dates come from GESIS-ZACAT and the GESIS data description pages (e.g. https://search.gesis.org/research_data/ZA7601).

To create tables (2), the data were downloaded with the [`gesisdata` package](https://github.com/fsolt/gesisdata), which automates the download of data from the GESIS Data Archive (registration to the GESIS archive is necessary; also note that using the GESIS Data Archive means you accept the terms and conditions).

A very simple function to create a list of variables from a labelled data table:

```
library(labelled)

create_codebook_short <- function(data) {
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(var_name = X1, var_label = X2)
}
```

### International Social Survey Programme codebooks for waves 1985-2018

[International Social Survey Programme](http://w.issp.org/menu-top/home/) codebooks for waves 1985-2018: list of variable names, labels and frequencies of realized values of variables, as well as wave numbers and GESIS Archive IDs.

[ISSP website at GESIS](https://www.gesis.org/en/issp/home)

## Funding

Polish [National Agency for Academic Exchange](https://nawa.gov.pl/) within the Bekker programme (BEK/2019/1/00133) and Poland's [National Science Centre](https://ncn.gov.pl/?language=en) (2019/32/C/HS6/00421).
