<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4049816.svg)](https://doi.org/10.5281/zenodo.4049816)
<!-- badges: end -->

## Survey metadata

This repository contains tables with survey metadata (variable names, variable labels, archive IDs, wave numbers, etc.) from cross-national surveys. Currently only Eurobarometer and International Social Survey Programme, but more projects coming soon. 


## International Social Survey Programme codebooks

[International Social Survey Programme](http://w.issp.org/menu-top/home/) codebooks for waves 1985-2019: list of variable names, labels and frequencies of realized values of variables, as well as wave numbers and GESIS Archive IDs.

[ISSP website at GESIS](https://www.gesis.org/en/issp/home)


## Eurobarometer variables lists

There are actually two lists:

(1) `eb_var_labels_groups_all.*` is a list of variable labels and variable groups from [GESIS ZACAT](https://zacat.gesis.org/webview/) matched to archive IDs and wave numbers and dates. The variable labels need not be identical to those in the actual data. The list may be incomplete (may not include all variables that are in the data) and contains occasional duplicated labels (same label in two different variable groups within the same EB wave).

(2) `eb_var_names_labels.*` is a list of all variable names and labels taken from the data sets (metadata embedded in the Stata files, to be precise), as well as archive IDs, wave numbers and descriptions, and wave dates. The wave descriptions and dates come from GESIS-ZACAT and the GESIS data description pages (e.g. https://search.gesis.org/research_data/ZA7750). 
**updated through EB94.2 (ZA7750), November-December 2020**

To create tables (2), the data were downloaded with the [`gesisdata` package](https://github.com/fsolt/gesisdata), which automates the download of data from the GESIS Data Archive (registration to the GESIS archive is necessary; also note that using the GESIS Data Archive means you accept the terms and conditions).

A very simple function to create a list of variables from a labelled data table:

```
library(labelled)

create_codebook_short <- function(data) {
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(var_name = X1, var_label = X2)
}
```

