# Packages ----------------

library(tidyverse)
library(haven)
library(labelled)
library(countrycode)
library(rio)

###
sessionInfo()

### opening the data and creating basic technical variables -----------

# EVS 2017 (ZA7500_v1-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
# EVS 1981-2008 (ZA4804_v3-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
# ESS 1-8 (ESS1-8e01.zip): https://www.europeansocialsurvey.org/downloadwizard/
# EQLS (eqls_integrated_trend_2003-2016.sav): https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7348

basic_vars <- c("t_country", "s_id", "t_id", "t_weight", "t_round", "t_table_name", "t_year")

evs_2017 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/EVS/ZA7500_v1-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = c_abrv,
         t_id = row_number(),
         s_id = as.character(id_cocas),
         t_weight = 1,
         t_round = 5,
         t_table_name = "EVS_2017",
         t_year = year,
         t_project = "EVS")

evs_1981_2008 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/EVS/ZA4804_v3-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = S009,
         t_id = row_number(),
         s_id = as.character(S006),
         t_weight = S017,
         t_round = S002EVS,
         t_table_name = "EVS_1981_2008",
         t_year = S020,
         t_project = "EVS")

ess_1_8 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/ESS/ESS1-8e01.zip", user_na = TRUE) %>%
  mutate(t_country = cntry,
         t_id = row_number(),
         s_id = as.character(idno),
         t_weight = dweight * pspwght,
         t_table_name = "ESS_1_8",
         t_project= "ESS",
         t_round = essround,
         t_year = ifelse(is.na(inwyr), inwyys, inwyr),
         t_year = ifelse(t_year == 9999, NA, t_year)) %>%
  group_by(essround, cntry) %>%
  mutate(t_year = round(mean(t_year, na.rm = TRUE)),
         t_year = ifelse(cntry == "EE" & essround == 5, 2011, t_year)) %>%
  ungroup()


eqls_1_4 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/EQLS/eqls_integrated_trend_2003-2016.sav", user_na = TRUE) %>%
  mutate(t_country = plyr::mapvalues(Y16_Country,
                                     c(1:36),
                                     c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", 
                                       "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
                                       "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "AL", "IS",
                                       "KS", "ME", "MK", "NO", "RS", "TR")),
         t_id = row_number(),
         s_id = as.character(Y16_uniqueid),
         t_weight = WCalib,
         t_round = Wave,
         t_year = plyr::mapvalues(Wave, c(1,2,3,4), c(2003, 2007, 2011, 2016)),
         t_table_name = "EQLS_1_4",
         t_project = "EQLS")

### ISSP all -------------------

path <- "C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/ISSP"
temp <- list.files(path = path, pattern = "*.sav")
f <- file.path(path, temp)

issp_all <- lapply(f, import)
names(issp_all) <- temp

for(i in temp) { 
  issp_all[[i]]$Source <- i
  var_label(issp_all[[i]]$Source) <- substr(i, 1, 6) 
  issp_all[[i]] <- issp_all[[i]] %>% select(Source, everything())
}

### Codebook -----------

create_codebook_issp <- function(data) {
  
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(varname = X1, varlabel = X2)
  
  name_of_file <- as.character(var_labels[1,2])
  
  freqs <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    #keep(function(x) nrow(x) < 1000) %>%
    do.call(rbind, .) %>% 
    tibble::rownames_to_column(var = "varname_value") %>%
    mutate(varname = gsub("(.+?)(\\..*)", "\\1", varname_value),
           value = gsub("^[^.]*.","",varname_value)) %>%
    group_by(varname) %>%
    mutate(npos = row_number(),
           value_n = paste(value, n, sep = ": ")) %>%
    select(varname, value_n, npos) %>%
    spread(npos, value_n) %>%
    mutate_at(vars(-varname), funs(ifelse(is.na(.), "", .))) %>%
    unite("valfreqs", c(2:ncol(.)), sep = "\n") %>%
    mutate(valfreqs = sub("\\s+$", "", valfreqs))
  
  full_join(var_labels, freqs, by = "varname") %>%
    mutate(file_name = name_of_file) %>%
    filter(varname != "Source")
}

file_names <- read.csv(paste(path, "/issp-file-names.csv", sep = ""))

issp_all_codebooks <- lapply(issp_all, create_codebook_issp)

issp_all_codebooks_df <- do.call(rbind, issp_all_codebooks) %>%
  left_join(file_names) %>%
  select(file_name, t_table_name, everything())
  

export(issp_all_codebooks_df, "codebooks-survey-projects/codebook_issp_1985_2017.xlsx")


