# code to identify all variables measuring trust in institutions in Standard Eurobarometer 1995-2020,
# clean them, and merge in a single data file

# 2021-05-31


# 0. Setup --------------------

library(tidyverse) # for data manipulation
library(haven) # for reading in SPSS files
library(labelled) # for dealing with labelled data


# 1. Select variables ----------------
# filter the variables from the metadata table

# dictionary with all variable names and labels
cdb <- rio::import("survey-metadata/eb_var_names_labels.xlsx")

# How many EB waves are in the dictionary?
cdb %>% count(archive_id)

# keep only variables that measure trust in institutions in a way that's
# useful for the analysis

cdb_trust <- cdb %>%
  # select variables that include the words "trust" or "rely"
  filter(grepl("TRUST|RELY", var_label),
         archive_id >= "ZA2637") %>%
  # exclude variables that do not measure what we want
  filter(!grepl("RADIOACT INFO|TRUST IN PEOPLE|INDEX|TRUST IN FOOD|TRUST THAT|CCS INFO|(REC)|INNOVATIVE", var_label),
         !grepl("REASON|EU COUNTRIES POL TRUST|FOOD QUALITY|FOOD SAFETY|FOOD INFO|FOOD RISK|INTERPERSONAL", var_label),
         !grepl("DATA PROTEC|SOC WEB|ANTIBIOTICS TRUSTWORTHY|CONSUMER RIGHTS INFO|COLLECTION REVELATIONS", var_label),
         !grepl("FOOD LAB|RELY ON OTHER|POLIT MATTERS SOURCE TRUST|TCC|EU PARL TRUST:|EUROP CONSTITUTION", var_label),
         !grepl("EU PARL NO TRUST:|EU COMM TRUST:|EU COMM NO TRUST:|EU COUN TRUST:|ECOLABEL|TACKLING", var_label),
         !grepl("EU COUN NO TRUST:|DEVELOPM AID INFO TRUST:|DEPENDENT ELDERLY|FIGHTING FRAUD EFFECTIVELY", var_label),
         !grepl("INFO SOCIETY|ENVIRONM INFO|ENVIRONM TRUST|RADIOACT W INFO|DISASTER EXPL|GLOBALIS CONTR", var_label),
         !grepl("VACCINE|EP TRUSTING|EP NOT TRUSTING|EU AND PEOPLE|FINANC ADVICE|AGRICULTURE INFO", var_label),
         !grepl("TRUST OTHER PEOPLE|ENVIRONM TRUST|HEALTH INFO|RARELY|EU FEELING|INFO PROTECTION", var_label),
         !grepl("EU FUTURE CONVENTION|INTERNET SEEN|INTERNET PURCH|INTERNET HEARD|INTERNET TRUST", var_label),
         !grepl("BIOTECH INFO TRUST|CHANGE: DISTRUST|CONSUMER RIGHTS|CAR SAFETY|BIO-MED INFO|NUCLEAR SAFETY", var_label),
         !grepl("SCIENCE INFO|RESEARCH INFO|CONSUMER RIGHTS|ECB NO TRUST|ECB TRUST:|CHEMICAL|TRUSTWORTHY", var_label),
         !grepl("NO LONGER|ANTIBIOTICS|CYBER SECURITY ACTION|ACCOMPANYING|COPE WITH|POLIT MATTERS", var_label),
         !grepl("ONLINE SOC NETWORKS STORY|NOT PURCHASE|NOT TRUSTED ONLINE|CRISIS EFFECT|DONT TRUST|IDENTITY PROT", var_label),
         !grepl("POLITICS: TRUST INFO|COMBATING POVERTY|TRUSTED PROVIDER|TRUSTED WEBSITE|DISASTER BACKGRND", var_label),
         !grepl("INFO MEDIA TRUST MOST|EUROP ELECT NOT VOTE|MORE/LESS|STATEMENTS|ENVIRONM SOLUTION", var_label),
         !grepl("CIV JUSTICE ACCESS|FX PHONE REPLACE|NUCL SAFETY|ENERGY INFO|EU PURCHASE", var_label),
         !grepl("WILLING|WORK CONDITIONS|CORONA|INTERNET WEBSITES", var_label)) %>%
  # create target variable names: get institution names out of variable labels and standardize
  mutate(target_var = tolower(var_label),
         target_var = gsub(":|/|-|^\\s+|&", "", target_var),
         target_var = gsub("trust institutions|trust|trust in|rely on|trust in institutions|eu attitudes", "", target_var),
         target_var = gsub("^\\s+|\\s+$", "", target_var),
         target_var = gsub("  ", " ", target_var),
         target_var = gsub(" ", "_", target_var),
         target_var = gsub("^[a-z]{1,2}[0-9]{1,2}_[0-9]_|", "", target_var),
         target_var = gsub("the_|_\\(10pscale\\)", "", target_var),
         # standardize variable labels
         target_var = gsub("nat_parliament|^parliament$", "national_parliament", target_var),
         target_var = gsub("nat_government|^government$", "national_government", target_var),
         target_var = gsub("nongovmnt|nongovnmt", "nongovernment", target_var),
         target_var = gsub("econsocial|economicsoc|soceconom|economic_social|soceconom", "economic_and_social", target_var),
         target_var = gsub("committee_of_regions|eu_commit_of_regions", "eu_committee_of_regions", target_var),
         target_var = gsub("rglc|regloc|regloc|reglocal", "regional_local", target_var),
         target_var = gsub("eur_court_of_auditors|europ_court_of_auditors|eu_court_of_auditors", "european_court_of_auditors", target_var),
         target_var = gsub("european_court_of_justice|europ_court_of_justice|eu_court_of_justice", "european_court_of_justice", target_var),
         target_var = gsub("written_press", "press", target_var),
         target_var = gsub("justice_nat_legal_system|^justice$", "justice_legal_system", target_var),
         target_var = gsub("religious_inst", "religious_institutions", target_var),
         target_var = gsub("consumer_assoc", "consumer_association", target_var),
         target_var = gsub("charitable_org", "charities", target_var),
         target_var = gsub("internet_websites", "internet", target_var),
         target_var = gsub("council_of_ministers", "council_of_eu", target_var),
         target_var = gsub("polit_parties", "political_parties", target_var))

# list of unique target variables (institutions) and counts
target_vars <- cdb_trust %>% count(target_var)

# list of archive IDs for which target variables were selected
archive_ids <- cdb_trust %>% distinct(archive_id) %>% pull(archive_id)

# keep only selected archive IDs from full dictionary
cdb_sel <- cdb %>%
  filter(archive_id %in% archive_ids)

# get variables with country names, one per wave (archive ID)
cdb_country <- cdb_sel %>% filter( grepl("ISO ", var_label) ) %>%
  mutate(target_var = "country")

# combine both variable tables
cdb_country_trust <- bind_rows(cdb_country, cdb_trust)



# 2. Survey data ------------
# read in survey data and keep only the variables selected earlier


# EB data are stored with each wave's data file in a separate folder
# named as the GESIS Archive ID.
# They have been converted to .RData format (but that's not necessary)
# e.g.: 
# ZA7649/ZA7649_v1-1-0.RData

# create empty list to store subsets (selected columns) of the EB files
eb_data <- list()

# for each EB wave, read in data, select only the trust and country variables,
# and put into eb_data list
for (i in archive_ids) {
  
  path <- paste0("path", i)
  temp <- list.files(path = path, pattern = ".RData$")
  f <- file.path(path, temp)
  f
  
  # get country variable name
  country_var <- cdb_country_trust %>%
    filter(archive_id == i,
           target_var == "country") %>%
    pull(var_name)
  
  # get trust variable names
  trust_vars <- cdb_country_trust %>%
    filter(archive_id == i,
           target_var != "country") %>%
    pull(var_name)
  
  # load data file
  load(f)
  
  # select only the country and trust variables
  eb_data[[i]] <- x %>%
    select(country_var, trust_vars)
  
}


# 3. Mapping table (crosswalk) ---------------

# crosswalk function
create_cwt <- function(data) {
  
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(var_name = X1, var_label = X2)
  
  freqs_cwt <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    keep(function(x) nrow(x) < 1000) %>%
    do.call(rbind, .) %>% 
    tibble::rownames_to_column(var = "var_name_value") %>%
    mutate(var_name = gsub("(.+?)(\\..*)", "\\1", var_name_value),
           value = gsub("^[^.]*.","",var_name_value),
           value_code = sub(".*\\[(\\S+?)\\].*$", "\\1", var_name_value, perl = TRUE),
           value_code = ifelse(str_sub(var_name_value, -2, -1) == "NA", "NA", value_code),
           value_code = ifelse(gsub(" ", "", fixed = TRUE, var_name_value) == var_name_value, 
                               gsub("^[^.]*.","",var_name_value), value_code)) %>%
    group_by(var_name) %>%
    mutate(npos = row_number(),
           value_n = paste(value, n, sep = ": ")) %>%
    select(npos, var_name, value_n, value, value_code)
  
  full_join(var_labels, freqs_cwt, by = "var_name")
  
}

# create cross-walk from data tables in the list
cwt <- lapply(eb_data, create_cwt)

# fill out cross-walk: assign target values to source values
# here ordinal values start with 0 for the lowest value (lowest trust) and consecutive
# integers for higher levels of trust
# Most waves have binary responses, but some have 4- or 10-point scales

cwt_df <- bind_rows(cwt, .id = "archive_id") %>%
  left_join(select(cdb_country_trust, archive_id, var_name, target_var)) %>%
  mutate(target_value = "",
         target_value = ifelse(target_var == "country", value_code, target_value),
         target_value = ifelse(grepl("know|DK|NA|Inap", value) & target_var != "country", "NA", target_value),
         # note different response scale than in other waves
         target_value = ifelse(archive_id %in% c("ZA5237", "ZA4975") & target_value == "", 
                               plyr::mapvalues(value_code, 
                                               c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                                               c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")),  
                               target_value),
         # note different response scale than in other waves
         target_value = ifelse(archive_id %in% c("ZA3086") & target_value == "", 
                               plyr::mapvalues(value_code, c("1", "2", "3", "4"), c("3", "2", "1", "0")), 
                               target_value),
         target_value = ifelse(target_value == "" & value_code == 1, 1, target_value),
         target_value = ifelse(target_value == "" & value_code == 2, 0, target_value),
         target_value = ifelse(target_value == "" & value_code == 3, "NA", target_value))


# 4. Harmonization ---------------

# remove labels from variables to avoid errors when combining datasets, and
# create long versions of the data files
eb_data_nolab <- list()
for (i in names(eb_data)) {
  
  eb_data_nolab[[i]] <- eb_data[[i]] %>%
    zap_labels() %>%
    mutate(caseid = row_number()) %>%
    gather(var_name, value_code, -caseid)
  
}

# for each EB wave, join the data with the crosswalk
eb_data_nolab_recoded <- list()
for (i in names(eb_data_nolab)) {
  
  cwt <- cwt_df %>% filter(archive_id == i)
  
  eb_data_nolab_recoded[[i]] <- eb_data_nolab[[i]] %>% 
    left_join(cwt) %>%
    drop_na(value_code) %>%
    select(caseid, target_var, target_value) %>%
    spread(target_var, target_value) %>%
    gather(var, value, -c(caseid, country)) %>%
    mutate(value = as.numeric(value))
  
}

# combined dataset in long format
eb_data_nolab_recoded_long <- bind_rows(eb_data_nolab_recoded, .id = "archive_id")

head(eb_data_nolab_recoded_long)

# read in EB wave metadata to get years of fieldwork
eb_waves <- rio::import('survey-metadata/eb_waves_filenames.xlsx') %>%
  mutate(year = as.numeric(gsub("\\D+", "", timeframe))) %>%
  select(archive_id, year, wave)


# 5. Harmonized data ----------------

# join survey data to wave metadata
eb_small_long_harm <- eb_data_nolab_recoded_long %>%
  left_join(eb_waves)


# number of waves per country
eb_small_long_harm %>%
  distinct(country, wave, year) %>%
  count(country) %>%
  ggplot(., aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "Identity", fill = "gray50") +
  theme_minimal() +
  ylab("number of waves") + xlab("") +
  ggtitle("Number of waves available per country") +
  coord_flip()



