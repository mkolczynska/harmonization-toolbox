# code to identify all variables measuring left-right self placement in Standard Eurobarometer surveys,
# clean them, and merge in a single data file

# 2022-11-26


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

cdb_lr <- cdb %>%
  # conducted in Poland only
  filter(archive_id != "ZA4412") %>%
  # select variables that include the words "left-right placement"
  filter(var_label == "LEFT-RIGHT PLACEMENT") %>%
  mutate(target_var = "lrscale")

# list of archive IDs for which target variables were selected
archive_ids <- cdb_lr %>% distinct(archive_id) %>% pull(archive_id)

# keep only selected archive IDs from full dictionary
cdb_sel <- cdb %>%
  filter(archive_id %in% archive_ids)

# get variable names with country codes, one per wave (archive ID)
cdb_country <- cdb_sel %>% filter( grepl("ISO ", var_label) ) %>%
  mutate(target_var = "country")

# get variable names with weighting variables
cdb_weight <- cdb_sel %>% 
  filter(var_label == "WEIGHT RESULT FROM TARGET" | 
           var_label == "WEIGHT RESULT FROM TARGET (REDRESSMENT)" |
           var_label == "WEIGHT RESULT FROM TARGET (EU + CC)" |
           var_label == "WEIGHT RESULT FROM TARGET (NORWAY=1)" |
           var_label == "WEIGHT RESULT FROM TARGET (INCL. NORWAY)" |
           var_label == "WEIGHT ADJUSTED TO STANDARD SIZE" |
           var_label == "NATION WEIGHT" |
           var_label == "NATION WEIGHT I - STAND CROSS" |
           var_label == "WEIGHT RESULT TARGET" |
           var_label == "EUROPEAN WEIGHT" | 
           var_label == "WEIGHT NETHERLANDS" |
           var_label == "WEIGHT EUROPE" ) %>%
  mutate(target_var = "weight") %>%
  group_by(archive_id) %>%
  mutate(nobs = n(),
         first = as.numeric(var_name == first(var_name))) %>%
  filter(first == 1) %>%
  select(-nobs, -first) %>%
  ungroup()

# get variables with weighting variables
cdb_id <- cdb_sel %>% 
  filter( grepl("id serial number|respondent id|unique case id|icpsr case id|unique id|tns case id|serial case id", 
                var_label, ignore.case = TRUE) ) %>%
  filter( grepl("^v|^uniqid|^unique_id", var_name, ignore.case = TRUE) |
            (archive_id == "ZA5964" & var_name == "caseid")) %>%
  mutate(target_var = "id") %>%
  group_by(archive_id) %>%
  mutate(nobs = n(),
         first = as.numeric(var_name == first(var_name))) %>%
  filter(first == 1) %>%
  select(-nobs, -first) %>%
  ungroup()

# combine both variable tables
cdb_country_lr <- bind_rows(cdb_country, cdb_weight, cdb_id, cdb_lr)


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
  
  path <- paste0("D:/gesis_data/", i)
  temp <- list.files(path = path, pattern = ".RData$")
  f <- file.path(path, temp)
  f
  
  # get country variable name
  country_var <- cdb_country_lr %>%
    filter(archive_id == i,
           target_var == "country") %>%
    pull(var_name)
  
  # get weight variable name
  weight_var <- cdb_country_lr %>%
    filter(archive_id == i,
           target_var == "weight") %>%
    pull(var_name)
  
  # get id variable name
  id_var <- cdb_country_lr %>%
    filter(archive_id == i,
           target_var == "id") %>%
    pull(var_name)
  
  # get trust variable names
  lr_var <- cdb_country_lr %>%
    filter(archive_id == i,
           target_var == "lrscale") %>%
    pull(var_name)
  
  # load data file
  load(f)
  
  # select only the country and trust variables
  eb_data[[i]] <- x %>%
    mutate(weight = eval(parse(text = weight_var)),
           id = eval(parse(text = id_var))) %>%
    select(country_var, id, weight, lr_var)
  
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
  left_join(select(cdb_country_lr, archive_id, var_name, target_var)) %>%
  filter(!target_var %in% c("weight", "id"),
         !var_name %in% c("weight", "id")) %>%
  mutate(target_value = "",
         target_value = ifelse(target_var == "country", value_code, target_value),
         target_value = ifelse(as.numeric(value_code) > 10 & 
                                 target_var != "country", NA, value_code))



# 4. Harmonization ---------------

# remove labels from variables to avoid errors when combining datasets, and
# create long versions of the data files
eb_data_nolab <- list()
for (i in names(eb_data)) {
  
  eb_data_nolab[[i]] <- eb_data[[i]] %>%
    zap_labels() %>%
    mutate(caseid = row_number()) %>%
    gather(var_name, value_code, -c(caseid, id, weight))
  
}

# for each EB wave, join the data with the crosswalk
eb_data_nolab_recoded <- list()
for (i in names(eb_data_nolab)) {
  
  cwt <- cwt_df %>% filter(archive_id == i)
  
  eb_data_nolab_recoded[[i]] <- eb_data_nolab[[i]] %>% 
    left_join(cwt) %>%
    drop_na(value_code) %>%
    select(caseid, id, weight, target_var, target_value) %>%
    spread(target_var, target_value)
  
}

# combined dataset
eb_data_nolab_recoded_df <- bind_rows(eb_data_nolab_recoded, .id = "archive_id")

head(eb_data_nolab_recoded_df)

# read in EB wave metadata to get years of fieldwork
eb_waves <- rio::import('survey-metadata/eb_waves_filenames.xlsx') %>%
  mutate(year = as.numeric(gsub("\\D+", "", timeframe)),
         months = gsub("[0-9]+|\\s", "", timeframe)) %>%
  select(archive_id, year, wave, months, timeframe) %>%
  separate(months, c("month1", "month2"), sep = "-") %>%
  mutate(month1s = match(month1, month.name),
         month2s = match(month2, month.name),
         month1s = str_pad(month1s, 2, pad = "0"),
         month2s = str_pad(month2s, 2, pad = "0"),
         date1 = paste(year, month1s, "01", sep = "-"),
         date2 = ifelse(is.na(month2s),
                        paste(year, month1s, "28", sep = "-"),
                        paste(year, month2s, "28", sep = "-")),
         mid_survey = as.Date(date1) + floor((as.Date(date2) - as.Date(date1)) / 2)) %>%
  select(archive_id, year, wave, mid_survey, timeframe)


# 5. Harmonized data ----------------

# join survey data to wave metadata
eb_small_harm <- eb_data_nolab_recoded_df %>%
  left_join(eb_waves) %>%
  # calibrate weights and replace missing / 0 weights with 1
  group_by(archive_id, country) %>%
  mutate(weight = ifelse(weight == 0 | is.na(weight), 1, weight),
         weight_cal = weight / mean(weight)) %>%
  ungroup() %>%
  mutate(lrscale = as.numeric(lrscale))

saveRDS(eb_small_harm, "not-shared/eurobarometer/eb_small_harm_lrscale_20221126.rds")
eb_small_harm <- readRDS("not-shared/eurobarometer/eb_small_harm_lrscale_20221126.rds")

weights_check <- eb_small_harm %>%
  group_by(archive_id, country) %>%
  summarise(mean = mean(weight_cal))


# 6. Plots ------------------

# countries with surveys before 2002
countries_prior_2004 <- eb_small_harm %>%
  filter(year < 2004) %>%
  distinct(country) %>%
  pull(country)


# get wave numbers
waves <- eb_small_harm %>%
  filter(country %in% countries_prior_2004) %>%
  filter(!country %in% c("", "CY-TCC", "RS-KM", "DE-E", "DE-W", "GB-GBN", "GB-NIR", "GB", "DE")) %>%
  distinct(wave) %>%
  arrange(wave) %>%
  mutate(rnumber = row_number(),
         group = ifelse(rnumber <= max(rnumber)/7, 1, NA),
         group = ifelse(rnumber > max(rnumber)/7 & rnumber <= max(rnumber)/5*2, 2, group),
         group = ifelse(rnumber > max(rnumber)/7*2 & rnumber <= max(rnumber)/5*3, 3, group),
         group = ifelse(rnumber > max(rnumber)/7*3 & rnumber <= max(rnumber)/5*4, 4, group),
         group = ifelse(rnumber > max(rnumber)/7*4 & rnumber <= max(rnumber)/5*5, 5, group),
         group = ifelse(rnumber > max(rnumber)/7*5 & rnumber <= max(rnumber)/5*6, 6, group),
         group = ifelse(rnumber > max(rnumber)/7*6, 7, group)) %>%
  group_by(group) %>%
  summarise(waves = paste(wave, collapse = ", ")) %>%
  summarise(waves = paste(waves, collapse = "\n")) %>%
  as.character()

eb_small_harm %>%
  filter(country == "IE") %>% 
  group_by(archive_id, year, wave, mid_survey, country) %>%
  summarise(mean = mean(lrscale*weight_cal, na.rm = T),
            se = sd(lrscale, na.rm = T) / sqrt(n())) %>%
  ungroup() %>% print(n = 50)
  mutate(countryn = countrycode::countrycode(country, "iso2c", "country.name")) %>%
  ggplot(., aes(x = mid_survey, y = mean, ymin = mean-1.96*se, ymax = mean+1.96*se)) +
  geom_pointrange(fatten = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 5.5, color = "gray60", linetype = "dashed") +
  scale_x_date(name = "", breaks = "10 years", date_labels = "%Y", minor_breaks = "5 year") +
  ylab("Left-right placement") +
  theme_minimal(14) +
  ylab("Average left-right placement") +
  facet_wrap("countryn") +
  labs(title = "Left-right placement",
       subtitle = "",
       caption = paste0("Weighted means and 95% confidence intervals.\nData source: Eurobarometer, waves:\n", 
                        waves, 
                        ". \nhttps://www.gesis.org/en/eurobarometer-data-service/search-data-access/data-access."))

ggsave("workflows/lrscale_countries_prior2004.png", height = 9, width = 11, scale = 1.1)
ggsave("workflows/lrscale_countries_prior2004.jpg", height = 9, width = 11, scale = 1.1)




eb_small_harm %>%
  filter(country %in% countries_prior_2004) %>%
  filter(!country %in% c("", "CY-TCC", "RS-KM", "DE-E", "DE-W", "GB-GBN", "GB-NIR", "GB", "DE")) %>%
  group_by(archive_id, year, wave, mid_survey, country) %>%
  summarise(mean = weighted.mean(is.na(lrscale), w = weight_cal)) %>%
  filter(mean < 1) %>%
  ungroup() %>%
  mutate(countryn = countrycode::countrycode(country, "iso2c", "country.name")) %>%
  ggplot(., aes(x = mid_survey, y = mean)) +
  geom_point(size = 0.75) +
  scale_x_date(name = "", breaks = "10 years", date_labels = "%Y", minor_breaks = "5 year") +
  theme_minimal(14) +
  ylab("Proportion of missing values") +
  ylim(0, NA) +
  facet_wrap("countryn") +
  labs(title = "Left-right placement",
       subtitle = "",
       caption = paste0("'Don't know' answers and refusals combined. Weighted proportions.\nData source: Eurobarometer.\n", 
                        "https://www.gesis.org/en/eurobarometer-data-service/search-data-access/data-access."))

ggsave("workflows/lrscale_countries_prior2004_missing.png", height = 9, width = 11, scale = 1.1)
ggsave("workflows/lrscale_countries_prior2004_missing.jpg", height = 9, width = 11, scale = 1.1)

