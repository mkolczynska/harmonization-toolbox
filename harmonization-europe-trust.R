# Packages ----------------

library(tidyverse)
library(haven)
library(labelled)
library(countrycode)
library(essurvey)

###
sessionInfo()

### opening the data and creating basic technical variables -----------

# EVS 2017 (ZA7500_v2-0-0.sav): https://dbk.gesis.org/dbksearch/sdesc2.asp?no=7500&db=e&doi=10.4232/1.13314
# EVS 1981-2008 (ZA4804_v3-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
# ESS 1-8 (ESS1-8e01.zip): https://www.europeansocialsurvey.org/downloadwizard/
# EQLS (eqls_integrated_trend_2003-2016.sav): https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7348

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



basic_vars <- c("table_name", "t_country", "s_id", "t_id", "t_weight", "t_round", "t_year")

ess_1_8 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/data-for-harm/ESS/ESS1-8e01.zip", user_na = TRUE) %>%
  mutate(t_country = cntry,
         t_id = row_number(),
         s_id = as.character(idno),
         t_weight = pspwght,
         table_name = "ESS_1_8",
         t_project= "ESS",
         t_round = essround,
         t_year = ifelse(is.na(inwyr), inwyys, inwyr),
         t_year = ifelse(t_year == 9999, NA, t_year)) %>%
  group_by(essround, cntry) %>%
  mutate(t_year = round(mean(t_year, na.rm = TRUE)),
         t_year = ifelse(cntry == "EE" & essround == 5, 2011, t_year)) %>%
  ungroup()

evs_5 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/data-for-harm/EVS/ZA7500_v2-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = c_abrv,
         t_id = row_number(),
         s_id = as.character(id_cocas),
         t_weight = gweight,
         t_round = 5,
         table_name = "EVS_5",
         t_year = year,
         t_project = "EVS")

evs_1_4 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/data-for-harm/EVS/ZA4804_v3-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = S009,
         t_id = row_number(),
         s_id = as.character(S006),
         t_weight = S017,
         t_round = S002EVS,
         table_name = "EVS_1_4",
         t_year = S020,
         t_project = "EVS")


eqls_1_4 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/data-for-harm/EQLS/eqls_integrated_trend_2003-2016.zip", user_na = TRUE) %>%
  mutate(t_country = plyr::mapvalues(Y16_Country,
                                     c(1:36),
                                     c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", 
                                       "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
                                       "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB", "AL", "IS",
                                       "KS", "ME", "MK", "NO", "RS", "TR")),
         t_id = row_number(),
         s_id = as.character(Y16_uniqueid),
         t_weight = WCalib,
         t_round = Wave,
         t_year = plyr::mapvalues(Wave, c(1,2,3,4), c(2003, 2007, 2011, 2016)),
         table_name = "EQLS_1_4",
         t_project = "EQLS")
  

### dealing with non-unique labels in evs_1981_2008

# https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x)
{
  if(!is.null(attr(x, "value.labels"))){
    vlab <- attr(x, "value.labels")
    if(sum(duplicated(vlab)) > 0)
      cat("Duplicated levels:", vlab, "\n")
    else if(sum(duplicated(names(vlab))) > 0)
      cat("Duplicated labels:",
          names(vlab)[duplicated(names(vlab))], "\n")
    else
      x <- factor(x, levels = as.numeric(vlab),
                  labels = names(vlab))
  }
  x
}

evs_1_4 <- lapply(evs_1_4, Int2Factor) %>% as.data.frame(., stringsAsFactors = FALSE)



### list of surveys ----------------------

create_surveys_list <- function(data_frame_vector) {
  
  surveys_list <- list()
  
  for (i in 1:length(data_frame_vector)) {
    
    surveys_list[[i]] <- eval(parse(text = data_frame_vector[i])) %>%
      select(t_project, t_round, t_country, t_year) %>%
      zap_labels() %>%
      zap_label() %>%
      mutate(t_survey = paste(t_project, t_round, t_country, sep = "")) %>%
      count(t_survey, t_project, t_round, t_country, t_year)
  }
  do.call(rbind, surveys_list)
}


lists <- create_surveys_list(c("eqls_1_4", "evs_5", "evs_1_4", "ess_1_8"))
rio::export(lists, "templates/surveys_list.xlsx")


### creating codebooks ----------------------

create_codebook <- function(data) {
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(var_name = X1, var_label = X2)
  
  freqs <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    keep(function(x) nrow(x) < 1000) %>%
    do.call(rbind, .) %>% 
    tibble::rownames_to_column(var = "var_name_value") %>%
    mutate(var_name = gsub("(.+?)(\\..*)", "\\1", var_name_value),
           value = gsub("^[^.]*.","",var_name_value)) %>%
    group_by(var_name) %>%
    mutate(npos = row_number(),
           value_n = paste(value, n, sep = ": ")) %>%
    select(var_name, value_n, npos) %>%
    spread(npos, value_n) %>%
    mutate_at(vars(-var_name), funs(ifelse(is.na(.), "", .))) %>%
    unite("val_freqs", c(2:ncol(.)), sep = "\n") %>%
    mutate(val_freqs = sub("\\s+$", "", val_freqs))
  
  full_join(var_labels, freqs, by = "var_name")
}


codebook_ess_1_8 <- create_codebook(ess_1_8) %>%
  mutate(table_name = "ESS_1_8")

codebook_evs_5 <- create_codebook(evs_5) %>%
  mutate(table_name = "EVS_5")

codebook_evs_1_4 <- create_codebook(evs_1_4) %>%
 mutate(table_name = "EVS_1_4")

codebook_eqls_1_4 <- create_codebook(eqls_1_4) %>%
  mutate(table_name = "EQLS_1_4")

codebook_all <- bind_rows(codebook_ess_1_8, codebook_evs_1_4, 
                          codebook_evs_5, codebook_eqls_1_4) %>%
  mutate(target_var = NA)

rio::export(codebook_all, "templates/codebook_ess_evs_eqls_all.xlsx")


### create cross-walk table ----------------------

create_cwt <- function(data) {
  
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(var_name = X1, var_label = X2)
  
  freqs_cwt <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    #keep(function(x) nrow(x) < 1000) %>%
    do.call(rbind, .) %>% 
    tibble::rownames_to_column(var = "var_name_value") %>%
    mutate(var_name = gsub("(.+?)(\\..*)", "\\1", var_name_value),
           value = gsub("^[^.]*.","",var_name_value),
           value_code = sub(".*\\[(.+)\\].*", "\\1", var_name_value, perl = TRUE),
           value_code = ifelse(str_sub(var_name_value, -2, -1) == "NA", "NA", value_code),
           value_code = ifelse(gsub(" ", "", fixed = TRUE, var_name_value) == var_name_value, 
                               gsub("^[^.]*.","",var_name_value), value_code)) %>%
    group_by(var_name) %>%
    mutate(npos = row_number(),
           value_n = paste(value, n, sep = ": ")) %>%
    select(npos, var_name, value_n, value, value_code)
  
  full_join(var_labels, freqs_cwt, by = "var_name")
  
}

codebook_all1 <- rio::import("templates/codebook_ess_evs_eqls_all.xlsx") %>%
  filter(!is.na(target_var))


### label target variables -------------

rio::export(unique(codebook_all1$target_var), "templates/var_names_labels.xlsx")


### create CWT from several tables -----------------

data_tables <- c("EVS_1_4", "EVS_5", "EQLS_1_4", "ESS_1_8")
cwt_all <- list()

for (i in (1:length(data_tables))) {

  table_name_input <- data_tables[i]
  var_names <- codebook_all1$var_name[codebook_all1$table_name == table_name_input]
  
  cwt_all[[i]] <- eval(parse(text = tolower(table_name_input))) %>%
    select(var_names) %>%
    create_cwt() %>%
    mutate(table_name = table_name_input)
}

cwt_all <- do.call(rbind, cwt_all) 

cwt_all <- cwt_all %>%
  select(-var_label) %>%
  left_join(codebook_all1, by = c("var_name", "table_name")) %>%
  mutate(target_value = NA) %>%
  select(table_name, target_var, 
         var_name, var_label, starts_with("c_"),
         value_n, value_code, target_value)


rio::export(cwt_all, "templates/cwt_ess_evs_eqls_all.xlsx")



### import filled-out cwt and harmonize! ----------------------

cwt_all1 <- rio::import("templates/cwt_ess_evs_eqls_all.xlsx")

harmonize <- function(table_name_input, cwt_name_input) {
  
  target_vars <- unique(eval(parse(text = tolower(cwt_name_input)))$target_var[eval(parse(text = tolower(cwt_name_input)))$table_name == table_name_input])
  source_vars <- unique(eval(parse(text = tolower(cwt_name_input)))$var_name[eval(parse(text = tolower(cwt_name_input)))$table_name == table_name_input])
  
  data_small <- eval(parse(text = tolower(table_name_input))) %>%
    select(basic_vars, source_vars) %>%
    zap_labels()
  
  harmonized_vars <- list()
  
  for (i in 1:length(target_vars)) {
    
    target_var_input = target_vars[i]
    
    source <- eval(parse(text = tolower(cwt_name_input))) %>% 
      filter(target_var == target_var_input, table_name == table_name_input) %>%
      pull(value_code)
    
    target <- eval(parse(text = tolower(cwt_name_input))) %>% 
      filter(target_var == target_var_input, table_name == table_name_input) %>%
      pull(target_value)
    
    source_var_name <- eval(parse(text = tolower(cwt_name_input))) %>%
      filter(target_var == target_var_input, table_name == table_name_input) %>%
      pull(var_name) %>% .[1]
    
    harmonized_vars[[i]] <- data_small %>%
      mutate(!!target_var_input := as.numeric(plyr::mapvalues(as.character(get(source_var_name)), 
                                                              source, target))) %>%
      select(basic_vars, target_var_input)
  }
  
  Reduce(merge,harmonized_vars)
  
}

evs_5_h <- harmonize("EVS_5", "cwt_all1")
rm(evs_5) # remove original data files
evs_1_4_h <- harmonize("EVS_1_4", "cwt_all1")
rm(evs_1_4)
ess_1_8_h <- harmonize("ESS_1_8", "cwt_all1")
rm(ess_1_8)
eqls_1_4_h <- harmonize("EQLS_1_4", "cwt_all1")
rm(eqls_1_4)


### create single harmonized dataset ------------------

all_data <- bind_rows(evs_5_h, evs_1_4_h,
                      ess_1_8_h, eqls_1_4_h) %>%
  mutate(t_project = sub("\\_.*", "", table_name),
         isced = coalesce(isced, isced1)) %>%
  select(table_name, starts_with("t_"), starts_with("s_"), 
         age, female = sex, school_years, educ3, isced, everything(), 
         -starts_with("hhinc"), -isced1)

skimr::skim(all_data)

all_data %>% group_by(t_project) %>% summarise(max(isced, na.rm = TRUE))

write.csv(all_data, file=gzfile("data/all_data.csv.gz"), row.names = FALSE)
all_data <- rio::import("data/all_data.csv.gz")



### value labels

val_labels(all_data$female) <- c(female = 1, male = 0, missing = NA)
val_labels(all_data$educ3) <- c(lowest = 1, medium = 2, highest = 3, missing = NA)

labels_0_10 <- names(all_data %>% select(matches("trust"), sat_life))
val_labels(all_data[,labels_0_10]) <- c("lowest" = 0, missing = NA)

val_labels(all_data$isced) <- c("primary or less" = 1,
                                "lower secondary" = 2,
                                "upper secondary" = 3,
                                "post-sec non-tertiary" = 4,
                                "BA or equivalent" = 5,
                                "MA, PhD, or equivalent" = 6,
                                missing = NA)


var_label(all_data$female) <- "Gender Female"
var_label(all_data$age) <- "Age years"
var_label(all_data$educ3) <- "Education 3 categories"
var_label(all_data$isced) <- "Education ISCED-like"
var_label(all_data$school_years) <- "Schooling years"
var_label(all_data$sat_life) <- "Satisfaction with life"
var_label(all_data$table_name) <- "Name of data table"
var_label(all_data$t_country) <- "Country ISO2c"
var_label(all_data$s_id) <- "Source case ID"
var_label(all_data$t_id) <- "Case ID"
var_label(all_data$t_weight) <- "Weighting factor"
var_label(all_data$t_round) <- "Project round"
var_label(all_data$t_year) <- "Survey year"
var_label(all_data$t_project) <- "Project abbrev."

var_label(all_data$soc_trust) <- "Social trust "

var_label(all_data$trust_church) <- "Trust Church"
var_label(all_data$trust_army) <- "Trust Army"
var_label(all_data$trust_educ) <- "Trust Education system"
var_label(all_data$trust_press) <- "Trust Press"
var_label(all_data$trust_tradeu) <- "Trust Trade unions"
var_label(all_data$trust_police) <- "Trust Police"
var_label(all_data$trust_parl) <- "Trust Parliament"
var_label(all_data$trust_civserv) <- "Trust Civil service"
var_label(all_data$trust_socsec) <- "Trust Social security"
var_label(all_data$trust_eu) <- "Trust European Union"
var_label(all_data$trust_un) <- "Trust United Nations"
var_label(all_data$trust_health) <- "Trust Healthcase system"
var_label(all_data$trust_jus) <- "Trust Legal system"
var_label(all_data$trust_companies) <- "Trust Major companies"
var_label(all_data$trust_envorg) <- "Trust Environmental organizations"
var_label(all_data$trust_polpart) <- "Trust Political parties"
var_label(all_data$trust_gov) <- "Trust Government"
var_label(all_data$trust_socmedia) <- "Trust Social media"
var_label(all_data$trust_nato) <- "Trust NATO"
var_label(all_data$trust_politicians) <- "Trust Politicians"
var_label(all_data$trust_ep) <- "Trust European Parliament"
var_label(all_data$trust_local) <- "Trust Local authorities"
var_label(all_data$trust_banks) <- "Trust Banks"
var_label(all_data$trust_charity) <- "Trust Charity organizations"
var_label(all_data$trust_pension) <- "Trust Pension system"
var_label(all_data$trust_socbenefit) <- "Trust Social benefits system"

write_sav(all_data, "C:/Users/mkolc/Google Drive/Work in progress/ITD/ITD_data_v_1_0.sav")

alldata1 <- read_sav("C:/Users/mkolc/Google Drive/Work in progress/ITD/ITD_data_v_1_0.sav")
codebook_itd <- create_codebook(alldata1)

skimr::skim(alldata1)
table(alldata1$educ3, exclude = "none")

rio::export(codebook_itd, "C:/Users/mkolc/Google Drive/Work in progress/ITD/ITD_target_codebook_v_1_0.xlsx")

### survey-year availability of trust items ---------------

table_trust_survey <- all_data %>%
  group_by(t_project, t_round, t_country, t_year) %>%
  summarise_at(vars(starts_with("trust")), funs(weighted.mean(., w = t_weight, na.rm = TRUE)))

### list of trust items by survey -----------------

list_surveys_trust <- table_trust_survey %>%
  gather(variable, value, 5:30, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(rio::import("data/var_names_labels.csv"))


### plot: availability of trust items -----------------

table_trust_survey %>%
  gather(variable, value, 5:30, na.rm = TRUE) %>%
  left_join(rio::import("data/var_names_labels.csv"), by = "variable") %>%
  group_by(label) %>%
  mutate(countn = n()) %>%
  count(t_project, label, countn) %>%
  ggplot(., aes(x = reorder(label, countn), y = n, fill = t_project)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("") +
  ylab("Number of surveys") +
  scale_fill_manual(name = "Project",
                     values = cbPalette[1:3]) +
  coord_flip()








### OLD CODE ----------------------------

### country-year availability of trust in police -----------------

country_year_police <- all_data %>%
  group_by(Tcountry, Tyear, Tproject) %>%
  summarise(trust_police = mean(trust_police, na.rm = TRUE)) %>%
  filter(!is.na(trust_police)) %>%
  spread(Tproject, trust_police) %>%
  mutate(EQLS = ifelse(!is.na(EQLS), "EQLS", ""),
         ESS = ifelse(!is.na(ESS), "ESS", ""),
         EVS = ifelse(!is.na(EVS), "EVS", ""),
         av = trimws(paste(EQLS, ESS, EVS), "left")) %>%
  select(Tcountry, Tyear, av) %>%
  spread(Tyear, av)
  

a <- all_data %>%
  group_by(Tcountry, Tyear, Tproject) %>%
  summarise(trust_police = mean(trust_police, na.rm = TRUE)) %>%
  filter(!is.na(trust_police))

### trust in police: comparison of aggregation (with CIs) ----------

library(reshape2)

dodge <- position_dodge(width=0.5)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


all_data %>%
  filter(!is.na(trust_police)) %>%
  group_by(Tproject, Tcountry, Tyear) %>%
  mutate(prop1 = ifelse(trust_police > 5, 1, 0),
         prop2 = ifelse(trust_police >= 5, 1, 0)) %>%
  summarise(nobs = sum(Tweight),
            me_prop1 = weighted.mean(prop1, w = Tweight, na.rm = TRUE)*10,
            me_prop2 = weighted.mean(prop2, w = Tweight, na.rm = TRUE)*10,
            me_mean = weighted.mean(trust_police, w = Tweight, na.rm = TRUE),
            se_prop1 = sqrt(weighted.mean(prop1, Tweight, na.rm = TRUE) * 
                                   (1 - weighted.mean(prop1, Tweight, na.rm = TRUE))/
                                   n())*10,
            se_prop2 = sqrt(weighted.mean(prop2, Tweight, na.rm = TRUE) * 
                              (1 - weighted.mean(prop1, Tweight, na.rm = TRUE))/
                              n())*10,
            se_mean = radiant.data::weighted.sd(trust_police, w = Tweight, na.rm = TRUE)/sqrt(nobs)) %>%
  gather(variable, value, 5:10) %>%
  mutate(stat = substr(variable, 1, 2),
         type = substr(variable, 4, 8)) %>%
  select(-variable) %>%
  spread(stat, value) %>%
  filter(!is.na(me)) %>%
  mutate(type = plyr::mapvalues(type, c("prop1", "prop2", "mean"),
                                c("Proportion > 5", "Proportion >= 5",
                                  "Mean (0-10)"))) %>%
  group_by(Tcountry, Tyear, type) %>%
  mutate(counts = n(),
         country = countrycode(Tcountry, "iso2c", "country.name")) %>%
  filter(counts > 1) %>%
  ggplot(., aes(x = reorder(paste(country, Tyear), desc(paste(country, Tyear))), 
                y = me, col = Tproject)) +
    #geom_line(aes(group = paste(country, Tyear))) +
    #geom_point(size = 2) +
    geom_pointrange(aes(ymin = me - 1.96*se, 
                        ymax = me + 1.96*se),
                    size = 0.5, alpha = 0.7) +
    xlab("") + ylab("Aggregate country-year level") +
    ylim(0,10) +
    coord_flip() +
    theme_minimal() +
    scale_color_manual(name = "Project",
                     values = cbPalette[1:3]) +
    ggtitle("Country-year levels of trust in the police by aggregate type") +
    labs(caption = "Data souce: ESS 1-8, EQLS 1-4, EVS 1-5.
         Proportions (second and third facet) multiplied by 10 for comparability.") +
    facet_wrap("type")
  


### trust in police: comparison of aggregation ----------

all_data %>%
  group_by(t_project, t_country, t_year) %>%
  summarise(`Proportion > 5` = weighted.mean(ifelse(t_trust_police > 5, 1, 0)*10, 
                                             w = t_weight, na.rm = TRUE),
            `Proportion >= 5` = weighted.mean(ifelse(t_trust_police >= 5, 1, 0)*10, 
                                              w = t_weight, na.rm = TRUE),
            `Mean (0-10)` = weighted.mean(t_trust_police, w = t_weight, na.rm = TRUE)) %>%
  gather(variable, value, 4:6) %>%
  filter(!is.na(value)) %>%
  group_by(t_country, t_year, variable) %>%
  mutate(counts = n()) %>%
  filter(counts > 1) %>%
  ggplot(., aes(x = reorder(paste(t_country, t_year), desc(paste(t_country, t_year))), y = value)) +
  geom_line(aes(group = paste(t_country, t_year))) +
  geom_point(aes(col = t_project), size = 2) +
  xlab("") + ylab("Aggregate country-year level") +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(name = "Project",
                     values = cbPalette[1:3]) +
  theme(legend.position="bottom") +
  labs(caption = "Data souce: ESS 1-8, EQLS 1-4, EVS 1-5.
       Proportions (second and third facet) multiplied by 10 for comparability.") +
  facet_wrap("variable")


### trust in police: Poland -------------

all_data %>%
  filter(t_country == "PL") %>%
  group_by(t_project, t_year) %>%
  summarise(mean = weighted.mean(t_trust_police, w = t_weight, na.rm = TRUE)) %>%
  filter(!is.na(mean)) %>%
  ggplot(., aes(x = t_year, y = mean, group = t_project, col = t_project)) +
  geom_line(size = 1) +
  expand_limits(y = 0) +
  theme_bw()
