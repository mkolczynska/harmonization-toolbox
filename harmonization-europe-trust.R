# Packages ----------------

library(tidyverse)
library(haven)
library(labelled)
library(countrycode)

###
sessionInfo()

### opening the data and creating basic technical variables -----------

# EVS 2017 (ZA7500_v1-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
# EVS 1981-2008 (ZA4804_v3-0-0.sav.zip): https://dbk.gesis.org/dbksearch/GDESC2.asp?no=0009&DB=E
# ESS 1-8 (ESS1-8e01.zip): https://www.europeansocialsurvey.org/downloadwizard/
# EQLS (eqls_integrated_trend_2003-2016.sav): https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7348

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



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


eqls_1_4 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/EQLS/eqls_integrated_trend_2003-2016.zip", user_na = TRUE) %>%
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

evs_1981_2008 <- lapply(evs_1981_2008, Int2Factor) %>% as.data.frame(., stringsAsFactors = FALSE)


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


lists <- create_surveys_list(c("eqls_1_4", "evs_2017", "evs_1981_2008", "ess_1_8"))
rio::export(lists, "templates/surveys_list.xlsx")


### creating codebooks ----------------------

create_codebook <- function(data) {
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(varname = X1, varlabel = X2)
  
  freqs <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    keep(function(x) nrow(x) < 1000) %>%
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
  
  full_join(var_labels, freqs, by = "varname")
}



codebook_evs_2017 <- create_codebook(evs_2017) %>%
  mutate(t_table_name = "EVS_2017")

codebook_evs_1981_2008 <- create_codebook(evs_1981_2008) %>%
 mutate(t_table_name = "EVS_1981_2008")

codebook_eqls_1_4 <- create_codebook(eqls_1_4) %>%
  mutate(t_table_name = "EQLS_1_4")

codebook_ess_1_8 <- create_codebook(ess_1_8) %>%
  mutate(t_table_name = "ESS_1_8")

codebook_all <- bind_rows(codebook_ess_1_8, codebook_evs_1981_2008, 
                          codebook_evs_2017, codebook_eqls_1_4) %>%
  mutate(target_var = NA)

rio::export(codebook_all, "codebook_all.xlsx")


### create cross-walk table ----------------------

create_cwt <- function(data) {
  
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(varname = X1, varlabel = X2)
  
  freqs_cwt <- lapply(data, function(x) { return(questionr::freq(x)) }) %>%
    keep(function(x) nrow(x) < 1000) %>%
    do.call(rbind, .) %>% 
    tibble::rownames_to_column(var = "varname_value") %>%
    mutate(varname = gsub("(.+?)(\\..*)", "\\1", varname_value),
           value = gsub("^[^.]*.","",varname_value),
           value_code = sub(".*\\[(.+)\\].*", "\\1", varname_value, perl = TRUE),
           value_code = ifelse(str_sub(varname_value, -2, -1) == "NA", "NA", value_code),
           value_code = ifelse(gsub(" ", "", fixed = TRUE, varname_value) == varname_value, 
                               gsub("^[^.]*.","",varname_value), value_code)) %>%
    group_by(varname) %>%
    mutate(npos = row_number(),
           value_n = paste(value, n, sep = ": ")) %>%
    select(npos, varname, value_n, value, value_code)
  
  full_join(var_labels, freqs_cwt, by = "varname")
  
}

codebook_all1 <- rio::import("templates/codebook_all1.xlsx") %>%
  filter(!is.na(target_var))


### label target variables -------------

rio::export(unique(codebook_all1$target_var), "templates/var_names_labels.xlsx")


### create CWT from several tables -----------------

data_tables <- c("EVS_2017", "EVS_1981_2008", "EQLS_1_4", "ESS_1_8")
cwt_all <- list()

for (i in (1:length(data_tables))) {

  table_name_input <- data_tables[i]
  varnames <- codebook_all1$varname[codebook_all1$t_table_name == table_name_input]
  
  cwt_all[[i]] <- eval(parse(text = tolower(table_name_input))) %>%
    select(varnames) %>%
    create_cwt() %>%
    mutate(t_table_name = table_name_input)
}

cwt_all <- do.call(rbind, cwt_all) 

cwt_all %>%
  select(-varlabel) %>%
  left_join(codebook_all1, by = c("varname", "t_table_name")) %>%
  mutate(target_value = NA) %>%
  select(t_table_name, target_var, 
         varname, varlabel, starts_with("c_"),
         value_n, value_code, target_value) %>%
  rio::export(., "cwt_all.xlsx")



### import filled-out cwt and harmonize! ----------------------

cwt_all1 <- rio::import("templates/cwt_all1.xlsx")


harmonize <- function(table_name_input, cwt_name_input) {
  
  target_vars <- unique(eval(parse(text = tolower(cwt_name_input)))$target_var[eval(parse(text = tolower(cwt_name_input)))$t_table_name == table_name_input])
  source_vars <- unique(eval(parse(text = tolower(cwt_name_input)))$varname[eval(parse(text = tolower(cwt_name_input)))$t_table_name == table_name_input])
  
  data_small <- eval(parse(text = tolower(table_name_input))) %>%
    select(basic_vars, source_vars) %>%
    zap_labels()
  
  harmonized_vars <- list()
  
  for (i in 1:length(target_vars)) {
    
    target_var_input = target_vars[i]
    
    source <- eval(parse(text = tolower(cwt_name_input))) %>% 
      filter(target_var == target_var_input, t_table_name == table_name_input) %>%
      pull(value_code)
    
    target <- eval(parse(text = tolower(cwt_name_input))) %>% 
      filter(target_var == target_var_input, t_table_name == table_name_input) %>%
      pull(target_value)
    
    source_varname <- eval(parse(text = tolower(cwt_name_input))) %>%
      filter(target_var == target_var_input, t_table_name == table_name_input) %>%
      pull(varname) %>% .[1]
    
    harmonized_vars[[i]] <- data_small %>%
      mutate(!!target_var_input := as.numeric(plyr::mapvalues(as.character(get(source_varname)), 
                                                              source, target))) %>%
      select(basic_vars, target_var_input)
  }
  
  Reduce(merge,harmonized_vars)
  
}

evs_2017_h <- harmonize("EVS_2017", "cwt_all1")
rm(evs_2017) # remove original data files
evs_1981_2008_h <- harmonize("EVS_1981_2008", "cwt_all1")
rm(evs_1981_2008)
ess_1_8_h <- harmonize("ESS_1_8", "cwt_all1")
rm(ess_1_8)
eqls_1_4_h <- harmonize("EQLS_1_4", "cwt_all1")
rm(eqls_1_4)


### create single harmonized dataset ------------------

all_data <- bind_rows(evs_2017_h, evs_1981_2008_h,
                      ess_1_8_h, eqls_1_4_h) %>%
  mutate(t_project = sub("\\_.*", "", t_table_name))

rio::export(all_data, "data/all_data.csv")
write.csv(all_data, file=gzfile("data/all_data.csv.gz"))
all_data <- rio::import("data/all_data.csv.gz")



### survey-year availability of trust items ---------------

table_trust_survey <- all_data %>%
  group_by(t_project, t_round, t_country, t_year) %>%
  summarise_at(vars(starts_with("t_trust")), funs(weighted.mean(., w = t_weight, na.rm = TRUE)))

### list of trust items by survey -----------------

list_surveys_trust <- all_data %>%
  group_by(t_project, t_round, t_country, t_year) %>%
  summarise_at(vars(starts_with("t_trust")), funs(weighted.mean(., w = t_weight, na.rm = TRUE))) %>%
  gather(variable, value, 5:32, na.rm = TRUE) %>%
  ungroup() %>%
  full_join(rio::import("templates/var_names_labels.xlsx"))


### plot: availability of turst items -----------------

all_data %>%
  group_by(t_project, t_round, t_country, t_year) %>%
  summarise_at(vars(starts_with("t_trust")), funs(weighted.mean(., w = t_weight, na.rm = TRUE))) %>%
  gather(variable, value, 5:32, na.rm = TRUE) %>%
  filter(variable != "t_trust_soc") %>%
  ungroup() %>%
  left_join(rio::import("paper/var_names_labels.csv")) %>%
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



### country-year availability of trust in police -----------------

country_year_police <- all_data %>%
  group_by(t_country, t_year, t_project) %>%
  summarise(trust_police = mean(t_trust_police, na.rm = TRUE)) %>%
  filter(!is.na(trust_police)) %>%
  spread(t_project, trust_police) %>%
  mutate(EQLS = ifelse(!is.na(EQLS), "EQLS", ""),
         ESS = ifelse(!is.na(ESS), "ESS", ""),
         EVS = ifelse(!is.na(EVS), "EVS", ""),
         av = trimws(paste(EQLS, ESS, EVS), "left")) %>%
  select(t_country, t_year, av) %>%
  spread(t_year, av)
  



### trust in police: comparison of aggregation (with CIs) ----------

library(reshape2)

dodge <- position_dodge(width=0.5)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


all_data %>%
  filter(!is.na(t_trust_police)) %>%
  group_by(t_project, t_country, t_year) %>%
  mutate(prop1 = ifelse(t_trust_police > 5, 1, 0),
         prop2 = ifelse(t_trust_police >= 5, 1, 0)) %>%
  summarise(nobs = sum(t_weight),
            me_prop1 = weighted.mean(prop1, w = t_weight, na.rm = TRUE)*10,
            me_prop2 = weighted.mean(prop2, w = t_weight, na.rm = TRUE)*10,
            me_mean = weighted.mean(t_trust_police, w = t_weight, na.rm = TRUE),
            se_prop1 = sqrt(weighted.mean(prop1, t_weight, na.rm = TRUE) * 
                                   (1 - weighted.mean(prop1, t_weight, na.rm = TRUE))/
                                   n())*10,
            se_prop2 = sqrt(weighted.mean(prop2, t_weight, na.rm = TRUE) * 
                              (1 - weighted.mean(prop1, t_weight, na.rm = TRUE))/
                              n())*10,
            se_mean = weighted.sd(t_trust_police, w = t_weight, na.rm = TRUE)/sqrt(nobs)) %>%
  gather(variable, value, 5:10) %>%
  mutate(stat = substr(variable, 1, 2),
         type = substr(variable, 4, 8)) %>%
  select(-variable) %>%
  spread(stat, value) %>%
  filter(!is.na(me)) %>%
  mutate(type = plyr::mapvalues(type, c("prop1", "prop2", "mean"),
                                c("Proportion > 5", "Proportion >= 5",
                                  "Mean (0-10)"))) %>%
  group_by(t_country, t_year, type) %>%
  mutate(counts = n()) %>%
  filter(counts > 1) %>%
  ggplot(., aes(x = reorder(paste(t_country, t_year), desc(paste(t_country, t_year))), 
                y = me, col = t_project)) +
    #geom_line(aes(group = paste(t_country, t_year))) +
    #geom_point(size = 2) +
    geom_pointrange(aes(ymin = me - 1.96*se, 
                        ymax = me + 1.96*se),
                    size = 0.5, alpha = 0.7) +
    xlab("") + ylab("Aggregate country-year level") +
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
