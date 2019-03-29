library(tidyverse)
library(haven)
library(labelled)
library(countrycode)
library(vtable)

vtable(evs_1981_2008_h)

###
sessionInfo()

### opening the data and creating basic technical variables -----------

basic_vars <- c("t_country", "s_id", "t_id", "t_weight", "t_round", "t_table_name", "t_year")

evs_2017 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/ZA7500_v1-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = c_abrv,
         t_id = row_number(),
         s_id = as.character(id_cocas),
         t_weight = 1,
         t_round = 5,
         t_table_name = "EVS_2017",
         t_year = year,
         t_project = "EVS")

evs_1981_2008 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/ZA4804_v3-0-0.sav.zip", user_na = TRUE) %>%
  mutate(t_country = S009,
         t_id = row_number(),
         s_id = as.character(S006),
         t_weight = S017,
         t_round = S002EVS,
         t_table_name = "EVS_1981_2008",
         t_year = S020,
         t_project = "EVS")

ess_1_8 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/ESS1-8e01.zip", user_na = TRUE) %>%
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


eqls_1_4 <- haven::read_sav("C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/eqls_integrated_trend_2003-2016.sav", user_na = TRUE) %>%
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
      mutate(t_survey = paste(t_project, t_round, t_country, sep = "")) %>%
      count(t_survey, t_year)
  }
  
  do.call(rbind, surveys_list)
}


lists <- create_surveys_list(c("eqls_1_4", "evs_2017", "evs_1981_2008", "ess_1_8"))



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
  
  full_join(var_labels, freqs, by = "varname") %>%
    select(t_table_name, varname, varlabel, valfreqs, target_var)
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


codebook_all1 <- rio::import("codebook_all1.xlsx") %>%
  filter(!is.na(target_var))



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

cwt_all1 <- rio::import("cwt_all1.xlsx")



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
rm(evs_2017)
evs_1981_2008_h <- harmonize("EVS_1981_2008", "cwt_all1")
rm(evs_1981_2008)
ess_1_8_h <- harmonize("ESS_1_8", "cwt_all1")
rm(ess_1_8)
eqls_1_4_h <- harmonize("EQLS_1_4", "cwt_all1")
rm(eqls_1_4)



all_data <- bind_rows(evs_2017_h, evs_1981_2008_h,
                      ess_1_8_h, eqls_1_4_h) %>%
  mutate(t_project = sub("\\_.*", "", t_table_name))

rio::export(all_data, "all_data.csv")

a <- all_data %>%
  group_by(t_project, t_country, t_year) %>%
  count() %>%
  group_by(t_project) %>%
  count(t_project)



d <- all_data %>%
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
  



all_data_cntry <- all_data %>%
  group_by(t_project, t_country, t_year) %>%
  summarise_at(vars(starts_with("t_trust")), list(~weighted.mean(., w = t_weight, na.rm = TRUE))) %>%
  gather(variable, value, 4:31) %>%
  filter(!is.na(value))

plot1 <- all_data_cntry %>%
  filter(variable == "t_trust_parl") %>%
  group_by(t_country, t_year) %>%
  mutate(nc = n()) %>%
  filter(nc > 1) %>%
  ggplot(., aes(x = reorder(paste(t_country, t_year), desc(paste(t_country, t_year))), y = value)) +
  geom_line(aes(group = paste(t_country, t_year))) +
  geom_point(aes(col = t_project), size = 2) +
  xlab("") + ylab("Mean trust in parliament") +
  coord_flip() +
  theme_minimal() +
  guides(color=guide_legend(title="Project"))


plot2 <- all_data %>%
  group_by(t_project, t_country, t_year) %>%
  summarise(trust_parl = weighted.mean(ifelse(t_trust_parl >= 5, 1, 0), 
                                       w = t_weight, na.rm = TRUE)) %>%
  group_by(t_country, t_year) %>%
  mutate(nc = n()) %>%
  filter(nc > 1) %>%
  ggplot(., aes(x = reorder(paste(t_country, t_year), desc(paste(t_country, t_year))), y = trust_parl)) +
  geom_line(aes(group = paste(t_country, t_year))) +
  geom_point(aes(col = t_project), size = 2) +
  xlab("") + ylab("Proportion trusting in parliament (>= 5)") +
  coord_flip() +
  theme_minimal() +
  guides(color=guide_legend(title="Project"))

plot3 <- all_data %>%
  group_by(t_project, t_country, t_year) %>%
  summarise(trust_parl = weighted.mean(ifelse(t_trust_parl > 5, 1, 0), 
                                       w = t_weight, na.rm = TRUE)) %>%
  group_by(t_country, t_year) %>%
  mutate(nc = n()) %>%
  filter(nc > 1) %>%
  ggplot(., aes(x = reorder(paste(t_country, t_year), desc(paste(t_country, t_year))), y = trust_parl)) +
  geom_line(aes(group = paste(t_country, t_year))) +
  geom_point(aes(col = t_project), size = 2) +
  xlab("") + ylab("Proportion trusting in parliament (> 5)") +
  coord_flip() +
  theme_minimal() +
  guides(color=guide_legend(title="Project"))

library(ggpubr)

gridExtra::grid.arrange(plot1, plot2, plot3, nrow = 1)


ggarrange(plot1, plot2, plot3, nrow = 1, ncol = 3, common.legend = TRUE, legend="bottom") %>%
  annotate_figure(., top = text_grob("Mean trust in parliament", color = "black", face = "bold", size = 12))




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
  guides(color=guide_legend(title="Project")) +
  ggtitle("Country-year levels of trust in the police by aggregate type") +
  labs(caption = "Data souce: ESS 1-8, EQLS 1-4, EVS 1-5.
       Proportions (second and third facet) multiplied by 10 for comparability.") +
  facet_wrap("variable")
  
  
