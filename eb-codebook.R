library(tidyverse) # for manipulating data
library(haven) # for opening SPSS files
library(labelled) # for using labels
library(questionr) # for getting frequencies
library(rio) # for exporting to different formats; here: XLSX

# path to folder with all ISSP data files
path <- "C:/Users/mkolc/Google Drive/Work in progress/R playground/data-for-harm/EB"
temp <- list.files(path = path, pattern = "*.sav")
f <- file.path(path, temp)

eb_all <- lapply(f, haven::read_sav, user_na = TRUE)
names(eb_all) <- temp

for(i in temp) { 
  eb_all[[i]]$Source <- i
  var_label(eb_all[[i]]$Source) <- substr(i, 1, 13)
  eb_all[[i]] <- eb_all[[i]] %>% dplyr::select(Source, everything())
}


### codebook function
create_codebook_eb <- function(data) {
  
  var_labels <- data.frame(cbind(names(var_label(data)), do.call(rbind, var_label(data)))) %>%
    rename(varname = X1, varlabel = X2)
  
  name_of_file <- as.character(var_labels[1,2])
  
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
    mutate(file_name = name_of_file) %>%
    filter(varname != "Source")
}

file_names <- read.csv(paste(path, "/eb-file-names.csv", sep = ""))

eb_all_codebooks <- lapply(eb_all, create_codebook_eb)

eb_all_codebooks_df <- do.call(rbind, eb_all_codebooks)

export(eb_all_codebooks_df, "codebook_eb_all.xlsx")
