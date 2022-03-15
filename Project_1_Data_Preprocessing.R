library(tidyr)
library(dplyr)
library(ggmap)
library(stringr)
#library(remotes)
#install_version("hashmap", "0.2.2")
library(hashmap)
source("Usefull_functions.R")

new_df <- read.csv("LCA_Disclosure_Data_FY2021_Q3.csv", na.strings = c("", "NA"))
new_df$EMPLOYER_NAME = str_to_lower(new_df$EMPLOYER_NAME)
new_df$EMPLOYER_NAME = str_trim(new_df$EMPLOYER_NAME)
new_df$JOB_TITLE = str_to_lower(new_df$JOB_TITLE)
new_df$JOB_TITLE = str_trim(new_df$JOB_TITLE)


new_df %>%    
  filter(FULL_TIME_POSITION == 'Y') %>%
  group_by(ORIGINAL_CERT_DATE) %>%
  summarise(COUNT = n()) %>%
  arrange(desc(COUNT)) -> na_orig_date

new_df %>%    
  filter(FULL_TIME_POSITION == 'Y') %>%
  group_by(EMPLOYER_PROVINCE) %>%
  summarise(COUNT = n()) %>%
  arrange(desc(COUNT)) -> na_empl_province

h1b_df = new_df %>%
  transmute(CASE_NUMBER = CASE_NUMBER,
         CASE_STATUS = CASE_STATUS,
         EMPLOYER_NAME = EMPLOYER_NAME,
         SOC_NAME = SOC_TITLE,
         DECISION_DATE = DECISION_DATE,
         SOC_CODE = SOC_CODE,
         JOB_TITLE = JOB_TITLE,
         FULL_TIME_POSITION = FULL_TIME_POSITION,
         PREVAILING_WAGE = PREVAILING_WAGE,
         PW_UNIT_OF_PAY = PW_UNIT_OF_PAY,
         WORKSITE_CITY = WORKSITE_CITY,
         WORKSITE_STATE = WORKSITE_STATE)

h1b_df <- h1b_df %>% drop_na()

## Adding the column year
h1b_df <- h1b_df %>%
  separate(DECISION_DATE, c("date","month", "year"), "/") %>% 
  mutate(YEAR=as.integer(year))

## Removing extra columns created
drops <- c("date", "month", "year")
h1b_df <- h1b_df[, !(names(h1b_df) %in% drops)]

## City Cleaning
h1b_df <- h1b_df %>%
  mutate(WORKSITE_CITY = str_to_title(str_trim(str_replace_all(WORKSITE_CITY, "[[:punct:]]", ""))))

state_abbs = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
               "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
               "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
               "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
               "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

state_full = str_to_title(c("alaska","alabama","arkansas","arizona","california","colorado",
               "connecticut","district of columbia","delaware","florida","georgia",
               "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
               "louisiana","massachusetts","maryland","maine","michigan","minnesota",
               "missouri","mississippi","montana","north carolina","north dakota",
               "nebraska","new hampshire","new jersey","new mexico","nevada",
               "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
               "rhode island","south carolina","south dakota","tennessee","texas",
               "utah","virginia","vermont","washington","wisconsin",
               "west virginia","wyoming"))

state_hash = hashmap(state_abbs,state_full)

# Adding a new column based on full state name
h1b_df$WORKSITE_STATE_FULL = sapply(h1b_df$WORKSITE_STATE, function(x,y) {return(toupper(y[[x]]))}, y = state_hash)

h1b_df %>%
  group_by(WORKSITE_CITY) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> sites_city_count

site_city_hash <- hashmap(sites_city_count$WORKSITE_CITY, sites_city_count$count)

## Checking spellings
insert_letters <- function(split_left,split_right, i, letters) {
  return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = split_right[i])))
}
# Getting inserts of a single letter

delete_letters <- function(split_left,split_right, i) {
  return(paste0(split_left[i], substr(split_right[i],2,nchar(split_right[i]))))
}
# Deletes one letter from word

replace_letters <- function(split_left,split_right, i,letters) {
  if(!is.null(split_right[i]) &  nchar(split_right[i]) > 0) {
    return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = substr(split_right[i],2,nchar(split_right[i])))))
  }
  return(NULL)
}
# Replace a letter by a-z or space

position_letters <- function(split_left, split_right,i) {
  if(!is.null(split_right[i]) & nchar(split_right[i]) > 1) {
    return(paste0(split_left[i],substr(split_right[i],2,2),substr(split_right[i],1,1),substr(split_right[i],3,nchar(split_right[i]))))
  }
  return(NULL)
}
#Interchanges the positions of adjacent letters

# All edits that are one edit away from site
edit_site <- function(site) {
  letters = toupper(strsplit("abcdefghijklmnopqrstuvwxyz ",split='')[[1]])
  site_len <- nchar(site)
  #print(site_len)
  if(site_len < 4) {
    return(site)
  }
  split_left <- sapply(seq(0,site_len), substr,x = site,start = 1)
  split_right <- sapply(seq(1,site_len+1), substr,x = site,stop = site_len)
  deletes <- sapply(seq(1,site_len+1),delete_letters, split_left = split_left, split_right = split_right)
  transposes <- unlist(sapply(seq(1,site_len+1),position_letters, split_left = split_left, split_right = split_right))
  replaces <- unlist(sapply(seq(1,site_len+1),replace_letters, split_left = split_left, split_right = split_right, letters=letters))
  inserts <- unlist(sapply(seq(1,site_len+1),insert_letters, split_left = split_left, split_right = split_right,letters = letters))
  
  return(unique(c(deletes,transposes,replaces,inserts)))
}

# All edits that are two edits away from `word`
edit_word <- function(site) {
  edits1_sites = edit_site(site)
  return (unlist(sapply(edits1_sites, edit_site)))
}

# Probability of site in the dataset
probability_site <- function(site, site_hash) {
  return(site_hash[[site]])
}

# The subset of candidate sites that appear in the dictionary of sites
cand_site <- function(sites,site_hash = site_hash) {
  return(sites[site_hash$has_keys(sites)])
}

# Generate possible(if any) spelling corrections for word
find_candidates <- function(site,...) {
  return(c(cand_site(site,...), cand_site(edit_site(site),...), c(site)))
}

# Correction for the site
correct_spell <- function(site,...) {
  candidates = find_candidates(site,...)
  best_candi = candidates[which.max(sapply(candidates,probability_site, ...))]
  
  return(best_candi)
}

site_count <- function(site, site_hash) {
  
  if(site_hash$has_key(site)) {
    return(site_hash[[site]])
  }
  return(site)
}

sites_city <- sites_city_count$WORKSITE_CITY
sites_city_before <- c()
sites_city_after <- c()

count <- 0
for(site in sites_city) {
  # Count of current Worksite
  curr_count <- site_count(site,site_city_hash)
  
  if(curr_count < 100) { # Threshold
    corrected <- correct_spell(site,site_city_hash)
    
    if(corrected != site) { # Correction occurred
      count <- count + 1
      sites_city_before[count] <- site
      sites_city_after[count] <- corrected
      corrected_count <- site_count(corrected,site_city_hash)
      print(paste0(site, " : ", curr_count,", ",corrected, " : ", corrected_count))
    }
  }  
}

sites_corrected_hash <- hashmap(sites_city_before,sites_city_after)

worksite_correct <- function(x, hash) {
  if(hash$has_key(x)) {
    return(hash[[x]])
  }
  return(x)
}

h1b_df$WORKSITE_CORRECTED <- sapply(h1b_df$WORKSITE_CITY, worksite_correct, hash=sites_corrected_hash)

h1b_df %>%
  select(-WORKSITE_CITY) %>%
  rename(WORKSITE_CITY = WORKSITE_CORRECTED) -> h1b_df

## Wage levelling
h1b_df$PREVAILING_WAGE = as.numeric(gsub("[\\$,]", "", h1b_df$PREVAILING_WAGE))

pw_unit_to_yearly <- function(prevailing_wage, pw_unit_of_pay) {
  return(if_else(pw_unit_of_pay == "Year",
                 prevailing_wage,
                 if_else(pw_unit_of_pay == "Hour",
                         2080*prevailing_wage,
                         if_else(pw_unit_of_pay== "Week",
                                 52*prevailing_wage,
                                 if_else(pw_unit_of_pay == "Month",
                                         12*prevailing_wage,
                                         26*prevailing_wage)))))
}


h1b_df <- h1b_df %>%
  filter(!is.na(PW_UNIT_OF_PAY)) %>%
  mutate(PREVAILING_WAGE = as.double(PREVAILING_WAGE)) %>%
  mutate(PREVAILING_WAGE =  pw_unit_to_yearly(PREVAILING_WAGE, PW_UNIT_OF_PAY)) %>%
  select(- PW_UNIT_OF_PAY)