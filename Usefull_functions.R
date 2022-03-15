require(lazyeval)
require(dplyr)

job_filter <- function(data,vec) {
  # This Fnctn filters only the rows from dataframe 
  # with the  given Job titles provided in the inputs
  # Inpts:
  # data       : The h1B data set 
  # vec        : vector of job types input
  # Opt     : filtered dataframe
  # incase of nothing matching, outputs an empty dataframe
  # creates a new column JOB_INPUT_CLASS to get the job type
  # Inase of  more than one job types inputs matching with a given datpoint(row) in the dataframe data, the 
  # outputs them in different rows each with unique JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(vec) == 0) {
    return(data %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  df <- data.frame()
  
  for(value in vec){
    df <- rbind(df, data %>% 
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
  }
  return(unique(df))
}


employer_filter <- function(data, vec) {
  # Function to filter only the rows in dataset with 
  # Employers provided as inputs
  # Inpts:
  # df         : h11b dataset 
  # vec        : input vctr job types input
  # Output     : filtered dataframe
  # incase of nothing matching, outputs an empty dataframe
  # Only difference from job_filter() is that there is no new column created
  if(length(input_vec) == 0) {
    return(df)
  }
  
  new_df <- data.frame()
  
  for(val in vec){
    df <- rbind(df, data %>% 
                      filter(regexpr(val,EMPLOYER_NAME,ignore.case=TRUE) != -1))
  }
  return(unique(df))
}

find_top <- function(data,z,metric, Ntop = 3) {
  # Function gives the highest values of x_feature based on metric value
  # Inpts:
  # data           : filtered dataframe from job_type, location, employer and year range inputs
  # z              : given feature in data against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric         : metric for data comparison 
  # Output         : gives us a list of highest values in z feature based on metric we input
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  data %>% 
    group_by_(z) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}


get_theme <- function() {
  # Function created for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)),
          axis.text.y = element_text(size=rel(1.5),face="bold"),
          axis.text.x = element_text(size=rel(1.5),face="bold")) 
  )
}

split_first <- function(word, split = " ") {
  # Function gives the 1st value in a  strsplit
  # Inpts:
  # word      : word to be split
  # split     : split parameter to be passed to strsplit
  return(strsplit(word,split= split)[[1]][1])
}
