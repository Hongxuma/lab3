# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
           # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}


cleanRedwoodData <- function(redwood_df, dates, loc) {
  # convert result_time to lubridate ymd_hms format, combining location and dates information
  data <- redwood_df %>% 
    mutate(result_time = ymd_hms(result_time))
  # lubridate the datetime format
  
  data1 = full_join(dates, data, by = "epoch") %>% 
    select(date = date, time = time, datetime = datetime, result_time = result_time, epoch = epoch,  
           nodeid = nodeid, voltage = voltage, humidity = humidity, humid_temp = humid_temp, 
           hamatop = hamatop, hamabot = hamabot) %>%
    full_join(loc, by = "nodeid") %>% select(-ID) %>% filter(!is.na(epoch) & !is.na(nodeid)) %>% 
    filter(!is.na(Height)) %>% 
    filter(!is.na(humidity) | !is.na(humid_temp) | !is.na(hamatop) | !is.na(hamabot))
  # combining dates, location information into the raw dataframe, and check for missing values.
  return(data1)
}

check_duplicate <- function(data){
  # This function checks the duplication in the data frame. 
  # If a pair of duplicated samples is spotted, then the variable values in the two sampled are compared.
  # If the values matches, then one of them is discarded Otherwise, both sample are discarded.
  
  n = dim(data)[1];
  data1 = data[n:1,];
  dup1 = data[duplicated(data[,c("nodeid","epoch")]),] %>% arrange(desc(nodeid), desc(epoch)) %>% 
    select(voltage = voltage, humidity = humidity, humid_temp = humid_temp, hamatop = hamatop, hamabot = hamabot);
  # checking duplication from begining
  dup2 = data1[duplicated(data1[,c("nodeid","epoch")]),] %>% arrange(desc(nodeid), desc(epoch)) %>% 
    select(voltage = voltage, humidity = humidity, humid_temp = humid_temp, hamatop = hamatop, hamabot = hamabot);
  # checking duplication from end
  ind = NULL;
  for(i in dim(dup1)[1])
    ind = c(ind, identical(dup1[i,], dup2[i,]))
  # check if the duplicated samples matches
  data_nodup = data[!duplicated(data[,c("nodeid","epoch")]) & !duplicated(data1[,c("nodeid","epoch")]),];
  data_full = bind_rows(data_nodup, dup1[ind,])
  # clean the duplicated data
  return(data_full)
}


