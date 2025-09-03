#Function: Process Initial Quiz Data

process_initquiz <- function(initquiz_path) {
  # Read the initial quiz data
  T_initquiz <- read.csv(initquiz_path)
  
  # Replace 999 with NA for specific columns
  T_initquiz$gender[T_initquiz$gender == 999] <- NA
  T_initquiz$education[T_initquiz$education == 999] <- NA
  T_initquiz$lifeSatisfaction[T_initquiz$lifeSatisfaction == 999] <- NA
  
  # Mutate gender to a factor with meaningful labels
  T_initquiz <- T_initquiz %>% 
    mutate(gender_group = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male",
      TRUE ~ "Other"
    ))
  
  # Function to clean birthYear column
  clean_birthYear <- function(df) {
    df$birthYear <- as.numeric(gsub("[^0-9]", "", df$birthYear))
    df$birthYear[df$birthYear < 1944 | df$birthYear == 0] <- NA
    df
  }
  
  # Clean birthYear and calculate age
  T_initquiz <- clean_birthYear(T_initquiz)
  T_initquiz$age <- 2024 - T_initquiz$birthYear
  T_initquiz <- T_initquiz %>%
    filter(!is.na(T_initquiz$age))
  
  # Mutate age to age_group and age_bin with meaningful labels
  T_initquiz <- T_initquiz %>%
    mutate(age_group = case_when(
      age >= 40 ~ "Above 40",
      age < 40 ~ "Below 40"
    )) %>%
    mutate(age_bin = case_when(
      age < 25 ~ "Below 25",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 ~ "65+"
    ))
  
  # Identify duplicate users based on age, deviceType, and phq8_total
  duplicateUser <- T_initquiz %>%
    group_by(userKey) %>%
    filter(n_distinct(age) > 1 | n_distinct(deviceType) > 1) %>%
    pull(userKey) %>%
    unique()
  
  # Filter out duplicate users
  T_initquiz <- T_initquiz %>%
    filter(!userKey %in% duplicateUser)
  
  # Return the cleaned and transformed data
  return(T_initquiz)
}


  

