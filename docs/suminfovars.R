library('tidyverse')
library('dplyr')
library('tidyr')
library('stringr')
library('lubridate')
library('ggplot2')


incarceration_trends <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
View(incarceration_trends)
variables <- colnames(incarceration_trends)
variables

# DFs

present_day_df <- incarceration_trends %>%
  filter(year == '2015')


past_df <- incarceration_trends %>%
  filter(year == '1990')



### Prison Numbers

imprisoned_black_pop_present <- round(sum(present_day_df$black_prison_pop,
                                    na.rm = TRUE) + sum(present_day_df$black_jail_pop,
                                                        na.rm = TRUE))
imprisoned_black_pop_past <- round(sum(past_df$black_prison_pop, na.rm = TRUE) +
  sum(past_df$black_jail_pop, na.rm =TRUE))

imprisoned_white_pop_present <- round(sum(present_day_df$white_prison_pop,
                                          na.rm = TRUE) + sum(present_day_df$white_jail_pop,
                                                              na.rm = TRUE))

imprisoned_white_pop_past <- round(sum(past_df$white_prison_pop, na.rm = TRUE) +
  sum(past_df$white_jail_pop, na.rm =TRUE))

total_imprisoned_present <- round(sum(present_day_df$total_prison_pop, na.rm = TRUE) + 
  sum(present_day_df$total_jail_pop, na.rm = TRUE))

total_imprisoned_past <- round(sum(past_df$total_prison_pop, na.rm =TRUE) +
  sum(past_df$total_jail_pop, na.rm =TRUE))

### Total Population (15-64)

total_population_present <- sum(present_day_df$total_pop_15to64)

total_population_past <- sum(past_df$total_pop_15to64)

black_pop_present <- sum(present_day_df$black_pop_15to64)

black_pop_past <- sum(past_df$black_pop_15to64, na.rm = TRUE)

white_pop_present <- sum(present_day_df$white_pop_15to64, na.rm = TRUE)

white_pop_past <- sum(past_df$white_pop_15to64, na.rm = TRUE)

### Northeast

northeast_df <- present_day_df %>%
  filter(grepl('CT|ME|MA|NH|RI|VT|NJ|NY|PA', state)) # State filter

total_pop_ne <- sum(northeast_df$total_pop_15to64)   # Total Working Population

black_pop_ne <- sum(northeast_df$black_pop_15to64)   # Total Black Working Population

white_pop_ne <- sum(northeast_df$white_pop_15to64)   # Total white working population

imprisoned_black_pop_ne <- sum(northeast_df$black_jail_pop, na.rm = TRUE) +
  sum(northeast_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_ne <- sum(northeast_df$white_jail_pop, na.rm =TRUE) +
  sum(northeast_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

total_imprisoned_pop_ne <- sum(northeast_df$total_jail_pop, na.rm = TRUE) + 
  sum(northeast_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_black_pop_ne / total_imprisoned_pop_ne) * 100  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_white_pop_ne / total_imprisoned_pop_ne) * 100 # Ratio of white prisoners within the total prison population

black_imprisoned_to_black_pop_ratio_ne <- (imprisoned_black_pop_ne / black_pop_ne) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_ne <- (imprisoned_white_pop_ne / white_pop_ne) * 100 # Ratio of white prisoners within the white population

black_pop_ratio_ne <- (black_pop_ne / total_pop_ne) * 100 # Ratio of black people in the total NE population

white_pop_ratio_ne <- (white_pop_ne / total_pop_ne) * 100 # Ratio of white people in the total NE population


### South

south_df <- present_day_df %>%
  filter(grepl('DE|DC|FL|GA|MD|NC|SC|VA|WV|AL|KY|MS|TN|AR|LA|OK|TX', state)) # State filter

total_pop_south <- sum(south_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_south <- sum(south_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_south <- sum(south_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

imprisoned_black_pop_south <- sum(south_df$black_jail_pop, na.rm = TRUE) +
  sum(south_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_south <- sum(south_df$white_jail_pop, na.rm =TRUE) +
  sum(south_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

total_imprisoned_pop_south <- sum(south_df$total_jail_pop, na.rm = TRUE) + 
  sum(south_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_south <- (imprisoned_black_pop_south / total_imprisoned_pop_south) * 100  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_south <- (imprisoned_white_pop_south / total_imprisoned_pop_south) * 100 # Ratio of white prisoners within the total prison population

black_imprisoned_to_black_pop_ratio_south <- (imprisoned_black_pop_south / black_pop_south) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_south <- (imprisoned_white_pop_south / white_pop_south) * 100 # Ratio of white prisoners within the white population

black_pop_ratio_south <- (black_pop_south / total_pop_south) * 100 # Ratio of black people in the total NE population

white_pop_ratio_south <- (white_pop_south / total_pop_south) * 100 # Ratio of white people in the total NE population


### West


west_df <- present_day_df %>%
  filter(grepl('AZ|CO|ID|MT|NV|NM|UT|WY|AK|CA|HI|OR|WA', state)) # State filter

total_pop_west <- sum(west_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_west <- sum(west_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_west <- sum(west_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

imprisoned_black_pop_west <- sum(west_df$black_jail_pop, na.rm = TRUE) +
  sum(west_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_west <- sum(west_df$white_jail_pop, na.rm =TRUE) +
  sum(west_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

total_imprisoned_pop_west<- sum(west_df$total_jail_pop, na.rm = TRUE) + 
  sum(west_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_west <- (imprisoned_black_pop_west/ total_imprisoned_pop_west) * 100  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_west <- (imprisoned_white_pop_west / total_imprisoned_pop_west) * 100 # Ratio of white prisoners within the total prison population

black_imprisoned_to_black_pop_ratio_west <- (imprisoned_black_pop_west / black_pop_west) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_west <- (imprisoned_white_pop_west / white_pop_west) * 100 # Ratio of white prisoners within the white population

black_pop_ratio_west <- (black_pop_west / total_pop_west) * 100 # Ratio of black people in the total west population

white_pop_ratio_west <- (white_pop_west / total_pop_west) * 100 # Ratio of white people in the total NE population


##Midwest


mw_df <- present_day_df %>%
  filter(grepl('IL|IN|MI|OH|WI|IA|KS|MN|MO|NE|ND|SD', state)) # State filter

total_pop_mw <- sum(mw_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_mw <- sum(mw_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_mw <- sum(mw_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

imprisoned_black_pop_mw <- sum(mw_df$black_jail_pop, na.rm = TRUE) +
  sum(mw_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_mw <- sum(mw_df$white_jail_pop, na.rm =TRUE) +
  sum(mw_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

total_imprisoned_pop_mw <- sum(mw_df$total_jail_pop, na.rm = TRUE) + 
  sum(mw_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_black_pop_mw / total_imprisoned_pop_mw) * 100  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_white_pop_mw / total_imprisoned_pop_mw) * 100 # Ratio of white prisoners within the total prison population

black_imprisoned_to_black_pop_ratio_mw <- (imprisoned_black_pop_mw / black_pop_mw) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_mw <- (imprisoned_white_pop_mw / white_pop_mw) * 100 # Ratio of white prisoners within the white population

black_pop_ratio_mw <- (black_pop_mw / total_pop_mw) * 100 # Ratio of black people in the total NE population

white_pop_ratio_mw <- (white_pop_mw / total_pop_mw) * 100 # Ratio of white people in the total NE population

## Admission Rates


adm_black_df <- incarceration_trends %>%
  filter(black_prison_adm != "NA") %>%
  filter(year >= 1985 & year <= 2015) %>%
  group_by(year) %>%
  summarise(avg_black_adm = mean(black_prison_adm)) %>%
  arrange(year)

adm_white_df <- incarceration_trends %>%
  filter(white_prison_adm != "NA") %>%
  filter(year >= 1985 & year <= 2015) %>%
  group_by(year) %>%
  summarise(avg_white_adm = mean(white_prison_adm)) %>%
  arrange(year)

adm_latinx_df <- incarceration_trends %>%
  filter(latinx_prison_adm != "NA") %>%
  filter(year >= 1985 & year <= 2015) %>%
  group_by(year) %>%
  summarise(avg_latinx_adm = mean(latinx_prison_adm)) %>%
  arrange(year)

adm_total_df <- incarceration_trends %>%
  filter(total_prison_adm != "NA") %>%
  filter(year >= 1985 & year <= 2015) %>%
  group_by(year) %>%
  summarise(total_prison_adm = mean(total_prison_adm)) %>%
  arrange(year)

adm_final <- adm_total_df
adm_final$black <- adm_black_df$avg_black_adm
adm_final$white <- adm_white_df$avg_white_adm
adm_final$latinx <- adm_latinx_df$avg_latinx_adm
