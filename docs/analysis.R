library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(wesanderson)
library(reshape2)
library(maps)
library(RColorBrewer)
library(qdap)


incarceration_trends <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
variables <- colnames(incarceration_trends)

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

latinx_pop_ne <- sum(northeast_df$latinx_pop_15to64, na.rm = TRUE)

aapi_pop_ne <- sum(northeast_df$aapi_pop_15to64, na.rm = TRUE)

imprisoned_black_pop_ne <- sum(northeast_df$black_jail_pop, na.rm = TRUE) +
  sum(northeast_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_ne <- sum(northeast_df$white_jail_pop, na.rm =TRUE) +
  sum(northeast_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

imprisoned_latinx_pop_ne <- sum(northeast_df$latinx_jail_pop, na.rm =TRUE) +
  sum(northeast_df$latinx_prison_pop, na.rm= TRUE)

imprisoned_aapi_pop_ne <- sum(northeast_df$aapi_jail_pop, na.rm =TRUE) +
  sum(northeast_df$aapi_prison_pop, na.rm= TRUE)

total_imprisoned_pop_ne <- sum(northeast_df$total_jail_pop, na.rm = TRUE) + 
  sum(northeast_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_black_pop_ne / total_imprisoned_pop_ne)  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_white_pop_ne / total_imprisoned_pop_ne) # Ratio of white prisoners within the total prison population

latinx_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_latinx_pop_ne/ total_imprisoned_pop_ne)

aapi_imprisoned_ratio_to_prison_pop_ne <- (imprisoned_aapi_pop_ne/ total_imprisoned_pop_ne)

black_imprisoned_to_black_pop_ratio_ne <- (imprisoned_black_pop_ne / black_pop_ne) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_ne <- (imprisoned_white_pop_ne / white_pop_ne) * 100 # Ratio of white prisoners within the white population

latinx_imprisoned_to_latinx_pop_ratio_ne <- (imprisoned_latinx_pop_ne / latinx_pop_ne) * 100

aapi_imprisoned_to_aapi_pop_ratio_ne <- (imprisoned_aapi_pop_ne / aapi_pop_ne) * 100

black_pop_ratio_ne <- (black_pop_ne / total_pop_ne) * 100 # Ratio of black people in the total NE population

white_pop_ratio_ne <- (white_pop_ne / total_pop_ne) * 100 # Ratio of white people in the total NE population

latinx_pop_ratio_ne <- (latinx_pop_ne / total_pop_ne) * 100

aapi_pop_ratio_ne<- (aapi_pop_ne / total_pop_ne) * 100


### South

south_df <- present_day_df %>%
  filter(grepl('DE|DC|FL|GA|MD|NC|SC|VA|WV|AL|KY|MS|TN|AR|LA|OK|TX', state)) # State filter

total_pop_south <- sum(south_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_south <- sum(south_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_south <- sum(south_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

latinx_pop_south <- sum(south_df$latinx_pop_15to64, na.rm = TRUE)

aapi_pop_south <- sum(south_df$aapi_pop_15to64, na.rm = TRUE)

imprisoned_black_pop_south <- sum(south_df$black_jail_pop, na.rm = TRUE) +
  sum(south_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_south <- sum(south_df$white_jail_pop, na.rm =TRUE) +
  sum(south_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

imprisoned_latinx_pop_south <- sum(south_df$latinx_jail_pop, na.rm =TRUE) +
  sum(south_df$latinx_prison_pop, na.rm= TRUE)

imprisoned_aapi_pop_south <- sum(south_df$aapi_jail_pop, na.rm =TRUE) +
  sum(south_df$aapi_prison_pop, na.rm= TRUE)

total_imprisoned_pop_south <- sum(south_df$total_jail_pop, na.rm = TRUE) + 
  sum(south_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_south <- (imprisoned_black_pop_south / total_imprisoned_pop_south)  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_south <- (imprisoned_white_pop_south / total_imprisoned_pop_south) # Ratio of white prisoners within the total prison population

latinx_imprisoned_ratio_to_prison_pop_south <- (imprisoned_latinx_pop_south/ total_imprisoned_pop_south)

aapi_imprisoned_ratio_to_prison_pop_south <- (imprisoned_aapi_pop_south/ total_imprisoned_pop_south)

black_imprisoned_to_black_pop_ratio_south <- (imprisoned_black_pop_south / black_pop_south) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_south <- (imprisoned_white_pop_south / white_pop_south) * 100 # Ratio of white prisoners within the white population

latinx_imprisoned_to_latinx_pop_ratio_south <- (imprisoned_latinx_pop_south / latinx_pop_south) * 100

aapi_imprisoned_to_aapi_pop_ratio_south <- (imprisoned_aapi_pop_south / aapi_pop_south) * 100

black_pop_ratio_south <- (black_pop_south / total_pop_south) * 100 # Ratio of black people in the total NE population

white_pop_ratio_south <- (white_pop_south / total_pop_south) * 100 # Ratio of white people in the total NE population

latinx_pop_ratio_south <- (latinx_pop_south / total_pop_south) * 100

aapi_pop_ratio_south <- (aapi_pop_west / total_pop_south) * 100



### West


west_df <- present_day_df %>%
  filter(grepl('AZ|CO|ID|MT|NV|NM|UT|WY|AK|CA|HI|OR|WA', state)) # State filter

total_pop_west <- sum(west_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_west <- sum(west_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_west <- sum(west_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

latinx_pop_west <- sum(west_df$latinx_pop_15to64, na.rm = TRUE)

aapi_pop_west <- sum(west_df$aapi_pop_15to64, na.rm = TRUE)

imprisoned_black_pop_west <- sum(west_df$black_jail_pop, na.rm = TRUE) +
  sum(west_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_west <- sum(west_df$white_jail_pop, na.rm =TRUE) +
  sum(west_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

imprisoned_latinx_pop_west <- sum(west_df$latinx_jail_pop, na.rm =TRUE) +
  sum(west_df$latinx_prison_pop, na.rm= TRUE)

imprisoned_aapi_pop_west <- sum(west_df$aapi_jail_pop, na.rm =TRUE) +
  sum(west_df$aapi_prison_pop, na.rm= TRUE)

total_imprisoned_pop_west<- sum(west_df$total_jail_pop, na.rm = TRUE) + 
  sum(west_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_west <- (imprisoned_black_pop_west/ total_imprisoned_pop_west)  # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_west <- (imprisoned_white_pop_west / total_imprisoned_pop_west) # Ratio of white prisoners within the total prison population

latinx_imprisoned_ratio_to_prison_pop_west <- (imprisoned_latinx_pop_west/ total_imprisoned_pop_west)

aapi_imprisoned_ratio_to_prison_pop_west <- (imprisoned_aapi_pop_west/ total_imprisoned_pop_west)

black_imprisoned_to_black_pop_ratio_west <- (imprisoned_black_pop_west / black_pop_west) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_west <- (imprisoned_white_pop_west / white_pop_west) * 100 # Ratio of white prisoners within the white population

latinx_imprisoned_to_latinx_pop_ratio_west <- (imprisoned_latinx_pop_west / latinx_pop_west) * 100

aapi_imprisoned_to_aapi_pop_ratio_west <- (imprisoned_aapi_pop_west / aapi_pop_west) * 100

black_pop_ratio_west <- (black_pop_west / total_pop_west) * 100 # Ratio of black people in the total west population

white_pop_ratio_west <- (white_pop_west / total_pop_west) * 100 # Ratio of white people in the total NE population

latinx_pop_ratio_west <- (latinx_pop_west / total_pop_west) * 100

aapi_pop_ratio_west <- (aapi_pop_west / total_pop_west) * 100


##Midwest


mw_df <- present_day_df %>%
  filter(grepl('IL|IN|MI|OH|WI|IA|KS|MN|MO|NE|ND|SD', state)) # State filter

total_pop_mw <- sum(mw_df$total_pop_15to64, na.rm = TRUE)   # Total Working Population

black_pop_mw <- sum(mw_df$black_pop_15to64, na.rm = TRUE)   # Total Black Working Population

white_pop_mw <- sum(mw_df$white_pop_15to64, na.rm = TRUE)   # Total white working population

latinx_pop_mw <- sum(mw_df$latinx_pop_15to64, na.rm = TRUE)

aapi_pop_mw <- sum(mw_df$aapi_pop_15to64, na.rm = TRUE)

imprisoned_black_pop_mw <- sum(mw_df$black_jail_pop, na.rm = TRUE) +
  sum(mw_df$black_prison_pop, na.rm = TRUE) # Number of imprisoned black people

imprisoned_white_pop_mw <- sum(mw_df$white_jail_pop, na.rm =TRUE) +
  sum(mw_df$white_prison_pop, na.rm= TRUE) # Number of imprisoned white people

imprisoned_latinx_pop_mw <- sum(mw_df$latinx_jail_pop, na.rm =TRUE) +
  sum(mw_df$latinx_prison_pop, na.rm= TRUE)

imprisoned_aapi_pop_mw <- sum(mw_df$aapi_jail_pop, na.rm =TRUE) +
  sum(mw_df$aapi_prison_pop, na.rm= TRUE)

total_imprisoned_pop_mw <- sum(mw_df$total_jail_pop, na.rm = TRUE) + 
  sum(mw_df$total_prison_pop, na.rm = TRUE) # Number of imprisoned Total

black_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_black_pop_mw / total_imprisoned_pop_mw) # Ratio of black prisoners within the total prison population

white_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_white_pop_mw / total_imprisoned_pop_mw) # Ratio of white prisoners within the total prison population

latinx_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_latinx_pop_mw/ total_imprisoned_pop_west)

aapi_imprisoned_ratio_to_prison_pop_mw <- (imprisoned_aapi_pop_mw/ total_imprisoned_pop_west)

black_imprisoned_to_black_pop_ratio_mw <- (imprisoned_black_pop_mw / black_pop_mw) * 100 # Ratio of black prisoners within the black population

white_imprisoned_to_white_pop_ratio_mw <- (imprisoned_white_pop_mw / white_pop_mw) * 100 # Ratio of white prisoners within the white population

latinx_imprisoned_to_latinx_pop_ratio_mw <- (imprisoned_latinx_pop_mw / latinx_pop_mw) * 100

aapi_imprisoned_to_aapi_pop_ratio_mw <- (imprisoned_aapi_pop_mw / aapi_pop_mw) * 100

black_pop_ratio_mw <- (black_pop_mw / total_pop_mw) * 100 # Ratio of black people in the total NE population

white_pop_ratio_mw <- (white_pop_mw / total_pop_mw) * 100 # Ratio of white people in the total NE population

latinx_pop_ratio_mw <- (latinx_pop_mw / total_pop_mw) * 100

aapi_pop_ratio_west <- (aapi_pop_mw / total_pop_mw) * 100


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

## For Variable Comparison 

south <- c(black_imprisoned_ratio_to_prison_pop_south,
           latinx_imprisoned_ratio_to_prison_pop_south,
           white_imprisoned_ratio_to_prison_pop_south,
           aapi_imprisoned_ratio_to_prison_pop_south)

midwest <- c(black_imprisoned_ratio_to_prison_pop_mw,
             latinx_imprisoned_ratio_to_prison_pop_mw,
             white_imprisoned_ratio_to_prison_pop_mw,
             aapi_imprisoned_ratio_to_prison_pop_mw)

northeast <- c(black_imprisoned_ratio_to_prison_pop_ne,
               latinx_imprisoned_ratio_to_prison_pop_ne,
               white_imprisoned_ratio_to_prison_pop_ne,
               aapi_imprisoned_ratio_to_prison_pop_ne)

west <- c(black_imprisoned_ratio_to_prison_pop_west,
          latinx_imprisoned_ratio_to_prison_pop_west,
          white_imprisoned_ratio_to_prison_pop_west,
          aapi_imprisoned_ratio_to_prison_pop_west)

race <- c('Black','Latinx','White','AAPI')

variable_comparison_df <- data.frame(south,midwest,northeast,west,race)
vc_final_melt <- melt(variable_comparison_df, id.vars='race')

## Sum Info

summary_info <- list()

summary_info$black_prisoner_increase <- imprisoned_black_pop_present -
  imprisoned_black_pop_past
summary_info$white_prisoner_increase <- imprisoned_white_pop_present -
  imprisoned_white_pop_past

summary_info$black_pop_percentage_imprisoned <- round(
  (imprisoned_black_pop_present /
     black_pop_present) * 100, digits = 2)
summary_info$white_pop_percentage_imprisoned <- round(
  (imprisoned_white_pop_present /
     white_pop_present) * 100, digits = 2)

summary_info$ne_percentage_of_prisoners_black <- 
  round(black_imprisoned_ratio_to_prison_pop_ne * 100)
summary_info$ne_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_ne * 100)


summary_info$west_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_west * 100)
summary_info$west_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_west * 100)

summary_info$mw_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_mw * 100)
summary_info$mw_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_mw * 100)

summary_info$south_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_south * 100)
summary_info$south_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_south * 100)

summary_info$ne_percentage_of_pop_black <- round(
  black_pop_ratio_ne)
summary_info$ne_percentage_of_pop_white <- round(
  white_pop_ratio_ne)

summary_info$west_percentage_of_pop_black <- round(
  black_pop_ratio_west)
summary_info$west_percentage_of_pop_white <- round(
  white_pop_ratio_west)

summary_info$mw_percentage_of_pop_black <- round(
  black_pop_ratio_mw)
summary_info$mw_percentage_of_pop_white <- round(
  white_pop_ratio_mw)

summary_info$south_percentage_of_pop_black <- round(
  black_pop_ratio_south)
summary_info$south_percentage_of_pop_white <- round(
  white_pop_ratio_south)

## Charts

adm_final_melt <- melt(adm_final, id.vars='year')

over_time_plot <- ggplot(adm_final_melt, aes(x=year, y=value, color=variable))+ 
  geom_line(size=1.3)+
  scale_color_manual(values= wes_palette("Rushmore1", n = 4), name="Race",
                     labels=c("total_prison_adm"="Total",
                              "black"="Black",
                              "white"="White",
                              "latinx"="Latinx"))+
  theme_classic()+
  labs(
    title = "Average Prison Admission in All Counties",
    subtitle = "(1985-2015)",
    y = "Average"
  )+
  theme(panel.background = element_rect(fill = 'Azure'))+
  theme(panel.grid.major = element_line(color = "grey",
                                        size = 0.5,
                                        linetype = 2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


variable_comparison_chart <- ggplot(vc_final_melt, aes(x = variable, y = value, fill = race))+
  geom_bar(position = "fill", stat = "identity", color='black', width=0.9)+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Percentage of Prisoners by Ethnicity in Each Region",
    subtitle = "(Data from 2015)"
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))



imprisoned_population <- incarceration_trends %>%
  filter(year == "2015")
imprisoned_population$state <- mgsub(state.abb, state.name, imprisoned_population$state)
imprisoned_population$state <- tolower(imprisoned_population$state)
imprisoned_population$region <- imprisoned_population$state
imprisoned_population <- imprisoned_population %>%
  select(black_prison_pop,black_jail_pop,region)%>%
  mutate_all(~replace(.,is.na(.),0))%>%
  group_by(region)%>%
  summarise(total = sum(black_jail_pop+black_prison_pop))


us_states_copy <- map_data("state")
us_states <- map_data("state")
us_states <- us_states%>%
  left_join(imprisoned_population, by="region")

map_chart <- ggplot(us_states, aes(long,lat,group=group))+
  geom_polygon(aes(fill=total),color="black")+
  scale_fill_gradient(name="# of Incarcerated", low="yellow",high="red")+
  labs(title= "Black Incarceration in the US",
       subtitle= "(Data from 2015)")+
  theme_minimal()+
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  )



