source('C:/Users/rezek/Documents/Info_201/a3-cameron-rezek/docs/suminfovars.R')

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
  white_pop_present) * 100, digits = 1)

summary_info$ne_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_ne)
summary_info$ne_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_ne)

summary_info$west_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_west)
summary_info$west_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_west)

summary_info$mw_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_mw)
summary_info$mw_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_mw)

summary_info$south_percentage_of_prisoners_black <- round(
  black_imprisoned_ratio_to_prison_pop_south)
summary_info$south_percentage_of_prisoners_white <- round(
  white_imprisoned_ratio_to_prison_pop_south)

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
