source('C:/Users/rezek/Documents/Info_201/a3-cameron-rezek/docs/suminfo.R')
library(wesanderson)
library(reshape2)

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
    title = "Average Prison Admissions\nby County",
    subtitle = "(1985-2015)",
    y = "Average"
  )+
  theme(panel.background = element_rect(fill = 'Azure'))


over_time_plot




