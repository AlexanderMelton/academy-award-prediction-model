library(ggplot2)
library(scales)
#Theme

summit_theme <- theme(panel.background=element_rect(fill="#F0F0F0"), 
                      plot.background=element_rect(fill="#F0F0F0"), 
                      panel.border= element_rect(colour="#F0F0F0", fill = NA),
                      panel.grid.major=element_line(colour="#e0e0e0",size=.75),
                      panel.grid.minor=element_line(colour="#e0e0e0",size=.75),
                      axis.ticks=element_blank(),
                      legend.position="bottom",
                      legend.key = element_blank(),
                      legend.background = element_blank(),
                      legend.key = element_rect(fill ="#F0F0F0"),
                      legend.margin=unit(0,"cm"),
                      plot.title=element_text(face="bold", colour="#3C3C3C",size=13),
                      axis.text.x=element_text(size=8,colour="#535353",face="bold"),
                      axis.text.y=element_text(size=8,colour="#535353",face="bold"),
                      axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5),
                      axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5))



#Burdget and Length
ggplot(master, aes(x=adj_budget, y = length, color = as.factor(winner))) +
  geom_point() +
  ggtitle("Budget vs. Length for Best Picture Nominees, 1990-2015") +
  xlab("Budget (in 2016 Million Dollars)") +
  ylab("Length (in minutes)") +
  #theme(legend.background = element_blank()) +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = c("Loser", "Winner"), values = c("#c6c4c6", "#53b6e9")) +
  summit_theme

#Line Plot
winners <- master %>% 
  filter(winner == 1) %>%
  group_by(year) %>%
  mutate(group = 'winner') %>%
  select(year, rt_score, group)
average <- master %>%
  group_by(year) %>%
  summarise(rt_score = mean(rt_score)) %>%
  mutate(group = 'average')
merged <- rbind.fill(winners, average)

ggplot() +
  geom_line(mapping = aes(x=year, y = rt_score, color = as.factor(group)),
            data = merged) +
  geom_point(mapping = aes(x=year, y = rt_score, color = as.factor(group)) , 
             data = merged) +
  summit_theme +
  ggtitle("Rotten Tomatoes Score, Winner and Average of Nominees") +
  xlab("Year") +
  ylab("Rotten Tomato Score") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(name= "", labels = c("Mean of Nominees", "Winner"),
                     values = c("#c6c4c6", "#53b6e9"))



