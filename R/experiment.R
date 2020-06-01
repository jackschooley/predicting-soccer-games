experiment = read.csv("./data/experiment.csv")
experiment$Date = as.Date(experiment$Date, format = "%m/%d/%Y")

library(ggplot2)
dif = 40
ggplot(experiment) + geom_col(aes(Date, Number.of.Bets * dif, fill = "Number of Bets")) + 
  geom_line(aes(Date, Balance, group = 1, color = "Balance"), size = 2) + 
  scale_color_manual(name = "", values = c("Balance" = "blue")) +
  scale_y_continuous(name = "Balance ($)", sec.axis = sec_axis(~ . / dif, name = "Number of Bets"),
                     limits = c(0, 250)) +  scale_x_date(date_breaks = "1 month") +
  theme(legend.position = "bottom", legend.title = element_blank())