library(tidyverse)
library(lubridate)
library(ggpubr)

# import data
DAS_leaf6 <- read.csv("output.csv.files/DAS.leaf6.csv")
view(DAS_leaf6)

lg_between_ligule <- read.csv("raw.data/lg.between.ligule.csv", sep = ";" , dec = ",")

# lg_between_ligule <- read_delim("raw.data/lg.between.ligule.csv",
#                                 delim = ";",
#                                 locale = locale(decimal_mark = ","),
#                                 col_types = cols(
#                                   Pot = col_integer(),
#                                   FLN = col_integer(),
#                                   distance.ligule = col_double(),
#                                   date = col_date()
#                                 ))

# problems(lg_between_ligule)
# spec(lg_between_ligule)

View(lg_between_ligule)
glimpse(lg_between_ligule)

lg_between_ligule <- lg_between_ligule |> 
  mutate(
    date = dmy(date),
    # add sowing.date column as 15-02-2021
    sowing.date = dmy("15-02-2021"),
    DAS = date - sowing.date
  )


# filter the groups where the minimum "distance.ligule" is less than or equal to 0.5.
pot_lg_distance_less_0.5 <- lg_between_ligule |> 
  group_by(Pot) |> 
  summarise(min_lg_distance = min(distance.ligule)) |> 
  # slice_min(distance.ligule, n = 1)
  filter(min_lg_distance <= 0.5)

pot_lg_distance_less_0.5

# Filter the "lg_between_ligule" to keep only rows where "Pot" is in the list of "Pot" values in the "pot_lg_distance_less_0.5" data frame
lg_between_ligule_filter <- lg_between_ligule |> 
  filter(Pot %in% pot_lg_distance_less_0.5$Pot) |> 
  # filter out pot with FLN being 8
  filter(FLN == 7)
lg_between_ligule_filter

lg_plot <- lg_between_ligule_filter |>
  filter(distance.ligule < 12) |> 
  mutate(Pot = as.character(Pot)) |> 
  # group_by(Pot) |> 
  ggscatter(x = "DAS", y = "distance.ligule",
            shape = "Pot",
            add = "reg.line",
            conf.int = TRUE, # Add confidence interval
            conf.level = 0.95,
            add.params = list(color = "#fc8d62",
                              fill = "gray")
  ) +
  # facet_wrap( ~ Pot, nrow = 4) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 36, label.y = 10) +
  stat_regline_equation(label.x = 36, label.y = 13) +
  labs_pubr()
lg_plot

ggpar(lg_plot, 
      # xlim = c(18, 38),
      ylim = c(0, 15),
      xlab = "DAS (day)",
      ylab = "Distance Ligule of the Main Stem "
)











lg_between_ligule_filter |> 
  group_by(Pot) |> 
  summarise(min_lg_distance = max(distance.ligule))
  
  
  
# do linear regression for each pot
lm_list <- lg_between_ligule_filter |> 
  group_by(Pot) |> 
  do(fit = lm(distance.ligule ~ DAS, data = .))
lm_list
# create scatter plot for each pot
p <- lg_between_ligule_filter |> 
  ggplot(aes(x = DAS, y = distance.ligule)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, 
              linewidth = 1, color = "red") +
  ggtitle("Linear Regression for Each Pot") +
  xlab("DAS") +
  ylab("distance.ligule") +
  facet_wrap(~ Pot, ncol = 3)
p
# create scatter plot for each pot
ggarrange(
  lg_between_ligule_filter %>% 
    group_by(Pot) %>% 
    do(plot = ggscatter(data = ., x = "DAS", y = "distance.ligule", 
                        add = "reg.line", 
                        conf.int = TRUE, 
                        cor.coef = TRUE, 
                        cor.method = "pearson"))
)

