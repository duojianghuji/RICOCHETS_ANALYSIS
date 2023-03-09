
# Data notes and libraries ----

# records are leaf lingule distance  of main stem for all plants.
# This is to create a plot of lingule_distance vs day_after_last_lingule_appearance 


library(tidyverse)
library(broom)
library(lubridate)
library(ggpubr)
library(plotly)
theme_set(theme_pubr())

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Import data ----

DAS_leaf6_by_plant <- read.csv("output.csv.files/DAS_leaf6_by_plant.csv")
view(DAS_leaf6_by_plant)

lg_between_ligule <- read.csv("raw.data/lg.between.ligule.csv", sep = ";" , dec = ",") |> 
  mutate(
    date = dmy(date),
    Pot = factor(Pot),
    FLN = factor(FLN),
    # pots (1-8) sowing at 08-Feb-2021, pots (9-90) sowing at 15-Feb-2021
    sowing.date = dmy("15-02-2021"),
    DAS = as.numeric(date - sowing.date)
  )

# lg_between_ligule <- read_delim("raw.data/lg.between.ligule.csv",
#                                 delim = ";",
#                                 locale = locale(decimal_mark = ","),
#                                 col_types = cols(
#                                   Pot = col_integer(),
#                                   FLN = col_integer(),
#                                   distance.ligule = col_double(),
#                                   date = col_date()
#                                 ))
glimpse(lg_between_ligule)
View(lg_between_ligule)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data processing ----

## select lg_distance_less_0.5 ----
# find pots of which the minimum "distance.ligule" is less than or equal to 0.5.
pot_lg_distance_less_0.5 <- lg_between_ligule |> 
  group_by(Pot) |> 
  # filter(FLN == 7) |> 
  summarise(min_lg_distance = min(distance.ligule)) |> 
  # slice_min(distance.ligule, n = 1)
  filter(min_lg_distance <= 0.5)

pot_lg_distance_less_0.5

## select lg_distance_less_0.5 ----
# Filter the "lg_between_ligule" to keep only rows the "pot_lg_distance_less_0.5" is true
lg_between_ligule_filter <- lg_between_ligule |> 
  filter(Pot %in% pot_lg_distance_less_0.5$Pot) 
  # filter out pot with FLN being 8
  # filter(FLN == 7)

lg_between_ligule_filter

## DAS_lg_distance_0_less_0.5 ----
# extract DAS for pots including 0 lingule distance
DAS_lg_distance_0_less_0.5mm <- lg_between_ligule_filter |> 
  group_by(Pot) |> 
  slice_min(distance.ligule, n = 1) |> 
  rename(DAS_lg_distance_0 = DAS)

DAS_lg_distance_0_less_0.5mm

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exploring data ----
## Fitting lingule_less_0.5 together ----

# filter the "lg_between_ligule" to keep only rows where the "pot_lg_distance_less_0.5" is true
lg_between_ligule |> 
  filter(Pot %in% pot_lg_distance_less_0.5$Pot) |> 
  # filter out pot with FLN being 8
  filter(FLN == 7) |>
  filter(DAS < 50) |>
  # filter(!Pot %in% c(73, 74 ,81)) |> 
  ggplot(aes(DAS, distance.ligule)) +
  geom_point(aes(shape = Pot), size = 3) +
  geom_smooth(method = "lm", color = "#fc8d62") +
  # facet_wrap( ~ Pot) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 36, label.y = 9.5, size = 4) +
  stat_regline_equation(label.x = 36, label.y = 10.5, size = 4) +
  labs(title = "lingule_distance_less_0.5_together") +
  labs_pubr()
ggsave("plots/lingule_DAS/lingule_distance_less_0.5_together.png", width = 12, height = 8)

## Fitting lingule_less_0.5 separately ----
lg_between_ligule |> 
  filter(Pot %in% pot_lg_distance_less_0.5$Pot) |> 
  # filter out pot with FLN being 8
  filter(FLN == 7, DAS < 50) |> 
  # filter(!Pot %in% c(73, 74 ,81)) |> 
  ggplot(aes(DAS, distance.ligule)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#fc8d62") +
  facet_wrap( ~ Pot, nrow = 2) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 36, label.y = 9.5, size = 3) +
  stat_regline_equation(label.x = 36, label.y = 11.5, size = 3) +
  labs(title = "lingule_distance_less_0.5_split") +
  ylim(0, 15) +
  labs_pubr()
ggsave("plots/lingule_DAS/lingule_distance_less_0.5_split.png", width = 12, height = 8)

## Fitting lingule_more_0.5 separately ----
lg_between_ligule |> 
  filter(!Pot %in% pot_lg_distance_less_0.5$Pot) |> 
  # filter out pot with FLN being 8
  filter(FLN == 7, DAS < 50) |> 
  ggscatter(x = "DAS", y = "distance.ligule",
            # facet.by = "plant", palette = "jco",
            add = "reg.line",
            fullrange = TRUE,
            conf.int = TRUE, # Add confidence interval
            # cor.method = "pearson", 
            conf.level = 0.95,
            add.params = list(color = "#fc8d62",
                              fill = "gray")
  ) +
  facet_wrap( ~ Pot, nrow = 2) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 36, label.y = 15.5, size = 3.5) +
  stat_regline_equation(label.x = 36, label.y = 18.5, size = 3.5) +
  labs(title = "lingule_distance_more_0.5_split") +
  ylim(0, 20) +
  labs_pubr()
ggsave("plots/lingule_DAS/lingule_distance_more_0.5_split.png", width = 12, height = 8)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Mean slope of fitted linear lines ----
# explore the slopes for each Pot and calculate the mean slope for them（including both FLN 7 and FLN 8）

ligule_slope_mean <- lg_between_ligule_filter |> 
  group_by(Pot) |> 
  filter(DAS < 50) |> 
  do(model = lm(distance.ligule ~ DAS, data = .)) |> 
  mutate(
    # adj_r_squared = summary(model)$adj.r.squared,
    slope = coef(model)[2],
    intercept = coef(model)[1],
    r_squared = summary(model)$r.squared,
    p_value = coef(summary(model))[2, 4]
  ) |> 
  ungroup() |> 
  arrange(slope) |> 
  # filter out p-value which is too high
  # filter(!Pot %in% c(73, 74 ,81)) |> 
  # mean and meadian
  # summarise(ligule_slope_median = median(slope), ligule_slope_mean = mean(slope))
  summarise(ligule_slope_mean = mean(slope, trim = 0.2)) |> 
  pull()

ligule_slope_mean

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate day_after_last_lingule_appearance ----

# extract the minimum distance of ligule value and inverse the DAS when lingule distance is 0
DAS_lg_distance_0_more_0.5mm <- lg_between_ligule |> 
  filter(!Pot %in% pot_lg_distance_less_0.5$Pot) |> 
  # filter out pot with FLN being 8
  filter(DAS < 50) |> 
  group_by(Pot) |> 
  slice_min(distance.ligule) |> 
  mutate(intercept = distance.ligule - ligule_slope_mean * DAS,
         DAS_lg_distance_0 = - intercept / ligule_slope_mean,
         DAS_lg_distance_0_rounded = round(DAS_lg_distance_0)
  )

DAS_lg_distance_0_more_0.5mm

DAS_lg_distance_0_less_0.5mm

DAS_lingule_distance_0 <- DAS_lg_distance_0_less_0.5mm |> 
  select(Pot, DAS_lg_distance_0) |> 
  full_join(DAS_lg_distance_0_more_0.5mm |> 
              select(Pot, DAS_lg_distance_0 = DAS_lg_distance_0_rounded)
            ) |> 
  ungroup() |> 
  right_join(lg_between_ligule) |> 
  mutate(day_after_last_lingule_appearance = DAS - DAS_lg_distance_0) |> 
  print(n = 190)

DAS_lingule_distance_0

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot_lg_distance_vs_day_after_appearance ----
## Regular version  ----
DAS_lingule_distance_0 |> 
  # filter(FLN == 7) |> 
  filter(DAS < 50) |>
  # filter(!Pot %in% c(73, 74 ,81)) |> 
  ggplot(aes(day_after_last_lingule_appearance, distance.ligule)) +
  geom_point(aes(color = FLN)) +
  geom_smooth(method = "lm", color = "#fc8d62") +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 10.5, size = 4) +
  stat_regline_equation(label.x = 0, label.y = 11.5, size = 4) +
  labs(
    title = "lg_distance.vs.day_after_apearance",
    y = "Lingule distance (mm)", x = "Day after last lingule appearance (day)") +
  ylim(0, 15) +
  scale_y_continuous(breaks = seq(0, 15, by = 2)) +
  labs_pubr()
ggsave("plots/lingule_DAS/lg_distance.vs.day_after_apearance.png", width = 12, height = 8)

## Jitter version ----
DAS_lingule_distance_0 |> 
  # filter(FLN == 7) |> 
  filter(DAS < 50) |>
  # filter(!Pot %in% c(73, 74 ,81)) |> 
  ggplot(aes(day_after_last_lingule_appearance, distance.ligule)) +
  geom_point(position = "jitter", aes(color = FLN)) +
  geom_smooth(method = "lm", color = "#fc8d62") +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 10.5, size = 4) +
  stat_regline_equation(label.x = 0, label.y = 11.5, size = 4) +
  labs(
    title = "lg_distance.vs.day_after_apearance",
    y = "Lingule distance (mm)", x = "Day after last lingule appearance (day)") +
  ylim(0, 15) +
  scale_y_continuous(breaks = seq(0, 15, by = 2)) +
  labs_pubr()
ggsave("plots/lingule_DAS/lg_distance.vs.day_after_apearance_jitter.png", width = 12, height = 8)
  


lg_between_ligule_filter |>
  filter(distance.ligule < 12) |> 
  ggplot(aes(DAS, distance.ligule, group = Pot, shape = Pot, color = Pot)) +
  # group_by(Pot) |>
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs_pubr()


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

ggsave("plots/lingule_DAS/lg_distance.vs.DAS.png", width = 12, height = 8)




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

