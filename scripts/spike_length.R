# Data notes and libraries ----

# this script is to analyse how the spike length is developing with time,
# and relate the spike length development to the leaf development

# DAS_leaf6_by_plant.csv is a output of leaf stage script, here to calculate
# day after leaf6 appearance of each spike sampling date

# spike length development is different when the final flag leaf number is different (7 or 8)


# output: 1) a graph of spike_length vs status_terminal_spikelet(TS) to determine the 
#         spike length when it is just before terminal spikelet stage
#         2) a graph of spike_length vs day_after_leaf6 to determine the spike length growth model with time


library(tidyverse)
library(lubridate)
library(ggpubr)
library(plotly)
library(InraeThemes)
theme_inrae()

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Import data ----

spike_length <- read.csv("raw.data/spike.length.csv", sep = ";" , dec = ",")
DAS_leaf6_by_plant <- read.csv("output.csv.files/DAS_leaf6_by_plant.csv", sep = ",")

glimpse(spike_length)
view(spike_length)
view(DAS_leaf6_by_plant)

spike_length <- spike_length |> 
  select(
    plant,
    scenario,
    sampling.date,
    tiller,
    spike.name,
    stage,
    FLN.BM,
    ligule.difference.mm = ligule.difference.mm.,
    spike.length.mm = Spike.length..mm.,
    obs,
    TS.stage
  ) |> 
  mutate(
    sampling.date = dmy(sampling.date),
    pot = parse_number(plant),
    # preliminary treatments for pots 1 to 8 which was sowing at 08-Feb-2021
    sowing.date = if_else(pot <= 8, dmy("08-Feb-2021"), dmy("15-Feb-2021")),
    DAS.sampling = as.numeric(sampling.date - sowing.date),
    .after = sampling.date,
  )

# spike_length |> 
#   filter(spike.length.mm < 7 & tiller == "BM") |> 
#   ggplot() +
#   geom_histogram(aes(x = spike.length.mm), bins = 100, na.rm = TRUE)

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Graphs ----

## spike_length_vs_TS_stage ----

spike_length_vs_TS_stage <- spike_length |> 
  filter(TS.stage %in% c(0, 1, 2)) |> 
  mutate(
    # Convert the variable TS.stage from a numeric to a factor variable
    TS.stage = as.factor(TS.stage),
    TS.stage = factor(TS.stage, levels = c(0, 1, 2), labels = c("Before", "On", "After"))
  ) |> 
  ggplot(aes(x = TS.stage, y = spike.length.mm)) +
  geom_violin(trim = TRUE, fill='#A4A4A4', color="darkred")+

  geom_boxplot(width = 0.2, fill = "#A4A4A4") +
  # geom_dotplot(binaxis ="y", stackdir = "center", dotsize = 1) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  
  # add horizontal line at y = 2
  geom_hline(yintercept = 1.92, linetype = "dashed") +
  stat_summary(fun = mean, geom ="point", shape = 20, color = "red", linewidth = 4) +
  # stat_summary(fun.y = mean, geom = "point", size = 2) +
  # stat_summary(fun.data = mean_sdl, mult=1, geom = "pointrange", color="red") +
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(title="Spike Length vs Terminal Spikelet Stage", x = "TS.Stage", y = "Spike Length (mm)")+
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    element_line(size = 1),
    text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    # legend.position = c(0.1, 0.95),
    # legend.text = element_text(face = "bold", size = 12),
    axis.ticks = element_line(linewidth = 1)
  )
spike_length_vs_TS_stage
ggsave("plots/spike_length/spike_length_vs_TS_stage.png", width = 12, height = 8)

## splike length vs day after appearance of leaf 6 ----

# remove duplicates and remove plants with 8 flag leaves
spike_length_vs_day_after_leaf6 <- spike_length  |> 
  # Notice!! exclude = NULL include the NA value as a level
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  filter(tiller == "BM") |> 
  left_join(DAS_leaf6_by_plant) |> 
  mutate(
    day_after_leaf6 = DAS.sampling - DAS_leaf6_rounded
  ) |> 
  # filter(plant %in% c("1rouge", "5blanc", "7rouge"))
  select(plant,FLN.BM, spike.length.mm, sampling.date, DAS.sampling, DAS_leaf6, DAS_leaf6_rounded, day_after_leaf6) |> 
  arrange(spike.length.mm)

spike_length_vs_day_after_leaf6

glimpse(spike_length_vs_day_after_leaf6)

# Create the plot with a smooth line
spike_length_vs_day_after_leaf6 |> 
  filter(day_after_leaf6 < 10) |>
  filter(FLN.BM != "7") |>
  ggplot(aes(day_after_leaf6, spike.length.mm,)) +
  geom_point(aes(group = FLN.BM, color = FLN.BM, shape = FLN.BM), size = 3) + 
 
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "gam", formula = y ~ s(x, k=3), se = FALSE)  +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ s(x, k=3), label.x = -1, label.y = 15)
  
  
  
   
  # exponential fitting
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "nls", formula = y ~ a * exp(r * x), se = FALSE, method.args = list(start = c(a = 1.0767, r = 0.027))) +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        method = "nls", formula = y ~ a * exp(r * x), label.x = -1, label.y = 15)
  
  
  
  # geom_smooth(data = spike_length_vs_day_after_leaf6 %>% filter(is.na(FLN.BM)), method = "lm", formula = y ~ poly(x, 3, raw = TRUE)) +
  # # difference between raw vs. orthogonal raw = TRUE or FALSE???????????????????????????
  # stat_regline_equation(data = spike_length_vs_day_after_leaf6 %>% filter(is.na(FLN.BM)), formula = y ~ poly(x, 3, raw = TRUE), label.x = -1, label.y = 15) +
  
  # polynomial fitting to the power of 3
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "lm", formula = y ~ poly(x, 3, raw = TRUE)) +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ poly(x, 3, raw = TRUE), label.x = -1, label.y = 15) +
  # geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3)) +
  # stat_regline_equation(formula = y ~ splines::bs(x, 3), label.x = -1, label.y = 5) +
  scale_x_continuous(breaks = seq(-1, 20, 1)) +
  scale_y_continuous(breaks = seq(0, 17, by = 3)) +
  labs_pubr()
  
  method = "gam", 
  
  
ggsave("plots/spike_length/spike_length_vs_day_after_leaf6.png", width = 12, height = 8)

spike_length_vs_day_after_leaf6 |> 
  select(plant, day_after_leaf6, spike.length.mm) |> 
  write_csv("output.csv.files/spike_length_vs_day_after_leaf6.csv")

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±















# to check how many samples of main stem number for each catogrory (7, 8, NA)
spike_length  |> 
  filter(tiller == "BM") |>
  # distinct(FLN.BM) |> 
  count(FLN.BM)

# check which plant is which catogry, finding plant 70 is duplicated(flag leaf with 8), check what happended
spike_length  |> 
  filter(tiller == "BM") |>
  # filter(FLN.BM %in% c(7, 8)) |>
  # filter((FLN.BM == 8)) |>  
  # filter(is.na(FLN.BM)) |>
  count(plant) |> 
  filter(n > 1)

# for_join_spike_length <- leaf_stage_day_after_leaf6 |> 
#   group_by(plant) |> 
#   select(date, DAS, day_after_leaf6, pot, plant, stage, FLN, sowing.date, DAS_leaf6) |> 
#   slice_max(stage, n = 1)

# for_join_spike_length

# print(for_join_spike_length, n = 100)
spike_length  |> 
  # mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7")))
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL))
  count(FLN.BM)


mutate(spike_length, FLN.BM = as_factor(FLN.BM))
levels(spike_length$FLN.BM)
glimpse(spike_length)
# remove duplicates and remove plants with 8 flag leaves
spike_length_vs_day_after_leaf6 <- spike_length  |> 
  # Notice!! exclude = NULL include the NA value as a level
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  filter(tiller == "BM") |> 
  left_join(DAS_leaf6_by_plant |> select(plant, DAS_leaf6, DAS_leaf6_rounded)) |> 
  mutate(
    day_after_leaf6 = DAS.sampling - DAS_leaf6_rounded
  ) |> 
  # filter(plant %in% c("1rouge", "5blanc", "7rouge"))
  select(plant,FLN.BM, spike.length.mm, sampling.date, DAS.sampling, DAS_leaf6, DAS_leaf6_rounded, day_after_leaf6) |> 
  arrange(spike.length.mm)

  # drop_na(day_after_leaf6)
  # filter(spike.length.mm < 12) |>

spike_length_vs_day_after_leaf6

glimpse(spike_length_vs_day_after_leaf6)


###############################################################################
spike_length_filtered
spike_length_filtered <- spike_length  |> 
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL)) |> 
  filter(tiller == "BM") |> 
  # filter(FLN.BM %in% c(7, 8)) |>
  # Notice!!!!!!!!!!!!!!!!!!!! : !FLN.BM == 8 doesn't equal FLN.BM == 7 & FLN.BM == NA, FLN.BM == NA was not included
  # filter(FLN.BM == 7) |> 
  filter(!FLN.BM == 7) |> 
  select(plant, sowing.date, sampling.date, DAS.sampling, sampling_stage = stage) |>  
  arrange(plant)

spike_length_filtered

plant_filtered <- spike_length_filtered$plant

plant_filtered

leaf_stage

# compare leaf stage data in spike length table with that in leaf stage table
leaf_stage |> 
  filter(plant %in% plant_filtered) |> 
  group_by(plant) |>
  # select(date, DAS, day_after_leaf6, pot, plant, stage, FLN, sowing.date, DAS_leaf6) |>
  slice_max(stage, n = 1) |> 
  select(plant, sowing.date, date, stage_leaf_stage = stage) |> 
  left_join(spike_length_filtered, join_by(plant))

# add info of plants "1rouge", "5blanc", "7rouge" to leaf stage table
to_be_appended_in_spike_to_leaf_stage <- leaf_stage |> 
  filter(plant %in% plant_filtered) |> 
  group_by(plant) |>
  # select(date, DAS, day_after_leaf6, pot, plant, stage, FLN, sowing.date, DAS_leaf6) |>
  slice_max(stage, n = 1) |> 
  select(plant, date, stage_leaf_stage = stage) |> 
  left_join(spike_length_filtered, join_by(plant)) |> 
  # arrange(plant) |> 
  # date and sampling date are different only for for 1rouge, 5blanc and 7rouge in main stems
  # add info of plants "1rouge", "5blanc", "7rouge" to leaf stage table due to only one point available
  # in leaf stage table and these points in spike length table are different sampling date
  filter(date != sampling.date) |> 
  # stage 5.5 for 1rouge might be a mistake(campare spike length and leaf stage table for 1rouge)
  rows_update(tibble(plant = "1rouge", sampling_stage = 5.8)) |> 
  select(plant, sowing.date, date = sampling.date, DAS = DAS.sampling, stage = sampling_stage)
  to_be_appended_in_spike_to_leaf_stage

# use stage in spike length table(which is more reasonable) to replace them in leaf stage table with same date
to_be_replaced_in_spike_to_leaf_stage <- leaf_stage |> 
  filter(plant %in% plant_filtered) |> 
  group_by(plant) |>
  # select(date, DAS, day_after_leaf6, pot, plant, stage, FLN, sowing.date, DAS_leaf6) |>
  slice_max(stage, n = 1) |> 
  select(plant, date, stage_leaf_stage = stage) |> 
  left_join(spike_length_filtered, join_by(plant)) |> 
  # arrange(plant) |> 
  filter(date == sampling.date, stage_leaf_stage != sampling_stage) |> 
  select(plant, sowing.date, date = sampling.date, DAS = DAS.sampling, stage = sampling_stage)
to_be_replaced_in_spike_to_leaf_stage

to_be_replaced_plants <- to_be_replaced_in_spike_to_leaf_stage$plant

###############################################################################
spike_length_vs_day_after_leaf6
spike_length_vs_day_after_leaf6 |> 
  filter(FLN.BM == "8")

# Create the plot with a smooth line
spike_length_vs_day_after_leaf6 |> 
  filter(day_after_leaf6 < 10) |>
  filter(FLN.BM != "7") |>
  ggplot(aes(day_after_leaf6, spike.length.mm,)) +
  geom_point(aes(group = FLN.BM, color = FLN.BM, shape = FLN.BM), size = 3) + 
  # geom_smooth(data = spike_length_vs_day_after_leaf6 %>% filter(is.na(FLN.BM)), method = "lm", formula = y ~ poly(x, 3, raw = TRUE)) +
  # # difference between raw vs. orthogonal raw = TRUE or FALSE???????????????????????????
  # stat_regline_equation(data = spike_length_vs_day_after_leaf6 %>% filter(is.na(FLN.BM)), formula = y ~ poly(x, 3, raw = TRUE), label.x = -1, label.y = 15) +
  geom_smooth(data = . %>% filter(FLN.BM == "NA-7"), 
              method = "lm", formula = y ~ poly(x, 3, raw = TRUE)) +
  stat_regline_equation(data = . %>% filter(FLN.BM == "NA-7"), 
                        formula = y ~ poly(x, 3, raw = TRUE), label.x = -1, label.y = 15) +
  # geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3)) +
  # stat_regline_equation(formula = y ~ splines::bs(x, 3), label.x = -1, label.y = 5) +
  scale_x_continuous(breaks = seq(-1, 20, 1)) +
  scale_y_continuous(breaks = seq(0, 17, by = 3)) +
  labs_pubr()
ggplotly()
ggsave("plots/spike_length/spike_length_vs_day_after_leaf6.png", width = 12, height = 8)

spike_length_vs_day_after_leaf6 |> 
  select(plant, day_after_leaf6, spike.length.mm)
  write_csv("output.csv.files/spike_length_vs_day_after_leaf6.csv")
###############################################################################

# select leaf stage info of main stem from spike length table to add to leaf stage table
# for plants "1rouge", "5blanc", "7rouge"
for_append_to_leaf_stage <- spike_length |> 
  select(
    plant,
    tiller,
    scenario,
    date = sampling.date,
    sowing.date,
    DAS = DAS.sampling,
    stage
  ) |> 
  filter(tiller == "BM", plant %in% c("1rouge", "5blanc", "7rouge")) |> 
  # stage 5.5 for 1rouge might be a mistake(campare spike length and leaf stage table for 1rouge) 
  rows_update(tibble(plant = "1rouge", stage = 5.8))
  
for_append_to_leaf_stage

###############################################################################











# Set y_intersection to 5
y_intersection <- 1.92

# Calculate the x value for the intersection point
x_intersection <- predict(lm(day_after_leaf6 ~ spike.length.mm, spike_length_regression), data.frame(spike.length.mm = y_intersection), se.fit = FALSE)


# Add the horizontal and vertical lines that intersect at the point
p +
  # geom_segment(x = x_intersection, xend = x_intersection,
  #              y = -Inf, yend = y_intersection,
  #              color = "red", linetype = "dashed") +
  geom_segment(x = -Inf, xend = x_intersection,
               y = y_intersection, yend = y_intersection,
               color = "red", linetype = "dashed")



rm(x_intersection, y_intersection)
























  # ylim(0,50)
  # select(plant, spike.length.mm)
  
  # distinct(plant, stage)
  
  
  






  # distinct(spike.name, stage, .keep_all = TRUE)

  
  
