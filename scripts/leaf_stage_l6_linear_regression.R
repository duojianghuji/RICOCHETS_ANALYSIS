
# Data notes and libraries ----

# records are leaf developmental stages of main stem for all plants.
# the main purpose of this script is to calculate the "DAS of leaf 6" for each plant,
# which is saved in folder of output.cvs.files.
# also, we will create a graph of "leaf stage vs DAS", which is a linear model.
# leaves below 3 are usually not considered for linear regression.

library(tidyverse)
library(lubridate)
library(plotly)
library(ggpubr)
theme_set(theme_pubr())
Sys.setenv(OPENAI_API_KEY = "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")

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Import data ----

# both files contain leaf stages information, which are not exactly the same
leaf_stage <- read_delim("raw.data/leaf.stage.csv", delim = ";", locale = locale(decimal_mark = ","))
view(leaf_stage)
glimpse(leaf_stage)

leaf_stage <- leaf_stage |>
  rename(scenario = "Scenario") |> 
  mutate(
    plant = fct(plant),
    date = dmy(date),
    # preliminary treatments for pots 1 to 8 which was sowing at 08-Feb-2021
    sowing.date = if_else(pot <= 8, dmy("08-Feb-2021"), dmy("15-Feb-2021")),
    .after = date,
    # # another way to check sowing date for each pot
    # sowing.date1 = date - DAS
  )

spike_length <- read.csv("raw.data/spike.length.csv", sep = ";" , dec = ",") |> 
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
# Call the function to update DAS_leaf6_by_plant
DAS_leaf6_by_plant <- calcu_DAS_leaf6_by_plant()
DAS_leaf6_by_plant


#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Graphs ----

## plant "35" ----
# examples of linear regression line for plant "35"
leaf_stage_regression_plot_35 <- leaf_stage |>
  # filter out leaf 3
  filter(stage >= 4.0) |> 
  filter(plant == "35") |>
  ggscatter(x = "DAS", y = "stage",
            # facet.by = "plant", # palette = "jco",
            add = "reg.line",
            conf.int = TRUE, # Add confidence interval
            add.params = list(color = "#fc8d62",
                              fill = "gray")
  ) +
  # stat_cor(method = "pearson",
  # aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
  # label.x = 18, label.y = 9) +
  # stat_regline_equation(label.x = 18, label.y = 9.4) +
  geom_segment(aes(x = -Inf, y = 6, xend = 26, yend = 6), linetype = "dashed", color = "red") +
  geom_segment(aes(x = 26, y = -Inf, xend = 26, yend = 6), linetype = "dashed", color = "red") +
  # geom_path(data = data.frame(x = c(-Inf, 4.5, 4.5), y = c(approx(X, Y, 4.5)$y, approx(X, Y, 4.5)$y, -Inf)), aes(x, y), color = "red", linetype = 2)
  labs_pubr()
ggpar(leaf_stage_regression_plot_35, 
      main = "plant 35",
      xlim = c(18, 38), xticks.by = 2,
      xlab = "DAS (day)",
      ylab = "Leaf Stage of the Main Stem "
)
# plot.title = element_text(hjust = 0.5)
ggsave("plots/leaf_stages/for_paper/leaf_L6_regression_plot_35.png", width = 12, height = 8)

## all tillers ----

### before modified ----
#### for us to see ----
# include all plants(preliminary + formal)


# all linear regression lines for all plant and tillers, before the data modified
leaf_stage_regression_all_tillers_split_original <- leaf_stage |>
  # leaf 3 is included 
  # filter(stage >= 4.0) |> 
  ggscatter(x = "DAS", y = "stage",
            # facet.by = "plant", palette = "jco",
            add = "reg.line",
            conf.int = TRUE, # Add confidence interval
            # cor.method = "pearson", 
            conf.level = 0.95,
            add.params = list(color = "#fc8d62",
                              fill = "gray")
  ) +
  facet_wrap( ~ plant, nrow = 8) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 15, label.y = 8.5, size = 2.5) +
  stat_regline_equation(label.x = 15, label.y = 9.5, size = 2.5) +
  labs_pubr()
leaf_stage_regression_all_tillers_split_original
ggsave("plots/leaf_stages/leaf_stage_regression_all_tillers_split_original.png", width = 24, height = 16)

#### for publication ----
# only formal plants

# all linear regression lines for all plant and tillers, before the data modified
leaf_stage_regression_all_tillers_split_formal <- leaf_stage |>
  # leaf 3 is excluded 
  # filter(stage >= 4.0) |> 
  
  # only formal plants
  filter(pot > 8) |> 
  ggscatter(x = "DAS", y = "stage",
            # facet.by = "plant", palette = "jco",
            add = "reg.line",
            conf.int = TRUE, # Add confidence interval
            # cor.method = "pearson", 
            conf.level = 0.95,
            add.params = list(color = "#fc8d62",
                              fill = "gray")
  ) +
  facet_wrap( ~ plant, nrow = 9) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 15, label.y = 8.5, size = 2.5) +
  # stat_regline_equation(label.x = 15, label.y = 9.5, size = 2.5) +
  labs_pubr()
leaf_stage_regression_all_tillers_split_formal
ggsave("plots/leaf_stages/for_paper/leaf_stage_regression_all_tillers_split_formal.png", width = 24, height = 16)

### after modified ----
# some duplicates are not reasonable, they are modified, see details in the function of plot_all_tillers_split()
plot_all_tillers_split()

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Functions

## DAS_leaf6 function ----
# Define a function to calculate and update DAS_leaf6_by_plant
calcu_DAS_leaf6_by_plant <- function(){
  # Filter BM tillers(main stem) in spike_length
  BM_filtered_spike_length <- spike_length |>
    filter(tiller == "BM")
  # Modify leaf_stage data
  modified_leaf_stage <- leaf_stage |> 
    select(plant, date, DAS, stage) |>
    drop_na(stage) |> 
    # BM_filtered_spike_length is from leaf stage table
    full_join(BM_filtered_spike_length |>
                select(plant, date = sampling.date, DAS = DAS.sampling, stage),
              by = c("plant","date")) |> 
    # filter(!is.na(stage.y)) |>
    # print(n = 510)
    mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y),
           DAS.x = if_else(is.na(DAS.x), DAS.y, DAS.x)
    ) |> 
    select(!stage.y & !DAS.y) |> 
    rename(stage = stage.x, DAS = DAS.x) |> 
    # filter(plant == "1rouge")
    # stage 5.5 for 1rouge might be a mistake(campare spike length and leaf stage table for 1rouge)
    rows_update(tibble(plant = "1rouge", DAS = 23, stage = 5.8), by = c("plant", "DAS")) |> 
    rows_update(tibble(plant = "8rouge", DAS = 28, stage = 7.2), by = c("plant", "DAS"))
  # Find duplicates within each plant group 
  # and replace bigger value of duplicates for each group with a bigger number(+ 0.3)
  dup_leaf_stage <- modified_leaf_stage |>
    group_by(plant, stage) |> 
    mutate(dup_count = n()) |> 
    filter(dup_count > 1) |>
    select(-dup_count, -date) |> 
    ungroup() |> 
    group_by(plant) |> 
    drop_na() |> 
    mutate(stage = if_else(DAS == max(DAS, na.rm = TRUE), stage + 0.3, stage)) |> 
    slice_max(stage)
  # update the leaf stage file considering the change of these duplicates
  dup_modified_leaf_stage <- modified_leaf_stage |> 
    left_join(dup_leaf_stage, join_by(plant, DAS)) |>
    mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y)) |> 
    select(-stage.y, stage = stage.x)
  # after data transformation and tiding, we fit linear model of "leaf stage vs DAS" for each plant,
  # leaves below 3 are usually not considered for linear regression.
  leaf_stage_lms <- dup_modified_leaf_stage |> 
    # filter out leaf 3
    filter(stage >= 4.0) |> 
    select(plant, DAS, stage) |> 
    # filter(plant %in% c("1blanc", "1bleu")) |> 
    group_by(plant) |> 
    do(model = lm(stage ~ DAS, data = .)) 
  # Calculate the x-intercept (DAS of leaf 6) when y (leaf stage) equals 6.0
  DAS_leaf6_by_plant <- leaf_stage_lms |> 
    # mutate(plant = as.character(plant)) |> 
    mutate(
      leaf_stage = 6.0, 
      DAS_leaf6 = (leaf_stage - coef(model)[1])/coef(model)[2],
      # number of ±0.5 around a interger will be rounded to DAS of this interger
      DAS_leaf6_rounded = round(DAS_leaf6),
      # adj_r_squared = summary(model)$adj.r.squared,
      slope = coef(model)[2],
      intercept = coef(model)[1],
      r_squared = summary(model)$r.squared,
      p_value = coef(summary(model))[4]
    ) |> 
    select(plant, DAS_leaf6, DAS_leaf6_rounded)
  
  write_csv(DAS_leaf6_by_plant, "output.csv.files/DAS_leaf6_by_plant.csv")
  
  # Return updated variable
  return(DAS_leaf6_by_plant)
}

## plot function ----
# Define a function to plot regression lines of all tillers 
plot_all_tillers_split <- function(){
  # Filter BM tillers(main stem) in spike_length
  BM_filtered_spike_length <- spike_length |>
    filter(tiller == "BM")
  # Modify leaf_stage data
  modified_leaf_stage <- leaf_stage |> 
    select(plant, date, DAS, stage) |>
    drop_na(stage) |> 
    # BM_filtered_spike_length is from leaf stage table
    full_join(BM_filtered_spike_length |>
                select(plant, date = sampling.date, DAS = DAS.sampling, stage),
              by = c("plant","date")) |> 
    # filter(!is.na(stage.y)) |>
    # print(n = 510)
    mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y),
           DAS.x = if_else(is.na(DAS.x), DAS.y, DAS.x)
    ) |> 
    select(!stage.y & !DAS.y) |> 
    rename(stage = stage.x, DAS = DAS.x) |> 
    # filter(plant == "1rouge")
    # stage 5.5 for 1rouge might be a mistake(campare spike length and leaf stage table for 1rouge)
    rows_update(tibble(plant = "1rouge", DAS = 23, stage = 5.8), by = c("plant", "DAS")) |> 
    rows_update(tibble(plant = "8rouge", DAS = 28, stage = 7.2), by = c("plant", "DAS"))
  # Find duplicates within each plant group 
  # and replace bigger value of duplicates for each group with a bigger number(+ 0.3)
  dup_leaf_stage <- modified_leaf_stage |>
    group_by(plant, stage) |> 
    mutate(dup_count = n()) |> 
    filter(dup_count > 1) |>
    select(-dup_count, -date) |> 
    ungroup() |> 
    group_by(plant) |> 
    drop_na() |> 
    mutate(stage = if_else(DAS == max(DAS, na.rm = TRUE), stage + 0.3, stage)) |> 
    slice_max(stage)
  # update the leaf stage file considering the change of these duplicates
  dup_modified_leaf_stage <- modified_leaf_stage |> 
    left_join(dup_leaf_stage, join_by(plant, DAS)) |>
    mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y)) |> 
    select(-stage.y, stage = stage.x)
  leaf_stage_regression_all_tillers_split_modified <- dup_modified_leaf_stage |>
    # mutate(plant = fct(plant)) |>
    # filter out leaf 3
    filter(stage >= 4.0) |> 
    ggscatter(x = "DAS", y = "stage",
              # facet.by = "plant", palette = "jco",
              add = "reg.line",
              conf.int = TRUE, # Add confidence interval
              # cor.method = "pearson", 
              conf.level = 0.95,
              add.params = list(color = "#fc8d62",
                                fill = "gray")
    ) +
    facet_wrap( ~ plant, nrow = 8) +
    stat_cor(method = "pearson",
             aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
             label.x = 15, label.y = 8.5, size = 2.5) +
    stat_regline_equation(label.x = 15, label.y = 9.5, size = 2.5) +
    labs_pubr()
  ggsave("plots/leaf_stages/leaf_stage_regression_all_tillers_split_modified.png", width = 24, height = 16)
  # Return the plot
  return(leaf_stage_regression_all_tillers_split_modified)
}

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±



















# add info of plants "1rouge", "5blanc", "7rouge" to leaf stage table due to only one point available
# in leaf stage table
to_be_appended_in_spike_to_leaf_stage # from spike length table

to_be_replaced_in_spike_to_leaf_stage # from spike length table

# append
leaf_stage |> 
  group_by(plant) |>
  filter(plant %in% c("1rouge", "5blanc", "7rouge")) |>
  rows_append(to_be_appended_in_spike_to_leaf_stage)

# replace
leaf_stage |> 
  group_by(plant) |>
  # select the latest leaf stage per main stem
  slice_max(stage, n = 1) |> 
  filter(plant %in% to_be_replaced_in_spike_to_leaf_stage$plant) |>   # to_be_replaced_in_spike_to_leaf_stage is from spike length table
  rows_update(tibble(plant = to_be_replaced_in_spike_to_leaf_stage$plant, stage = to_be_replaced_in_spike_to_leaf_stage$stage))

# append and replace
leaf_stage |> 
  # filter(plant %in% c("1rouge", "5blanc", "7rouge")) |>
  # append points "1rouge", "5blanc", "7rouge" from spike length table
  rows_append(to_be_appended_in_spike_to_leaf_stage) |> 
  select(plant, date, DAS, stage) |> 
  left_join(to_be_replaced_in_spike_to_leaf_stage |> select(plant, date, DAS, stage), by = c("date", "plant")) |> 
  # filter(!is.na(stage.y)) |> 
  mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y)) |> 
  select(!stage.y & !DAS.y) |> 
  rename(stage = stage.x, DAS = DAS.x) |> 
  filter(!is.na(stage))


glimpse(leaf_stage)

modified_leaf_stage <- leaf_stage |> 
  select(plant, date, DAS, stage) |>
  drop_na(stage) |> 
  # BM_filtered_spike_length is from leaf stage table
  full_join(BM_filtered_spike_length |> select(plant, date = sampling.date, DAS = DAS.sampling, stage), by = c("plant","date")) |> 
  # filter(!is.na(stage.y)) |>
  # print(n = 510)
  mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y),
         DAS.x = if_else(is.na(DAS.x), DAS.y, DAS.x)
         ) |> 
  select(!stage.y & !DAS.y) |> 
  rename(stage = stage.x, DAS = DAS.x) |> 
  # filter(plant == "1rouge")
  # stage 5.5 for 1rouge might be a mistake(campare spike length and leaf stage table for 1rouge)
  rows_update(tibble(plant = "1rouge", DAS = 23, stage = 5.8), by = c("plant", "DAS")) |> 
  rows_update(tibble(plant = "8rouge", DAS = 28, stage = 7.2), by = c("plant", "DAS"))

modified_leaf_stage |> 
  filter(plant == "8rouge")


# check duplicates stages for each plant(same stage in different dates, which is not reasonable)
modified_leaf_stage |>  
  count(plant, stage) |> 
  filter(n >1)
  # print(n = 50)
  distinct(stage)
  
  
# Find duplicates within each group
dup_leaf_stage <- modified_leaf_stage |>
  group_by(plant, stage) |> 
  mutate(dup_count = n()) |> 
  filter(dup_count > 1) |>
  select(-dup_count, -date) |> 
  ungroup() |> 
  group_by(plant) |> 
  drop_na() |> 
  mutate(stage = if_else(DAS == max(DAS, na.rm = TRUE), stage + 0.3, stage)) |> 
  slice_max(stage)
dup_leaf_stage  
  
print(dup_leaf_stage, n = 66)

dup_modified_leaf_stage <- modified_leaf_stage |> 
  left_join(dup_leaf_stage, join_by(plant, DAS)) |>
  mutate(stage.x = if_else(is.na(stage.y), stage.x, stage.y)) |> 
  select(-stage.y, stage = stage.x)

dup_modified_leaf_stage





to_be_replaced_in_spike_to_leaf_stage$date
leaf_stage  
  
  # replace points of latest leaf stage for sevaral main stems from spike length table
  rows_update(tibble(plant = to_be_replaced_in_spike_to_leaf_stage$plant, stage = to_be_replaced_in_spike_to_leaf_stage$stage)) |> 
  # ungroup for later linear fitting
  ungroup()

leaf_stage
  
#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

view(leaf_stage)

distinct(leaf_stage, sowing.date)

print(leaf_stage, n = 200)

glimpse(leaf_stage)
        
ggplot(leaf_stage) +
  geom_histogram(aes(x = stage), na.rm = TRUE, bins = 30)


leaf_stage |> 
  # filter out leaf 3
  filter(stage >= 4.0) |> 
  # group_by(plant) |> 
  ggplot(aes(x = DAS, y = stage)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  labs(
    x = "DAS (day)",
    y = "Leaf stage"
  ) +
  facet_wrap(~plant)

leaf_stage |> 
  # filter out leaf 3
  filter(stage >= 4.0) |> 
  ggplot() +
  geom_histogram(aes(x = stage), bins = 50)

glimpse(modified_leaf_stage)

# linear regression for all tillers together

leaf_L6_regression_all_tillers_together <- leaf_stage |>
  # filter out leaf 3
  filter(stage >= 4.0) |> 
  ggscatter(x = "DAS", y = "stage",
            # color = "plant", palette = "jco",
            add = "reg.line",
            conf.int = TRUE, # Add confidence interval
            conf.level = 0.95,
            add.params = list(color = "#fc8d62",
                              fill = "gray")
    ) +
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 16, label.y = 9) +
  stat_regline_equation(label.x = 16, label.y = 9.4)
leaf_L6_regression_all_tillers_together
ggsave("plots/leaf_L6_regression_all_tillers_together.png", width = 12, height = 8)







 
  
leaf_stage |>
  filter(stage >= 4.0) |> 
  group_by(plant) |> 
  summarise(cor = cor(stage, DAS)) |> 
print(n = 40)



leaf_stage_lms <- dup_modified_leaf_stage |> 
  # mutate(plant = fct(plant)) |>
  # filter out leaf 3
  filter(stage >= 4.0) |> 
  select(plant, DAS, stage) |> 
  # filter(plant %in% c("1blanc", "1bleu")) |> 
  group_by(plant) |> 
  do(model = lm(stage ~ DAS, data = .))

view(leaf_stage_lms)


# Extract x-intercept from each model
# DAS_L6 <- leaf_stage_lms |> 
#   mutate(x_intercept = -coef(model)[1]/coef(model)[2])

# Calculate the x-intercept (DAS of leaf 6) when y equals 6.0
DAS_leaf6_by_plant <- leaf_stage_lms |> 
  # mutate(plant = as.character(plant)) |> 
  mutate(
    leaf_stage = 6.0, 
    DAS_leaf6 = (leaf_stage - coef(model)[1])/coef(model)[2],
    # number of ±0.5 around a interger will be rounded to DAS of this interger
    DAS_leaf6_rounded = round(DAS_leaf6),
    # adj_r_squared = summary(model)$adj.r.squared,
    slope = coef(model)[2],
    intercept = coef(model)[1],
    r_squared = summary(model)$r.squared,
    p_value = coef(summary(model))[4]
  )


#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


  

DAS_leaf6_by_plant |> 
  filter(r_squared != 0) |> 
  ggplot() +
    geom_histogram(aes(x = r_squared), bins = 60, na.rm = TRUE)
ggsave("plots/leaf_leaf6_regression_r_squared_histogram.png", width = 8, height = 8)



DAS_leaf6_by_plant |> 
  filter(r_squared != 0) |> 
  ggplot() +
  geom_histogram(aes(x = DAS_leaf6), bins = 50, na.rm = TRUE)
ggsave("plots/leaf_leaf6_regression_DAS_histogram.png", width = 8, height = 8)



# check for missing values
# DAS_leaf6_by_plant <- DAS_leaf6_by_plant |> 
#   filter(!is.na(DAS_leaf6) | !is.na(p_value))
# DAS_leaf6_by_plant

DAS_leaf6_by_plant |> 
  filter(plant %in% c("1rouge", "5blanc", "7rouge"))

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# There is only one point for plants "1rouge", "5blanc", "7rouge". To calculate day_after_leaf6 ,
# firstly calculate the average of other fitted slopes as the slope for these three plants,
# together with one known point,then predict the corresponding day_after_leaf6 for each.

plants_1r_5b_7r <- leaf_stage |> 
  select(plant, DAS, stage) |> 
  filter(plant %in% c("1rouge", "5blanc", "7rouge"))
plants_1r_5b_7r

DAS_leaf6_by_plant |> 
  filter(r_squared != 0) |> 
  ggplot() +
  geom_histogram(aes(x = slope), bins = 50, na.rm = TRUE)  


mean_slope <-  DAS_leaf6_by_plant |> 
  filter(slope < 0.25) |> 
  ungroup() |> 
  summarise(mean_slope = mean(slope, na.rm = TRUE, trim = 0.1)) |> 
  # Extract a single number from a tibble
  pull()


DAS_leaf_6_1r_5b_7r <- plants_1r_5b_7r |>
  mutate(intercept = stage - mean_slope * DAS,
         DAS_leaf6 = (6 - intercept) / mean_slope
  )

DAS_leaf_6_1r_5b_7r


# mannual input day_after_leaf6 for plants "1rouge", "5blanc", "7rouge", from excel tab "stage"
DAS_leaf6_by_plant |> 
  rows_update(tibble(plant = c("1rouge", "5blanc", "7rouge"), DAS_leaf6 = c(24.7, 22, 24.7), slope = mean_slope)) |> 
  filter(plant %in% c("1rouge", "5blanc", "7rouge"))

DAS_leaf6_by_plant <- DAS_leaf6_by_plant |> 
  rows_update(tibble(plant = c("1rouge", "5blanc", "7rouge"), DAS_leaf6 = c(24.7, 22, 24.7), slope = mean_slope))

print(DAS_leaf6_by_plant, n = 120)
write_csv(DAS_leaf6_by_plant, "output.csv.files/DAS_leaf6_by_plant.csv")
#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

leaf_stage_day_after_leaf6 <- leaf_stage |> 
  # join calculated DAS_leaf6 to leaf stage table
  left_join(DAS_leaf6_by_plant |> select(!model & !leaf_stage), by = "plant") |> 
  # calculate leaf stage as day after appearance of leaf 6
  mutate(day_after_leaf6 = DAS - DAS_leaf6, .after = DAS) 
  
leaf_stage_day_after_leaf6 |> 
  filter(plant %in% c("1rouge", "5blanc", "7rouge"))

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±







# Check if this is correct to extract p-value?????????????????

# p_value = summary(leaf_stage_lms)$coef[1, 4]
# pval = coef(summary(model))[4]
# view(leaf_stage_lms)

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±






















# DAS_L6_by_plant <- leaf_stage_lms |> 
#     mutate(plant = leaf_stage_lms$plant) |> 
#     select(plant, model) |> 
#     unnest(model) |> 
#     select(plant, term, estimate, std.error, statistic, p.value)
# 
# 
# results <- models %>% 
#   mutate(group = as.character(group)) %>% 
#   mutate(slope = coef(model)[2], 
#          p_value = summary(model)$coefficients[,4], 
#          adj_r_squared = summary(model)$adj.r.squared)
# 
# results
# 
# 
# 
# 
# 
# 
# p_value <- coef(leaf_stage_lms)[2,4]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# summary(leaf_stage_lm)$coefficients[, "p-value"]
# 
# leaf_stage_lm |> 
#   summarise(coefs = coef(model)) |> 
#   map_df(coefs)
# 
# 
# 
# 
# leaf_stage_lm %>% 
#   tidyr::unnest() %>%
#   filter(term == "(Intercept)") %>%
#   filter(y==6)
# 
# 
# library(dplyr)
# data %>% 
#   group_by(group) %>% 
#   do(model = lm(y ~ x, data = .)) %>% 
#   summarise(coefs = coef(model))
#   
# 
# 
# 
# # Fit the linear regression model for each group
# models <- leaf_stage %>% group_by(plant) %>% do(model = lm(stage ~ DAS, data = .))
# 
# # Extract the coefficients for each model
# coefs <- models$model %>% map_df(coef)
# 
# # Find the x-intercept when y equals 6.0 for each group
# x_intercepts <- -coefs$`(Intercept)`/coefs$x
# 
# x_intercepts
# 
# 
# 
# 
# 
# 
# 
# 
# save.image()

