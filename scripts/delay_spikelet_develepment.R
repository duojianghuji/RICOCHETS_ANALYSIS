
# Data notes and libraries ----

# Only scenario (optimal control + priming control) (T1 T6 T0a and T0b) are found in the data.??????????????


library(tidyverse)
library(lubridate)
library(ggpubr)
library(plotly)
theme_set(theme_pubr())

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Import data ----

spikelet_flowering_time <- c("raw.data/flowering.spikelet.csv", "raw.data/flowering.spikelet.S0.csv")

# read 2 files together
spikelet_flowering_time <- read_delim(spikelet_flowering_time, delim = ";", id = "plant_type") |> 
  rename(plant = plante, scenario = traitement, tiller = talle, flowering.date = `flowering date`) |> 
  mutate(
    scenario = factor(scenario),
    flowering.date = dmy(flowering.date),
    plant_type = if_else(plant_type == "raw.data/flowering.spikelet.csv", "formal", "preliminary")
  )

glimpse(spikelet_flowering_time)

spikelet_flowering_time |>
  count(plant_type, scenario, plant, tiller)
  filter(n>1) |> 
  distinct(tiller)
  print(n= 133)

spikelet_flowering_time |>
  count(plant_type, scenario, plant, tiller) |> 
filter(n>1) |> 
  distinct(scenario)
print(n= 133)
  

spikelet_flowering_time |>
  filter(plant == 1, tiller == "BM")


spikelet_flowering_time |>
  count(plant) |> 
  filter(n > 1) |> 
  print(n = 150)

spikelet_flowering_time |>
  # filter(plant_type == "preliminary") |> 
  count(plant, tiller) |> 
  filter(tiller == 431)
  print(n = 133)
 
spikelet_flowering_time |>
  filter(plant_type == "formal", plant == 30) |>
  print(n = 132)


mean_median_delay_flowering <- spikelet_flowering_time |>
  drop_na(flowering.date) |> 
  # filter(plant_type == "formal") |>  
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", spikelet.nb = 18, flowering.date = ymd("2021-03-29")),
              by = c("plant", "tiller", "spikelet.nb")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
         ) |> 
  group_by(tiller) |> 
  filter(!(tiller == "BM" & delay_flowering > 0))
  summarise(median_delay_flowering = median(delay_flowering), .groups = "drop")
mean_median_delay_flowering
print(mean_median_delay_flowering, n =132)




# Boxplot ----
## boxplot ggplot ----
spikelet_flowering_time
spikelet_flowering_time |>
  drop_na(flowering.date) |>
  # filter(plant_type == "formal") |>  
  group_by(plant, tiller) |> 
  # Use with_ties = FALSE to return the first n rows when there are many same min values
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |> 
  ungroup() |> 
  filter(!(tiller == "BM" & delay_flowering > 0)) |> 
  mutate(tiller = factor(tiller, levels=c("BM", "T1", "T2", "T3", "T0", "T1.1", "T2.1"))) |> 
  # group_by(tiller) |>
  # filter(plant_type == "formal") |> 
  # filter(tiller != "BM") |> 
  ggplot(aes(x = tiller, y = delay_flowering)) +
  # ggplot(aes(x = reorder(tiller, delay_flowering, mean), y = delay_flowering, fill = tiller)) +
  # geom_boxplot() +
  geom_point(stat = "identity")
  # geom_jitter(shape=16, position=position_jitter(0.15)) +
  stat_mean(geom = "point", shape = 17, color = "red", size = 2) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "none") +
  labs(title = "formal + preliminary", x = "Tiller", y = "Flowering delay (day)") +
  labs_pubr()

## ggboxplot ggpubr ----

spikelet_flowering_time |>
  drop_na(flowering.date) |> 
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |> 
  print(n=132)
  # group_by(tiller) |> 
  # filter(plant_type == "formal") |> 
  filter(!(tiller == "BM" & delay_flowering != 0)) |> 
  # mutate(tiller = factor(tiller)) |>
  ggviolin(
    x = "tiller",
    y = "delay_flowering", 
    title = "formal + preliminary",
    xlab = "Tiller", 
    ylab = "Flowering delay (day)",
    fill = "tiller",
    palette = "Blues",
    add = "boxplot",
    add.params = list(fill = "lightgray"),
    # outlier.shape = 25,
    order = c("BM", "T1", "T2", "T3", "T0", "T1.1", "T2.1"),
    trim = TRUE
  ) +
  stat_mean(geom = "point", shape = 17, color = "#e6550d", size = 2.5) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10.0, by = 1.0)) +
  labs_pubr()


# Bar plot ----
## barplot ggplot ----
spikelet_flowering_time |>
  drop_na(flowering.date) |> 
  # filter(plant_type == "formal") |>  
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |> 
  group_by(tiller) |> 
  filter(!(tiller == "BM" & delay_flowering > 0)) |> 
  # filter(plant_type == "formal") |> 
  summarise(
    n = n(),
    mean_delay_flowering = mean(delay_flowering),
    median_delay_flowering = median(delay_flowering),
    sd = sd(delay_flowering),
    # se = sd/sqrt(n),
    .groups = "drop") |> 
  arrange(mean_delay_flowering) |> 
  # write_csv("output.csv.files/tiller_flowering_delay.csv")
  pivot_longer(mean_delay_flowering:median_delay_flowering, names_to = "mean_or_median") |> 
  ggplot(aes(y = value, x = reorder(tiller, value), fill = mean_or_median)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.8) +
  geom_errorbar(aes(ymin=value - sd, ymax=value + sd), width = .2, position = position_dodge(.9)) +
  geom_text(aes(label = round(value, 1)), position = position_dodge(0.9), vjust = 2, size = 5) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  labs(title = "formal + preliminary", x = "Tiller", y = "flowering delay (day)") +
  scale_fill_discrete(name = NULL,labels=c('mean', 'median')) +
  labs_pubr()



## ggbarplot ggpubr ----
spikelet_flowering_time |>
  drop_na(flowering.date) |>
  # filter(plant_type == "formal") |>
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  # modify one point
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |> 
  group_by(tiller) |>
  # removing outliers
  # (Z-score method), assuming approximately normal data,
  # mutate(mean = mean(delay_flowering),
  #        sd = sd(delay_flowering),
  #        z_score = abs((delay_flowering - mean) / sd)
  # ) |> 
  # filter(z_score <= 2 ) |> 
  
  # remove outliers, Interquartile Range(IQR) method (Tukey's method), more robust to non-normal distributions
  # filter(!(tiller == "BM" & delay_flowering > 0)) |> 
  mutate(IQR = IQR(delay_flowering),
         O_upper = quantile(delay_flowering, probs=c( .75), na.rm = FALSE)+1.5*IQR,
         O_lower = quantile(delay_flowering, probs=c( .25), na.rm = FALSE)-1.5*IQR
  ) |>
  filter(O_lower <= delay_flowering & delay_flowering <= O_upper) |> 

  ggbarplot(
    x = "tiller",
    y = "delay_flowering", 
    title = "Mean_SD",
    xlab = "Tiller", 
    ylab = "Flowering delay (day)",
    label = TRUE,
    lab.nb.digits = 1,
    lab.vjust = -1.1,
    lab.hjust = -0.2,
    fill = "tiller",
    palette = "Blues",
    add = "mean_sd",
    order = c("BM", "T1", "T2", "T3", "T0", "T1.1", "T2.1"),
  ) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10.0, by = 1.0)) +
  labs_pubr()


# Histogram ----
## ggplot ----
spikelet_flowering_time |>
  drop_na(flowering.date) |>
  # filter(plant_type == "formal") |>  
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |> 
  # filter(!(tiller == "BM" & delay_flowering > 0)) |> 
  group_by(plant_type, tiller) |> 
  mutate(mean_delay_flowering = mean(delay_flowering),
         median_delay_flowering = median(delay_flowering)) |> 
  # arrange(tiller) |>
  ggplot(aes(x = delay_flowering, color = plant_type, fill = plant_type)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.2) +
  facet_wrap(~tiller, nrow = 1) +
  geom_vline(aes(xintercept = mean_delay_flowering, color = plant_type, group = plant_type),
             linetype="dashed", show.legend = TRUE) +
  scale_x_continuous(breaks = seq(0, 10.0, by = 2))
  # labs_pubr()

  
## ggpubbr ----
spikelet_flowering_time |>
  drop_na(flowering.date) |>
  # filter(plant_type == "formal") |>  
  group_by(plant_type, plant, tiller) |> 
  slice_min(flowering.date, with_ties = FALSE) |> 
  rows_update(tibble(plant = 1, tiller = "BM", flowering.date = ymd("2021-03-29")), by = c("plant", "tiller")) |>
  distinct(plant, tiller, flowering.date) |> 
  group_by(plant) |> 
  # calculate the delay for all tillers
  mutate(flowering.date.plant = min(flowering.date),
         delay_flowering   = as.numeric(flowering.date - flowering.date.plant)
  ) |>
  # group_by(tiller) |> 
  gghistogram(x = "delay_flowering", y = "count",
              binwidth = 1.0,
              add = "mean",
              facet.by = "tiller", nrow = 1, scales = "fixed",
              color = "plant_type", fill = "plant_type", alpha = 0.2,
              position = "dodge",
              xticks.by = 2)
  # labs_pubr()
  
  # Line plot spikelet delay ----
  
  ## spikelet delay together ----
  spikelet_flowering_time |>
    # nan value will affect the analysis and visualization significantly
    drop_na(flowering.date) |>
    # modify the outlier
    rows_update(tibble(plant = 1, tiller = "BM", spikelet.nb = 18, flowering.date = ymd("2021-03-29")),
                by = c("plant", "tiller", "spikelet.nb")) |> 
    group_by(plant, tiller) |> 
    # firstly calculate earliest flowering time within each tiller, then calculate the flowering delay
    # relative to the first flowering of the plant(usually located in the main spike)
    mutate(flowering.date.tiller = min(flowering.date),
           flowering.date.plant = if_else(tiller == "BM", flowering.date.tiller, NA_Date_),
    ) |> 
    group_by(plant) |> 
    fill(flowering.date.plant, .direction = "down") |> 
    mutate(flowering.date.plant = min(flowering.date),
           delay_tiller_to_plant   = as.numeric(flowering.date.tiller - flowering.date.plant),
           delay_spikelet_to_tiller = as.numeric(flowering.date - flowering.date.tiller),
           delay_spikelet_to_plant = as.numeric(flowering.date - flowering.date.plant)
    ) |> 
    drop_na(spikelet.nb) |>
    group_by(plant, tiller) |> 
    
    # remove outliers, Interquartile Range(IQR) method (Tukey's method), more robust to non-normal distributions
    # mutate(IQR = IQR(delay_spikelet_to_tiller),
    #        O_upper = quantile(delay_spikelet_to_tiller, probs=c( .75), na.rm = FALSE)+1.5*IQR,
    #        O_lower = quantile(delay_spikelet_to_tiller, probs=c( .25), na.rm = FALSE)-1.5*IQR
    # ) |>
    # filter(O_lower <= delay_spikelet_to_tiller & delay_spikelet_to_tiller <= O_upper) |> 
    
    # removing outliers
    # (Z-score method), assuming approximately normal data,
    mutate(mean = mean(delay_spikelet_to_tiller),
           sd = sd(delay_spikelet_to_tiller),
           z_score = abs((delay_spikelet_to_tiller - mean) / sd)
    ) |>
    filter(z_score < 2 ) |> 
    ungroup() |> 
    # filter(spikelet.nb %in% c(22, 23)) |> 
    # all tiller type together(BM, T1, T2, T3)
    ggline(
      x = "spikelet.nb",
      y = "delay_spikelet_to_tiller",
      xlab = "Spikelet order",
      ylab = "Flowering delay of spikelet within spike (days)", 
      add = "mean_se", error.plot = "pointrange",
      color = "steelblue",
      size = 1.05,
      # facet.by = "scenario"
      yticks.by = 0.5
    ) +
    labs_pubr()
  
  
  # filter(tiller == "BM")
  ## spikelet delay split ----
  spikelet_flowering_time |>
    # nan value will affect the analysis and visualization significantly
    drop_na(flowering.date) |>
    # filter(plant_type == "formal") |>  
    # modify the outlier
    rows_update(tibble(plant = 1, tiller = "BM", spikelet.nb = 18, flowering.date = ymd("2021-03-29")),
                by = c("plant", "tiller", "spikelet.nb")) |> 
    group_by(plant, tiller) |> 
    # firstly calculate earliest flowering time within each tiller, then calculate the flowering delay
    # relative to the first flowering of the plant(usually located in the main spike)
    mutate(flowering.date.tiller = min(flowering.date),
           flowering.date.plant = if_else(tiller == "BM", flowering.date.tiller, NA_Date_),
    ) |> 
    group_by(plant) |> 
    fill(flowering.date.plant, .direction = "down") |> 
    mutate(flowering.date.plant = min(flowering.date),
           delay_tiller_to_plant   = as.numeric(flowering.date.tiller - flowering.date.plant),
           delay_spikelet_to_tiller = as.numeric(flowering.date - flowering.date.tiller),
           delay_spikelet_to_plant = as.numeric(flowering.date - flowering.date.plant)
    ) |> 
    drop_na(spikelet.nb) |>
    group_by(plant, tiller) |> 
    
    # remove outliers, Interquartile Range(IQR) method (Tukey's method), more robust to non-normal distributions
    # mutate(IQR = IQR(delay_spikelet_to_tiller),
    #        O_upper = quantile(delay_spikelet_to_tiller, probs=c( .75), na.rm = FALSE)+1.5*IQR,
    #        O_lower = quantile(delay_spikelet_to_tiller, probs=c( .25), na.rm = FALSE)-1.5*IQR
    # ) |>
    # filter(O_lower <= delay_spikelet_to_tiller & delay_spikelet_to_tiller <= O_upper) |> 
    
    # removing outliers
    # (Z-score method), assuming approximately normal data,
    mutate(mean = mean(delay_spikelet_to_tiller),
           sd = sd(delay_spikelet_to_tiller),
           z_score = abs((delay_spikelet_to_tiller - mean) / sd)
    ) |>
    filter(z_score < 2 ) |> 
    ungroup() |> 
    # all tiller type together(BM, T1, T2, T3)
    ggline(
      x = "spikelet.nb",
      y = "delay_spikelet_to_tiller",
      xlab = "Spikelet order",
      ylab = "Flowering delay of spikelet within spike (days)",
      add = "mean",
      color = "tiller", palette = "jco",
      linetype = "tiller", shape = "tiller", size = 1.08,
      # facet.by = "scenario",
      yticks.by = 0.5
    ) +
    labs_pubr()





