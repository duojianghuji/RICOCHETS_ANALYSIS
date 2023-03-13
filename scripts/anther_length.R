
# Data notes and libraries ----

# 
library(tidyverse)
library(readxl)
library(lubridate)
library(ggpubr)
library(plotly)
theme_set(theme_pubr())

#±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
# Import data ----
anther_length <- read_excel("raw.data/anthere.length.new.xlsx")
  select(plant = plante,
         scenario = traitement,
         sampling.date = `date prélèvement`,
         tiller = talle,
         anther.length.mm = `Lg anthères (mm)`,
         adjusted_spike_age_after_TS = age_épi_ajust,
         ) |> 
  mutate(sampling.date = ymd(sampling.date),
         scenario = factor(scenario),
         pot = parse_number(plant),
         .before = 1,
         sowing.date = if_else(pot <= 8, dmy("08-Feb-2021"), dmy("15-Feb-2021")),
         DAS = as.numeric(sampling.date - sowing.date),
         rounded_adjusted_spike_age_after_TS = round(adjusted_spike_age_after_TS)
         )

anther_length |> 
  ggplot(aes(adjusted_spike_age_after_TS, anther.length.mm)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3))
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  theme_bw()
  
  
  # Fit polynomial regression line and add labels
  formula <- y ~ poly(x, 3, raw = TRUE)
p <- ggplot(my.data, aes(x, y2, color = group)) +
  geom_point() +
  stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  theme_bw()
ggpar(p, palette = "jco")
P
  
  
  
  
  
anther_length |> 
  ggscatter(
    x = "rounded_adjusted_spike_age_after_TS",
    y = "anther.length.mm",
    add = "loess",
    conf.int = TRUE, # Add confidence interval
    conf.level = 0.95,
    add.params = list(color = "#fc8d62", fill = "gray")
    
  
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

DAS_leaf6_by_plant <- read.csv("output.csv.files/DAS_leaf6_by_plant.csv", sep = ",")

average_stage <- read_excel("raw.data/average.stage.BM.xlsx")

DAS_leaf6_by_plant

spike_length_vs_day_after_leaf6

# remove duplicates and remove plants with 8 flag leaves
spike_length_vs_day_after_leaf6 <- spike_length  |> 
  # Notice!! exclude = NULL include the NA value as a level
  mutate(FLN.BM = factor(FLN.BM, exclude = NULL, labels = c(levels(factor(FLN.BM)), "NA-7"))) |> 
  # include all tillers
  filter(tiller == "BM") |>
  left_join(DAS_leaf6_by_plant, by = c("plant")) |> 
  mutate(
    day_after_leaf6 = DAS.sampling - DAS_leaf6_rounded
  ) |> 
  # filter(plant %in% c("1rouge", "5blanc", "7rouge"))
  select(plant, tiller, FLN.BM, spike.length.mm, sampling.date, DAS.sampling, DAS_leaf6, DAS_leaf6_rounded, day_after_leaf6) |> 
  arrange(spike.length.mm)












anther_length |> 
  # filter(tiller == "BM") |>
  left_join(spike_length_vs_day_after_leaf6, by = c("plant")) 
  # print(n=85)
  ggscatter(
    x = "day_after_leaf6",
    y = "anther.length.mm"
  )
  
