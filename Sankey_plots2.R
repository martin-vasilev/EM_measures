rm(list= ls())

load("data/Provo.Rda") # Provo corpus
load("data/geco.Rda") # GECO corpus
textFont <- read.csv("data/Vasilev2021_word_data.csv")
Oz<- read.csv("data/Oz_words.csv")


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F", "orange") # "Classic & trustworthy"

geco$word_length<- geco$word_len
geco.c<- subset(geco, nfixAll<100)

# combine all datasets
dat_all <- bind_rows(
  geco     %>% mutate(corpus = "GECO",     sub = as.character(sub)),
  Provo    %>% mutate(corpus = "Provo",    sub = as.character(sub)),
  Oz       %>% mutate(corpus = "Oz",       sub = as.character(sub)),
  textFont %>% mutate(corpus = "Text Font", sub = as.character(sub))
)


# =========================================================
# TVT / TFD Sankey using ggsankey
# =========================================================

library(dplyr)
library(tibble)
library(ggplot2)
library(ggsankey)

# ---------------------------------------------------------
# Prepare TVT components
# ---------------------------------------------------------

sankey_data <- dat_all %>%
  mutate(
    first_pass_fixated   = nfix1 > 0,
    first_pass_refixated = nfix1 > 1,
    second_pass_fixated  = nfix1 > 0 & TVT > GD,
    skipped_later        = nfix1 == 0 & nfixAll > 0,
    
    FFD_dur = ifelse(first_pass_fixated, FFD, 0),
    refix_dur = ifelse(first_pass_refixated, pmax(GD - FFD, 0), 0),
    secondpass_dur = ifelse(second_pass_fixated, pmax(TVT - GD, 0), 0),
    skipped_later_dur = ifelse(skipped_later, TVT, 0)
  )

TOTAL_TIME <- sum(
  sankey_data$FFD_dur,
  sankey_data$refix_dur,
  sankey_data$secondpass_dur,
  sankey_data$skipped_later_dur,
  na.rm = TRUE
)

firstpass_yes <- sum(
  sankey_data$FFD_dur +
    sankey_data$refix_dur +
    sankey_data$secondpass_dur,
  na.rm = TRUE
)

firstpass_no <- sum(
  sankey_data$skipped_later_dur,
  na.rm = TRUE
)

FFD_total <- sum(sankey_data$FFD_dur, na.rm = TRUE)
refix_total <- sum(sankey_data$refix_dur, na.rm = TRUE)
secondpass_total <- sum(sankey_data$secondpass_dur, na.rm = TRUE)
skip_later_total <- sum(sankey_data$skipped_later_dur, na.rm = TRUE)

# ---------------------------------------------------------
# Define full paths
# ---------------------------------------------------------

tvt_paths <- tibble(
  id = c(
    "FFD",
    "refix",
    "secondpass",
    "skipped_later"
  ),
  
  stage1 = "TFD\n100.0%",
  
  stage2 = c(
    paste0(
      "First-pass fixated\n",
      sprintf("%.1f%%", 100 * firstpass_yes / TOTAL_TIME)
    ),
    paste0(
      "First-pass fixated\n",
      sprintf("%.1f%%", 100 * firstpass_yes / TOTAL_TIME)
    ),
    paste0(
      "First-pass fixated\n",
      sprintf("%.1f%%", 100 * firstpass_yes / TOTAL_TIME)
    ),
    paste0(
      "First-pass skipped\n",
      sprintf("%.1f%%", 100 * firstpass_no / TOTAL_TIME)
    )
  ),
  
  stage3 = c(
    paste0(
      "FFD\n",
      sprintf("%.1f%%", 100 * FFD_total / TOTAL_TIME)
    ),
    paste0(
      "First-pass refixation\n(GD - FFD)\n",
      sprintf("%.1f%%", 100 * refix_total / TOTAL_TIME)
    ),
    paste0(
      "Second-pass fixation\n(TVT - GD)\n",
      sprintf("%.1f%%", 100 * secondpass_total / TOTAL_TIME)
    ),
    paste0(
      "Later fixation after skip\n",
      sprintf("%.1f%%", 100 * skip_later_total / TOTAL_TIME)
    )
  ),
  
  value = c(
    100 * FFD_total / TOTAL_TIME,
    100 * refix_total / TOTAL_TIME,
    100 * secondpass_total / TOTAL_TIME,
    100 * skip_later_total / TOTAL_TIME
  )
) %>%
  filter(value > 0)

# ---------------------------------------------------------
# Convert paths to long ggsankey format
# ---------------------------------------------------------

tvt_sankey_long <- tvt_paths %>%
  make_long(stage1, stage2, stage3, value = value) %>%
  mutate(
    id = rep(tvt_paths$id, each = 3),
    x = factor(x, levels = c("stage1", "stage2", "stage3")),
    next_x = factor(next_x, levels = c("stage1", "stage2", "stage3"))
  )

# ---------------------------------------------------------
# Force node order to reduce crossing
# ---------------------------------------------------------

node_order <- rev(c(
  unique(tvt_paths$stage1),
  unique(tvt_paths$stage2),
  tvt_paths$stage3
))

tvt_sankey_long <- tvt_sankey_long %>%
  mutate(
    node = factor(node, levels = node_order),
    next_node = factor(next_node, levels = node_order)
  )



# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------

p_sankey_tvt <- ggplot(
  tvt_sankey_long,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    value = value,
    fill = node,
    label = node
  )
) +
  geom_sankey(
    aes(group = id),
    flow.alpha = 0.45,
    node.color = "grey30",
    show.legend = FALSE
  ) +
  geom_sankey_label(
    size = 4,
    color = "black",
    fill = "white"
  ) +
  scale_x_discrete(
    labels = c(
      stage1 = "Stage 1",
      stage2 = "Stage 2",
      stage3 = "Stage 3"
    )
  ) +
  theme_sankey(base_size = 18) +
  labs(
    title = "a)",
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(5, 5, 5, 5)
  )

p_sankey_tvt


ggsave(filename = 'Plots/sankey_TVT.pdf',
       plot = p_sankey_tvt, height = 8, width = 10)


# =========================================================
# Sankey decomposition of GPT
# ONLY words fixated on first pass (nfix1 > 0)
# =========================================================

# =========================================================
# GPT Sankey using ggsankey
# Native geom_sankey_label() labels, slightly nudged right
# =========================================================

library(dplyr)
library(tibble)
library(ggplot2)
library(ggsankey)

# ---------------------------------------------------------
# Prepare GPT components
# ---------------------------------------------------------

sankey_data_gpt <- dat_all %>%
  filter(nfix1 > 0) %>%
  mutate(
    first_pass_refixated = nfix1 > 1,
    second_pass_fixated  = TVT > GD,
    
    FFD_dur = FFD,
    
    refix_dur = ifelse(
      first_pass_refixated,
      pmax(GD - FFD, 0),
      0
    ),
    
    left_regression_dur = ifelse(
      !is.na(GPT) & !is.na(GD),
      pmax(GPT - GD, 0),
      0
    ),
    
    secondpass_dur = ifelse(
      second_pass_fixated,
      pmax(TVT - GD, 0),
      0
    )
  )

# ---------------------------------------------------------
# Total GPT-related duration pool
# ---------------------------------------------------------

TOTAL_TIME_GPT <- sum(
  sankey_data_gpt$FFD_dur,
  sankey_data_gpt$refix_dur,
  sankey_data_gpt$left_regression_dur,
  sankey_data_gpt$secondpass_dur,
  na.rm = TRUE
)

# ---------------------------------------------------------
# Component totals
# ---------------------------------------------------------

FFD_total_gpt <- sum(
  sankey_data_gpt$FFD_dur,
  na.rm = TRUE
)

refix_total_gpt <- sum(
  sankey_data_gpt$refix_dur,
  na.rm = TRUE
)

left_regression_total_gpt <- sum(
  sankey_data_gpt$left_regression_dur,
  na.rm = TRUE
)

secondpass_total_gpt <- sum(
  sankey_data_gpt$secondpass_dur,
  na.rm = TRUE
)

# ---------------------------------------------------------
# Define GPT paths
# ---------------------------------------------------------

gpt_paths <- tibble(
  id = c(
    "FFD",
    "refix",
    "left_regression",
    "secondpass"
  ),
  
  stage1 = "GPT\n100.0%",
  
  stage2 = c(
    paste0(
      "FFD\n",
      sprintf("%.1f%%", 100 * FFD_total_gpt / TOTAL_TIME_GPT)
    ),
    paste0(
      "First-pass refixation\n(GD - FFD)\n",
      sprintf("%.1f%%", 100 * refix_total_gpt / TOTAL_TIME_GPT)
    ),
    paste0(
      "Fixations to left\n(GPT - GD)\n",
      sprintf("%.1f%%", 100 * left_regression_total_gpt / TOTAL_TIME_GPT)
    ),
    paste0(
      "Second-pass fixation\n(TVT - GD)\n",
      sprintf("%.1f%%", 100 * secondpass_total_gpt / TOTAL_TIME_GPT)
    )
  ),
  
  value = c(
    100 * FFD_total_gpt / TOTAL_TIME_GPT,
    100 * refix_total_gpt / TOTAL_TIME_GPT,
    100 * left_regression_total_gpt / TOTAL_TIME_GPT,
    100 * secondpass_total_gpt / TOTAL_TIME_GPT
  )
) %>%
  filter(value > 0)

# ---------------------------------------------------------
# Convert paths to long ggsankey format
# ---------------------------------------------------------

gpt_sankey_long <- gpt_paths %>%
  make_long(stage1, stage2, value = value) %>%
  mutate(
    id = rep(gpt_paths$id, each = 2),
    x = factor(x, levels = c("stage1", "stage2")),
    next_x = factor(next_x, levels = c("stage1", "stage2"))
  )

# ---------------------------------------------------------
# Force node order
# ---------------------------------------------------------

node_order_gpt <- rev(c(
  unique(gpt_paths$stage1),
  gpt_paths$stage2
))

gpt_sankey_long <- gpt_sankey_long %>%
  mutate(
    node = factor(node, levels = node_order_gpt),
    next_node = factor(next_node, levels = node_order_gpt)
  )

# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------

p_sankey_gpt <- ggplot(
  gpt_sankey_long,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    value = value,
    fill = node,
    label = node
  )
) +
  geom_sankey(
    aes(group = id),
    flow.alpha = 0.45,
    node.color = "grey30",
    show.legend = FALSE
  ) +
  geom_sankey_label(
    size = 4.5,
    color = "black",
    fill = "white",
    hjust = 0,
    nudge_x = 0.08
  ) +
  scale_x_discrete(
    labels = c(
      stage1 = "Stage 1",
      stage2 = "Stage 2"
    )
  ) +
  theme_sankey(base_size = 12) +
  labs(
    title = "c)",
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.margin = margin(5, 30, 5, 5)
  )

p_sankey_gpt


ggsave(filename = 'Plots/sankey_GPT.pdf',
       plot = p_sankey_gpt, height = 8, width = 10)

#### stacked graph:

dat_all <- dat_all %>%
  mutate(
    FFD_time        = FFD,
    Refix_time      = GD - FFD,
    Regression_time = TVT - GD
  )

dat_all %>%
  #  group_by(corpus) %>%
  summarise(
    Refix_time_M      = mean(Refix_time, na.rm = TRUE),
    Refix_time_SD   = sd(Refix_time, na.rm = TRUE)  
  )

dat_long_time <- dat_all %>%
  group_by(corpus) %>%
  summarise(
    FFD_time        = mean(FFD_time, na.rm = TRUE),
    Refix_time      = mean(Refix_time, na.rm = TRUE),
    Regression_time = mean(Regression_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(FFD_time, Refix_time, Regression_time),
    names_to = "Component",
    values_to = "Time"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c("Regression_time", "Refix_time", "FFD_time")
    )
  )

fixation_comps<- ggplot(dat_long_time, aes(x = corpus, y = Time, fill = Component)) +
  geom_col(width = 0.7) +
  labs(
    x = "Corpus",
    y = "Mean total fixation\nduration per word",
    fill = "Reading time component",
    title= 'a)'
  ) +
  scale_fill_manual(
    values = c(
      "FFD_time"        = pallete1[2],
      "Refix_time"      = pallete1[3],
      "Regression_time" = pallete1[4]
    ),
    labels = c(
      "FFD_time"        = "First fixation duration (FFD)",
      "Refix_time"      = "Refixation duration (GD - FFD)",
      "Regression_time" = "Regression duration (TFD - GD)"
    )
  ) +
  theme_bw(12)+
  theme(legend.position = 'right',
        legend.direction = "vertical")

fixation_comps




### Combine in a single graph:
final_panel <-  p_sankey_tvt/ p_sankey_gpt
 


ggsave(
  "Plots/final_panel.pdf",
  final_panel,
  width = 10,
  height = 12,
  units = "in"
)

