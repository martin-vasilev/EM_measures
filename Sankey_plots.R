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
# Sankey decomposition of TVT
# =========================================================
library(dplyr)
library(tibble)
library(networkD3)


sankey_data <- dat_all %>%
  mutate(
    
    # Fixation states
    first_pass_fixated   = nfix1 > 0,
    skipped_first_pass   = nfix1 == 0,
    first_pass_refixated = nfix1 > 1,
    second_pass_fixated  = nfix1 > 0 & TVT > GD,
    skipped_later        = nfix1 == 0 & nfixAll > 0,
    
    # -----------------------------------------------------
    # Duration components
    # -----------------------------------------------------
    
    # First fixation duration
    FFD_dur = ifelse(
      first_pass_fixated,
      FFD,
      0
    ),
    
    # Additional first-pass refixation duration
    refix_dur = ifelse(
      first_pass_refixated,
      pmax(GD - FFD, 0),
      0
    ),
    
    # Additional second-pass rereading duration
    secondpass_dur = ifelse(
      second_pass_fixated,
      pmax(TVT - GD, 0),
      0
    ),
    
    # Initially skipped but later fixated
    skipped_later_dur = ifelse(
      skipped_later,
      TVT,
      0
    )
  )

# =========================================================
# Total duration pool
# =========================================================

TOTAL_TIME <- sum(
  sankey_data$FFD_dur,
  sankey_data$refix_dur,
  sankey_data$secondpass_dur,
  sankey_data$skipped_later_dur,
  na.rm = TRUE
)

# =========================================================
# Compute node values
# =========================================================

firstpass_yes <- sankey_data %>%
  summarise(
    v = sum(
      FFD_dur +
        refix_dur +
        secondpass_dur,
      na.rm = TRUE
    )
  ) %>%
  pull(v)

firstpass_no <- sankey_data %>%
  summarise(
    v = sum(
      skipped_later_dur,
      na.rm = TRUE
    )
  ) %>%
  pull(v)

FFD_total <- sankey_data %>%
  summarise(
    v = sum(FFD_dur, na.rm = TRUE)
  ) %>%
  pull(v)

refix_total <- sankey_data %>%
  summarise(
    v = sum(refix_dur, na.rm = TRUE)
  ) %>%
  pull(v)

secondpass_total <- sankey_data %>%
  summarise(
    v = sum(secondpass_dur, na.rm = TRUE)
  ) %>%
  pull(v)

skip_later_total <- sankey_data %>%
  summarise(
    v = sum(skipped_later_dur, na.rm = TRUE)
  ) %>%
  pull(v)

# =========================================================
# Create Sankey links
# =========================================================

links <- data.frame(
  
  source = c(
    "TFD",
    "TFD",
    
    "First-pass fixated",
    "First-pass fixated",
    "First-pass fixated",
    
    "First-pass skipped"
  ),
  
  target = c(
    "First-pass fixated",
    "First-pass skipped",
    
    "FFD",
    "First-pass refixation\n(GD - FFD)",
    "Second-pass fixation\n(TVT - GD)",
    
    "Later fixation after skip"
  ),
  
  value = c(
    100 * firstpass_yes / TOTAL_TIME,
    100 * firstpass_no / TOTAL_TIME,
    
    100 * FFD_total / TOTAL_TIME,
    100 * refix_total / TOTAL_TIME,
    100 * secondpass_total / TOTAL_TIME,
    
    100 * skip_later_total / TOTAL_TIME
  )
)

# Remove empty flows
links <- links %>%
  filter(value > 0)

# =========================================================
# Create nodes with percentages
# =========================================================

node_percents <- links %>%
  group_by(target) %>%
  summarise(
    percent = sum(value),
    .groups = "drop"
  ) %>%
  rename(name = target) %>%
  bind_rows(
    tibble(
      name = "TFD",
      percent = 100
    )
  ) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    label = paste0(
      name,
      "\n",
      sprintf("%.1f%%", percent)
    )
  )

nodes <- data.frame(
  name = unique(c(links$source, links$target))
) %>%
  left_join(node_percents, by = "name") %>%
  mutate(
    label = ifelse(
      is.na(label),
      name,
      label
    )
  )

# =========================================================
# Convert node labels to IDs
# =========================================================

links$IDsource <- match(
  links$source,
  nodes$name
) - 1

links$IDtarget <- match(
  links$target,
  nodes$name
) - 1

# =========================================================
# Plot Sankey
# =========================================================

sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "label",
  fontSize = 70,
  nodeWidth = 35,
  width = 3000,
  height = 1000,
  sinksRight = FALSE,
  margin = list(top = 20, right = 20, bottom = 20, left = 20))

library(webshot2)
# Save HTML
saveNetwork(
  sankey,
  "Plots/sankey.html",
  selfcontained = TRUE
)

# # Direct HTML -> PDF
# webshot(
#   "Plots/sankey.html",
#   file = "sankey.pdf",
#   vwidth = 1800,
#   vheight = 1200,
#   zoom = 2
# )

webshot2::webshot(
  "Plots/sankey.html",
  file = "Plots/sankey.png",
  vwidth = 3000,
  vheight = 2000,
  zoom = 2
)


library(dplyr)
library(tibble)
library(networkD3)

# =========================================================
# Sankey decomposition of GPT
# ONLY words fixated on first pass (nfix1 > 0)
# =========================================================

sankey_data_gpt <- dat_all %>%
  
  # Keep only words fixated during first pass
  filter(nfix1 > 0) %>%
  
  mutate(
    
    # -----------------------------------------------------
    # Fixation states
    # -----------------------------------------------------
    
    first_pass_refixated = nfix1 > 1,
    
    # Later rereading after first-pass reading
    second_pass_fixated = TVT > GD,
    
    # -----------------------------------------------------
    # GPT decomposition
    # -----------------------------------------------------
    
    # First fixation duration
    FFD_dur = FFD,
    
    # Additional first-pass fixation time
    refix_dur = ifelse(
      first_pass_refixated,
      pmax(GD - FFD, 0),
      0
    ),
    
    # Time spent regressing to the left
    # before progressing past the word
    left_regression_dur = ifelse(
      !is.na(GPT) & !is.na(GD),
      pmax(GPT - GD, 0),
      0
    ),
    
    # Later rereading AFTER first-pass progression
    secondpass_dur = ifelse(
      second_pass_fixated,
      pmax(TVT - GD, 0),
      0
    )
  )

# =========================================================
# Total duration pool
# =========================================================

TOTAL_TIME_GPT <- sum(
  sankey_data_gpt$FFD_dur,
  sankey_data_gpt$refix_dur,
  sankey_data_gpt$left_regression_dur,
  sankey_data_gpt$secondpass_dur,
  na.rm = TRUE
)

# =========================================================
# Component totals
# =========================================================

FFD_total <- sankey_data_gpt %>%
  summarise(v = sum(FFD_dur, na.rm = TRUE)) %>%
  pull(v)

refix_total <- sankey_data_gpt %>%
  summarise(v = sum(refix_dur, na.rm = TRUE)) %>%
  pull(v)

left_regression_total <- sankey_data_gpt %>%
  summarise(v = sum(left_regression_dur, na.rm = TRUE)) %>%
  pull(v)

secondpass_total <- sankey_data_gpt %>%
  summarise(v = sum(secondpass_dur, na.rm = TRUE)) %>%
  pull(v)

# =========================================================
# Create Sankey links
# =========================================================

links_gpt <- data.frame(
  
  source = c(
    "GPT",
    "GPT",
    "GPT",
    "GPT"
  ),
  
  target = c(
    "FFD",
    "First-pass refixation\n(GD - FFD)",
    "Fixations to left\n(GPT - GD)",
    "Second-pass fixation\n(TVT - GD)"
  ),
  
  value = c(
    100 * FFD_total / TOTAL_TIME_GPT,
    100 * refix_total / TOTAL_TIME_GPT,
    100 * left_regression_total / TOTAL_TIME_GPT,
    100 * secondpass_total / TOTAL_TIME_GPT
  )
)

links_gpt <- links_gpt %>%
  filter(value > 0)

# =========================================================
# Create nodes with percentages
# =========================================================

node_percents_gpt <- links_gpt %>%
  group_by(target) %>%
  summarise(
    percent = sum(value),
    .groups = "drop"
  ) %>%
  rename(name = target) %>%
  bind_rows(
    tibble(
      name = "GPT",
      percent = 100
    )
  ) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    label = paste0(
      name,
      "\n",
      sprintf("%.1f%%", percent)
    )
  )

nodes_gpt <- data.frame(
  name = unique(c(links_gpt$source, links_gpt$target))
) %>%
  left_join(node_percents_gpt, by = "name") %>%
  mutate(
    label = ifelse(
      is.na(label),
      name,
      label
    )
  )

# =========================================================
# Convert node labels to IDs
# =========================================================

links_gpt$IDsource <- match(
  links_gpt$source,
  nodes_gpt$name
) - 1

links_gpt$IDtarget <- match(
  links_gpt$target,
  nodes_gpt$name
) - 1

# =========================================================
# Plot Sankey
# =========================================================

sankey_gpt <- sankeyNetwork(
  Links = links_gpt,
  Nodes = nodes_gpt,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "label",
  fontSize = 50,
  nodeWidth = 45,
  width = 2000,
  height = 1000,
  sinksRight = FALSE,
  margin = list(top = 20, right = 20, bottom = 20, left = 20)
)

sankey_gpt

# Save HTML
saveNetwork(
  sankey_gpt,
  "Plots/sankey_GPT.html",
  selfcontained = TRUE
)

# # Direct HTML -> PDF
# webshot(
#   "Plots/sankey_GPT.html",
#   file = "sankey.pdf",
#   vwidth = 1800,
#   vheight = 1200,
#   zoom = 2
# )

webshot2::webshot(
  "Plots/sankey_GPT.html",
  file = "Plots/sankey_GPT.png",
  vwidth = 2200,
  vheight = 1000,
  zoom = 2
)


library(magick)

image_read("Plots/sankey.png") %>%
  image_trim(fuzz = 10) %>%
  image_write("Plots/sankey_trim.png")

image_read("Plots/sankey_GPT.png") %>%
  image_trim(fuzz = 10) %>%
  image_write("Plots/sankey_GPT_trim.png")


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

library(png)
library(grid)
library(ggplotify)
library(patchwork)

# Read Sankey PNGs
sankey_tvt_img <- png::readPNG("Plots/sankey.png")
sankey_gpt_img <- png::readPNG("Plots/sankey_GPT.png")

# Convert to ggplot objects
p_sankey_tvt <- as.ggplot(
  grid::rasterGrob(sankey_tvt_img, interpolate = TRUE)
) +
  labs(title = "b)") +
  theme(plot.title = element_text(size = 16, face = "bold"))

p_sankey_gpt <- as.ggplot(
  grid::rasterGrob(sankey_gpt_img, interpolate = TRUE)
) +
  labs(title = "c)") +
  theme(plot.title = element_text(size = 16, face = "bold"))


# Combine into panel
final_panel <- p_sankey_tvt /
                                    p_sankey_gpt
 


ggsave(
  "Plots/final_panel.pdf",
  final_panel,
  width = 7,
  height = 7,
  units = "in"
)

