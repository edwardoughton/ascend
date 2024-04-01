### Visualization code for current and future NASA demand
library(tidyverse)
library(ggpubr)
library(readxl)

filename = 'Oughton et al. (2024) Ascend v0.16.0.xlsx'

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current")

#exclude TDRS
data = data[(data$`Exclude from mission count...5` != 1),]

data = select(data, `Code...1`, `Name...2`,  
              `Full...3`, `Use Case...4`, `DTE Minutes 2023`
              )

data = data[1:60,]

#############################################################

ranking = data %>%
  group_by(`Code...1`, `Name...2`,`Use Case...4`) %>%
  summarize(
    minutes = round(sum(`DTE Minutes 2023`)/1e6,4)
  ) %>%
  ungroup() 

ranking <- ranking[with(ranking,order(-minutes)),]
top_8 <- ranking[1:7,]
top_8$mission_id = top_8$`Name...2`
bottom <- ranking[8:26,]
bottom$mission_id = 'Other'
ranking = rbind(top_8, bottom)

df = ranking %>%
  group_by(mission_id,`Use Case...4`) %>%
  summarize(
    minutes = round(sum(minutes),4)
  ) %>%
  ungroup() 

df$`Use Case...4` = factor(
  df$`Use Case...4`,
  levels = c("Human Space Flight","Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial",
             "Unknown Use Case" 
             ),
  labels = c("Human Space Flight", 
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial",
             "Unknown Use Case" )
)

df$mission_id = factor(
  df$mission_id,
  levels = c("AQUA","AURA","ICESAT-2","ISS","LRO","MetOp-B","SMAP","Other"),
  labels = c("Aqua","Aura","ICESAT-2","ISS","LRO","MetOp-B","SMAP","Other")
)

totals <- df %>%
  group_by(`Use Case...4`) %>%
  summarize(minutes = signif(sum(minutes)))

plot1 = 
  ggplot(df, aes(x = `Use Case...4`, y = minutes)) +
  geom_bar(stat = "identity", aes(fill = mission_id)) +
  geom_text(
    aes(
      x = `Use Case...4`,
      y = minutes ,
      label = signif(minutes, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1,
    position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(A) NASA SCaN Direct-To-Earth (DTE) 2023 Demand for All Current Operational Missions",
    subtitle = "Reported by NASA SCaN use case with mission ID provided for major users.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "Mission ID"
  ) +
  scale_y_continuous(
    limits = c(0, 4.55),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 3000, by = 0.5)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    # axis.title.y = element_markdown(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8)
  ) 

#############################################################

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current")

#exclude TDRS
data = data[(data$`Exclude from mission count...18` != 1),]

data = data[(data$`Use Case...17` != "Deep Space Robotic"),]

data = select(data, `Code...14`, `Name...15`,  
              `Full...16`, `Use Case...17`, `SR Minutes 2023`
)

data = data[1:69,]

#############################################################

ranking = data %>%
  group_by(`Code...14`, `Name...15`,`Use Case...17`) %>%
  summarize(
    minutes = round(sum(`SR Minutes 2023`)/1e6,8)
  ) %>%
  ungroup() 

ranking <- ranking[with(ranking,order(-minutes)),]
top <- ranking[1:7,]
top$mission_id = top$`Name...15`
bottom <- ranking[8:69,]
bottom$mission_id = 'Other'
ranking = rbind(top, bottom)

df = ranking %>%
  group_by(mission_id,`Use Case...17`) %>%
  summarize(
    minutes = round(sum(minutes),8)
  ) %>%
  ungroup() 

df$`Use Case...17` = factor(
  df$`Use Case...17`,
  levels = c("Human Space Flight","Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial",
             "Unknown Use Case" 
  ),
  labels = c("Human Space Flight", 
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial",
             "Unknown Use Case" )
)

df$mission_id = factor(
  df$mission_id,
  levels = c("AQUA","AURA","FGST","GPM","HST","ISS","TERRA","Other"),
  labels = c("Aqua","Aura","FGST","GPM","HST","ISS","Terra","Other       ")
)

totals <- df %>%
  group_by(`Use Case...17`) %>%
  summarize(minutes = signif(sum(minutes)))

plot2 = ggplot(df, aes(x = `Use Case...17`, y = minutes)) +
  geom_bar(stat = "identity", aes(fill = mission_id)) +
  geom_text(
    aes(
      x = `Use Case...17`,
      y = minutes ,
      label = signif(minutes, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1,
    position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(B) NASA SCaN Space Relay (SR) 2023 Demand for All Current Operational Missions",
    subtitle = "Reported by NASA SCaN use case, with mission ID provided for major users.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "Mission ID"
  ) +
  scale_y_continuous(
    limits = c(0, 4.55),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 3000, by = 0.5)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    # axis.title.y = element_markdown(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8)
  ) 

#######################################################

panel <- ggarrange(
  plot1,
  plot2,
  nrow = 2,
  ncol = 1,
  common.legend = F,
  legend = "right",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'a_current_mission_demand.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8,
  height = 5.8,
  res = 480
)

print(panel)
dev.off()