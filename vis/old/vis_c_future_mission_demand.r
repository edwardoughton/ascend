### Visualization code for current and future NASA demand
library(tidyverse)
library(ggpubr)
library(readxl)

filename = 'Oughton et al. (2024) Ascend v0.16.0.xlsx'
limit = 9.9

################################################################
# Mission count

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[102:108,]

df = data[,4:6]

df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Baseline",0)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
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

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value)))

plot1 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(A) NASA SCaN Future Mission Scenarios",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Future Mission Count"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 250, by = 10)
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

################################################################
## Service time
## ADD - Direct To Earth

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[102:108,]

df = data[,7:9]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = round(as.numeric(df$value)/1e6, 4)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Baseline",0)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case"
  ),
  labels = c("Human Space Flight",
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case" 
             )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value),3))

plot2 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(B) ADD Direct-To-Earth Demand by Scenario",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, limit),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, limit, by = 1)
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


################################################################
## Service time
# ADD - Space Relay

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[114:120,]

df = data[,7:9]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = round(as.numeric(df$value)/1e6, 4)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Baseline",0)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case"
  ),
  labels = c("Human Space Flight",
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case" 
  )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value),3))

plot3 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(C) ADD Space Relay Demand by Scenario",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, limit),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, limit, by = 1)
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


################################################################
## Service time
# ADD - Launch Event

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[126:127,]

df = data[,7:9]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)
df[df == "-"] <- 0

df$`Use Case` = 'Launch Events'

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = round(as.numeric(df$value)/1e6, 4)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

use_case <- c("Human Space Flight",
                "Near Earth Robotic - LEO Science",
                "Near Earth Robotic - GEO and Near Earth",
                "Near Earth Robotic - Low Latency & Complex Needs",
                "Terrestrial & Aerial",
                "Unknown Use Case")
scenario <- c('Baseline','Baseline','Baseline','Baseline','Baseline','Baseline')
value = c(0,0,0,0,0,0)
df2 <- data.frame(use_case, scenario, value)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case"
  ),
  labels = c("Human Space Flight",
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case" 
  )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value),3))

plot4 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(D) ADD Launch Event Demand by Scenario",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, limit),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, limit, by = 1)
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

################################################################
## Service time
# FDDN - Direct To Earth

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[136:142,]

df = data[,7:9]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = round(as.numeric(df$value)/1e6, 4)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Baseline",0)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case"
  ),
  labels = c("Human Space Flight",
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case" 
  )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value),3))

plot5 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(E) FDDN Direct-To-Earth Demand by Scenario",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, limit),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, limit, by = 1)
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


################################################################
## Service time
# FDDN - Space Relay

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[148:154,]

df = data[,7:9]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low,Baseline,High)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = round(as.numeric(df$value)/1e6, 4)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Baseline",0)
names(df2)<-c("Use Case","scenario","value")
df <- rbind(df, df2)

df$`Use Case` = factor(
  df$`Use Case`,
  levels = c("Human Space Flight",
             "Near Earth Robotic - LEO Science",
             "Near Earth Robotic - GEO and Near Earth",
             "Near Earth Robotic - Low Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case"
  ),
  labels = c("Human Space Flight",
             "Near Earth Robotic\nLEO Science",
             "Near Earth Robotic\nGEO and Near Earth",
             "Near Earth Robotic\nLow Latency & Complex Needs",
             "Launch Events",
             "Terrestrial & Aerial"#,
             # "Unknown Use Case" 
  )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)

totals <- df %>%
  group_by(scenario) %>%
  summarize(value = signif(sum(value),3))

plot6 = ggplot(df, aes(x = scenario, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = scenario,
      y = value ,
      label = signif(value, 2)
    ),
    size = 2.5,
    data = totals,
    vjust = 0.5,
    hjust = -0.1#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(F) FDDN Space Relay Demand by Scenario",
    subtitle = "Reported by NASA SCaN use case.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, limit),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, limit, by = 1)
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
## Panel

panel <- ggarrange(
  plot1,
  plot2,
  plot3,
  plot4,
  plot5,
  plot6,
  nrow = 3,
  ncol = 2,
  # labels = c("A", "B", "C", "D", "E", "F"),
  common.legend = T,
  legend = "bottom",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'c_future_mission_demand.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8,
  height = 6,
  res = 480
)

print(panel)
dev.off()
