### Visualization code for current and future NASA demand
library(tidyverse)
library(ggpubr)
library(readxl)

filename = 'Oughton et al. (2024) Ascend v0.16.0.xlsx'
limit = 9.8
all_data = data.frame()

################################################################
# add_dte

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[79:85,]

df = data[,4:4]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "ADD DTE" 
all_data = rbind(all_data, df)

################################################################
# add_sr

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[91:97,]

df = data[,4:4]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "ADD SR" 
all_data = rbind(all_data, df)

################################################################
# add_le

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[103:104,]

df = data[,4:4]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df$`Use Case` = 'Launch Events'
df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

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

df$type = "ADD LE" 
all_data = rbind(all_data, df)

################################################################
# fddn_dte

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[113:119,]

df = data[,4:4]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "FDDN DTE" 
all_data = rbind(all_data, df)

################################################################
# add_sr

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[125:131,]

df = data[,4:4]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "FDDN SR" 
all_data = rbind(all_data, df)

##############################################################


df = all_data %>%
  group_by(`Use Case`, type) %>%
  summarize(
    value = round(sum(value)/1e6,4)
  )

df$type = factor(
  df$type,
  levels = c("ADD DTE", "ADD SR", "ADD LE", "FDDN DTE", "FDDN SR"),
  labels = c("ADD DTE", "ADD SR", "ADD LE", "FDDN DTE", "FDDN SR")
)

totals <- df %>%
  group_by(type) %>%
  summarize(value = signif(sum(value),3))

plot1 = ggplot(df, aes(x = type, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = type,
      y = value ,
      label = signif(value, 3)
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
    title = "(A) NASA SCaN 2023 Demand for All Current Operational Missions",
    subtitle = "Reported by NASA SCaN use case for Assured Data Delivery (ADD) and File Data Delivery & Networking (FDDN).",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 4.9),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 5, by = .5)
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
## Plot costs
################################################################

limit = 9.8
all_data = data.frame()

################################################################
# add_dte

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[79:85,]

df = data[,5:5]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "ADD DTE" 
all_data = rbind(all_data, df)

################################################################
# add_sr

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[91:97,]

df = data[,5:5]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "ADD SR" 
all_data = rbind(all_data, df)

################################################################
# add_le

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[103:104,]

df = data[,5:5]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df$`Use Case` = 'Launch Events'
df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

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

df$type = "ADD LE" 
all_data = rbind(all_data, df)

################################################################
# fddn_dte

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[113:119,]

df = data[,5:5]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "FDDN DTE" 
all_data = rbind(all_data, df)

################################################################
# add_sr

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Current_Cost")

data = data[125:131,]

df = data[,5:5]

df = cbind(df, data[1])
colnames(df) <- c("Current","Use Case")
df = select(df, `Use Case`, Current)
df[df == "-"] <- 0

df = pivot_longer(df, -c(`Use Case`), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","Current",0)
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

df$type = "FDDN SR" 
all_data = rbind(all_data, df)

##############################################################

df = all_data %>%
  group_by(`Use Case`, type) %>%
  summarize(
    value = round(sum(value)/1e6,4)
  )

df$type = factor(
  df$type,
  levels = c("ADD DTE", "ADD SR", "ADD LE", "FDDN DTE", "FDDN SR"),
  labels = c("ADD DTE", "ADD SR", "ADD LE", "FDDN DTE", "FDDN SR")
)

totals <- df %>%
  group_by(type) %>%
  summarize(value = signif(sum(value),3))

plot2 = ggplot(df, aes(x = type, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = type,
      y = value ,
      label = signif(value, 3)
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
    title = "(B) NASA SCaN 2023 Cost for All Current Operational Missions",
    subtitle = "Reported by NASA SCaN use case for Assured Data Delivery (ADD) and File Data Delivery & Networking (FDDN).",
    x = NULL,
    y = bquote("Service Cost (US$ Millions)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 399),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 400, by = 50)
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
  nrow = 2,
  ncol = 1,
  common.legend = T,
  legend = "bottom",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'b_current_mission_cost.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8,
  height = 5,
  res = 480
)

print(panel)
dev.off()

