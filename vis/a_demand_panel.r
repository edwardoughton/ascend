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
  labels = c("Human Space\nFlight", 
             "NER LEO\nScience",
             "NER GEO and\nNear Earth",
             "NER Low Latency\n& Complex Needs",
             "Launch Events",
             "Terrestrial &\nAerial",
             "Unknown Use\nCase" )
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
    title = "(A) NASA Direct-To-Earth (DTE) 2023 Demand",
    subtitle = "Reported by NASA SCaN use case with mission ID provided for major users.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "Mission\nID"
  ) +
  scale_y_continuous(
    limits = c(0, 4.75),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 10, by = 0.5)
  ) + guides(fill=guide_legend(ncol=6, nrow=2)) +
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
  labels = c("Human Space\nFlight", 
             "NER LEO\nScience",
             "NER GEO and\nNear Earth",
             "NER Low Latency\n& Complex Needs",
             "Launch Events",
             "Terrestrial &\nAerial",
             "Unknown Use\nCase" )
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
    title = "(B) NASA Space Relay (SR) 2023 Demand",
    subtitle = "Reported by NASA SCaN use case with mission ID provided for major users.",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "Mission ID"
  ) +
  scale_y_continuous(
    limits = c(0, 4.75),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 10, by = 0.5)
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


# limit = 9.8
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

# df$`Use Case` = factor(
#   df$`Use Case`,
#   levels = c("Human Space Flight",
#              "Near Earth Robotic - LEO Science",
#              "Near Earth Robotic - GEO and Near Earth",
#              "Near Earth Robotic - Low Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial",
#              "Unknown Use Case"
#   ),
#   labels = c("Human Space Flight",
#              "Near Earth Robotic\nLEO Science",
#              "Near Earth Robotic\nGEO and Near Earth",
#              "Near Earth Robotic\nLow Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial",
#              "Unknown Use Case" )
# )

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

# df$`Use Case` = factor(
#   df$`Use Case`,
#   levels = c("Human Space Flight",
#              "Near Earth Robotic - LEO Science",
#              "Near Earth Robotic - GEO and Near Earth",
#              "Near Earth Robotic - Low Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial"#,
#              # "Unknown Use Case"
#   ),
#   labels = c("Human Space Flight",
#              "Near Earth Robotic\nLEO Science",
#              "Near Earth Robotic\nGEO and Near Earth",
#              "Near Earth Robotic\nLow Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial"#,
#              # "Unknown Use Case" 
#   )
# )

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

# df$`Use Case` = factor(
#   df$`Use Case`,
#   levels = c("Human Space Flight",
#              "Near Earth Robotic - LEO Science",
#              "Near Earth Robotic - GEO and Near Earth",
#              "Near Earth Robotic - Low Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial"#,
#              # "Unknown Use Case"
#   ),
#   labels = c("Human Space Flight",
#              "Near Earth Robotic\nLEO Science",
#              "Near Earth Robotic\nGEO and Near Earth",
#              "Near Earth Robotic\nLow Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial"#,
#              # "Unknown Use Case" 
#   )
# )

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

# df$`Use Case` = factor(
#   df$`Use Case`,
#   levels = c("Human Space Flight",
#              "Near Earth Robotic - LEO Science",
#              "Near Earth Robotic - GEO and Near Earth",
#              "Near Earth Robotic - Low Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial",
#              "Unknown Use Case"
#   ),
#   labels = c("Human Space Flight",
#              "Near Earth Robotic\nLEO Science",
#              "Near Earth Robotic\nGEO and Near Earth",
#              "Near Earth Robotic\nLow Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial",
#              "Unknown Use Case" )
# )

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

# df$`Use Case` = factor(
#   df$`Use Case`,
#   levels = c("Human Space Flight",
#              "Near Earth Robotic - LEO Science",
#              "Near Earth Robotic - GEO and Near Earth",
#              "Near Earth Robotic - Low Latency & Complex Needs",
#              "Launch Events",
#              "Terrestrial & Aerial"#,
#              # "Unknown Use Case"
#   ),
#   labels = c("Human Space Flight",
#              "NER LEO\nScience",
#              "NER GEO and\nNear Earth",
#              "NER Low Latency &\nComplex Needs",
#              "Launch Events",
#              "Terrestrial &\nAerial"#,
#              # "Unknown Use Case" 
#   )
# )

df$type = "FDDN SR" 
all_data = rbind(all_data, df)

##############################################################

df = all_data %>%
  group_by(`Use Case`, type) %>%
  summarize(
    value = round(sum(value)/1e6,4)
  )

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
  labels = c("Human Space\nFlight",
             "NER LEO\nScience",
             "NER GEO and\nNear Earth",
             "NER Low Latency &\nComplex Needs",
             "Launch Events",
             "Terrestrial &\nAerial"#,
             # "Unknown Use Case"
  )
)

df$type = factor(
  df$type,
  levels = c("ADD DTE", "ADD SR", "ADD LE", "FDDN DTE", "FDDN SR"),
  labels = c("ADD DTE\nCurrent", "ADD SR\nCurrent", "ADD LE\nCurrent", 
             "FDDN DTE\nCurrent", "FDDN SR\nCurrent")
)

totals <- df %>%
  group_by(type) %>%
  summarize(value = signif(sum(value),3))

plot3 = ggplot(df, aes(x = type, y = value)) +
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
    title = "(C) Total NASA 2023 Demand for Current Operational Missions",
    subtitle = "Reported for Assured Data Delivery (ADD) and File Data Delivery & Networking (FDDN).",
    x = NULL,
    y = bquote("Service Demand (Millions of Minutes)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 4.75),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 10, by = .5)
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
# Mission count

all_data = data.frame()
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[102:108,]

df = data[,4:6]

df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low, Baseline, High)
df$type = 'ADD DTE'
  
df = pivot_longer(df, -c(`Use Case`,type), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events",'ADD DTE',"Baseline",0)
names(df2)<-c("Use Case","type","scenario","value")
df <- rbind(df, df2)
all_data = rbind(all_data, df)

################################################################
## Service time
## ADD - Direct To Earth

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[114:120,]

df = data[,4:6]

df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low, Baseline, High)
df$type = 'ADD SR'

df = pivot_longer(df, -c(`Use Case`,type), values_to = "value", names_to = "scenario")
df$value = as.numeric(df$value)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events",'ADD DTE',"Baseline",0)
names(df2)<-c("Use Case","type","scenario","value")
df <- rbind(df, df2)
all_data = rbind(all_data, df)

################################################################
## Service time
# ADD - Launch Event

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[126:127,]

df = data[,4:6]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low, Baseline, High)
df[df == "-"] <- 0
df$type = 'ADD LE'

df$`Use Case` = 'Launch Events'

df = pivot_longer(df, -c(`Use Case`,type), values_to = "value", names_to = "scenario")
# df$value = round(as.numeric(df$value)/1e6, 6)

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

# use_case <- c("Human Space Flight",
#               "Near Earth Robotic - LEO Science",
#               "Near Earth Robotic - GEO and Near Earth",
#               "Near Earth Robotic - Low Latency & Complex Needs",
#               "Terrestrial & Aerial",
#               "Unknown Use Case")
# type <- c('ADD LE','ADD LE','ADD LE','ADD LE','ADD LE','ADD LE')
# scenario <- c('Baseline','Baseline','Baseline','Baseline','Baseline','Baseline')
# value = c(0,0,0,0,0,0)
# df2 <- data.frame(use_case, type, scenario, value)
# names(df2)<-c("Use Case","type","scenario","value")
# df <- rbind(df, df2)
all_data = rbind(all_data, df)

################################################################
## Service time
# FDDN - Direct To Earth

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[136:142,]

df = data[,4:6]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low, Baseline, High)
df[df == "-"] <- 0
df$type = 'FDDN DTE'

df = pivot_longer(df, -c(`Use Case`, type), 
                  values_to = "value", 
                  names_to = "scenario")

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","FDDN DTE","Baseline",0)
names(df2)<-c("Use Case",'type',"scenario","value")
df <- rbind(df, df2)
all_data = rbind(all_data, df)

################################################################
## Service time
# FDDN - Space Relay

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Future_Cost")

data = data[148:154,]

df = data[,4:6]
df = cbind(df, data[1])
colnames(df) <- c("Low","Baseline","High","Use Case")
df = select(df, `Use Case`, Low, Baseline, High)
df[df == "-"] <- 0
df$type = "FDDN SR"

df = pivot_longer(df, -c(`Use Case`,type), 
                  values_to = "value", names_to = "scenario")

df = df[(df$`Use Case` != "Deep Space Robotic"),]
df = df[(df$`Use Case` != "Mission Operations"),]

df2<-data.frame("Launch Events","FDDN SR","Baseline",0)
names(df2)<-c("Use Case",'type',"scenario","value")
df <- rbind(df, df2)
all_data = rbind(all_data, df)

df = all_data

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
  labels = c("Human Space\nFlight",
             "NER LEO\nScience",
             "NER GEO and\nNear Earth",
             "NER Low Latency &\nComplex Needs",
             "Launch Events",
             "Terrestrial &\nAerial",
             "Unknown Use Case"
  )
)

df$scenario = factor(
  df$scenario,
  levels = c("Low", "Baseline", "High"),
  labels = c("Low", "Baseline", "High")
)
df$value <- as.numeric(df$value)

df$combined = paste(df$type, df$scenario)

df$combined = factor(
  df$combined,
  levels = c("ADD DTE Low","ADD DTE Baseline","ADD DTE High",
             "ADD SR Low","ADD SR Baseline","ADD SR High",
             "ADD LE Low","ADD LE Baseline","ADD LE High",
             "FDDN DTE Low","FDDN DTE Baseline","FDDN DTE High",
             "FDDN SR Low","FDDN SR Baseline","FDDN SR High"
  ),
  labels = c("ADD DTE Low","ADD DTE Baseline","ADD DTE High",
             "ADD SR Low","ADD SR Baseline","ADD SR High",
             "ADD LE Low","ADD LE Baseline","ADD LE High",
             "FDDN DTE Low","FDDN DTE Baseline","FDDN DTE High",
             "FDDN SR Low","FDDN SR Baseline","FDDN SR High"
             )
)

totals <- df %>%
  group_by(combined) %>%
  summarize(value = signif(sum(value)))

plot4 = 
  ggplot(df, aes(x = combined, y = value)) +
  geom_bar(stat = "identity", aes(fill = `Use Case`)) +
  geom_text(
    aes(
      x = combined,
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
    title = "(D) NASA SCaN Future Mission Scenarios",
    subtitle = "Reported for ADD and FDDN services by NASA SCaN use case.",
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

#######################################################

panel <- ggarrange(
  plot1,
  plot2,
  plot3,
  plot4,
  nrow = 2,
  ncol = 2,
  common.legend = F,
  legend = "bottom",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'a_demand_panel.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 10,
  height = 8,
  res = 480
)

print(panel)
dev.off()