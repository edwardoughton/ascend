### Visualization code for current and future NASA demand
library(tidyverse)
library(ggpubr)

filename = 'results.csv'
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', 'results', filename)
data <- read_csv(path)

data = data[(data$other_minutes_sold == "No"),]
data = data[,-which(names(data) == "other_minutes_sold")]

df = pivot_longer(data, 
                  -c(use_case, type, method, scenario, missions, minutes), 
                  values_to = "value", names_to = "scenario_cost")

df$combined = paste(df$method, df$scenario)

df = df[(df$use_case != "Deep Space Robotic"),]
df = df[(df$use_case != "Mission Operations"),]

df$use_case = factor(
  df$use_case,
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

demand = df[(df$scenario_cost == 'cost_usd_S1'),]
# folder <- dirname(rstudioapi::getSourceEditorContext()$path)
# path = file.path(folder, '..', 'results', filename)
# write.csv(demand, file.path(folder, '..', 'results','demand.csv'))
demand = select(demand, type, scenario, method, missions, minutes)
demand = demand %>%
  group_by(method, scenario) %>%
  summarize(missions = sum(missions),
            minutes = round(sum(minutes)/1e6,2))

################################
##### DTE - Use Case
################################
subset = df[(df$method == 'DTE'),]

subset$combined = factor(
  subset$combined,
  levels = c("DTE Current",
             "DTE Low",
             "DTE Baseline",
             "DTE High"
  ),
  labels = c(
    "Current",
    "Low",
    "Baseline",
    "High"
  )
)

subset$scenario_cost = factor(
  subset$scenario_cost,
  levels = c("cost_usd_S1", 
             "cost_usd_S2",
             "cost_usd_S3",
             "cost_usd_S4",
             "cost_usd_S5",
             "cost_usd_S6"
  ),
  labels = c("CS1 (2 Providers)", 
             "CS2 (3 Providers)",
             "CS3 (4 Providers)",
             "CS4 (5 Providers)",
             "CS5 (6 Providers)",
             "CS6 (7 Providers)"
  )
)

subset$value = subset$value / 1e6

totals <- subset %>%
  group_by(combined, scenario_cost) %>%
  summarize(value = signif(sum(value)))

plot1 = ggplot(subset, aes(x=combined, y=value)) +
  geom_bar(stat = "identity", aes(fill = use_case)) +
  coord_flip() +
  geom_text(
    aes(
      x = combined,
      y = value ,
      label = paste("$",signif(value,3), 'Mn')
    ),
    size = 1.8,
    data = totals,
    vjust = 0.5,
    hjust = -0.1
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    #Scenario Analysis of Future Direct-To-Earth (DTE) Costs for NASA SCaN
    title = "(A) Annual DTE Costs via Exclusive Network Assets", 
    subtitle = "Reported for Missions, Cost Scenarios (CS), and NASA Use Cases.",
    x = 'Missions',
    y = bquote("Annual Service Cost (US$ Millions)"),
    fill = "",
  ) +
  scale_y_continuous(
    limits = c(0, 950),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 950, by = 200)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  ) +
 facet_wrap(~scenario_cost, nrow=1) 

################################
##### SR - Use Case
################################
subset = df[(df$method == 'SR'),]

subset$combined = factor(
  subset$combined,
  levels = c("SR Current",
             "SR Low",
             "SR Baseline",
             "SR High"
  ),
  labels = c(
    "Current",
    "Low",
    "Baseline",
    "High"
  )
)


subset$scenario_cost = factor(
  subset$scenario_cost,
  levels = c("cost_usd_S1",
             "cost_usd_S2",
             "cost_usd_S3",
             "cost_usd_S4",
             "cost_usd_S5",
             "cost_usd_S6"
  ),
  labels = c("CS1 (2 LEO)",
             "CS2 (1 LEO, 1 GEO)",
             "CS3 (2 LEO, 2 GEO)",
             "CS4 (3 LEO 3 GEO)",
             "CS5 (4 LEO 2 GEO)",
             "CS6 (2 LEO 4 GEO)"
  )
)

subset$value = subset$value / 1e6

totals <- subset %>%
  group_by(combined, scenario_cost) %>%
  summarize(value = signif(sum(value)))

plot2 = 
  ggplot(subset, aes(x=combined, y=value)) +
  geom_bar(stat = "identity", aes(fill = use_case)) +
  coord_flip() +
  geom_text(
    aes(
      x = combined,
      y = value ,
      label = paste("$",signif(value,3), 'Mn')
    ),
    size = 1.8,
    data = totals,
    vjust = 0.5,
    hjust = -0.1
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(C) Annual SR Costs via Exclusive Network Assets", 
    subtitle = "Reported for Missions, Cost Scenarios (CS), and NASA Use Cases.",
    x = 'Missions',
    y = bquote("Annual Service Cost (US$ Millions)"),
    fill = "",
  ) +
  scale_y_continuous(
    limits = c(0, 950),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 950, by = 200)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~scenario_cost, nrow=1) 
  

# #######################################################
# #######################################################
# #######################################################

filename = 'results.csv'
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, '..', 'results', filename)
data <- read_csv(path)

data = data[(data$other_minutes_sold == "Yes"),]

data = data[,-which(names(data) == "other_minutes_sold")]

df = pivot_longer(data, 
                  -c(use_case, type, method, scenario, missions, minutes), 
                  values_to = "value", names_to = "scenario_cost")

df$combined = paste(df$method, df$scenario)

df = df[(df$use_case != "Deep Space Robotic"),]
df = df[(df$use_case != "Mission Operations"),]

df$use_case = factor(
  df$use_case,
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

################################
##### DTE - Use Case
################################
subset = df[(df$method == 'DTE'),]

subset$combined = factor(
  subset$combined,
  levels = c("DTE Current",
             "DTE Low",
             "DTE Baseline",
             "DTE High"
  ),
  labels = c(
    "Current",
    "Low",
    "Baseline",
    "High"
  )
)

subset$scenario_cost = factor(
  subset$scenario_cost,
  levels = c("cost_usd_S1", 
             "cost_usd_S2",
             "cost_usd_S3",
             "cost_usd_S4",
             "cost_usd_S5",
             "cost_usd_S6"
  ),
  labels = c("CS1 (2 Providers)", 
             "CS2 (3 Providers)",
             "CS3 (4 Providers)",
             "CS4 (5 Providers)",
             "CS5 (6 Providers)",
             "CS6 (7 Providers)"
  )
)

subset$value = subset$value / 1e6

totals <- subset %>%
  group_by(combined, scenario_cost) %>%
  summarize(value = signif(sum(value)))

plot3 = ggplot(subset, aes(x=combined, y=value)) +
  geom_bar(stat = "identity", aes(fill = use_case)) +
  coord_flip() +
  geom_text(
    aes(
      x = combined,
      y = value ,
      label = paste("$",signif(value,3), 'Mn')
    ),
    size = 1.8,
    data = totals,
    vjust = 0.5,
    hjust = -0.1
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(B) Annual DTE Costs via Non-Exclusive Network Assets", 
    subtitle = "Reported for Missions, Cost Scenarios (CS), and NASA Use Cases.",
    x = 'Missions',
    y = bquote("Annual Service Cost (US$ Millions)"),
    fill = "",
  ) +
  scale_y_continuous(
    limits = c(0, 950),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 950, by = 200)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~scenario_cost, nrow=1) 

################################
##### SR - Use Case
################################
subset = df[(df$method == 'SR'),]

subset$combined = factor(
  subset$combined,
  levels = c("SR Current",
             "SR Low",
             "SR Baseline",
             "SR High"
  ),
  labels = c(
    "Current",
    "Low",
    "Baseline",
    "High"
  )
)

subset$scenario_cost = factor(
  subset$scenario_cost,
  levels = c("cost_usd_S1",
             "cost_usd_S2",
             "cost_usd_S3",
             "cost_usd_S4",
             "cost_usd_S5",
             "cost_usd_S6"
  ),
  labels = c("CS1 (2 LEO)",
             "CS2 (1 LEO, 1 GEO)",
             "CS3 (2 LEO, 2 GEO)",
             "CS4 (3 LEO 3 GEO)",
             "CS5 (4 LEO 2 GEO)",
             "CS6 (2 LEO 4 GEO)"
  )
)

subset$value = subset$value / 1e6

totals <- subset %>%
  group_by(combined, scenario_cost) %>%
  summarize(value = signif(sum(value)))

plot4 = 
  ggplot(subset, aes(x=combined, y=value)) +
  geom_bar(stat = "identity", aes(fill = use_case)) +
  coord_flip() +
  geom_text(
    aes(
      x = combined,
      y = value ,
      label = paste("$",signif(value,3), 'Mn')
    ),
    size = 1.8,
    data = totals,
    vjust = 0.5,
    hjust = -0.1
  )  +
  scale_fill_brewer(palette = "Dark2") + coord_flip() +
  labs(
    colour = NULL,
    title = "(D) Annual SR Costs via Non-Exclusive Network Assets", 
    subtitle = "Reported for Missions, Cost Scenarios (CS), and NASA Use Cases.",
    x = 'Missions',
    y = bquote("Annual Service Cost (US$ Millions)"),
    fill = "",
  ) +
  scale_y_continuous(
    limits = c(0, 950),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 950, by = 200)
  ) +
  scale_x_discrete(limits = rev) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~scenario_cost, nrow=1) 

#######################################################

panel <- ggarrange(
  plot1,
  plot3,
  plot2,
  plot4,
  nrow = 4,
  ncol = 1,
  # labels = c("A", "B", "C", "D", "E", "F"),
  common.legend = T,
  legend = "bottom",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'c_costs_panel.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8,
  height = 8,
  res = 480
)

print(panel)
dev.off()