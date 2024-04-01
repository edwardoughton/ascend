### Visualization code for current and future NASA demand
library(tidyverse)
library(ggpubr)
library(readxl)

filename = 'Oughton et al. (2024) Ascend v0.16.0.xlsx'

################################################################
# DTE Cost

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Costs - DTE")

data = data[1:27,]

df = data[,29:37]

colnames(df) <- df[1,]
df <- df[-1, ] 

df = select(df, Minutes, `Average fixed cost (US$/Min)`,
            `Average variable cost (US$/Min)`)

df = pivot_longer(df, -c(`Minutes`), values_to = "value", names_to = "metric")
df$value = as.numeric(df$value)
df$Minutes = round(as.numeric(df$Minutes)/1e6,3)

df = df[(df$Minutes != 0),]

df$metric = factor(
  df$metric,
  levels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)"
  ),
  labels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)")
)

totals <- df %>%
  group_by(Minutes) %>%
  summarize(value = signif(sum(value)))

plot_dte = ggplot(df, aes(x = Minutes, y = value)) +
  geom_bar(stat = "identity", aes(fill = `metric`)) +
  geom_text(
    aes(
      x = Minutes,
      y = value ,
      label = paste("$",signif(value, 3))
    ),
    size = 2,
    data = totals,
    vjust = -.3, #angle = 90,
    hjust = .5#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    colour = NULL,
    title = "(A) Representative DTE Site Average Total Cost Per Minute",
    subtitle = "Reported by fixed and variable cost.",
    x = "Minutes of Service Sold (Millions of Minutes)",
    y = bquote("Average Total Cost\n(US$/Minute)"),
    fill = "",
  ) +
  scale_y_continuous(
    limits = c(0, 214),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 214, by = 25)
  ) +
  scale_x_continuous(
    limits = c(0, 12.9),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 12.9, by = 1)
  ) +
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
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  )

################################################################
# LEO Cost

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Costs - SR - LEO")

data = data[1:180,]

df = data[,27:37]

colnames(df) <- df[1,]
df <- df[-1, ] 

df = select(df, Minutes, `Average fixed cost (US$/Min)`,
            `Average variable cost (US$/Min)`)

df = df[(df$Minutes != 0),]
df$Minutes = as.numeric(df$Minutes)
df = df[(df$Minutes < 500000000),]

# df = df %>%
#   slice(which(row_number() %% 6 == 1))

df = pivot_longer(df, -c(`Minutes`), values_to = "value", names_to = "metric")
df$value = as.numeric(df$value)
df$Minutes = round(as.numeric(df$Minutes)/1e6,3)

df$metric = factor(
  df$metric,
  levels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)"
  ),
  labels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)")
)

totals <- df %>%
  group_by(Minutes) %>%
  summarize(value = signif(sum(value)))

plot_leo = 
  ggplot(df, aes(x = Minutes, y = value)) +
  geom_bar(stat = "identity", aes(fill = `metric`)) +
  geom_text(
    aes(
      x = Minutes,
      y = value ,
      label = paste("$",signif(value, 3))
    ),
    size = 2,
    data = totals,
    vjust = -.3, #angle = 90,
    hjust = .5#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    colour = NULL,
    title = "(B) Representative LEO Network Average Total Cost Per Minute",
    subtitle = "Reported by fixed and variable cost.",
    x = "Minutes of Service Sold (Millions of Minutes)",
    y = bquote("Average Total Cost\n(US$/Minute)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 79),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 79, by = 10)
  ) +
  scale_x_continuous(
    limits = c(0, 500),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 500, by = 50)
  ) +
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
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  )


################################################################
# GEO Cost

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

path = file.path(folder, '..', filename)
data <- read_excel(path, sheet = "Costs - SR - GEO")

colnames(data) <- data[1,]
data <- data[-1, ] 

data = data[2:53,]

df = data[,28:38]

df$Minutes = as.numeric(df$Minutes)
df = df[(df$Minutes < 500000000),]

df = select(df, Minutes, `Average fixed cost (US$/Min)`,
            `Average variable cost (US$/Min)`)
# df = df %>%
#   slice(which(row_number() %% 2 == 1))

df = pivot_longer(df, -c(`Minutes`), values_to = "value", names_to = "metric")
df$value = as.numeric(df$value)
df$Minutes = round(as.numeric(df$Minutes)/1e6,3)

# df = df[(df$Minutes != 0),]

df$metric = factor(
  df$metric,
  levels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)"
  ),
  labels = c("Average variable cost (US$/Min)", "Average fixed cost (US$/Min)")
)

totals <- df %>%
  group_by(Minutes) %>%
  summarize(value = signif(sum(value)))

plot_geo = 
  ggplot(df, aes(x = Minutes, y = value)) +
  geom_bar(stat = "identity", aes(fill = `metric`)) +
  geom_text(
    aes(
      x = Minutes,
      y = value ,
      label = paste("$",signif(value, 3))
    ),
    size = 2,
    data = totals,
    vjust = -.3, #angle = 90,
    hjust = .5#,
    # position = position_dodge(1),
  )  +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    colour = NULL,
    title = "(C) Representative GEO Network Average Total Cost Per Minute",
    subtitle = "Reported by fixed and variable cost.",
    x = "Minutes of Service Sold (Millions of Minutes)",
    y = bquote("Average Total Cost\n(US$/Minute)"),
    fill = "",#"Use\nCase"
  ) +
  scale_y_continuous(
    limits = c(0, 79),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 79, by = 10)
  ) +
  scale_x_continuous(
    limits = c(0, 499),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0),
    breaks = seq(0, 499, by = 50)
  ) +
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
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  )

#######################################################
## Panel

panel <- ggarrange(
  plot_dte,
  plot_leo,
  plot_geo,
  nrow = 3,
  ncol = 1,
  # labels = c("A", "B", "C", "D", "E", "F"),
  common.legend = T,
  legend = "bottom",
  font.label = list(size = 9)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
path = file.path(folder, 'figures', 'b_cost_curves.png')
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8,
  height = 7,
  res = 480
)

print(panel)
dev.off()

