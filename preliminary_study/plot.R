library(ggplot2)
library(dplyr)
library(patchwork)
library(here)

setwd(paste(here(), "preliminary_study", sep="/"))
getwd()

df.tmp <- read.csv("data/AIArt_judgements.csv")

df <- read.csv("data/unmelted.csv")

udc.max <- (df$Undecided > df$AI.Program) & (df$Undecided > df$Human)
sum(udc.max)

df <- df %>%
  rowwise %>%
  mutate(n.valid=AI.Program + Human) %>%
  mutate(n=AI.Program + Human + Undecided)

rowwise.entropy <- function(row) {
  tmp <- c(row["AI.Program"], row["Human"])
  return (entropy::entropy(tmp))
}

entropy.results <- apply(df, 1, rowwise.entropy)

df$entropy <- entropy.results

valid <- df %>% 
  arrange(desc(entropy), desc(n.valid))

### use this to select how many you want to visualize
valid <- valid[1:10, ]

melted.df <- reshape2::melt(valid, measure.vars=c("Human", "AI.Program", "Undecided"), variable.name="img_judge")

melted.df$img_judge <- factor(melted.df$img_judge,
                       levels = c("AI.Program", "Undecided", "Human"),
                       labels = c("AI Program", "Undecided", "Human"))

melted.df <- melted.df %>%
  mutate(perc = value/n)


melted.df <- melted.df %>%
  arrange(img_judge, desc(entropy)) %>%
  mutate(img_n = factor(img_n, unique(img_n)))

melted.df$perc <- melted.df$perc * 100

melted.df$n_ordered <- 1:nrow(melted.df)


plot <- ggplot(melted.df, aes(x=img_n, y=perc, fill=img_judge)) +
  geom_bar(stat="identity", width=0.6) +
  # geom_hline(yintercept = c(60, 40), size=0.7) + 
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_manual(
    values=c("dodgerblue3", "gray", "firebrick3")
  ) +
  ylab("Percentage (%)") +
  xlab("Image #") +
  labs(fill="") +
  theme(
    axis.text.x = element_text(size=10.5),
    legend.text = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=14, margin=margin(0, 8, 0, 0)),
    axis.title.x = element_text(size=14, margin=margin(4, 0, 0, 0)),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.margin = margin(1, 2, 1, 2)
   ) + 
  geom_rect(xmin=0.5, xmax=10.5, ymin=-1, ymax=101, fill='transparent', color='black', size=1.2) +
  scale_x_discrete(labels=1:58) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)) ; plot

