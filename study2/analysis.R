library(dplyr)
library(psych)
library(lme4)
library(reshape2)
library(margins)
library(emmeans)
library(lmerTest)
library(ggplot2)
library(rstatix)
library(patchwork)
library(here)

setwd("C:/Users/Gabriel/Desktop/Research/AI Art/GitHub/study2")
getwd()

df <- read.csv("data/data-clean.csv")

table(df$condition)

summary(df$patiencyV)
summary(df$agencyV)
summary(df$moralV)
summary(df$patiency7V)
summary(df$agency7V)

mindIdx <- c("agencyV", "patiencyV", "agency7V", "patiency7V", "moralV")

describe(df[, mindIdx])

t <- Hmisc::rcorr(as.matrix(df[, mindIdx]), type=c("pearson")); t
stargazer::stargazer(t$r)


####################################################

df <- df %>%
  mutate(timeEval = 
           ifelse(treatment == "pre",
                  studyEnd - study0End,
                  study0End - introEnd)) %>%
  mutate(timeMind = 
           ifelse(treatment == "pre",
                  study2End - introEnd,
                  studyEnd - study0End))

t_test(timeEval ~ condition, data=df, p.adjust.method = "bonferroni")
t_test(timeMind ~ condition, data=df, p.adjust.method = "bonferroni")
df %>% group_by(condition) %>%
  summarise(mean=mean(timeMind))

###################################################

t_test(agencyV ~ condition, data=df, p.adjust.method = "bonferroni")
df %>% group_by(condition) %>%
  summarise(mean=mean(agencyV))
cohens_d(df, agencyV ~ condition)

t_test(patiencyV ~ condition, data=df, p.adjust.method = "bonf")
t_test(moralV ~ condition, data=df, p.adjust.method = "bonf")
t_test(agency7V ~ condition, data=df, p.adjust.method = "bonf")
t_test(patiency7V ~ condition, data=df, p.adjust.method = "bonf")

####################################################

setInitMelted <- function(x) {
  tmp <- gsub("changeEvalV", "", x[["img"]])
  idx <- paste(tmp, "initialEvalV", sep="")
  return (x[[idx]])
}

imgIdx <- c("img1changeEvalV", "img2changeEvalV", "img3changeEvalV", "img4changeEvalV", "img5changeEvalV", 
            "img6changeEvalV", "img7changeEvalV", "img8changeEvalV", "img9changeEvalV", "img10changeEvalV")

imgInitIdx <- c("img1initialEvalV", "img2initialEvalV", "img3initialEvalV", "img4initialEvalV", "img5initialEvalV", 
                "img6initialEvalV", "img7initialEvalV", "img8initialEvalV", "img9initialEvalV", "img10initialEvalV")

controls <- c("condition", "X", "control", "csKnowledge", "artKnowledge")

melted.df <- melt(df[, c(imgIdx, mindIdx, imgInitIdx, controls)], id.vars = c(mindIdx, imgInitIdx, controls), variable.name="img", value.name="change")

melted.df$initV <- as.integer(apply(melted.df, 1, setInitMelted))

melted.df <- melted.df %>% mutate(condition = factor(condition)) %>% mutate(condition = relevel(condition, ref = "Pre"))

model <- lmer(change ~ 1 + condition + img + initV + control + csKnowledge + artKnowledge + (1 | X),
              data = melted.df)

summary(model)
anova(model, type="II")
model <- lmer(change ~ 1 + condition + img + initV + (1 | X),
              data = melted.df)
summary(model)
anova(model, type="II")
m <- emmeans(model, ~ condition); m
pairs(m)
plt.data <- as.data.frame(m) %>%
  mutate(condition = factor(condition,
                            levels = c("Pre", "Min", "Median", "Max"),
                            labels = c("Pre", "Undervalue", "Median", "Overvalue")))

p1 <- ggplot(plt.data, aes(x=condition, y=emmean, color=condition, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge(0.6), width=0.6) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), color="black", position=position_dodge(0.6), width=0.1, size=0.6) +
  theme_bw() +
  labs(x="", y="Evaluation Change ($)", fill="") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size=14),
    legend.text = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=14, margin=margin(0, 8, 0, 0)),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-13, 0, 0, 0),
    plot.margin = margin(1, 2, 1, 5)
  ) +
  scale_color_manual(values = c("#003f5c", "#ffba3b", "#ffa600", "#b57500")) +
  scale_fill_manual(values = c("#003f5c", "#ffba3b", "#ffa600", "#b57500")) +
  geom_hline(yintercept = 0, color="gray66") + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)); p1


####################################################

df <- df %>% mutate(condition = factor(condition)) %>% mutate(condition = relevel(condition, ref = "Pre"))

plt.data <- data.frame()

av <- aov(agencyV ~ condition*changeEvalV + initEvalV + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(agencyV ~ condition*changeEvalV + initEvalV, data=df)
summary(av)
m <- emmeans(av, ~ condition | changeEvalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Agency"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(patiencyV ~ condition*changeEvalV + initEvalV + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(patiencyV ~ condition*changeEvalV + initEvalV, data=df)
summary(av)
m <- emtrends(av, ~ condition, var="changeEvalV"); m
pairs(m)
emmip(av, condition ~ changeEvalV, cov.reduce = range)

m <- emmeans(av, ~ condition | changeEvalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Experience"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(moralV ~ condition*changeEvalV + initEvalV + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(moralV ~ condition*changeEvalV + initEvalV, data=df)
summary(av)
m <- emmeans(av, ~ condition | changeEvalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Moral Status"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(agency7V ~ condition*changeEvalV + initEvalV + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(agency7V ~ condition*changeEvalV + initEvalV, data=df)
summary(av)
m <- emmeans(av, ~ condition | changeEvalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Art Agency"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(patiency7V ~ condition*changeEvalV + initEvalV + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(patiency7V ~ condition*changeEvalV + initEvalV, data=df)
summary(av)
m <- emmeans(av, ~ condition | changeEvalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Art Experience"
plt.data <- rbind(plt.data, tmp.data)

plt.data <- plt.data %>% 
  mutate(condition = factor(condition,
                            levels = c("Pre", "Min", "Median", "Max"),
                            labels = c("Pre", "Undervalue", "Median", "Overvalue"))) %>%
  mutate(var = factor(var, levels = 
                        c("Agency", "Experience", "Moral Status", "Art Agency", "Art Experience")
  ))

p2 <- ggplot(plt.data, aes(x=var, y=emmean, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge(0.6), width=0.6) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position=position_dodge(0.6), width=0.1, size=0.5) +
  theme_bw() +
  labs(x="", y="", fill="") +
  # ylim(0, 4) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=14),
    legend.text = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-13, 0, 0, 0),
    plot.margin = margin(1, 2, 1, 0)
  ) +
  scale_fill_manual(values = c("#003f5c", "#ffba3b", "#ffa600", "#b57500")) +
  annotate("text", x=1, y=2.45, label="p<.01", size=4) +
  annotate("segment", x=0.775, xend=1.225, y=2.3, yend=2.3, colour = "black", size=0.4) +
  annotate("segment", x=0.775, xend=0.775, y=2.225, yend=2.375, colour = "black", size=0.4) +
  annotate("segment", x=1.225, xend=1.225, y=2.225, yend=2.375, colour = "black", size=0.4) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)); p2

####################################################

t_test(finalEvalV ~ condition, data=df, p.adjust.method = "bonferroni")

setChangeMelted <- function(x) {
  tmp <- gsub("finalEvalV", "", x["img"])
  idx <- paste(tmp, "changeEvalV", sep="")
  return (x[idx])
}

imgIdx <- c("img1changeEvalV", "img2changeEvalV", "img3changeEvalV", "img4changeEvalV", "img5changeEvalV", 
            "img6changeEvalV", "img7changeEvalV", "img8changeEvalV", "img9changeEvalV", "img10changeEvalV")

mindIdx <- c("agencyV", "patiencyV", "agency7V", "patiency7V", "moralV")

imgFinalIdx <- c("img1finalEvalV", "img2finalEvalV", "img3finalEvalV", "img4finalEvalV", "img5finalEvalV", 
                "img6finalEvalV", "img7finalEvalV", "img8finalEvalV", "img9finalEvalV", "img10finalEvalV")

controls <- c("condition", "X", "control", "csKnowledge", "artKnowledge")

melted.df <- melt(df[, c(imgIdx, mindIdx, imgFinalIdx, controls)], id.vars = c(mindIdx, imgIdx, controls), variable.name="img", value.name="final")

melted.df$changeV <- as.integer(apply(melted.df, 1, setChangeMelted))

melted.df <- melted.df %>% mutate(condition = factor(condition)) %>% mutate(condition = relevel(condition, ref = "Pre"))

model <- lmer(final ~ 1 + condition*changeV + img + control + csKnowledge + artKnowledge +  (1 | X),
              data = melted.df)
summary(model)

model <- lmer(final ~ 1 + condition*changeV + img +  (1 | X),
              data = melted.df)
summary(model)
anova(model, type="I")

m <- emmeans(model, ~ condition | changeV); m
p <- pairs(m); p

plt.data <- as.data.frame(m) %>% 
  mutate(condition = factor(condition,
                            levels = c("Pre", "Min", "Median", "Max"),
                            labels = c("Pre", "Undervalue", "Median", "Overvalue")))

p3 <- ggplot(plt.data, aes(x=condition, y=emmean, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge(0.6), width=0.6) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position=position_dodge(0.6), width=0.1, size=0.5) +
  theme_bw() +
  labs(x="", y="Evaluation ($)", fill="") +
  # ylim(0, 4) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size=14),
    legend.text = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=14, margin=margin(0, 8, 0, 0)),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-13, 0, 0, 0),
    plot.margin = margin(1, 2, 1, 2)
  ) +
  scale_fill_manual(values = c("#003f5c", "#ffba3b", "#ffa600", "#b57500")) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)); p3


m <- emmeans(model, ~ img); m
plt.data <- as.data.frame(m) %>% 
  mutate(img = sub("finalEvalV", "", sub("img", "", img))) %>%
  mutate(img = factor(
    img,
    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  ))

ggplot(plt.data, aes(x=img, y=emmean)) +
  geom_bar(stat="identity", position=position_dodge(0.5), width=0.4, fill="firebrick") +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position=position_dodge(0.6), width=0.1, size=0.5) +
  theme_bw() +
  labs(x="Image #", y="Evaluation ($)", fill="") +
  # ylim(0, 4) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size=14),
    legend.text = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(size=14, margin=margin(0, 8, 0, 0)),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-13, 0, 0, 0),
    axis.title.x = element_text(size=14, margin=margin(4, 0, 0, 0)),
    plot.margin = margin(5, 2, 1, 2)
  )

layout <- "
AABBBB
##CC##
"

p1 + p2 + p3 + 
  plot_layout(design = layout)

