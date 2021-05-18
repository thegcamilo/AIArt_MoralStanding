library(dplyr)
library(psych)
library(lme4)
library(reshape2)
library(margins)
library(emmeans)
library(lmerTest)
library(ggplot2)
library(patchwork)
library(here)

setwd("C:/Users/Gabriel/Desktop/Research/AI Art/GitHub/study1")
getwd()

df <- read.csv("data/data-clean.csv")
table(df$treatment)

df <- df %>% mutate(timeSpent=studyEnd/1000)
summary(df$timeSpent)

summary(df$patiencyV)
summary(df$agencyV)
summary(df$agency7V)
summary(df$patiency7V)

mindIdx <- c("agencyV", "patiencyV", "agency7V", "patiency7V")

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
           ifelse(treatment == "post",
                  studyEnd - study0End,
                  study0End - introEnd))

t.test(timeEval ~ treatment, data=df)
lsr::cohensD(timeEval ~ treatment, data=df)

t.test(timeMind ~ treatment, data=df)
lsr::cohensD(timeMind ~ treatment, data=df)

####################################################

t.test(agencyV ~ treatment, data=df)
lsr::cohensD(agencyV ~ treatment, data=df)
t.test(patiencyV ~ treatment, data=df)
lsr::cohensD(patiencyV ~ treatment, data=df)

t.test(agency7V ~ treatment, data=df)
lsr::cohensD(agency7V ~ treatment, data=df)
t.test(patiency7V ~ treatment, data=df)
lsr::cohensD(patiency7V ~ treatment, data=df)

t.test(evalV ~ treatment, data=df)
lsr::cohensD(evalV~ treatment, data=df)

####################################################

plt.data <- data.frame()

av <- aov(agencyV ~ evalV*treatment + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(agencyV ~ evalV*treatment, data=df)
summary(av)
m <- emmeans(av, ~ treatment | evalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Agency"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(patiencyV ~ evalV*treatment + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(patiencyV ~ evalV*treatment, data=df)
summary(av)
m <- emmeans(av, ~ treatment | evalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Experience"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(agency7V ~ evalV*treatment + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(agency7V ~ evalV*treatment, data=df)
summary(av)
m <- emmeans(av, ~ treatment | evalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Art Agency"
plt.data <- rbind(plt.data, tmp.data)

av <- aov(patiency7V ~ evalV*treatment + control + csKnowledge + artKnowledge, data=df)
summary(av)
av <- aov(patiency7V ~ evalV*treatment, data=df)
summary(av)
m <- emmeans(av, ~ treatment | evalV); m
pairs(m)
tmp.data <- as.data.frame(m)
tmp.data$var <- "Art Experience"
plt.data <- rbind(plt.data, tmp.data)

plt.data <- plt.data %>% 
  mutate(treatment = factor(treatment, levels = c("pre", "post"), labels = c("Pre", "Post"))) %>%
  mutate(treatment = relevel(treatment, ref="Pre")) %>%
  mutate(var = factor(var, levels = 
                        c("Agency", "Experience", "Art Agency", "Art Experience")
                      ))

melted.df <- melt(df, measure.vars = c("agencyV", "patiencyV", "agency7V", "patiency7V")) %>% 
  mutate(treatment = factor(treatment, levels = c("pre", "post"), labels = c("Pre", "Post"))) %>%
  mutate(treatment = relevel(treatment, ref="Pre")) %>%
  mutate(var = factor(variable, 
                      levels = c("agencyV", "patiencyV", "agency7V", "patiency7V"),
                      labels = c("Agency", "Experience", "Art Agency", "Art Experience")
  ))


p1 <- ggplot(plt.data, aes(x=var, y=emmean, fill=treatment)) +
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
    plot.margin = margin(1, 2, 1, 2)
  ) +
  scale_fill_manual(values = c("#003f5c", "#ffa600")) + 
  scale_color_manual(values = c("#003f5c", "#ffa600")) +
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)); p1


####################################################

imgIdx <- c("img10EvalV", "img1EvalV", "img2EvalV", "img3EvalV", "img4EvalV", "img5EvalV", "img6EvalV", "img7EvalV", "img8EvalV", "img9EvalV")
controls <- c("treatment", "X", "control", "csKnowledge", "artKnowledge")

melted.df <- melt(df[, c(imgIdx, controls)], id.vars = controls, variable.name="img", value.name="eval")

model <- lmer(eval ~ 1 + treatment + img + control + csKnowledge + artKnowledge + (1 | X),
              data = melted.df)

summary(model)

model <- lmer(eval ~ 1 + treatment + img + (1 | X),
              data = melted.df)
anova(model, type="II")
summary(model)
m <- emmeans(model, ~ treatment); m
pairs(m)

plt.data <- as.data.frame(m) %>% 
  mutate(treatment = factor(treatment, levels = c("pre", "post"), labels = c("Pre", "Post")))


p2 <- ggplot(plt.data, aes(x=treatment, y=emmean, fill=treatment)) +
  geom_bar(stat="identity", position=position_dodge(0.5), width=0.4) +
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
  scale_fill_manual(values = c("#003f5c", "#ffa600")) + 
  scale_color_manual(values = c("#003f5c", "#ffa600")) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 16)); p2

p1 + p2

m <- emmeans(model, ~ img); m
plt.data <- as.data.frame(m) %>% 
  mutate(img = sub("EvalV", "", sub("img", "", img)), sep="") %>%
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
