library(dplyr)
library(psych)
library(forcats)
library(here)

setwd("C:/Users/Gabriel/Desktop/Research/AI Art/GitHub/study2")
getwd()

df <- read.csv("data/study3-315.csv")

map_likert_5 <- function(x) {
  case_when(
    x %in% c("Strongly Disagree") ~ 0,
    x %in% c("Somewhat Disagree") ~ 1,
    x %in% c("Neither Agree nor Disagree") ~ 2,
    x %in% c("Somewhat Agree") ~ 3,
    x %in% c("Strongly Agree") ~ 4
  )
}

map_mind <- function(x) {
  case_when(
    x %in% c("Not at all") ~ 0,
    x %in% c("Slightly") ~ 1,
    x %in% c("Somewhat") ~ 2,
    x %in% c("Moderately") ~ 3,
    x %in% c("Extremely") ~ 4
  )
}

table(df$treatment)
table(df$priceTreatment)

df$condition <- ifelse(df$treatment == "pre", "pre", df$priceTreatment)

attCheckIdx <- (df$treatment == "pre" & df$attCheck == "Strongly Agree") | (df$treatment == "post" & df$attCheck == "Strongly Disagree")

df <- df[attCheckIdx,]

df <- df[df$previousStudy == "No",]

table(df$condition)

df <- df %>%
  mutate(agency1V = map_mind(agency1)) %>%
  mutate(agency2V = map_mind(agency2)) %>%
  mutate(agency3V = map_mind(agency3)) %>%
  mutate(agency4V = map_mind(agency4)) %>%
  mutate(agency5V = map_mind(agency5)) %>%
  mutate(agency6V = map_mind(agency6)) %>%
  mutate(agency7V = map_mind(agency7)) %>% 
  mutate(patiency1V = map_mind(patiency1)) %>%
  mutate(patiency2V = map_mind(patiency2)) %>%
  mutate(patiency3V = map_mind(patiency3)) %>%
  mutate(patiency4V = map_mind(patiency4)) %>%
  mutate(patiency5V = map_mind(patiency5)) %>%
  mutate(patiency6V = map_mind(patiency6)) %>%
  mutate(patiency7V = map_mind(patiency7)) %>%
  mutate(moral1V = map_mind(moral1)) %>%
  mutate(moral2V = map_mind(moral2)) %>%
  mutate(moral3V = map_mind(moral3)) %>%
  mutate(moral4V = map_mind(moral4)) %>%
  mutate(moral5V = map_mind(moral5)) %>%
  mutate(moral6V = map_mind(moral6))


agencyIdx <- c("agency1V", "agency2V", "agency3V", "agency4V", "agency5V", "agency6V")
patiencyIdx <- c("patiency1V", "patiency2V", "patiency3V", "patiency4V", "patiency5V", "patiency6V")
artIdx <- c("patiency7V", "agency7V")

moralIdx <- c("moral1V", "moral2V", "moral3V", "moral4V", "moral5V", "moral6V")

pca <- principal(df[c(agencyIdx, patiencyIdx, moralIdx, artIdx)], nfactors=4)
pca$values
pca <- principal(df[c(agencyIdx, patiencyIdx, moralIdx, artIdx)], nfactors=4, rotate="varimax")
pca$values
pca

stargazer::stargazer(as.data.frame.matrix(pca$loadings), summary = FALSE)

psych::alpha(df[, agencyIdx])
psych::alpha(df[, patiencyIdx])
psych::alpha(df[, moralIdx])

cor.test(df$agency7V, df$patiency7V)

df <- df %>%
  mutate(agencyV = (agency1V +  agency2V + agency3V + agency4V + agency5V + agency6V)/6) %>%
  mutate(patiencyV = (patiency1V +  patiency2V + patiency3V + patiency4V + patiency5V + patiency6V)/6) %>%
  mutate(moralV = (moral1V +  moral2V + moral3V + moral4V + moral5V + moral6V)/6) %>%
  mutate(artV=(agency7V + patiency7V)/2)

getChangeEval <- function(x, i) {
  idx <- paste("img", i, "ChangedEval", sep="")
  last.eval <- paste("img", i, "LastEval", sep="")
  first.eval <- paste("img", i, "FirstEval", sep="")
  if (x[idx] == "True") {
    val <- as.integer(x[last.eval]) - as.integer(x[first.eval])
    return (val)
  } else {
    return (0);
  }
}

getInitialEval <- function(x, i) {
  idx <- paste("img", i, "ChangedEval", sep="")
  last.eval <- paste("img", i, "LastEval", sep="")
  first.eval <- paste("img", i, "FirstEval", sep="")
  if (x[idx] == "True") {
    return (as.integer(x[first.eval]))
  } else {
    return (as.integer(x[last.eval]))
  }
}

n.msgs <- 0:9

for (i in n.msgs) {
  df[, paste("img", i, "changeEvalTmp", sep="")] = apply(df, 1, getChangeEval, i=i)
  df[, paste("img", i, "initialEvalTmp", sep="")] = apply(df, 1, getInitialEval, i=i)
  df[, paste("img", i, "finalEvalTmp", sep="")] = as.integer(df[, paste("img", i, "LastEval", sep="")])
}

for (i in n.msgs) {
  ## for all i-th evaluation
  for (j in 1:nrow(df)) {
    tmp.row <- df[j, ]
    img.idx <- tmp.row[paste("img", i, sep="")]
    
    new.change.idx <- paste("img", img.idx, "changeEvalV", sep="")
    new.init.idx <- paste("img", img.idx, "initialEvalV", sep="")
    new.final.idx <- paste("img", img.idx, "finalEvalV", sep="")
    
    df[j, new.change.idx] <- tmp.row[paste("img", i, "changeEvalTmp", sep="")]
    df[j, new.init.idx] <- tmp.row[paste("img", i, "initialEvalTmp", sep="")]
    df[j, new.final.idx] <- tmp.row[paste("img", i, "finalEvalTmp", sep="")]
  }
}

df <- df %>%
  mutate(initEvalV=(img1initialEvalV + img2initialEvalV + img3initialEvalV + img4initialEvalV + img5initialEvalV +
                      img6initialEvalV + img7initialEvalV + img8initialEvalV + img9initialEvalV + img10initialEvalV)/10) %>%
  mutate(finalEvalV=(img1finalEvalV + img2finalEvalV + img3finalEvalV + img4finalEvalV + img5finalEvalV +
                       img6finalEvalV + img7finalEvalV + img8finalEvalV + img9finalEvalV + img10finalEvalV)/10) %>%
  mutate(changeEvalV=(img1changeEvalV + img2changeEvalV + img3changeEvalV + img4changeEvalV + img5changeEvalV +
                       img6changeEvalV + img7changeEvalV + img8changeEvalV + img9changeEvalV + img10changeEvalV)/10)
  
df <- df %>%
   mutate(control1V = map_likert_5(controlQ1)) %>%
   mutate(control2V = map_likert_5(controlQ2)) %>%
   mutate(control3V = map_likert_5(controlQ3)) %>%
   mutate(control4V = map_likert_5(controlQ4)) %>%
   mutate(control5V = map_likert_5(controlQ5)) %>%
   mutate(control6V = map_likert_5(controlQ6)) %>%
   mutate(control = (control1V + control2V + control3V + control4V + control5V + control6V)/6)

df$condition <- factor(df$condition,
                       levels = c("pre", "Min", "Mean", "Max"),
                       labels = c("Pre", "Min", "Median", "Max"))

df <- df %>% mutate(condition=relevel(condition, ref="Pre"))


df <- df %>% mutate(ageV = 2021 - age)
summary(df$ageV)

df <- df %>% mutate(csKnowledge = ifelse(csKnowledge == "Yes", 1, 0)) %>%
  mutate(artKnowledge = ifelse(artKnowledge == "Yes", 1, 0))

table(df$condition)

write.csv(df, "data/data-clean.csv") 
 