library(dplyr)
library(psych)
library(forcats)
library(here)

setwd("C:/Users/Gabriel/Desktop/Research/AI Art/GitHub/study1")
getwd()

df <- read.csv("data/data-160.csv")

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

df <- df %>%
  mutate(agency1V = map_mind(agency1)) %>%
  mutate(agency2V = map_mind(agency2)) %>%
  mutate(agency3V = map_mind(agency3)) %>%
  mutate(agency4V = map_mind(agency4)) %>%
  mutate(agency5V = map_mind(agency5)) %>%
  mutate(agency6V = map_mind(agency6)) %>%
  mutate(agency7V = map_mind(agency7)) %>% 
  mutate(agencyV = (agency1V +  agency2V + agency3V + agency4V + agency5V + agency6V)/6) %>%
  mutate(patiency1V = map_mind(patiency1)) %>%
  mutate(patiency2V = map_mind(patiency2)) %>%
  mutate(patiency3V = map_mind(patiency3)) %>%
  mutate(patiency4V = map_mind(patiency4)) %>%
  mutate(patiency5V = map_mind(patiency5)) %>%
  mutate(patiency6V = map_mind(patiency6)) %>%
  mutate(patiency7V = map_mind(patiency7)) %>%
  mutate(patiencyV = (patiency1V +  patiency2V + patiency3V + patiency4V + patiency5V + patiency6V)/6)

n.msgs <- 0:9

for (i in n.msgs) {
  ## for all i-th evaluation
  for (j in 1:nrow(df)) {
    tmp.row <- df[j, ]
    img.idx <- tmp.row[paste("img", i, sep="")]
    
    new.idx <- paste("img", img.idx, "EvalV", sep="")
    
    df[j, new.idx] <- tmp.row[paste("img", i, "Eval", sep="")]
  }
}

df <- df %>%
  mutate(evalV=(img1EvalV + img2EvalV + img3EvalV + img4EvalV + img5EvalV +
                  img6EvalV + img7EvalV + img8EvalV + img9EvalV +  img10EvalV)/10)


df <- df %>%
   mutate(control1V = map_likert_5(controlQ1)) %>%
   mutate(control2V = map_likert_5(controlQ2)) %>%
   mutate(control3V = map_likert_5(controlQ3)) %>%
   mutate(control4V = map_likert_5(controlQ4)) %>%
   mutate(control5V = map_likert_5(controlQ5)) %>%
   mutate(control6V = map_likert_5(controlQ6)) %>%
   mutate(control = (control1V + control2V + control3V + control4V + control5V + control6V)/6)

table(df$artKnowledge) 
table(df$previousStudy) 
table(df$attCheck) 

table(df$treatment, df$attCheck)
 
attCheckIdx <- (df$treatment == "pre" & df$attCheck == "Strongly Agree") | (df$treatment == "post" & df$attCheck == "Strongly Disagree")
 
df <- df[attCheckIdx,]

df <- df[df$previousStudy == "No",]

nrow(df)

agencyIdx <- c("agency1V", "agency2V", "agency3V", "agency4V", "agency5V", "agency6V")
patiencyIdx <- c("patiency1V", "patiency2V", "patiency3V", "patiency4V", "patiency5V", "patiency6V")
artIdx <- c("agency7V", "patiency7V")

pca <- principal(df[c(agencyIdx, patiencyIdx, artIdx)], nfactors=2, rotate = "none")
pca$values
pca <- principal(df[c(agencyIdx, patiencyIdx, artIdx)], nfactors=2, rotate = "varimax")
pca$values
pca

stargazer::stargazer(as.data.frame.matrix(pca$loadings), summary = FALSE)
 
psych::alpha(df[, agencyIdx])
psych::alpha(df[, patiencyIdx])

cor.test(df$agency7V, df$patiency7V)

df <- df %>% mutate(csKnowledge=ifelse(csKnowledge == "Yes", 1, 0)) %>%
  mutate(artKnowledge=ifelse(artKnowledge == "Yes", 1, 0))

df <- df %>% mutate(ageV = 2021 - age)
summary(df$ageV)

write.csv(df, "data/data-clean.csv") 
 