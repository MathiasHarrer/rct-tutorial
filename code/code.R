# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   EVALUATION OF RANDOMIZED CONTROLLED TRIALS USING                          #
#   R: A TUTORIAL FOR MENTAL HEALTH RESEARCHERS                               #
#   Complete Analysis Code                                                    #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 1. Introduction ------------------------------------------------------------

## 2. Preparation -------------------------------------------------------------

### 2.1. R and RStudio --------------------------------------------------------

### 2.2. Packages -------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)


### 2.3. Importing Data -------------------------------------------------------

library(openxlsx)
data <- read.xlsx("data.xlsx")


### 2.3. Functions ------------------------------------------------------------

sqrt(9)
max(data$age)
mean(data$cesd.1)
mean(data$cesd.1, na.rm = TRUE)


### 2.5 Objects ---------------------------------------------------------------

birth_year <- 1985
# Call the saved object "birth_year"
birth_year

1985 -> birth_year 
birth_year = 1985
assign("birth_year", 1985)

vector <- c(8, 74, 1, 6)

class(data$age)

glimpse(data)

name <- c("Sarah", "Paula", "Antonia", "Peter")
age <- c(45, 31, 31, 27)

df <- data.frame(name, age, likes_liquorice); df

colnames(df)   # Show the dataframe's columns names
ncol(df)       # Show the number of columns
rownames(df)   # Show the dataframe's row names
nrow(df)       # Show the number of rows

name <- "liquorice data"
list <- list(data = df, title = name)


### 2.6 Operators -------------------------------------------------------------

# Using a semi-colon, we can write several lines of code in one:
5+8; 6^3; 6*(10/3) 

# Check if "Capitalized" is equal to "capitalized" 
# Note the upper vs. lowercase spelling
"Capitalized" == "capitalized"

# Define two objects (x,y); then test if two conditions are fulfilled:
x <- 20; y <- 9
x > 10 & y != 10

data$age > 45

data %>% pull(age) %>% mean() %>% log()


### 2.7 Slicing & Indexing ----------------------------------------------------

data[4,7]

data[1:3, c(16, 6)]

data[,21]

data[1,"age"]

data[data$age > 40,"cesd.0"]

data %>%
  filter(age > 40, sex == 0) %>% 
  select(cesd.0, cesd.1, cesd.2) %>%
  head(3)


## 3. Descriptive Analysis ----------------------------------------------------

library(skimr) 
skim(data)

data$sex <- as.numeric(data$sex)
data$prevpsychoth <- as.numeric(data$prevpsychoth)
data$prevtraining <- as.numeric(data$prevtraining)
data$child <- as.numeric(data$child)
data$employ <- as.numeric(data$employ)
data$degree <- as.numeric(data$degree)

data %>% filter(group == 0) %>% skim() # Control group
data %>% filter(group == 1) %>% skim() # Intervention group

library(psych)
multi.hist(data %>% select(cesd.0, cesd.1, cesd.2), ncol = 3)

x <- c(1, 2, 3, 4, 5, NA, NA, 200)   # Create vector
sum(is.na(x))                        # Count the number of missings


# All trial data
with(data, {
  c(sum(is.na(cesd.0)),
    sum(is.na(cesd.1)),
    sum(is.na(cesd.2)))
}) -> na.all

# Calculate percentages
na.all.p <- na.all/nrow(data)
na.all.p

# Do the same for intervention group only...
data %>%
  filter(group == 1) %>%
  with({
    c(sum(is.na(cesd.0)),
      sum(is.na(cesd.1)),
      sum(is.na(cesd.2)))
  }) -> na.ig

na.ig.p <- na.ig/nrow(data %>% filter(group == 1))

# ... and control group only
data %>%
  filter(group == 0) %>%
  with({
    c(sum(is.na(cesd.0)),
      sum(is.na(cesd.1)),
      sum(is.na(cesd.2)))
  }) -> na.cg

na.cg.p <- na.cg/nrow(data %>% filter(group == 0))

# Collect all data in dataframe
na <- data.frame(na.all, na.all.p = na.all.p*100,
                 na.ig, na.ig.p = na.ig.p*100,
                 na.cg, na.cg.p = na.cg.p*100)

# Give the rows fitting names
rownames(na) = c("t0", "t1", "t2")
round(na, 2)


## 4. Missing Data ------------------------------------------------------------

# Define some example data; x has three missings
y <- 1:10
x <- c(1, NA, NA, NA, 3, 5, 8, 10, -1, 10)
summary(lm(y ~ x))


### 4.1 Missing Data Mechanism ------------------------------------------------

### 4.2 Multiple Imputation ---------------------------------------------------

library(mice)
library(miceadds)

md.pattern(data)

flux(data)[,1:3]

data$R <- !is.na(data$cesd.1)
data.cg <- data %>% filter(group == 0)
data.ig <- data %>% filter(group == 1)

# R is not needed for other analyses, so
# we remove it from the original data
data$R = NULL 

glm(R ~ scale(cesd.0) + scale(badssf.0) + scale(hadsa.0) +
      scale(age) + sex + prevpsychoth + prevtraining + child + 
      employ + as.factor(rel) + as.factor(degree) + 
      scale(atsphs.0) + scale(ceq), 
    binomial(link = "logit"), data.ig) %>% summary()

glm(R ~ scale(cesd.0) + scale(badssf.0) + scale(hadsa.0) +
      scale(age) + sex + prevpsychoth + prevtraining + child + 
      employ + as.factor(rel) + as.factor(degree) + 
      scale(atsphs.0) + scale(ceq), 
    binomial(link = "logit"), data.cg) %>% summary()

inlist <- c("badssf.0", "hadsa.0", "prevpsychoth", "rel")

imp0 <- mice(data, maxit = 0)

imp0$loggedEvents

# Include all variables with logged events ("id" in our case)
# in our "outlist"; then remove those variables from the data frame
outlist <- imp0$loggedEvents$out
imp.data <- data %>% select(-all_of(outlist))

pred <- quickpred(imp.data,
                  mincor = 0.05,
                  minpuc = 0.05,
                  include = inlist)

# Mean number of predictors
table(rowSums(pred))[-1] %>%
  {as.numeric(names(.)) %*% . / sum(.)}

# Set predictor "group" to zero
pred[,"group"] = 0

library(plot.matrix)
plot(pred, main = "Imputation Matrix",
     col = c("grey", "blue"),
     xlab = "predicts",
     ylab = "is predicted",
     adj = 0, las = 2, cex.axis = 0.5)

imp0 = mice(imp.data, maxit = 0,
            predictorMatrix = pred)
imp0$method

# Find variables without missings
no.missings = imp0$method == ""

# Set imputation method to "bygroup"
imp0$method %>%
  replace(., . != "", "bygroup") -> imp.method

# Define the actual imputation method for all
# "bygroup" variables 
imp0$method[!no.missings] %>% as.list() -> imp.function

library(purrr)
rep("group", length(imp.method[!no.missings])) %>%
  set_names(names(imp.method[!no.missings])) %>%
  as.list() -> imp.group.variable

mice(imp.data,
     predictorMatrix = pred,
     method = imp.method,
     imputationFunction = imp.function,
     group = imp.group.variable,
     m = 50, maxit = 50,
     seed = 123) -> imp

save(imp, file="imp.rda")

plot(imp, layout = c(4, ceiling(sum(!no.missings)/2)))

densityplot(imp, ~ cesd.1 + cesd.2 | as.factor(group))

remotes::install_github("UCL/RefBasedMI")
library(RefBasedMI)

data$id = 1:nrow(data)

pivot_longer(data = data, 
             ends_with(c(".1", ".2")),
             values_to = "value",
             names_to = c("outcome", "time"),
             names_pattern = "(.*).(.)") -> data.long

data.long$time

data.long %>% 
  mutate(time = recode(time, `1` = 7, `2` = 12)) -> data.long

data.long[1:10, c("id", "cesd.0", "group", 
                  "outcome", "time", "value")]

data.long %>% 
  filter(outcome == "cesd") -> data.long.cesd


imp.j2r <- RefBasedMI(data = data.long.cesd, 
                      covar = c(age, sex, prevpsychoth,
                                prevtraining, child, employ,
                                rel, cesd.0, atsphs.0, ceq,
                                badssf.0, hadsa.0), 
                      depvar = value, 
                      treatvar = group, 
                      idvar = id, timevar = time, 
                      method = "J2R", reference = 0, 
                      M = 50, seed = 123) %>% 
                        as.mids()

save(imp.j2r, file="imp.j2r.rda")


### 4.3 Rubin's Combination Rules ---------------------------------------------

### 4.4 Functional Programming ------------------------------------------------

library(mitml)
implist <- mids2mitml.list(imp)

# Create an empty vector in which our results will be stored
means <- vector()

# Loop through all m=50 imputation sets
for (i in 1:50){
  means[i] <- implist[[i]] %>% pull(cesd.1) %>% mean()
}
mean(means)

implist %>% map_dbl(~mean(.$cesd.1)) %>% mean()

# 'x' represents the individual list element in 'list'
map(list, function(x) mean(x$variable))

# '.' represents the individual list element in 'list'
map(list, ~ mean(.$variable))



## 5. Effectiveness Analysis --------------------------------------------------

### 5.1 Continuous Endpoints --------------------------------------------------

library(mitml)
library(purrr)
library(miceadds)

m.lm <- lm(cesd.1 ~ 1 + group + scale(cesd.0), data = imp$data)

summary(m.lm)

anova(m.lm)

implist <- mids2mitml.list(imp)

class(implist)

with(implist, lm(cesd.1 ~ 1 + group + cesd.0)) %>%
  testEstimates()

# df.com is defined as sample size minus the number of parameters:
df.com <- nrow(imp$data) - length(coef(m.lm))

with(implist, lm(cesd.1 ~ 1 + group + cesd.0)) %>%
  testEstimates(df.com = df.com)

with(implist, lm(cesd.1 ~ 1 + group + cesd.0)) %>%
  map_dbl(~anova(.)$`F value`[1]) -> Fvalues

micombine.F(Fvalues, df1=1)

implist.j2r <- mids2mitml.list(imp.j2r)
map(implist.j2r, function(x){
  x %>% 
    mutate(time = recode(time, `7` = 1, `12` = 2)) %>% 
    pivot_wider(names_from = c(outcome, time),
                names_sep = ".",
                values_from = value)
}) -> implist.j2r

class(implist.j2r)

implist.j2r <- as.mitml.list(implist.j2r)

with(implist.j2r, lm(cesd.1 ~ 1 + group + cesd.0)) %>%
  testEstimates()

# Calculate mean SD for both groups across imputed data sets
implist %>% 
  map_dbl(~ filter(., group==1) %>% pull(cesd.1) %>% sd()) %>% 
  mean() -> s_ig

implist %>% 
  map_dbl(~ filter(., group==0) %>% pull(cesd.1) %>% sd()) %>% 
  mean() -> s_cg

# Define sample size in both groups
n_ig <- n_cg <- 273

# Calculate pooled SD
sqrt(((n_ig-1)*s_ig^2 + (n_cg-1)*s_cg^2)/
       (n_ig+n_cg-2))

(m.es <- with(implist, lm(I(cesd.1/8.586) ~ 1 + group + cesd.0)) %>%
    testEstimates())

confint(m.es)

(m.es.cca <- lm(I(cesd.1/8.586) ~ 1 + group + cesd.0, 
                data = imp$data))
confint(m.es.cca)

(m.es.j2r <- 
    with(implist.j2r, lm(I(cesd.1/8.586) ~ 1 + group + cesd.0)) %>%
    testEstimates())
confint(m.es.j2r)


### 5.2 Binary Endpoints ------------------------------------------------------

rci <- function(y0, y1, ryy){
  diff = y1-y0
  sdiff = sqrt(2*((sd(y0)*sqrt(1-ryy))^2))
  return(diff/sdiff)
} 

ryy <- 0.85

implist %>% 
  map_dbl(~sqrt(2*((sd(.$cesd.0)*sqrt(1-ryy))^2))) %>% 
  mean() -> s_diff

s_diff

implist %>%
  map(function(x){
    
    # Calculate RCI for each person
    x$rci = with(x, rci(y0 = cesd.0, y1 = cesd.1, ryy = ryy))
    
    # Create binary indicator for 
    # reliable improvement & deterioration
    x$ri = ifelse(x$rci <= -1.96, 1, 0)
    x$rd = ifelse(x$rci >= 1.96, 1, 0)
    x
    
  }) -> implist

# Get the number of imputation sets
m <- length(implist)

# Generate a reliable improvement counts table
# Aggregate the values across all data sets.
implist %>%
  map(~ with(., table(group, ri))) %>%
  {Reduce(`+`, .)/m} %>%
  as.matrix() %>%
  round() -> table.ri

table.ri

# Calculate sample size in both groups
n.ig <- sum(table.ri[2,])
n.cg <- sum(table.ri[1,])

# Calculate proportion of reliable improvement cases
p.ig <- table.ri[2,2]/n.ig
p.cg <- table.ri[1,2]/n.cg

# Calculate NNT (inverse risk reduction)
(p.ig - p.cg)^-1

dat.plt.ig <- implist[[1]] %>% filter(group == 1)
dat.plt.cg <- implist[[1]] %>% filter(group == 0)

# We require the scales package to be loaded.
library(scales)

# intervention group plot
plot(dat.plt.ig$cesd.0, dat.plt.ig$cesd.1, 
     xlim = c(0,50), ylim = c(0,50),
     xlab = "CES-D (Baseline)", ylab = "CES-D (Post-Test)",
     pch = 16, col = alpha("black", 0.3),
     main = "Intervention Group")
abline(coef = c(0,1), col = "blue")
polygon(c(0,0,50,50), c(0-s_diff*qnorm(.975),0+s_diff*qnorm(.975),
                        50+s_diff*qnorm(.975),50-s_diff*qnorm(.975)), 
        col = alpha("lightblue", 0.3), border = NA)
text(50, 0, "Improvement", pos=2, col="darkgreen")
text(0, 50, "Deterioration", pos=4, col="darkred")

# control group plot
plot(dat.plt.cg$cesd.0, dat.plt.cg$cesd.1, 
     xlim = c(0,50), ylim = c(0,50),
     xlab = "CES-D (Baseline)", ylab = "CES-D (Post-Test)",
     pch = 16, col = alpha("black", 0.3),
     main = "Control Group")
abline(coef = c(0,1), col = "blue")
polygon(c(0,0,50,50), c(0-s_diff*qnorm(.975),0+s_diff*qnorm(.975),
                        50+s_diff*qnorm(.975),50-s_diff*qnorm(.975)), 
        col = alpha("lightblue", 0.3), border = NA)
text(50, 0, "Improvement", pos=2, col="darkgreen")
text(0, 50, "Deterioration", pos=4, col="darkred")

with(implist, glm(ri ~ group + scale(cesd.0), 
                  family = binomial("logit"))) %>% 
  testEstimates() -> m.glm; m.glm

exp(m.glm$estimates["group", "Estimate"])

with(implist, glm(ri ~ group, family = binomial("logit"))) %>% 
  testEstimates() -> m.glm.ua
exp(m.glm.ua$estimates["group", "Estimate"])

m.glm <- glm(ri ~ group + scale(cesd.0), 
             data = implist[[1]], family = binomial("logit"))

library(stdReg)
m.std <- stdGlm(m.glm, X = "group", data = implist[[1]])

summary(m.std, contrast = "ratio", reference = 0)  

summary(m.std, contrast = "difference", reference = 0)  

# Fit model with treatment-covariate interactions
m.glm2 <- glm(ri ~ group*(scale(cesd.0) + scale(age) + sex), 
              data = implist[[1]], family = binomial)

# Return marginal risk ratio
m.std2 <- stdGlm(m.glm2, X = "group", data = implist[[1]])
summary(m.std2, contrast = "ratio", reference = 0)  

implist %>% 
  map_dfr(function(x){
    
    # Run GLM and standardize
    std.glm <- glm(ri ~ group + scale(cesd.0), data = x,
                   family = binomial) %>% 
      stdGlm(., X = "group", data = x)
    
    # Calculate the standardized log-RR
    summary(std.glm, contrast = "ratio", transform = "log",
            reference = 0)$est.table["1", 1:2] 
    
  }) -> std.mi

pool.scalar(
  Q = std.mi$Estimate,
  U = std.mi$`Std. Error`^2)[c("qbar", "t", "df")] -> res.std.rr


within(res.std.rr, {
  
  # Calculate CI based on t-distribution with MI degrees of freedom
  lower.qbar <- qbar - qt(0.975, df) * sqrt(t)
  upper.qbar <- qbar + qt(0.975, df) * sqrt(t)
  pval <- pt(qbar/sqrt(t), df, lower = FALSE)*2
  
  # Use antilog to transform logRR values to RR
  rr <- exp(qbar)
  lower.rr <- exp(lower.qbar)
  upper.rr <- exp(upper.qbar)
  
}) -> res.RR

res.RR[c("rr", "lower.rr", "upper.rr")]

implist %>% 
  map_dfr(function(x){
    
    # Run GLM and standardize
    std.glm <- glm(ri ~ group + scale(cesd.0), data = x,
                   family = binomial) %>% 
      stdGlm(., X = "group", data = x)
    
    # Calculate the standardized log-RR
    summary(std.glm, contrast = "difference", transform = "logit",
            reference = 0)$est.table["1", 1:2] 
    
  }) -> std.mi


pool.scalar(
  Q = std.mi$Estimate,
  U = std.mi$`Std. Error`^2)[c("qbar", "t", "df")] -> res.std.rd

within(res.std.rd, {
  
  # Calculate CI based on t-distribution with MI degrees of freedom
  lower.qbar <- qbar - qt(0.975, df) * sqrt(t)
  upper.qbar <- qbar + qt(0.975, df) * sqrt(t)
  pval <- pt(qbar/sqrt(t), df, lower = FALSE)*2
  
  # Use inverse logit to transform logit RD values to RD
  # We also have to subtract the inverse logit RD for the reference group
  rd <- plogis(qbar) - plogis(0)
  lower.rd <- plogis(lower.qbar) - plogis(0)
  upper.rd <- plogis(upper.qbar) - plogis(0)
  
}) -> res.RD

res.RD[c("rd", "lower.rd", "upper.rd")]

as.numeric(res.RD[c("rd", "lower.rd", "upper.rd")])^-1


## 6. Miscellaneous Topics ----------------------------------------------------

### 6.1 Multiple Testing ------------------------------------------------------

# ANCOVA for post-test CES-D
with(implist, lm(cesd.1 ~ 1 + group + cesd.0)) %>%
  map_dbl(~anova(.)$`F value`[1]) %>%
  micombine.F(df1=1) -> F.cesd

# ANCOVA for post-test BADS-SF
with(implist, lm(badssf.1 ~ 1 + group + badssf.0)) %>%
  map_dbl(~anova(.)$`F value`[1]) %>%
  micombine.F(df1=1) -> F.badssf

# ANCOVA for post-test HADS-A
with(implist, lm(hadsa.1 ~ 1 + group + hadsa.0)) %>%
  map_dbl(~anova(.)$`F value`[1]) %>%
  micombine.F(df1=1) -> F.hadsa

# Collect all three p-values
(p <- c(F.cesd["p"], F.badssf["p"], F.hadsa["p"]))

p.adjust(p, method = "bonferroni")

p.adjust(p, method = "holm")


### 6.2 Longitudinal Data -----------------------------------------------------

implist %>% 
  map(function(x){
    x %>% 
      mutate(id = 1:nrow(x)) %>% 
      pivot_longer(ends_with(c(".1", ".2")),
                   values_to = "value",
                   names_to = c("outcome", "time"),
                   names_pattern = "(.*).(.)") %>% 
      mutate(time = recode(time, `1` = 7, `2` = 12))
  }) -> implist.long

# Extract a slice of the first imputed data set
implist.long[[1]][1:10, c("id", "outcome", "time", "value", 
                          "cesd.0", "badssf.0", "hadsa.0")]

implist.long %>% 
  map(~ filter(., outcome == "cesd") %>%
        mutate(id = as.factor(id),
               group = as.factor(group),
               time = recode(time, `7` = "7 wks", `12` = "12 wks") %>% 
                 as.factor())
  ) -> implist.long

library(mmrm)
m.mmrm <- mmrm(value ~ group + scale(cesd.0) + us(time|group/id), 
               method = "Kenward-Roger", data = implist.long[[1]])
summary(m.mmrm)

implist.long %>% 
  map(~ mmrm(value ~ group + scale(cesd.0) + us(time|group/id), 
             method = "Kenward-Roger", data = .)) %>% 
  testEstimates()

implist.long %>% 
  map(~ mmrm(value ~ group*time + scale(cesd.0) + us(time|group/id), 
             method = "Kenward-Roger", data = .)) %>% 
  testEstimates()

implist.long %>% 
  map(~ mutate(., time = 
                 factor(time, levels = c("7 wks", "12 wks")))) %>% 
  map(~ mmrm(value ~ group*time + scale(cesd.0) + us(time|group/id), 
             method = "Kenward-Roger", data = .)) %>% 
  testEstimates() -> m.mmrm.7wk

m.mmrm.7wk

confint(m.mmrm.7wk)


### 6.3 Reporting & Reproducibility -------------------------------------------

m <- length(implist)
catvars <- c("sex", "prevpsychoth", "prevtraining",
             "child", "employ", "rel", "degree", "group")

implist %>%
  map(~mutate_at(., catvars, as.factor)) -> implist

implist %>%
  map(~skimReport(.)) %>% 
  {.[[1]]$numerics[,1] ->> var.names;.} %>% 
  map(~.$numerics[,2:4]) %>% 
  Reduce(`+`, .)/m -> num.desc.full

implist %>%
  map(~filter(., group == 1)) %>%
  map(~skimReport(.)) %>% 
  map(~.$numerics[,2:4]) %>% 
  Reduce(`+`, .)/m -> num.desc.ig

implist %>%
  map(~filter(., group == 0)) %>%
  map(~skimReport(.)) %>% 
  map(~.$numerics[,2:4]) %>% 
  Reduce(`+`, .)/m -> num.desc.cg

tab.desc <- cbind(var.names, num.desc.full, num.desc.ig, num.desc.cg)
tab.desc

library(openxlsx)
write.xlsx(tab.desc, file="imputed_descriptives.xlsx")

