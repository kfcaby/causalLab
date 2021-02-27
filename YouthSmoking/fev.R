library(tidyverse)
library(MatchIt)
library(Zelig)

fev = read.table(file = "Downloads/fev.dat.txt")
head(fev)
colnames(fev) = c("AGE", "FEV", "HEIGHT", "SEX", "SMOKE")
fev = as.tibble(fev)


model1 = lm(FEV ~ SMOKE + AGE + SEX, data = fev)
summary(model1)
confint(model1)

model_unadjusted = lm(FEV ~ SMOKE, data = fev)
summary(model_unadjusted)
confint(model_unadjusted)

t.test(fev$FEV[fev$SMOKE == 1], fev$FEV[fev$SMOKE == 0])

fev %>% ggplot(aes(x = AGE, y = FEV)) + 
  geom_point() + geom_smooth(method = "lm")

match.exact = matchit(SMOKE ~ AGE + SEX, data = fev, method = "exact")

matched.data = match.data(match.exact)

model.matched = lm(FEV ~ SMOKE + SEX + AGE*SMOKE, data = matched.data, weights = weights)
summary(model.matched)
confint(model.matched)

fev %>% ggplot(aes(x = AGE, y = FEV, color = factor(SMOKE))) + geom_point()


summary(model1)

nonsmoke = 0.234 + 0.226*15 + 0.315
smoke = 0.234 + 0.226*15 + 0.315 - 0.153

smoke/nonsmoke
