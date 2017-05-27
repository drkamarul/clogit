library(survival)
#create pairs

## change s_s here, test s_s <- 10, 20, 50, 100 then go for larger s_s

s_s <- 20

########################

set.seed(1234)
pair <- rep(1:10, each = 2, len =s_s)
pair
status <- rep(x = c('case','control'), s_s/2)
status
gender <- factor(rbinom(n = s_s, size = 1, prob = 0.5),
                 labels = c('male', 'female'))
gender
race <- factor(rbinom(n=s_s, size = 1, prob = 0.5),
               labels = c('raceA', 'raceB'))
race
age <- rnorm(n = s_s, mean = 100, sd = 20)

data1 <- data.frame(pair, status, age, gender, race)
head(data1)

str(data1)


mod1 <- clogit(I(status == 'case') ~ age + gender + strata(pair), data = data1)
summary(mod1)

mod2 <- clogit(I(status == 'case') ~ age + gender + race + strata(pair), data = data1)
summary(mod2)

mod3 <- clogit(I(status == 'case') ~ age + gender + race +
         gender:race + strata(pair), data = data1)
summary(mod3)

mod4 <- clogit(I(status == 'case') ~ gender + race +
                 gender:race + strata(pair), data = data1)
summary(mod4)
