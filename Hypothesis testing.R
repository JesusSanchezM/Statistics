library(tidyverse)
library(patchwork)
library(gapminder)

attach(gapminder)
table(country)

summary(gapminder$lifeExp)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#23.60   48.20   60.71   59.47   70.85   82.60 
#proof that the population lifeExp mean is different of 59.47

#Null hypothesis: mu=59.47
#Alternative hypothesis: mu!=59.47

gapminder %>% 
  select(lifeExp) %>% 
  t.test(mu=(mean(lifeExp)))

gapminder %>% 
  ggplot(aes(x=lifeExp) )+ geom_density()

my_ttest <- gapminder %>% 
  select(lifeExp) %>% 
  t.test(mu=(mean(lifeExp)))

attributes(my_ttest)
my_ttest$p.value













