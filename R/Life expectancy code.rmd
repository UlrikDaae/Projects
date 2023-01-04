---
title: "Life Expectancy in Kenya"
output:
  word_document: default
  html_notebook: default
---

Hello Reader. This Rmd-file is my first basic analysis of a dataset in my journey to learn R. I really enjoy learning, and learning with R. It is however difficult. As it is with all types of learning though, slow and steady progress brings you further towards your goal. This has taken many, many hours - far more than I thought it would take. I think many who have learnt R can relate to a similar experience in the first phase of their learning. However, this is my first example of how progress is slow, but progress nonetheless. This notebook gives some insight, perhaps more into me and my coding rather than the dataset itself. Thus, this serves as a display of my initial skills in R.

Please note that this dataset has some data inconsistencies and is not guaranteed to be accurate.

Ulrik

I start with:

library(tidyverse) # 
life_expect <- read_csv("Life Expectancy Data.csv") # Reading in data

# Initial exploratory analysis. Just want to see what this data is made up of. 
view(life_expect)
str(life_expect)

After loading in the data, I created a table with the columns I wanted to look at. These names were unpractical. Some had single quotes around them and they didn't have underscores between words.

# Creating a first basic selection of collumns I want to look at. Renaming for ease. 

basics_df <- life_expect %>% 
  rename(
    country = Country,
    year = Year,
    life_expectancy = `Life expectancy`,
    status = Status,
    adult_mortality = `Adult Mortality`,
    infant_mortality = `infant deaths`,
    population = Population
  )

Let's first make a general histogram of what the general life expectancy is. To not overpopulate the graph too much, we're only looking at the period 2010-2015.

life_year <- basics_df %>% 
  select(life_expectancy,
         year) %>% 
  filter(year >= 2010)

ggplot(life_year) + 
  geom_histogram(aes(x = life_expectancy), fill = "#244747") +
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  facet_wrap(~ year)

This histogram shows the number of countries and their corresponding life expectancy. It's an interesting chart, perhaps most interestingly is the large spike in the ≈ 75 range. However, much is unsaid from this graph. Let's move on.

I'm fascinated by what impacts life expectancy. The most obvious assessment is that GDP will correlate with the life expectancy. Let's see if that's the case with this dataset.

{r}
gdp_life <- basics_df %>% # Creating my dataset. 
  select(life_expectancy,
         GDP, 
         status, 
         country)

# I'm mapping this out with a scatter plot. 
ggplot(gdp_life) +
  geom_point(mapping = aes(x = life_expectancy, y = GDP, color = status)) +
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "GDP vs Life Expectancy") +
  geom_smooth(method = lm, formula = y ~ x, aes(life_expectancy, GDP), color = "black", se = FALSE) # Simple regression added to this model. 

So this graph is quite interesting to me. What we can clearly see is that there are many countries that have a low life expectancy, and they hava correspondingly low GDP, and vice versa. There are however exceptions as can be seen. The simple linear regression show's the simple trend between higher life expectancy and GDP. In other words - our earlier assumption was correct.

This graph also discerns between "developed" and "developing" countries and color them accordingly. Here we see that developed countries exclusively live longer, whilst often having higher GDP as well.

I want to dive into this more since this data set provides it. Note that this is without a context for what a "developing" or "developed" country is. So, what is the distribution of developed vs developing countries?

{r}
ggplot(basics_df) +
  geom_bar(aes(x = "", y = status, fill = status), stat = "identity") +
  coord_polar("y", start = 0) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "Distribution of developing vs undeveloped countries") +
  scale_fill_discrete(name = "Status") 

This pie chart shows that there's far more developing countries than there are developed countries. In my eyes, it's far more interesting to look at the developing countries, so that's what we're going to be doing now.

It's natural to think that higher GDP will have an impact on whether a country is developed or developing. But what can influence the GDP? Well, let's see. First: GDP vs schooling

{r}
gdp_school <- basics_df %>% 
  select(
    Schooling,
    GDP,
    status)

gdp_school_ren <- gdp_school %>% # Again, renaming for some consistency.  
  rename(
    schooling = Schooling)

ggplot(gdp_school_ren) + 
  geom_point(mapping = aes(x = schooling, y = GDP, color = status)) +
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "GDP vs Schooling comparison") +
  geom_smooth(method = lm, formula = y ~ x, aes(schooling, GDP), color = "black", se = FALSE)

Similar trend to the previous scatter plot. Higher levels of schooling means higher GDP. Looking at these kinds of population-based metrics is quite interesting, and is something I want to dive a little deeper into, specifically with Kenya as a case.

{r}
kenya_df <- basics_df %>% 
  select(
    country,
    population,
    year,
    Schooling,
    Diphtheria,
    Polio,
    `Hepatitis B`,
    Measles,
    `HIV/AIDS`,
    infant_mortality,
    adult_mortality,
    GDP) %>% 
  rename(diphteria = Diphtheria, # again, rename for ease. 
    schooling = Schooling,
    polio = Polio,
    hep_b = `Hepatitis B`,
    measles = Measles,
    HIV_AIDS = `HIV/AIDS`) %>% 
  filter(country == "Kenya")

ggplot(kenya_df) + 
  geom_col(aes(x = year, y = GDP), fill = "#244747") +
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "Kenyas GDP increase from 2000-2015")

As we can see, this dataset has inaccuracies which I haven't been able to resolve. Despite that however, it's clear that Kenya has had a stark increase in GDP over the period from 2000-2015. May this be impacted by schooling as previously charted?

{r}
kenya_schools <- kenya_df %>% 
  select(
    schooling,
    year,
    GDP)

ggplot(kenya_schools) +
  geom_line(mapping = aes(x = year, y = schooling)) +
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "Kenyas increase in schooling from 2000-2015") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))

At the very least we see a stark increase in the level of schooling over this period. However, as with many of these analyses, it's hard to discern the complete picture. Regardless, we have up to this point shown how things move in the positive direction for Kenya, such as the GDP and level of schooling. 

On the opposite end I would like to finish with some charts on the mortality rates for adults and children. These have declined in this period. 

{r}
mortality_yy <- basics_df %>%
  select(
    country,
    infant_mortality,
    adult_mortality,
    year) %>% 
  filter(
    country == "Kenya"
  )

ggplot(mortality_yy) + 
  geom_line(aes(x = year, y = infant_mortality), color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "Decline in infant mortality in Kenya")

{r}
ggplot(mortality_yy) + 
  geom_line(aes(x = year, y = adult_mortality), color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) + 
  theme(
    panel.background = element_rect(fill = "#efe8d1")) +
  labs(title = "Decline in adult mortality in Kenya")

This analysis has been quite small and is very limited, but I hope it can serve as some introduction to me and my skills with R. 

Thanks,

Ulrik

