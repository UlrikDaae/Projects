#In this very first project I am going to be making an analysis of life expectancy data globally, and then look at some specific metrics for Kenya in the time period 2000-2015. 

library(tidyverse) # I'm doing this with Tidy
life_expect <- read_csv("Life Expectancy Data.csv") # Reading in

view(life_expect)
str(life_expect)

#Creating a first basic selection of things I want to look at. Renaming for ease. 
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

# First I want to see GDP vs Life expectancy.
gdp_life <- basics_df %>% 
  select(life_expectancy,
         GDP, 
         status, 
         country)

# Plot of life expectancy vs GDP where the color shows developed vs developing countries. Shows, unsurprisingly, that countries on the higher end of life expectancy have higher GDP. I tried to make a regression line (lm), but failed.. 

ggplot(gdp_life) +
  geom_point(mapping = aes(x = life_expectancy, y = GDP, color = status)) +
  labs(title = "GDP vs Life Expectancy")
  
# Now, to give another view of the scope of socio-economic status globally, what is the distribution of developed vs developing countries?

ggplot(basics_df) +
  geom_bar(aes(x ="", y = status, fill = status), stat = "identity") + # remember stat!
  coord_polar("y", start = 0) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFA8E")) + # Excuse the ugly background...
  labs(title = "Distribution of developing vs undeveloped countries") +
  scale_fill_discrete(name = "Status") #changes name of legend

#So besides the ugly background, we have a little visualization of how the distribution is between developed and undeveloped countries. 


#So, what is the impact of schooling on GDP? Let's look at that. 
gdp_school <- basics_df %>% 
  select(
    Schooling,
    GDP,
    status)

gdp_school_ren <- gdp_school %>% 
  rename(
    schooling = Schooling
  )

ggplot(gdp_school_ren) + 
  geom_point(mapping = aes(x = schooling, y = GDP, color = status)) +
  labs(title = "GDP vs Schooling comparison")
# More schooling tends to yield higher GDP. 


# So now I want to look more specifically at one country - China
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
  geom_line(aes(x = year, y = infant_mortality)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme(
    panel.background = element_rect(fill = "#FFFB9E")) +
  labs(title = "Decline in child mortality in Kenya")


diseases_Kenya <- basics_df %>% 
  select(
        country,
        year,
        Diphtheria,
         Polio,
         `Hepatitis B`,
         Measles,
         `HIV/AIDS`) %>% 
  filter(
    country == "Kenya"
  )


# what may influence this decline? Perhaps a reduction in diseases? Let's see.
ggplot(diseases_Kenya) +
  geom_line(aes(x = year, y = `HIV/AIDS`))
