# Questions:
# 1. At what rate is India's population growing over time? 
# 2. What is the correlation between GDP per capita and life expectancy?
# 3. Rank countries by GDP per capita from 2007. Select the top 3.
# 4. How has life expectancy in the US changed over time?
# 5. Which countries changed most in life expectancy between 1952 and 2007?

# Setup
library(gapminder)
library(ggplot2)
library(dplyr)

#Q1: At what rate is India's population growing over time? 

india_pop <- gapminder |>
  filter(country == "India") |>
  group_by(year) |> 
  summarize(total_pop = sum(pop)) |>
  mutate(growth_rate = (total_pop - lag(total_pop)) / lag(total_pop) * 100)

mean(india_pop$growth_rate, na.rm = TRUE)


#Q2: What is the correlation between GDP per capita and life expectancy?

cor(gapminder$gdpPercap, gapminder$lifeExp)

#Q3: Rank countries by GDP per capita from 2007. Select the top 3.

gapminder |>
  filter(year== 2007) |> 
  arrange(desc(gdpPercap)) |>
  head(3)

#Q4: How has life expectancy in the US changed over time?

gapminder |> filter(country == "United States") |>
  select(year, lifeExp)

#Q5: Which countries changed most in life expectancy between 1952 and 2007?

data_1952 <- gapminder |>
  filter(year == 1952)
data_2007 <- gapminder |>
  filter(year == 2007)

comparison <- data_1952 |>
  inner_join(data_2007,by="country",suffix=c("_1952","_2007"))

comparison$lifeExp_change <- comparison$lifeExp_2007 - comparison$lifeExp_1952

comparison |> arrange(desc(lifeExp_change)) |> head(3)

