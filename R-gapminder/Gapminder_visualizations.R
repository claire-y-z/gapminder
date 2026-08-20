library(gapminder)
library(ggplot2)
library(dplyr)

View(gapminder)

#GDP Per Capita vs life expectancy

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point(alpha =0.6, size=2) +
  labs(
    title = "GDP Per Capita vs Life Expectancy",
    subtitle = "Colored by continent",
    x= "GDP Per Capita ($)",
    y= "Life Expectancy (years)"
    ) +
  
  theme_light()+
  
  theme(
    plot.title = element_text(face="bold", size=14, hjust=0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.subtitle.position = "plot",
    plot.title.position = "plot"
  )

#US life expectancy over time

US_data <- gapminder |> filter(country == "United States")
ggplot(US_data, aes(x=year, y=lifeExp, color = year)) +
  geom_line(
    linewidth= 1.5,
    lineend="round",
    alpha=0.6
  ) + 
  
  labs(
    title ="US Life Expectancy Over Time",
    x = "Year",
    y ="US Life Expectancy"
    ) +
  
theme_bw()+
  
  theme(
    plot.title=element_text(face="bold", size=14, hjust=0.5),
    plot.title.position="plot"
  )

#India population over time

India_data <- gapminder |> filter(country=="India")

ggplot(India_data, aes(x=year, y=pop, color = year))+
  geom_line(
    linewidth=1.5,
    lineend="round",
    alpha=0.6
  ) +
  
  labs(
    title = "India Population Over Time",
    x = "Year",
    y = "India Population"
    )+ 
  
  theme_bw()+

India_data <- gapminder |> filter(country=="India")

ggplot(India_data, aes(x=year, y=pop, color = year))+
  geom_line(
    linewidth=1.5,
    lineend="round",
    alpha=0.6
  ) +
  
  labs(
    title = "India Population Over Time",
    x = "Year",
    y = "India Population"
    )+ 
  
  theme_bw()+

theme(
  plot.title = element_text(face="bold",size=14, hjust=0.5),
  plot.title.position= "plot"
)

theme(
  plot.title = element_text(face="bold",size=14, hjust=0.5),
  plot.title.position= "plot"
)

#Bonus Practice

#1 What is the average GDP per capita by continent


avg_gdp_continent <- gapminder |> 
  group_by(continent) |>
  summarize(avg_gdp =mean(gdpPercap)) |>
  arrange(desc(avg_gdp))
print(avg_gdp_continent)

ggplot(avg_gdp_continent, aes(x= reorder(continent, -avg_gdp),y=avg_gdp)) +
  geom_col(fill="blue")+
  labs(
    title="Average GDP per Capita by Continent",
    x="Continent",
    y="Average GDP per Capita (international dollars)"
    
  )+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=25,hjust=1))

#2 How was life expectancy distributed across all countries in 2007?

data_2007 <- gapminder |> filter(year == 2007)

ggplot(data_2007, aes(x = lifeExp)) +
  geom_histogram(bins = 20, fill = "mediumpurple", color = "black", alpha = 0.75) +
  geom_vline(aes(xintercept = mean(lifeExp)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Life Expectancy Across Countries (2007)",
    x = "Life Expectancy (years)",
    y = "Number of Countries"
  ) +
  theme_minimal()

summary(data_2007$lifeExp)
