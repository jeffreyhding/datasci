# Run this code to load the required packages
suppressMessages(suppressWarnings(suppressPackageStartupMessages({
  library(coursekata)
})))

wcmatches <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

glimpse(gapminder)

gf_histogram(~life_expectancy, data = gapminder, fill = "coral")%>%
gf_labs(title = "Life expectancy of countries\n(empirical sample)", x = "Life expectancy (in years)", y = "Number of countries")

gapminder <- gapminder[!(is.na(gapminder$gdp)), ]
gapminder$gdpPerCapita <- gapminder$gdp / gapminder$population
gapminder$gdpPerCapita.group <- ntile(gapminder$gdpPerCapita, 3)
gapminder$gdpPerCapita.group <- factor(gapminder$gdpPerCapita.group, levels = c(1,2,3), labels = c("low", "medium", "high"))

gf_histogram(~life_expectancy, data = gapminder, fill = "coral")%>%
gf_facet_grid(gdpPerCapita.group ~ .)%>%
gf_labs(title = "Life expectancy of countries by GDP per capita\n(empirical sample)", x = "Life expectancy (in years)", y = "Number of countries")

mean(~life_expectancy, data = subset(gapminder, gdpPerCapita.group == "low"))
mean(~life_expectancy, data = subset(gapminder, gdpPerCapita.group == "medium"))
mean(~life_expectancy, data = subset(gapminder, gdpPerCapita.group == "high"))

data.frame_1 <- gapminder%>%
group_by(gdpPerCapita.group)%>%
summarize(mean = mean(life_expectancy))

gf_histogram(~life_expectancy, data = gapminder, fill = "coral")%>%
gf_facet_grid(gdpPerCapita.group ~ .)%>%
gf_vline(xintercept = ~mean, data = data.frame_1, color = "black")%>%
gf_labs(title = "Life expectancy of countries by GDP per capita\n(empirical sample)", x = "Life expectancy (in years)", y = "Number of countries")

gapminder$gdpPerCapita.group_shuffled.1 <- shuffle(gapminder$gdpPerCapita.group)
gapminder$gdpPerCapita.group_shuffled.2 <- shuffle(gapminder$gdpPerCapita.group)
gapminder$gdpPerCapita.group_shuffled.3 <- shuffle(gapminder$gdpPerCapita.group)
# separated otherwise right half of data frame is cut off in PDF
head(select(gapminder, country, life_expectancy, gdpPerCapita, gdpPerCapita.group), 5)
head(select(gapminder, gdpPerCapita.group_shuffled.1, gdpPerCapita.group_shuffled.2, gdpPerCapita.group_shuffled.3), 5)

# simulated sample 1
data.frame_2 <- gapminder%>%
group_by(gdpPerCapita.group_shuffled.1)%>%
summarize(mean = mean(life_expectancy))

gf_histogram(~life_expectancy, data = gapminder, fill = "aquamarine3")%>%
gf_facet_grid(gdpPerCapita.group_shuffled.1 ~ .)%>%
gf_vline(xintercept = ~mean, data = data.frame_2, color = "black")%>%
gf_labs(title = "Life expectancy of countries by GDP per capita\n(simulated sample #1)", x = "Life expectancy (in years)", y = "Number of countries")

# simulated sample 2
data.frame_3 <- gapminder%>%
group_by(gdpPerCapita.group_shuffled.2)%>%
summarize(mean = mean(life_expectancy))

gf_histogram(~life_expectancy, data = gapminder, fill = "lightcoral")%>%
gf_facet_grid(gdpPerCapita.group_shuffled.2 ~ .)%>%
gf_vline(xintercept = ~mean, data = data.frame_3, color = "black")%>%
gf_labs(title = "Life expectancy of countries by GDP per capita\n(simulated sample #2)", x = "Life expectancy (in years)", y = "Number of countries")

# simulated sample 3
data.frame_4 <- gapminder%>%
group_by(gdpPerCapita.group_shuffled.3)%>%
summarize(mean = mean(life_expectancy))

gf_histogram(~life_expectancy, data = gapminder, fill = "lightskyblue")%>%
gf_facet_grid(gdpPerCapita.group_shuffled.3 ~ .)%>%
gf_vline(xintercept = ~mean, data = data.frame_4, color = "black")%>%
gf_labs(title = "Life expectancy of countries by GDP per capita\n(simulated sample #3)", x = "Life expectancy (in years)", y = "Number of countries")
