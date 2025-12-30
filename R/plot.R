if (!require(plotly)) install.packages("plotly")
if (!require(tidyverse)) install.packages("tidyverse")

city_data = read.csv("./city_lifestyle_dataset.csv", header = TRUE)
city_data <- df[1]

print(head(mtcars))
print(head(city_data))
print(class(mtcars))
print(class(city_data))



print_plot1 <- function() {

data <- mtcars %>% mutate(cyl = factor(cyl),
                          Model = rownames(mtcars))

plot1 <- data %>% ggplot(aes(x = wt, y = mpg, size = hp)) +
         geom_point(alpha = 0.5)

print(plot1)
# print(rownames(mtcars))
# print(mpg)

}

print_plot1()

print_plot2 <- function() {

print(rownames(city_data))

data2 <- city_data %>% mutate(happiness_score = factor(happiness_score),
                              Model = rownames(city_data))

plot1 <- data %>% ggplot(aes(x = population_density, y = avg_income, size = public_transport_score)) +
         geom_point(alpha = 0.5)

print(plot1)


}

print_plot2()
