?mean
?ChickWeight


5+6

a <- 5
b <- 6
sum(a,b)




11+5
b+6
sum(a,b)
c <- c(5,6)
sum(c)


names <- c("Yasir", "Nawaz")

friends <- data.frame(names , c)
View(friends)
str(friends)

friends$names
friends$c


friends[1,1]
friends[1, ]
friends[ ,1]


data()
View(ChickWeight)

#install.packages("tidyverse")

library(tidyverse)
View(starwars)

starwars %>% 
  filter(height> 150 & mass < 200) %>% 
  mutate(height_in_meters = height/100) %>% 
  select(height_in_meters , mass) %>% 
  arrange(mass) %>% 
  plot()
  #View()

str(starwars)



#explore
View(msleep)

glimpse(msleep)
head(msleep)


class(msleep$name)
class(msleep$genus)
class(msleep$awake)

length(msleep)
length(msleep$name)

names(msleep)

unique(msleep$vore)


missing <- !complete.cases(msleep)

msleep[missing, ]


#Select Variables
names(starwars)
starwars %>% 
  select(name,height, mass)

starwars %>% 
  select(1:3)

starwars %>% 
  select(ends_with("color"))
#channging variable names
starwars %>% 
  rename("characters" = "name") %>% 
  head()



class(starwars$hair_color)

starwars$hair_color<- as.factor(starwars$hair_color)
class(starwars$hair_color)

starwars %>% 
  mutate(hair_color =  as.character(hair_color)) %>% 
  glimpse()

df<- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)

df <-df %>% 
  mutate(sex = factor(sex, levels = c("male", "female" , "hermaphroditic" , "none")))

levels(df$sex)
starwars %>% 
  select(mass, sex) %>% 
  filter(mass <55 & 
           sex == "male")



starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex , "male" = "man" , "female" = "woman"))

mean(starwars$height)  #cant handle missing values
mean(starwars$height , na.rm = TRUE)


starwars %>% 
  select(name, gender, hair_color , height)


Names<- c("Peter", "John" , "Andrew" , "Peter")
Age <- c(22 , 33 , 44 , 22)

friends <- data.frame(Names , Age)
str(friends)
friends

friends %>% 
  distinct()

friends


distinct(friends)

#Create or Change a Variable 

starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name , height_m , height )


starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name , height_m , height ) %>% 
  mutate(tallness = 
           if_else(height_m < 1 ,
                   "short" ,
                   "tall"))

#install.packages("gapminder")

library(gapminder)
view(gapminder)

names(gapminder)

data<- select(gapminder , country , year , lifeExp)
str(data)
data


wide_date<- data %>% 
  pivot_wider(names_from = year , values_from = lifeExp)

wide_date
View(wide_date)


long_data <- wide_date %>% 
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")
view(long_data)


#Descirbe

view(msleep)

min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)


mean(msleep$awake)
median(msleep$awake)


var(msleep$awake)

summary(msleep$awake)


msleep %>% 
  select(awake, sleep_total) %>% 
  summary()


msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = 
              max(sleep_total) - min(sleep_total)) %>% 
  arrange(Average) %>% 
  view()

table(msleep$vore)

msleep %>% 
  select(vore , order) %>% 
  filter(order %in% c("Rodentia" , "Primates")) %>% 
  table()

#Visualise

plot(pressure)

ggplot(data = starwars,
       mapping = aes(x = gender )) + geom_bar(fill = "steelblue")

starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x = height)) + geom_histogram()


starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x = height)) + 
  geom_boxplot(fill = "steelblue") +
  theme_bw() + 
  labs(title = "Boxplot of heigt",
       x = "Height of characters")


#density Plots

starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c("male" , "female")) %>% 
  
  ggplot(mapping = aes(x=height,
                       color = sex,
                       fill = sex )) +
  geom_density(alpha = 0.3) +
  theme_bw()



starwars %>% 
  filter(mass <200) %>% 
  filter(sex %in% c("male" , "female")) %>% 
  ggplot(aes(x= height , y = mass , color = sex )) +
  geom_point(size = 2 , alpha = 0.5) +
  theme_classic() +
  labs(title = "Height and mass by sex")



starwars %>% 
  filter(mass <200) %>% 
  filter(sex %in% c("male" , "female")) %>% 
  ggplot(aes(x= height , y = mass , color = sex )) +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw() +
  labs(title = "Height and mass by sex")
  


#Analyze

view(gapminder)
t_test_plot()

gapminder %>% 
  filter(continent %in% c("Africa" , "Europe")) %>% 
  t.test(lifeExp~ continent , data = . ,
         alternative = "two.sided")

anova_plot


gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas" , "Europe" , "Asia")) %>% 
  aov(lifeExp ~ continent, data = . ) %>% 
  summary()


gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas" , "Europe" , "Asia")) %>% 
  aov(lifeExp ~ continent, data = . ) %>% 
  TukeyHSD()



gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas" , "Europe" , "Asia")) %>% 
  aov(lifeExp ~ continent, data = . ) %>% 
  TukeyHSD() %>% 
  plot()


#chi squared

chi_plot
head(iris)

flowers<- iris %>% 
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small", "Medium" , "Large"))) %>% 
  select(Species , size)


flowers %>% 
  select(size) %>%
  table() %>% 
  chisq.test()


flowers %>% 
  table() %>% 
  chisq.test()



head(cars, 10)

cars %>% 
  lm(dist ~ speed , data = . ) %>% 
  summary()
