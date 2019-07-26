########
# Getting data from the dslabs package
#
#
######

path <- system.file("extdata", package = "dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath    # just printing the full path of the file we want to copy
file.copy(fullpath, getwd())   # actual copy
file.exists(filename) # double checking the file was indeed copied over

library(tidyverse)
read_lines("extdata/murders.csv", n_max = 3)

filename <- "extdata/murders.csv"
dat <- read_csv(filename)
head(dat) #object is a tibble, with the content of the file just read
dat2 <- read.csv(filename)
head(dat2) #object is a data.frame when using the built in function for reading the file
class(dat2)

# downloading from the web
# using temp names as a safety measure

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat3 <- read_csv(tmp_filename)
file.remove(tmp_filename)

url14 <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url14)
url15 <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat15 <- read_csv(url15, col_names = FALSE) # if file doesn't have a header, you must specify this argument to the function so it assigns column names automatically

url17 <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
co2_mauna_loa <- read_table(url17, skip = 57, col_names = c("year", "mean", "unc"))

###########################
#
# Section 2: Tidy Data
#
###########################

library(tidyverse)
library(dslabs)
data(gapminder)
# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)
# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()
# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

################
#
# Separate and unite functions
#
#####################################

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]
# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))
# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")
# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")
# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 
# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")
# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

#####################################
#
#  2.2 Combining Tables
#
#####################################

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)
# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)
# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)
# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2
# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

# Binding

bind_cols(a = 1:3, b = 4:6)
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

# set operators
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)
# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)
# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
library(dplyr)
setdiff(tab1, tab2)
# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)

library(dplyr)
setequal(tab1, tab2)

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_salary1 <- Salaries %>% filter(yearID == 2016) %>% left_join(top_names1) %>% select(nameFirst, nameLast, teamID, HR, salary)

top10_with_awards_2016 <- top %>% semi_join(AwardsPlayers)
AwardsPlayers
top_10_HR_hitters <- AwardsPlayers %>% filter(yearID == 2016) %>% group_by(playerID) %>% summarise()# %>% anti_join(top)

awarded_but_not_top10 <- top_10_HR_hitters %>% anti_join(top)
#####################################
#
#  2.3 WEB scrapping
#
#####################################
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state&oldid=843135608")

library(tidyverse)
library(rvest)
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab
tab[[3]]

tab <- tab[[3]] %>% html_table()
class(tab)

tab <- tab%>% setNames(c("state", "population", "total", "murder_rate"))
head(tab)




#####################################
#
# 3.1 String Processing Part 1
#
#####################################

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not
s <- "10""    # error - unclosed quotes
s <- '10"'    # correct
# cat shows what the string actually looks like inside R
cat(s)
s <- "5'"
cat(s)
# to include both single and double quotes in string, escape with \
s <- '5'10"'    # error
s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)
