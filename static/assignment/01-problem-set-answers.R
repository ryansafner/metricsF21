# 1 -------------------
# install.packages("babynames")

# Note I've “commented” out some of these commands  (with a #) so they do not run when I knit this document
# You should **never** install a package inside a .Rmd document, just do that in R Studio itself
# Of course, you do need to load everything with library() in a .Rmd document!

library(babynames)
library(tidyverse)
# ?babynames()


# 1-a ------------------

# save as a new tibble
top_5_boys_2017 <- babynames %>% # take data
  filter(sex=="M", # filter by males
         year==2017) %>% # and for 2007
  arrange(desc(n)) %>% # arrange in largest-to-smallest order of n (number)
  slice(1:5) %>% # optional, look only at first 5 rows; head(., n=5) also works
  mutate(percent = round(prop*100, 2)) # also optional, make a percent variable rounded to 2 decimals

# look at our new tibble
top_5_boys_2017

top_5_boys_2017 %>%
  select(name,percent) %>%
  knitr::kable()

top_5_boys_2017_alt <- c("Liam", "Noah", "William", "James", "Logan")

top_5_boys_2017_alt
# you could alternatively add a command, 
# %>% pull(name) to the first chunk of code, 
# and it would do the same thing, but we'd want to save it, 
# for example:

top_5_boys_2017_alt <- babynames %>%
  filter(sex=="M",
         year==2017) %>%
  arrange(desc(n)) %>% 
  slice(1:5) %>%
  mutate(percent = round(prop*100, 2)) %>%
  pull(name)

top_5_boys_2017_alt

# 1-b -----------------

# save as a new tibble
top_5_girls_2017 <- babynames %>% # take data
  filter(sex=="F", # filter by females
         year==2017) %>% # and for 2007
  arrange(desc(n)) %>% # arrange in largest-to-smallest order of n (number)
  slice(1:5) %>% # optional, look only at first 5 rows; head(., n=5) also works
  mutate(percent = round(prop*100, 2)) # also optional, make a percent variable rounded to 2 decimals

# look at our new tibble
top_5_girls_2017

top_5_girls_2017 %>%
  select(name,percent) %>%
  knitr::kable()

top_5_girls_2017_alt <- c("Emma", "Olivia", "Ava", "Isabella", "Sophia")

# 2 ------------------

# for boys
ggplot(data = top_5_boys_2017)+
  aes(x = reorder(name, n), #note this reorders the x variable from small to large n
      y = percent, # you can use prop if you didn't make a percent variable
      fill = name)+ # optional color!
  geom_col()+
  
  # all of the above is sufficient, now I'm just making it pretty
  scale_y_continuous(labels = function(x){paste0(x, "%")}, # add percent signs
                     breaks = seq(from = 0, # make line breaks every 0.25%
                                  to = 1,
                                  by = 0.25),
                     limits = c(0,1), # limit axis to between 0 and 1
                     expand = c(0,0))+ # don't let it go beyond this
  labs(x = "Name",
       y = "Percent of All Babies With Name",
       title = "Most Popular Boys Names Since 1880",
       fill = "Boy's Name",
       caption = "Source: SSA")+
  ggthemes::theme_pander(base_family = "Fira Sans Condensed", base_size=16)+
  coord_flip()+ # flip axes to make horizontal!
  scale_fill_viridis_d(option = "default")+ # use viridis discrete color palette
  theme(legend.position = "") # hide legend

# for girls 
ggplot(data = top_5_girls_2017)+
  aes(x = reorder(name, n), #note this reorders the x variable from small to large n
      y = percent, # you can use prop if you didn't make a percent variable
      fill = name)+ # optional color!
  geom_col()+
  # all of the above is sufficient, now I'm just making it pretty
  scale_y_continuous(labels = function(x){paste0(x, "%")}, # add percent signs
                     breaks = seq(from = 0, # make line breaks every 0.25%
                                  to = 1.25,
                                  by = 0.25),
                     limits = c(0,1.3), # limit axis to between 0 and 1.2
                     expand = c(0,0))+ # don't let it go beyond this
  labs(x = "Name",
       y = "Percent of All Girls With Name",
       title = "Most Popular Girls Names Since 1880",
       fill = "Girl's Name",
       caption = "Source: SSA")+
  ggthemes::theme_pander(base_family = "Fira Sans Condensed", base_size=16)+
  coord_flip()+ # flip axes to make horizontal!
  scale_fill_viridis_d(option = "default")+ # use viridis discrete color palette
  theme(legend.position = "") # hide legend

# 3 -----------------

babynames %>%
  filter(name == "Ryan") %>%
  count(sex, wt=n) %>%
  mutate(percent = round((n/sum(n)*100),2))

# 4 -----------------

# note here I'm going to wrangle the data and then pipe it directly into ggplot
# you can wrangle the data and save it as a different tibble, then use THAT tibble
# for your (data = ...) command in ggplot

# first wrangle data
babynames %>%
  filter(name == "Ryan") %>%
  
  # now we pipe into ggplot
  ggplot(data = .)+ # the "." is a placeholder for the stuff above!
  aes(x = year,
      y = n,
      color = sex)+
  geom_line(size=1)+
  scale_color_manual(values = c("F" = "#e64173", # make my own colors
                                "M" = "#0047AB"))+
  labs(x = "Year",
       y = "Number of Babies",
       title = "Popularity of Babies Named 'Ryan'",
       color = "Sex",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 5 -----------------

# 5-a -----------------

babynames %>%
  group_by(year) %>% # we want one observation per year
  filter(sex == "M",
         year>1979) %>% # or >==1980
  arrange(desc(n))%>% # start with largest n first
  slice(1) # take first row only

# 5-b -----------------

babynames %>%
  group_by(year) %>% # we want one observation per year
  filter(sex == "F",
         year>1979) %>% # or >==1980
  arrange(desc(n))%>% # start with largest n first
  slice(1) # take first row only


# 6 ----------------

# 6-a -----------------

# for boys
babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="M") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5) 

# make a vector of the names (we'll need this for our graph below)
top_boys_names<-c("James", "John", "Robert", "Michael", "William")

# you could alternatively add a command, 
# %>% pull(name) to the first chunk of code, 
# and it would do the same thing, but we'd want to save it, 
# for example:

babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="M") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5) %>%
  pull(name)

# for girls

babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="F") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5)

# make a vector of the names (we'll need this for our graph below)
top_girls_names<-c("Mary", "Elizabeth", "Patricia", "Jennifer", "Linda")

# you could alternatively add a command, %>% pull(name) to the first chunk of code, and it would do the same thing, but we'd want to save it

# b -----------------

# for boys

babynames %>%
  group_by(year) %>%
  filter(sex == "M",
         name %in% top_boys_names) %>%
  ggplot(data = .,
         aes(x = year,
             y = prop,
             color = name))+
  geom_line(size = 1)+
  labs(x = "Year",
       y = "Proportion of Babies with Name",
       title = "Most Popular Boys Names Since 1880",
       color = "Boy's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# for girls
babynames %>%
  group_by(year) %>%
  filter(sex == "F",
         name %in% top_girls_names) %>%
  ggplot(data = .,
         aes(x = year,
             y = prop,
             color = name))+
  geom_line(size = 1)+
  labs(x = "Year",
       y = "Proportion of Babies with Name",
       title = "Most Popular Girls Names Since 1880",
       color = "Girl's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 7 (BONUS) ----------------

# there's a whole lot to this one, I suggest you read the full answer key

babynames %>%
  mutate(male = ifelse(sex == "M", n, 0),
         female = ifelse(sex == "F", n, 0)) %>%
  group_by(name) %>%
  summarize(Male = sum(male),
            Female = sum(female))%>%
  mutate(perc_male = (Male/(Male+Female)*100)) %>%
  arrange(perc_male) %>%
  filter(perc_male > 48,
         perc_male < 52) %>%
  mutate(total = Male+Female) %>%
  arrange(desc(total)) %>%
  slice(1:10)

# 8 ---------------

# import data with read_csv from readr

# note these file paths assume you have a folder called "data" in your working directory

# if you used an R project and did just that (or downloaded my R Project from the website)
# then you already have this done

econ<-read_csv("data/econ_freedom.csv")
pol<-read_csv("data/pol_freedom.csv")

# look at each dataframe
econ
pol

# 9 ---------------

freedom <- left_join(econ, pol, by=c("country", "year"))
freedom

# 10 --------------

## 10-a ------------

# grab the top 10 countries by ef in 2018
ef_10<-freedom %>%
  filter(year == 2018) %>%
  arrange(desc(ef_index)) %>%
  slice(1:10)

# look at it just to check
ef_10

# now plot it
ggplot(data = ef_10)+
  aes(x = fct_reorder(ISO, desc(ef_index)), # reorder ISO by ef in order
      y = ef_index)+
  geom_col(aes(fill = continent))+ # coloring is optional
  # above is sufficient, now let's just make it prettier
  geom_text(aes(label = ef_index), # add the score onto the bar
            vjust = 1.2, # adjust it vertically
            color = "white"
  )+
  scale_y_continuous(breaks = seq(0,10,2),
                     limits = c(0,10),
                     expand = c(0,0)
  )+
  labs(x = "Country",
       y = "Economic Freedom Score",
       title = "Top 10 Countries by Economic Freedom",
       caption = "Source: Frasier Institute",
       fill = "Continent")+
  theme_minimal(base_family = "Fira Sans Condensed")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = rel(1.5))
  )

## 10-b ------------

# grab the top 10 countries by fh in 2018
pf_10<-freedom %>%
  filter(year == 2018) %>%
  arrange(desc(fh_score)) %>%
  slice(1:10)

# look at it just to check
pf_10

# now plot it
ggplot(data = pf_10)+
  aes(x = fct_reorder(ISO, desc(fh_score)),
      y = fh_score)+
  geom_col(aes(fill = continent))+ # coloring is optional
  # above is sufficient, now let's just make it prettier
  geom_text(aes(label = fh_score), # add the score onto the bar
            vjust = 1.2, # adjust it vertically
            color = "white")+
  scale_y_continuous(breaks = seq(0,100,20),
                     limits = c(0,100),
                     expand = c(0,0))+
  labs(x = "Country",
       y = "Political Freedom Score",
       title = "Top 10 Countries by Political Freedom",
       caption = "Source: Freedom House",
       fill = "Continent")+
  theme_minimal(base_family = "Fira Sans Condensed")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = rel(1.5))
  )
# 11 --------------

ggplot(data = freedom)+
  aes(x = ef_index,
      y = fh_score)+
  # doing just geom_point() is fine, but since there's a lot of overlap, here are some things I like to do:
  geom_point(aes(fill = continent), # fill the points with color by continent
             alpha = 0.9, # make points slightly transparent
             color = "white", # outline the points with a white border
             pch = 21, # this shape has an outline and a fill color
             size = 3)+
  scale_x_continuous(breaks = seq(0,10,2),
                     limits = c(0,10),
                     expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,100,20),
                     limits = c(0,105),
                     expand = c(0,0))+
  labs(x = "Economic Freedom Score",
       y = "Political Freedom Score",
       caption = "Sources: Frasier Institute, Freedom House",
       title = "Economic Freedom & Political Freedom",
       fill = "Continent")+
  theme_minimal(base_family = "Fira Sans Condensed")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = rel(1.5))
  )

# Note, I meant to ask you to look at one year only, e.g. 2018. We would just have to filter first:

p <- freedom %>%
  filter(year == "2018") %>%
  ggplot(data = .)+
  aes(x = ef_index,
      y = fh_score)+
  geom_point(aes(fill = continent),
             alpha = 0.9, 
             color = "white",
             pch = 21, 
             size = 3)+
  scale_x_continuous(breaks = seq(0,10,2),
                     limits = c(0,10),
                     expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,100,20),
                     limits = c(0,105),
                     expand = c(0,0))+
  labs(x = "Economic Freedom Score",
       y = "Political Freedom Score",
       caption = "Sources: Frasier Institute, Freedom House",
       title = "Economic Freedom & Political Freedom",
       fill = "Continent")+
  theme_minimal(base_family = "Fira Sans Condensed")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = rel(1.5))
  )

# look at it
p

# 12 --------------

library(ggrepel)

# pick some countries
some_countries <- freedom %>%
  filter(year==2018,
         country %in% c("United States",
                        "United Kingdom",
                        "Sweden",
                        "China",
                        "Singapore",
                        "Russian Federation",
                        "Korea, Rep.",
                        "Hong Kong SAR, China"))
# add layer 
p + geom_label_repel(data = some_countries, # or whatever object name you created
                     aes(x = ef_index,
                         y = fh_score,
                         label = ISO, # show ISO as label (you could do country instead)
                         color = continent),
                     alpha = 0.75, # make it a bit transparent
                     box.padding = 0.75, # control how far labels are from points
                     show.legend = F) # don't want this to add to the legend

# 13 --------------

# save plot as us
us<-freedom %>%
  filter(ISO == "USA") %>%
  ggplot(data = .)+
  aes(x = ef_index,
      y = fh_score)+
  geom_point(color = "red")+
  geom_path(color = "red")+
  geom_text_repel(aes(label = year),
                  color = "red")+
  scale_x_continuous(breaks = seq(8,8.5,0.05),
                     limits = c(8,8.5),
                     expand = c(0,0))+
  scale_y_continuous(breaks = seq(85,95,1),
                     limits = c(85,95),
                     expand = c(0,0))+
  labs(x = "Economic Freedom Score",
       y = "Political Freedom Score",
       caption = "Sources: Frasier Institute, Freedom House",
       title = "U.S. Political & Economic Freedom, 2013—2018",
       fill = "Continent")+
  theme_minimal(base_family = "Fira Sans Condensed")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = rel(1.5))
  )
# look at it
us

# rescale
us+
  scale_x_continuous(breaks = seq(0,10,1),
                     limits = c(0,10),
                     expand = c(0,0)
  )+
  scale_y_continuous(breaks = seq(0,100,10),
                     limits = c(0,100),
                     expand = c(0,0)
  )