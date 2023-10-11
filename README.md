
# Likert Survey Analysis

## Loading the libraries

``` r
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)
library(readxl)
library(stringr)
```

## Part 1

Loading the data that is in the `data` folder.

``` r
students <- read_excel("data/likert_survey.xlsx", sheet = "students") %>%
  clean_names() 

professors <- read_excel("data/likert_survey.xlsx", sheet = "professors")%>%
  clean_names()
```

## Part 2

We are now looking into the total number of answers obtained by
students, professors and if each question obtained the same number of
answers.

To answer the first two questions I am using the `tidyverse` way,
putting together the dataset, to then create long format data. This will
allow me to exploit the power of grouping and summarising.

``` r
# let's first put the two datasets together

students <- students %>% 
  mutate(respondent = "student") # create a respondent column

professors <- professors %>% 
  mutate(respondent = "professor") # create a respondent column

# We can now bind together the two tibbles

total <- bind_rows(students, professors)

# use head(total) to inspect the element

# And now we can use pivot_longer to get it in a tidy format

total <- total %>% 
  pivot_longer(names_to = "response_type",
               values_to = "value",
               cols= c(-question_id, -respondent))

#head(total)
```

After having wrangled the data to have it in tidy format, I can respond
to the first two questions.

``` r
total %>% 
  group_by(respondent) %>% 
  summarise(total_answer = sum(value)) %>% 
  ungroup() %>% 
  rename('Total Answer' = total_answer,
         Respondent = respondent) %>% 
  knitr::kable()
```

| Respondent | Total Answer |
|:-----------|-------------:|
| professor  |         3654 |
| student    |         7748 |

We are now checking if each question have the same number of answers.

``` r
total %>% 
  group_by(respondent, question_id) %>%  # we group_by respondent and question_id
  summarise(total_answer = sum(value)) %>% # we calculate the total answer per question and respondent
  ungroup() %>% # we ungroup!
  distinct(respondent, total_answer) %>%  # we keep only the unique values
  knitr::kable()
```

    ## `summarise()` has grouped output by 'respondent'. You can override using the
    ## `.groups` argument.

| respondent | total_answer |
|:-----------|-------------:|
| professor  |          115 |
| professor  |          113 |
| professor  |          112 |
| professor  |          111 |
| professor  |          103 |
| student    |          244 |
| student    |          233 |
| student    |          234 |
| student    |          239 |
| student    |          241 |
| student    |          238 |
| student    |          230 |

The answer is *No* as we can see that not all questions have been
answered the same amount of time, for both professors and students. It
means that some respondents left some questions unanswered.

## Part 3

I am now comparing the distribution of answers of the students versus
the professors.

``` r
total %>%
  ggplot(mapping = aes(x =response_type,
                       y = value,
                       fill = respondent)) +
  geom_col(position = "dodge") + 
  facet_wrap(vars(question_id)) + 
  labs(title = "Comparaison of answers for students vs professors",
       subtitle = "Each facet shows a different question",
       x="Response type", 
       y="Number of Responses",
       fill = "respondent") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90)) # tilting the text
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

From this chart, we can see that basically for each question, students
gave more answers.

## Part 4

Now I am computing the mode for the students and professors for each of
the questions.

In order to make the data nice to present in a table I select the
`response_type`, `respondent` and `question_id` and use `pivot_wider()`
so to have one line per `question_id`.

``` r
total %>%
  group_by(question_id, respondent) %>%
  slice_max(order_by = value, n = 1) %>% # we keep the top 1 value for each group
  ungroup() %>% 
  select(-value) %>% 
  pivot_wider(names_from = respondent, values_from = response_type) %>% 
  knitr::kable()
```

| question_id | professor     | student          |
|------------:|:--------------|:-----------------|
|           0 | totally_agree | totally_agree    |
|           1 | agree         | agree            |
|           2 | agree         | agree            |
|           3 | agree         | agree            |
|           4 | agree         | agree            |
|           5 | agree         | agree            |
|           6 | agree         | agree            |
|           7 | disagree      | disagree         |
|           8 | disagree      | disagree         |
|           9 | disagree      | disagree         |
|          10 | agree         | agree            |
|          11 | agree         | agree            |
|          12 | agree         | agree            |
|          13 | agree         | agree            |
|          14 | agree         | agree            |
|          15 | totally_agree | totally_disagree |
|          16 | totally_agree | totally_disagree |
|          17 | agree         | totally_disagree |
|          18 | agree         | totally_disagree |
|          19 | agree         | totally_disagree |
|          20 | totally_agree | totally_agree    |
|          21 | agree         | totally_agree    |
|          22 | totally_agree | totally_agree    |
|          23 | disagree      | totally_agree    |
|          24 | totally_agree | agree            |
|          25 | agree         | agree            |
|          26 | agree         | agree            |
|          27 | agree         | agree            |
|          28 | totally_agree | agree            |
|          29 | agree         | agree            |
|          30 | agree         | agree            |
|          31 | agree         | disagree         |
