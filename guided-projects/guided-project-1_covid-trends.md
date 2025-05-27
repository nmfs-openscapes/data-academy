# COVID-19 Trends


This is a guided project for dataquest, investigating COVID-19 trends.

## 2. Understanding the data

``` r
library(readr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

Here I’m reading the csv directly from the URL provided; you can
download the file and read it locally as well.

I specify `show_col_types = FALSE` to suppress the verbose messages
showing all of the column types it guessed. It’s often useful to see
this information at least the first time you read in a dataset to
confirm it is treating the columns as you would expect.

``` r
covid_df <- read_csv(
  "https://dq-content.s3.amazonaws.com/505/covid19.csv",
  show_col_types = FALSE
)
```

Here we use `dim()` to show the number of rows and columns, then
`colnames()` to get the column names. They are stored in the object
called `vector_cols`. By running the object `vector_cols` on its own it
prints the contents of that object (the column names).

``` r
dim(covid_df)
```

    [1] 10903    14

``` r
vector_cols <- colnames(covid_df)
vector_cols
```

     [1] "Date"                    "Continent_Name"         
     [3] "Two_Letter_Country_Code" "Country_Region"         
     [5] "Province_State"          "positive"               
     [7] "hospitalized"            "recovered"              
     [9] "death"                   "total_tested"           
    [11] "active"                  "hospitalizedCurr"       
    [13] "daily_tested"            "daily_positive"         

`vector_cols` represents a *character vector*

``` r
head(covid_df)
```

    # A tibble: 6 × 14
      Date       Continent_Name Two_Letter_Country_C…¹ Country_Region Province_State
      <date>     <chr>          <chr>                  <chr>          <chr>         
    1 2020-01-20 Asia           KR                     South Korea    All States    
    2 2020-01-22 North America  US                     United States  All States    
    3 2020-01-22 North America  US                     United States  Washington    
    4 2020-01-23 North America  US                     United States  All States    
    5 2020-01-23 North America  US                     United States  Washington    
    6 2020-01-24 Asia           KR                     South Korea    All States    
    # ℹ abbreviated name: ¹​Two_Letter_Country_Code
    # ℹ 9 more variables: positive <dbl>, hospitalized <dbl>, recovered <dbl>,
    #   death <dbl>, total_tested <dbl>, active <dbl>, hospitalizedCurr <dbl>,
    #   daily_tested <dbl>, daily_positive <dbl>

``` r
glimpse(covid_df)
```

    Rows: 10,903
    Columns: 14
    $ Date                    <date> 2020-01-20, 2020-01-22, 2020-01-22, 2020-01-2…
    $ Continent_Name          <chr> "Asia", "North America", "North America", "Nor…
    $ Two_Letter_Country_Code <chr> "KR", "US", "US", "US", "US", "KR", "US", "US"…
    $ Country_Region          <chr> "South Korea", "United States", "United States…
    $ Province_State          <chr> "All States", "All States", "Washington", "All…
    $ positive                <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 4, 0, 3, 0, 0, 0, 0, 1…
    $ hospitalized            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ recovered               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ death                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ total_tested            <dbl> 4, 1, 1, 1, 1, 27, 1, 1, 0, 0, 0, 0, 0, 0, 0, …
    $ active                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ hospitalizedCurr        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ daily_tested            <dbl> 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ daily_positive          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

`head()` shows you the first `n` rows (where `n` = 6 by default, but you
can change it).

`glimpse()` gives you a preview of all of the columns and their types

## 3. Isolating the rows we need

Here we use `filter()` to only keep the rows where the value in the
`Province_State` is equal to `"All States"`. Then we use `select()` and
specify `-Province_state` - the `-` sign *excludes* that column and
keeps the rest.

``` r
covid_df_all_states <- covid_df |>
  filter(Province_State == "All States") |>
  select(-Province_State)
```

## 4. Isolating the columns we need

Here we use `select()` again, but this time specify the columns we want
to keep.

``` r
covid_df_all_states_daily <- covid_df_all_states |>
  select(
    Date,
    Country_Region,
    active,
    hospitalizedCurr,
    daily_tested,
    daily_positive
  )
```

## 5. Extracting the Top Ten Countries with Most Covid-19 Cases

Using `group_by()` + `summarize()` is a very powerful and common way to
aggregate values for different levels of a variable. Here we
`group_by(Country_Region)` and then for each value (level) of
`Country_Region` we calculate the sum of several numeric variables.

Then we sort using `arrange()`, and specify `desc(tested)` to sort by
`tested` in descending order. This can also be done with
`arrange(-tested)`.

``` r
covid_df_all_states_daily_sum <- covid_df_all_states_daily |>
  group_by(Country_Region) |>
  summarise(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) |>
  arrange(desc(tested))

covid_df_all_states_daily_sum
```

    # A tibble: 108 × 5
       Country_Region   tested positive  active hospitalized
       <chr>             <dbl>    <dbl>   <dbl>        <dbl>
     1 United States  17282363  1877179       0            0
     2 Russia         10542266   406368 6924890            0
     3 Italy           4091291   251710 6202214      1699003
     4 India           3692851    60959       0            0
     5 Turkey          2031192   163941 2980960            0
     6 Canada          1654779    90873   56454            0
     7 United Kingdom  1473672   166909       0            0
     8 Australia       1252900     7200  134586         6655
     9 Peru             976790    59497       0            0
    10 Poland           928256    23987  538203            0
    # ℹ 98 more rows

We can then use `head()` with `n = 10` on the sorted data frame to get
the top 10.

``` r
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
```

## 6. Identifying the Highest Positive Against Tested Cases

Extract the different columns into standalone vectors

``` r
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized
```

Assign the vectors of values (cases) the names of the countries, using
the `countries` character vector.

``` r
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
```

Divide positive cases by tested to get positivity rates - look at these
manually, and subset the two vectors by those country names

``` r
positive_cases / tested_cases
```

     United States         Russia          Italy          India         Turkey 
       0.108618191    0.038546552    0.061523368    0.016507300    0.080711720 
            Canada United Kingdom      Australia           Peru         Poland 
       0.054915490    0.113260617    0.005746668    0.060910738    0.025840932 

``` r
positive_tested_top_3 <- positive_cases[c(
  "United Kingdom",
  "United States",
  "Turkey"
)] /
  tested_cases[c("United Kingdom", "United States", "Turkey")]

positive_tested_top_3
```

    United Kingdom  United States         Turkey 
        0.11326062     0.10861819     0.08071172 

### 6a. An alternative using vectors

``` r
positivity <- sort(positive_cases / tested_cases, decreasing = TRUE)

positive_tested_top_3 <- positivity[1:3]
```

### 6b. An alternative using data frames

The exercise is about testing your understanding of using and
manipulating vectors, so the answers above do that well. However if I
was doing this in real life I would keep the data frames and use dplyr
to find what I want:

``` r
covid_top_10 |>
  mutate(positivity = positive / tested) |>
  arrange(desc(positivity)) |>
  slice_head(n = 3)
```

    # A tibble: 3 × 6
      Country_Region   tested positive  active hospitalized positivity
      <chr>             <dbl>    <dbl>   <dbl>        <dbl>      <dbl>
    1 United Kingdom  1473672   166909       0            0     0.113 
    2 United States  17282363  1877179       0            0     0.109 
    3 Turkey          2031192   163941 2980960            0     0.0807

``` r
## OR

covid_top_10 |>
  mutate(positivity = positive / tested) |>
  slice_max(positivity, n = 3)
```

    # A tibble: 3 × 6
      Country_Region   tested positive  active hospitalized positivity
      <chr>             <dbl>    <dbl>   <dbl>        <dbl>      <dbl>
    1 United Kingdom  1473672   166909       0            0     0.113 
    2 United States  17282363  1877179       0            0     0.109 
    3 Turkey          2031192   163941 2980960            0     0.0807

## 7. Keeping relevant information

``` r
united_kingodm <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
```

``` r
covid_mat <- rbind(united_kingodm, united_states, turkey)

colnames(covid_mat) <- c(
  "Ratio",
  "tested",
  "positive",
  "active",
  "hospitalized"
)

covid_mat
```

                   Ratio   tested positive  active hospitalized
    united_kingodm  0.11  1473672   166909       0            0
    united_states   0.10 17282363  1877179       0            0
    turkey          0.08  2031192   163941 2980960            0

## 8. Putting it all together: Lists

``` r
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

question
```

    [1] "Which countries have had the highest number of positive cases against the number of tests?"

``` r
answer
```

    Positive tested cases.United Kingdom  Positive tested cases.United States 
                              0.11326062                           0.10861819 
            Positive tested cases.Turkey 
                              0.08071172 

*Today I learned that assigning a name to an already named vector using
`c()` concatenates the new name with the old names, separated by a `.`!*

``` r
data_structure_list <- list(
  dataframes = list(
    covid_df,
    covid_df_all_states,
    covid_df_all_states_daily,
    covid_top_10
  ),
  matrices = list(covid_mat),
  vectors = list(vector_cols, countries)
)
```

``` r
covid_analysis_list <- list(
  question,
  answer,
  data_structure_list
)

covid_analysis_list[[2]]
```

    Positive tested cases.United Kingdom  Positive tested cases.United States 
                              0.11326062                           0.10861819 
            Positive tested cases.Turkey 
                              0.08071172 
