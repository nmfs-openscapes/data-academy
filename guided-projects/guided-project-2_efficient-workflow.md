# Creating An Efficient Data Analysis Workflow


This is a guided project for dataquest, investigating COVID-19 trends.

### 2. Getting Familiar with the Data

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.0.4     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Read the csv after downloading it and saving it in the working
directory.

I specify `show_col_types = FALSE` to suppress the verbose messages
showing all of the column types it guessed. It’s often useful to see
this information at least the first time you read in a dataset to
confirm it is treating the columns as you would expect.

``` r
book_reviews <- read_csv(
  "book_reviews.csv",
  show_col_types = FALSE
)
```

#### 1.

``` r
glimpse(book_reviews)
```

    Rows: 2,000
    Columns: 4
    $ book   <chr> "R Made Easy", "R For Dummies", "R Made Easy", "R Made Easy", "…
    $ review <chr> "Excellent", "Fair", "Excellent", "Poor", "Great", NA, "Great",…
    $ state  <chr> "TX", "NY", "NY", "FL", "Texas", "California", "Florida", "CA",…
    $ price  <dbl> 19.99, 15.99, 19.99, 19.99, 50.00, 19.99, 19.99, 19.99, 29.99, …

We see that there are `nrow(book_reviews)` rows and `ncol(book_reviews)`
columns.

#### 2.

The column names are: `cat(colnames(book_reviews), sep = ", ")`.

They represent the book title, a one-word review, the state (whether
this represents where the review was written, or where the book was
published, or something else is unknown), and the price.

#### 3.

The columns are all of type `character`, except for `price` which is
`double`.

#### 4.

``` r
for (col in colnames(book_reviews)) {
  unique_vals <- unique(book_reviews[[col]])
  cat(paste0("Unique values in ", col, ":\n"))
  print(unique_vals)
}
```

    Unique values in book:
    [1] "R Made Easy"                        "R For Dummies"                     
    [3] "Secrets Of R For Advanced Students" "Top 10 Mistakes R Beginners Make"  
    [5] "Fundamentals of R For Beginners"   
    Unique values in review:
    [1] "Excellent" "Fair"      "Poor"      "Great"     NA          "Good"     
    Unique values in state:
    [1] "TX"         "NY"         "FL"         "Texas"      "California"
    [6] "Florida"    "CA"         "New York"  
    Unique values in price:
    [1] 19.99 15.99 50.00 29.99 39.99

### 3. Handling Missing Data

#### 1.

``` r
for (col in colnames(book_reviews)) {
  missing_vals <- sum(is.na(book_reviews[[col]]))
  cat(paste0("Missing values in ", col, ": ", missing_vals, "\n"))
}
```

    Missing values in book: 0
    Missing values in review: 206
    Missing values in state: 0
    Missing values in price: 0

#### 2.

``` r
book_reviews_full <- book_reviews |>
  filter(!is.na(review))

dim(book_reviews_full)
```

    [1] 1794    4

#### 3.

We removed `nrow(book_reviews) - nrow(book_reviews_full)` rows that had
missing values in the `review` column.

#### 4.

First see the unique values so we know what we are working with:

``` r
unique(book_reviews_full$state)
```

    [1] "TX"         "NY"         "FL"         "Texas"      "Florida"   
    [6] "CA"         "California" "New York"  

Then we can standardize:

``` r
book_reviews_full <- book_reviews_full |>
  mutate(
    state = case_when(
      state == "NY" ~ "New York",
      state == "FL" ~ "Florida",
      state == "TX" ~ "Texas",
      state == "CA" ~ "California",
      .default = state
    )
  )
```

#### 5.

``` r
book_reviews <- book_reviews |>
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    ),
    is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )
```

#### 6.

Calculate “Profitability”:

``` r
book_reviews |>
  group_by(book) |>
  summarize(
    n_purchased = n(),
    total_income = sum(price),
    avg_review = mean(review_num, na.rm = TRUE),
    n_reviews = sum(!is.na(review))
  )
```

    # A tibble: 5 × 5
      book                             n_purchased total_income avg_review n_reviews
      <chr>                                  <int>        <dbl>      <dbl>     <int>
    1 Fundamentals of R For Beginners          410       16396.       3.01       366
    2 R For Dummies                            410        6556.       2.83       361
    3 R Made Easy                              389        7776.       2.97       352
    4 Secrets Of R For Advanced Stude…         406       20300        2.96       360
    5 Top 10 Mistakes R Beginners Make         385       11546.       3.05       355

“Fundamentals of R for Beginners” sold the most books, but “Secrets of R
for Advanced Students” had the highest total income. “Top 10 Mistakes R
Beginners Make” has the highest average review.

## Part 2 (Lesson 4; Specialized Data Processing)

### 1. Introduction

I downloaded the data from
https://dsserver-prod-resources-1.s3.amazonaws.com/516/sales2019.csv and
saved it in the working directory as `sales2019.csv`.

``` r
sales2019 <- read_csv("sales2019.csv", show_col_types = FALSE)
```

### 2. Data Exploration

Get the size (dimensions) and column names of the data:

``` r
dim(sales2019)
```

    [1] 5000    5

``` r
names(sales2019)
```

    [1] "date"                  "user_submitted_review" "title"                
    [4] "total_purchased"       "customer_type"        

We can get a prevew of the data and the data using `glimpse`:

``` r
glimpse(sales2019)
```

    Rows: 5,000
    Columns: 5
    $ date                  <chr> "5/22/19", "11/16/19", "6/27/19", "11/6/19", "7/…
    $ user_submitted_review <chr> "it was okay", "Awesome!", "Awesome!", "Awesome!…
    $ title                 <chr> "Secrets Of R For Advanced Students", "R For Dum…
    $ total_purchased       <dbl> 7, 3, 1, 3, NA, 1, 5, NA, 7, 1, 7, NA, 3, 2, 0, …
    $ customer_type         <chr> "Business", "Business", "Individual", "Individua…

We can see in the `glimpse` output that the `total_purchased` column
contains some missing (`NA`) values, but we can check more
comprehensively with:

``` r
library(purrr)

anyNA(sales2019)
```

    [1] TRUE

``` r
map(sales2019, anyNA)
```

    $date
    [1] FALSE

    $user_submitted_review
    [1] TRUE

    $title
    [1] FALSE

    $total_purchased
    [1] TRUE

    $customer_type
    [1] FALSE

We can also get a count of the number of missing values in each column:

``` r
count_na <- function(x) {
  sum(is.na(x))
}

map(sales2019, count_na)
```

    $date
    [1] 0

    $user_submitted_review
    [1] 885

    $title
    [1] 0

    $total_purchased
    [1] 718

    $customer_type
    [1] 0

It’s also sometimes useful just to look at the data itself. You can
click on the data frame in the Environment pane in RStudio to view it in
a spreadsheet-like format, or you can use the `View` function:

``` r
View(sales2019)
```

### 3. Handling Missing Data

#### Removing Rows with Missing Values in the `user_submitted_review` Column

``` r
sales2019_cleaned <- sales2019 %>%
  filter(!is.na(user_submitted_review))

rows_removed <- nrow(sales2019) - nrow(sales2019_cleaned)
rows_removed
```

    [1] 885

#### Fill in average for `total_purchased`

``` r
average_purchased <- mean(sales2019_cleaned$total_purchased, na.rm = TRUE)

sales2019_cleaned <- sales2019_cleaned %>%
  mutate(
    total_purchased = if_else(
      is.na(total_purchased),
      average_purchased,
      total_purchased
    )
  )

# Check if there are any remaining missing values
anyNA(sales2019_cleaned$total_purchased)
```

    [1] FALSE

#### Processing Review Data

Find positive reviews using `str_detect`:

Using `case_when()` is not necessary here, as we can directly create a
logical column indicating whether the review is positive or not.

``` r
library(stringr)

sales2019_cleaned <- sales2019_cleaned %>%
  mutate(
    positive_review = str_detect(
      user_submitted_review,
      regex(
        "(good)|(great)|(excellent)|(awesome)|(learned)",
        ignore_case = TRUE
      )
    )
  )
```

#### Comparing Book Sales Between Pre- and Post-Program Sales

USe `?strftime` to see options for different ways of specifying the date
format.

``` r
sales2019_cleaned <- sales2019_cleaned %>%
  mutate(
    date = parse_date(date, format = "%m/%d/%y"),
  )
```

``` r
program_start_date <- ymd("2019-07-01")
```

Create a new column indicating whether the sale was before or after the
program started, and then summarize the total purchases and positive
reviews before and after the program started. Note that
`positive_review` is a logical column, and when using mathematical
functions on logical values, `TRUE` is interpreted as 1, and `FALSE` is
interpreted as 0. Thus, summing `positive_review` gives the count of
positive reviews, and calculating the mean gives the proportion of
positive reviews.

``` r
sales2019_cleaned <- sales2019_cleaned |>
  mutate(
    pre_post_program = if_else(
      date < program_start_date,
      "pre-progam",
      "post-program"
    )
  )

program_summary <- sales2019_cleaned |>
  group_by(pre_post_program) |>
  summarize(
    total_purchases = n(),
    total_books_purchased = sum(total_purchased, na.rm = TRUE)
  )

program_summary
```

    # A tibble: 2 × 3
      pre_post_program total_purchases total_books_purchased
      <chr>                      <int>                 <dbl>
    1 post-program                2065                 8190.
    2 pre-progam                  2050                 8211.

#### Comparing Book Sales Within Customer Type

``` r
program_summary_by_customer_type <- sales2019_cleaned |>
  group_by(pre_post_program, customer_type) |>
  summarize(
    total_purchases = n(),
    total_books_purchased = sum(total_purchased, na.rm = TRUE)
  )
```

    `summarise()` has grouped output by 'pre_post_program'. You can override using
    the `.groups` argument.

``` r
program_summary_by_customer_type
```

    # A tibble: 4 × 4
    # Groups:   pre_post_program [2]
      pre_post_program customer_type total_purchases total_books_purchased
      <chr>            <chr>                   <int>                 <dbl>
    1 post-program     Business                 1451                 5742.
    2 post-program     Individual                614                 2448.
    3 pre-progam       Business                 1405                 5612.
    4 pre-progam       Individual                645                 2599.

#### Comparing Review Sentiment Between Pre- and Post-Program Sales

``` r
program_summary_by_customer_type <- sales2019_cleaned |>
  group_by(pre_post_program, customer_type) |>
  summarize(
    total_purchases = n(),
    total_books_purchased = sum(total_purchased, na.rm = TRUE),
    positive_reviews = sum(positive_review, na.rm = TRUE),
    prop_positive_reviews = mean(positive_review, na.rm = TRUE)
  )
```

    `summarise()` has grouped output by 'pre_post_program'. You can override using
    the `.groups` argument.

``` r
program_summary_by_customer_type
```

    # A tibble: 4 × 6
    # Groups:   pre_post_program [2]
      pre_post_program customer_type total_purchases total_books_purchased
      <chr>            <chr>                   <int>                 <dbl>
    1 post-program     Business                 1451                 5742.
    2 post-program     Individual                614                 2448.
    3 pre-progam       Business                 1405                 5612.
    4 pre-progam       Individual                645                 2599.
    # ℹ 2 more variables: positive_reviews <int>, prop_positive_reviews <dbl>
