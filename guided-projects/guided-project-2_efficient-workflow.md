# Creating An Efficient Data Analysis Workflow


This is a guided project for dataquest, investigating COVID-19 trends.

## 2. Getting Familiar with the Data

``` r
library(readr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

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

### 1.

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

### 2.

The column names are: `cat(colnames(book_reviews), sep = ", ")`.

They represent the book title, a one-word review, the state (whether
this represents where the review was written, or where the book was
published, or something else is unknown), and the price.

### 3.

The columns are all of type `character`, except for `price` which is
`double`.

### 4.

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

## 3. Handling Missing Data

### 1.

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

### 2.

``` r
book_reviews_full <- book_reviews |>
  filter(!is.na(review))

dim(book_reviews_full)
```

    [1] 1794    4

### 3.

We removed `nrow(book_reviews) - nrow(book_reviews_full)` rows that had
missing values in the `review` column.

### 4.

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

### 5.

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

### 6.

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
