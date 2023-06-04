Group Project_Jose
================

## Open data file

I’m going to load the data I saved after the last exercise.

``` r
applications <- read_feather("C:/Users/jmqui/McGill University/Jose_Group - People Analytics/app_data_starter.feather")

# lists all variables (columns) in the data
applications |> tbl_vars()
```

    ## <dplyr:::vars>
    ##  [1] "application_number"   "filing_date"          "examiner_name_last"  
    ##  [4] "examiner_name_first"  "examiner_name_middle" "examiner_id"         
    ##  [7] "examiner_art_unit"    "uspc_class"           "uspc_subclass"       
    ## [10] "patent_number"        "patent_issue_date"    "abandon_date"        
    ## [13] "disposal_type"        "appl_status_code"     "appl_status_date"    
    ## [16] "tc"                   "gender"               "race"                
    ## [19] "earliest_date"        "latest_date"          "tenure_days"

``` r
library(tidyverse)

# Assuming your data frame is named "applications"
applications <- applications %>%
  mutate(WG = substr(examiner_art_unit, 1, 3))
# Print the table with the new column
print(applications)
```

    ## # A tibble: 2,018,477 × 22
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 18 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, WG <chr>

``` r
# The "WG" column now contains the first three digits of "examiner_art_unit"
```

## Excel Excercise

For each TC let’s count the number of individuals in each gender
category

``` r
GroupProject<-applications %>%
  filter(disposal_type %in% c("ISS", "ABN")) %>%
  select(filing_date, appl_status_date,tenure_days, appl_status_code,disposal_type,gender, race, WG, examiner_art_unit, examiner_id, tc) %>%
  mutate(Status_Date = as.Date(appl_status_date, format = "%d%b%Y %H:%M:%S")) %>%
  mutate(Processing_Days = as.numeric(Status_Date - filing_date))
  # mutate(Processing_Speed = ifelse(Processing_Days <= 2000, "Fast",ifelse(Processing_Days <= 4000, "Regular", "Slow")))
```

``` r
dataset <- GroupProject %>%
  # changed variable name 
  rename(id=examiner_id, status_date = Status_Date, processing_days = Processing_Days, wg = WG, art_unit = examiner_art_unit) %>%
  # created a new table with selected variables
  select(id, gender, race, disposal_type, status_date, filing_date, processing_days, tc, wg, art_unit, tenure_days ) %>%
  # drop all NAs for gender 
  drop_na(gender)%>%
  drop_na(processing_days)
```

``` r
average_gender_matrix_organization <- dataset %>%
  group_by(disposal_type, gender) %>%
  summarize(average_processing_days_gender = mean(processing_days, na.rm = TRUE)) %>%
  pivot_wider(names_from = disposal_type, values_from = average_processing_days_gender)
```

    ## `summarise()` has grouped output by 'disposal_type'. You can override using the
    ## `.groups` argument.

``` r
# Convert the matrix to a long format
average_gender_matrix_organization_long <- average_gender_matrix_organization %>%
  gather(disposal_type, average_processing_days_gender, -gender)

# Create the bar chart
bar_chart <- ggplot(average_gender_matrix_organization_long, aes(x = gender, y = average_processing_days_gender, fill = disposal_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Processing Time by Gender Across Organization",
       x = "Gender",
       y = "Average Processing Days",
       fill = "Disposal Type")

bar_chart
```

![](Group-Project_Jose_files/figure-gfm/processing%20time%20of%20female%20and%20male%20in%20organizaion%20level%20by%20histogram-1.png)<!-- -->

``` r
average_race_matrix_organization <- dataset %>%
  group_by(disposal_type, race) %>%
  summarize(average_processing_days_race = mean(processing_days, na.rm = TRUE)) %>%
  pivot_wider(names_from = disposal_type, values_from = average_processing_days_race)
```

    ## `summarise()` has grouped output by 'disposal_type'. You can override using the
    ## `.groups` argument.

``` r
# Convert the matrix to a long format
average_race_matrix_organization_long <- average_race_matrix_organization %>%
  gather(disposal_type, average_processing_days_race, -race)

# Create the bar chart
bar_chart_2 <- ggplot(average_race_matrix_organization_long, aes(x = race, y = average_processing_days_race, fill = disposal_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Processing Time by Race Across Organization",
       x = "Race",
       y = "Average Processing Days",
       fill = "Disposal Type")

bar_chart_2
```

![](Group-Project_Jose_files/figure-gfm/processing%20time%20of%20race%20in%20organization%20level%20by%20histogram-1.png)<!-- -->
\`\`\`

# `{r gender distribution} #  # applications %>%  #    mutate(Processing_Speed = case_when(Processing_Days <= 2000 ~ "Fast",Processing_Days > 2000 & Processing_Days <= 4000 ~ "Regular",Processing_Days > 4000 ~ "Slow")) #`
