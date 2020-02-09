Data analysis assignment 2
================
Helen Clara Retzlaff, student ID: 680035432
05.02.2020

In this assignment you will work with relational data, i.e. data coming
from different data tables that you can combine using keys. Please read
ch.13 from R for Data Science before completing this assignment –
<https://r4ds.had.co.nz/relational-data.html>.

## Read data

We will work with three different tables: household roster from wave 8
(*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and
household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("/Users/helenretzlaff/Documents/Data 3/datan3_2020/data/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("/Users/helenretzlaff/Documents/Data 3/datan3_2020/data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("/Users/helenretzlaff/Documents/Data 3/datan3_2020/data/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

## Filter household roster data (10 points)

The **egoalt8** data table contains data on the kin and other
relationships between people in the same household. In each row in this
table you will have a pair of individuals in the same household: ego
(identified by *pidp*) and alter (identified by *apidp*).
*h\_relationship\_dv* shows the type of relationship between ego and
alter. You can check the codes in the Understanding Society codebooks
here –
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and
wives or cohabiting partners (codes 1 and 2). For convenience, we also
want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household
identifier), *h\_relationship\_dv*, *h\_esex* (ego’s sex), and *h\_asex*
(alter’s sex).

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv==1|h_relationship_dv==2) %>%
        select(pidp,apidp,h_hidp,h_relationship_dv,h_sex,h_asex)

#note: there is no variable called h_esex in the data set so I used h_sex
```

Each couple now appears in the data twice: 1) with one partner as ego
and the other as alter, 2) the other way round. Now we will only focus
on heterosexual couples, and keep one observation per couple with women
as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%
        # filter out same-sex couples
        filter(h_sex!=1&h_asex==1|h_sex!=2&h_asex==2) %>%
        # keep only one observation per couple with women as egos
        filter(h_sex==2)
```

## Recode data on ethnicity (10 points)

In this assignment we will explore ethnic endogamy, i.e. marriages and
partnerships within the same ethnic group. First, let us a create a
version of the table with stable individual characteristics with two
variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp,racel_dv)
```

Let’s code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable
with the following values: “White” (codes 1 to 4) and “non-White” (all
other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = case_when(
                between(racel_dv,1,4)~"White",
                between(racel_dv,5,97)~"non-White"
        ))
```

## Join data (30 points)

Now we want to join data from the household roster (*Hetero8*) and the
data table with ethnicity (*Stable2*). First let us merge in the data on
ego’s ethnicity. We want to keep all the observations we have in
*Hetero8*, but we don’t want to add any other individuals from
*Stable2*.

``` r
JoinedEthn <- Hetero8 %>%
        left_join(Stable2,by="pidp")
```

Let us rename the variables for ethnicity to clearly indicate that they
refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter’s ethnicity. Note that in this
case the key variables have different names in two data tables; please
refer to the documentation for your join function (or the relevant
section from R for Data Science) to check the solution for this problem.

``` r
JoinedEthn <- JoinedEthn %>%
        inner_join(Stable2,by=c("apidp"="pidp"))
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

Let us start by looking at the joint distribution of race (White
vs. non-White) of both partners.

``` r
TableRace <- JoinedEthn %>%
        # filter out observations with missing data
        filter(complete.cases(.)) %>%
        count(egoRace,alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have
a White partner, 2) for a White woman to have a non-White partner, 3)
for a non-White woman to have a White partner, 4) for a non-White woman
to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the
code will not be reproducible: if the data change the code will need to
be changed, too. Your task is to write reproducible code producing a
table with the required four probabilities.

``` r
TableRace %>%
        # group by ego's race to calculate sums
        group_by(egoRace) %>%
        # create a new variable with the total number of women by race
        mutate(total=sum(n)) %>% 
        # create a new variable with the required probabilities 
        mutate(prob = n / total)
```

    ## # A tibble: 4 x 5
    ## # Groups:   egoRace [2]
    ##   egoRace   alterRace     n total   prob
    ##   <chr>     <chr>     <int> <int>  <dbl>
    ## 1 non-White non-White  1790  2116 0.846 
    ## 2 non-White White       326  2116 0.154 
    ## 3 White     non-White   266  9960 0.0267
    ## 4 White     White      9694  9960 0.973

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

1)  Join the individual-level file with the household-level data from
    wave 8 (specifically, we want the variable for the number of
    children in the household).
2)  Select only couples that are ethnically endogamous (i.e. partners
    come from the same ethnic group) for the following groups: White
    British, Indian, and Pakistani.
3)  Produce a table showing the mean and median number of children in
    these households by ethnic group (make sure the table has meaningful
    labels for ethnic groups, not just numerical codes).
4)  Write a short interpretation of your results. What could affect your
    findings?

<!-- end list -->

``` r
#1.
Hh8<-Hh8 %>% 
        select(h_hidp,h_nkids_dv)

joined<-JoinedEthn %>%
        left_join(Hh8,by="h_hidp")

head(joined)
```

    ## # A tibble: 6 x 11
    ##     pidp  apidp h_hidp h_relationship_… h_sex h_asex egoRacel_dv egoRace
    ##    <dbl>  <dbl>  <dbl>            <dbl> <dbl>  <dbl>       <dbl> <chr>  
    ## 1 7.62e4 1.42e8 1.42e8                1     2      1           1 White  
    ## 2 2.80e5 7.56e8 7.56e8                1     2      1           1 White  
    ## 3 2.27e6 8.25e8 8.25e8                2     2      1           1 White  
    ## 4 2.33e6 2.33e6 4.16e8                1     2      1          NA <NA>   
    ## 5 3.49e6 3.49e6 4.17e8                1     2      1          NA <NA>   
    ## 6 6.80e7 6.80e7 6.80e7                1     2      1           1 White  
    ## # … with 3 more variables: alterRacel_dv <dbl>, alterRace <chr>,
    ## #   h_nkids_dv <dbl>

``` r
#2.

#1 is White British
#9 is Indian
#10 is Pakistani 

eth.endo<-joined %>%
        filter(egoRacel_dv==1&alterRacel_dv==1|egoRacel_dv==9&alterRacel_dv==9|egoRacel_dv==10&alterRacel_dv==10)

head(eth.endo)
```

    ## # A tibble: 6 x 11
    ##     pidp  apidp h_hidp h_relationship_… h_sex h_asex egoRacel_dv egoRace
    ##    <dbl>  <dbl>  <dbl>            <dbl> <dbl>  <dbl>       <dbl> <chr>  
    ## 1 7.62e4 1.42e8 1.42e8                1     2      1           1 White  
    ## 2 2.80e5 7.56e8 7.56e8                1     2      1           1 White  
    ## 3 6.80e7 6.80e7 6.80e7                1     2      1           1 White  
    ## 4 6.80e7 6.81e7 6.80e7                2     2      1           1 White  
    ## 5 6.80e7 6.81e7 6.81e7                2     2      1           1 White  
    ## 6 6.80e7 6.80e7 6.81e7                1     2      1           1 White  
    ## # … with 3 more variables: alterRacel_dv <dbl>, alterRace <chr>,
    ## #   h_nkids_dv <dbl>

``` r
#3. 

table.eth<-eth.endo %>% 
        group_by(egoRacel_dv) %>% 
        summarise(mean=mean(h_nkids_dv,na.rm = TRUE),median=median(h_nkids_dv,na.rm = TRUE))

table.eth$egoRacel_dv <- factor(table.eth$egoRacel_dv,
levels = c(1,9,10),
labels = c("White British", "Indian", "Pakistani"))

table.eth
```

    ## # A tibble: 3 x 3
    ##   egoRacel_dv    mean median
    ##   <fct>         <dbl>  <dbl>
    ## 1 White British 0.565      0
    ## 2 Indian        0.955      1
    ## 3 Pakistani     1.81       2

4.  Interpretation

White British households have the lowest number of children, namely 0.56
on average (one obviously cannot have “half a child” so, looking at the
median, these households usually have 0 children). Indian households
have an average of 0.95 (or 1) child and Pakistanis have the highest
number of children per household: nearly 2 on average. These findings
indicate quite stark differences between the number of children
different ethnic groups get. Common wisedom refers to cultural
differences and higher fertility rates for minorities. Minority
populations are also younger than Whites, so the findings would reflect
demographic trends. Another factor that could influence these results is
that multiple Indian or Pakistani families live together in a “big
household”.
