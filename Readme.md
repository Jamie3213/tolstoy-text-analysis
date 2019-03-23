The Kingdom of God Is Within You
================
Jamie Hargreaves
23 March, 2019

<style>
body{
  font-family: Lato;
  font-size: 12pt;
}
</style>
Setting up
----------

#### Loading libraries

We'll start by loading the relevant libraries and importing the data: we'll be using tidyverse for packages such as dplyr and stringr, tidytext as the basis for our text mining, magrittr for its pipes (mainly `%<>%`, since `%>%` is already loaded with tidyverse), and gutenbergr for our book.

``` r
library(tidyverse)
library(tidytext)
library(magrittr)
library(gutenbergr)
```

#### Importing our data

We'll be using [Project Gutenberg](http://www.gutenberg.org/) as the source of our text data. We can see the list of books available from Project Gutenberg using the gutenbergr package:

``` r
gutenberg_metadata
```

    ## # A tibble: 51,997 x 8
    ##    gutenberg_id title author gutenberg_autho… language gutenberg_books…
    ##           <int> <chr> <chr>             <int> <chr>    <chr>           
    ##  1            0 <NA>  <NA>                 NA en       <NA>            
    ##  2            1 The … Jeffe…             1638 en       United States L…
    ##  3            2 "The… Unite…                1 en       American Revolu…
    ##  4            3 John… Kenne…             1666 en       <NA>            
    ##  5            4 "Lin… Linco…                3 en       US Civil War    
    ##  6            5 The … Unite…                1 en       American Revolu…
    ##  7            6 Give… Henry…                4 en       American Revolu…
    ##  8            7 The … <NA>                 NA en       <NA>            
    ##  9            8 Abra… Linco…                3 en       US Civil War    
    ## 10            9 Abra… Linco…                3 en       US Civil War    
    ## # … with 51,987 more rows, and 2 more variables: rights <chr>,
    ## #   has_text <lgl>

We can use dplyr and stringr to look for any books similar to our title, since there might be multiple versions available from Project Gutenberg:

``` r
gutenberg_metadata %>%
  filter(str_detect(str_to_lower(title), "kingdom of god")) %>%
  select(title, gutenberg_id)
```

    ## # A tibble: 5 x 2
    ##   title                                                        gutenberg_id
    ##   <chr>                                                               <int>
    ## 1 "\"The Kingdom of God Is Within You\"\r\nChristianity Not a…         4602
    ## 2 "The Kingdom of God is Within You\nChristianity Not as a My…        43302
    ## 3 The Kingdom of God is Within You / Christianity and Patriot…        43372
    ## 4 The Kingdom of God is Within You, What is Art                       43409
    ## 5 The Kingdom of God, Part 1                                          46244

We want the second book in the list, so we'll pass its ID to the `gutenberg_download` function:

``` r
book <- gutenberg_download(43302)
book %<>%
  select(-one_of("gutenberg_id"))
book %>%
  head()
```

    ## # A tibble: 6 x 1
    ##   text                         
    ##   <chr>                        
    ## 1 "  \"THE KINGDOM OF GOD IS"  
    ## 2 "  WITHIN YOU\""             
    ## 3 ""                           
    ## 4 "  [Illustration: titlepage]"
    ## 5 ""                           
    ## 6 "  \"THE KINGDOM OF GOD IS"
