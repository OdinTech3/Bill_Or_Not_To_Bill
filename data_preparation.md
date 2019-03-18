Data Preparation & Cleaning
================

``` r
library(readxl)
library(dplyr)
library(magrittr)
library(tm)
```

## 1\. Loading The Data

We will only load the first 100 observations

``` r
# Encode specific kinds of values as NA while reading excel
non_bill_df <- read_excel("data/december_non-bill_calls.xlsx", na = c("", "---"), n_max = 100)
billed_df <- read_excel("data/december_billed_calls.xlsx", na = c("", "---"), n_max = 100)
```

We will combine `non_bill_df` and `billed_df` into a dataframe called
`billing_df`.

``` r
billing_df <- bind_rows(non_bill_df, billed_df)

str(billing_df, nchar.max = 20, vec.len = 3)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    200 obs. of  29 variables:
    ##  $ Invoiced (Y/N)                    : chr  "N" "N" "N" ...
    ##  $ SR Number                         : chr  "1-ICUU0R5" "1-IR7JD4O" "1-IYEXL4A" ...
    ##  $ Activity Facts Call Num           : num  5.92e| __truncated__ ...
    ##  $ Br Branch Desc                    : chr  "114"| __truncated__ "114"| __truncated__ "115"| __truncated__ ...
    ##  $ SR Owner (Q#)                     : chr  "FS5-0000000034" "FS5-0000000010" "FS5-0000000010" ...
    ##  $ Billing Notes                     : chr  NA NA "1. "| __truncated__ ...
    ##  $ Call Text                         : chr  "ATM"| __truncated__ "DEC"| __truncated__ "DEC"| __truncated__ ...
    ##  $ Cash Vendor & Consumable Contracts: logi  NA NA NA NA ...
    ##  $ SR Type                           : chr  "TR - Trouble Call" "TR - Trouble Call" "TR - Trouble Call" ...
    ##  $ Coverage Type                     : chr  NA "PL "| __truncated__ "CP "| __truncated__ ...
    ##  $ SR Coverage Hours...11            : chr  NA "FK" "FK" ...
    ##  $ SR Device                         : chr  "VLT" "VAT" "VLT" ...
    ##  $ Item Desc                         : chr  NA "CVN"| __truncated__ "VAU"| __truncated__ ...
    ##  $ Activity Trouble Code             : chr  "REPAIR" "REPAIR" "PS_PRODUCT SALE" ...
    ##  $ SR Address Line 1                 : chr  "CAS"| __truncated__ "4510 KINGWOOD DR" "350"| __truncated__ ...
    ##  $ SR City                           : chr  "Cumming" "KINGWOOD" "RALEIGH" ...
    ##  $ SR State                          : chr  "GA" "TX" "NC" ...
    ##  $ SR Site                           : num  40482| __truncated__ ...
    ##  $ SR Serial Number                  : chr  NA "930506000074" "30321003411" ...
    ##  $ SR Contact Date                   : POSIXct, format: "201"| __truncated__ "201"| __truncated__ ...
    ##  $ Activity Completed Date           : POSIXct, format: "201"| __truncated__ "201"| __truncated__ ...
    ##  $ Br Region Desc                    : chr  "P24"| __truncated__ "P24"| __truncated__ "P24"| __truncated__ ...
    ##  $ Br Area Desc                      : chr  "P24"| __truncated__ "P24"| __truncated__ "P24"| __truncated__ ...
    ##  $ Activity Type                     : chr  "Field Repair" "Field Repair" "Field Repair" ...
    ##  $ SR Status                         : chr  "Closed" "Closed" "Closed" ...
    ##  $ Activity Status                   : chr  "Closed Complete" "Closed Complete" "Closed Complete" ...
    ##  $ Charges Status                    : chr  "Final" "Final" "Final" ...
    ##  $ SR Coverage Hours...28            : chr  NA "FK" "FK" ...
    ##  $ Base Call YN                      : chr  "Y" "Y" "Y" ...

Based on initial discussions and research into the meaning of some of
the features in this dataset, we have categorized the following features
as being not **important**.

    ## [1] "SR Address Line 1"       "SR City"                
    ## [3] "SR Status"               "Activity Status"        
    ## [5] "Charges Status"          "SR Coverage Hours...11" 
    ## [7] "SR Coverage Hours...28"  "Br Region Desc"         
    ## [9] "Activity Facts Call Num"

> The features `SR Coverage Hours...11` and `SR Coverage Hours...28`
> were created by R because the excel contained two columns with the
> name `SR Coverage Hours`.

The features have been stored in variable called `features_to_rm`, the
next step is to remove these 9 features from the `billing_df` dataset.
This step reduces our number of features from **29 to 20**.

``` r
billing_df <- billing_df %>% select(-features_to_rm)

str(billing_df, nchar.max = 20, vec.len = 3)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    200 obs. of  20 variables:
    ##  $ Invoiced (Y/N)                    : chr  "N" "N" "N" ...
    ##  $ SR Number                         : chr  "1-ICUU0R5" "1-IR7JD4O" "1-IYEXL4A" ...
    ##  $ Br Branch Desc                    : chr  "114"| __truncated__ "114"| __truncated__ "115"| __truncated__ ...
    ##  $ SR Owner (Q#)                     : chr  "FS5-0000000034" "FS5-0000000010" "FS5-0000000010" ...
    ##  $ Billing Notes                     : chr  NA NA "1. "| __truncated__ ...
    ##  $ Call Text                         : chr  "ATM"| __truncated__ "DEC"| __truncated__ "DEC"| __truncated__ ...
    ##  $ Cash Vendor & Consumable Contracts: logi  NA NA NA NA ...
    ##  $ SR Type                           : chr  "TR - Trouble Call" "TR - Trouble Call" "TR - Trouble Call" ...
    ##  $ Coverage Type                     : chr  NA "PL "| __truncated__ "CP "| __truncated__ ...
    ##  $ SR Device                         : chr  "VLT" "VAT" "VLT" ...
    ##  $ Item Desc                         : chr  NA "CVN"| __truncated__ "VAU"| __truncated__ ...
    ##  $ Activity Trouble Code             : chr  "REPAIR" "REPAIR" "PS_PRODUCT SALE" ...
    ##  $ SR State                          : chr  "GA" "TX" "NC" ...
    ##  $ SR Site                           : num  40482| __truncated__ ...
    ##  $ SR Serial Number                  : chr  NA "930506000074" "30321003411" ...
    ##  $ SR Contact Date                   : POSIXct, format: "201"| __truncated__ "201"| __truncated__ ...
    ##  $ Activity Completed Date           : POSIXct, format: "201"| __truncated__ "201"| __truncated__ ...
    ##  $ Br Area Desc                      : chr  "P24"| __truncated__ "P24"| __truncated__ "P24"| __truncated__ ...
    ##  $ Activity Type                     : chr  "Field Repair" "Field Repair" "Field Repair" ...
    ##  $ Base Call YN                      : chr  "Y" "Y" "Y" ...

## 2\. Cleaning The Data

### 2.1 Encoding The Variables

We can notice that R has miss categorized some of the features in our
dataset. There are certain features that are supposed to be read as
categorical such as:

    ##  [1] "Invoiced (Y/N)"                    
    ##  [2] "Activity Type"                     
    ##  [3] "Activity Trouble Code"             
    ##  [4] "Coverage Type"                     
    ##  [5] "Base Call YN"                      
    ##  [6] "SR Type"                           
    ##  [7] "SR Device"                         
    ##  [8] "SR Site"                           
    ##  [9] "SR Owner (Q#)"                     
    ## [10] "SR Serial Number"                  
    ## [11] "Cash Vendor & Consumable Contracts"

Lets encode the features in `char_to_factors` as factors

``` r
billing_df <- billing_df %>% mutate_at(char_to_factors, factor)

billing_df %>% summary()
```

    ##  Invoiced (Y/N)  SR Number         Br Branch Desc            SR Owner (Q#)
    ##  N:100          Length:200         Length:200         FS5-0000000023:78   
    ##  Y:100          Class :character   Class :character   FS5-0000000008:36   
    ##                 Mode  :character   Mode  :character   FS5-0000000017:32   
    ##                                                       FS5-0000000010:18   
    ##                                                       FS5-REGIONAL  :11   
    ##                                                       FS5-0000000015:10   
    ##                                                       (Other)       :15   
    ##  Billing Notes       Call Text         Cash Vendor & Consumable Contracts
    ##  Length:200         Length:200         NA's:200                          
    ##  Class :character   Class :character                                     
    ##  Mode  :character   Mode  :character                                     
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##                  SR Type   
    ##  FL - First Line Call: 37  
    ##  IN - BW             : 17  
    ##  PM/CL               :  5  
    ##  TR - Trouble Call   :141  
    ##                            
    ##                            
    ##                            
    ##                                             Coverage Type SR Device
    ##  CP PLAN:  Parts and Labor                         :81    MSC: 3   
    ##  SI PLAN:  Silver Coverage - ATM Products          :36    TAB:94   
    ##  PL PLAN:  Platinum Coverage - Conventional & ATM P: 9    VAT:14   
    ##  BR PLAN:  Bronze Coverage - ATM Products          : 5    VLT:89   
    ##  FIRST LINE OPTEVA 500E - MANAGED PLAN             : 5             
    ##  (Other)                                           :18             
    ##  NA's                                              :46             
    ##   Item Desc                    Activity Trouble Code   SR State        
    ##  Length:200         REPAIR                :68        Length:200        
    ##  Class :character   XX_NONE OF THE ABOVE  :29        Class :character  
    ##  Mode  :character   INSTALLATION          :24        Mode  :character  
    ##                     DRILLING_FORCED       :22                          
    ##                     FL_FIRSTLINE          :17                          
    ##                     BC_BANK CUSTOMER ERROR: 7                          
    ##                     (Other)               :33                          
    ##     SR Site        SR Serial Number SR Contact Date              
    ##  516880 :  2   1522FGR08678:  2     Min.   :2018-01-23 02:53:00  
    ##  1846360:  2   1529FGR00945:  2     1st Qu.:2018-06-18 08:00:00  
    ##  1846660:  2   612001409   :  2     Median :2018-08-15 11:13:30  
    ##  1848940:  2   613000491   :  2     Mean   :2018-08-26 23:07:40  
    ##  1856914:  2   613001012   :  2     3rd Qu.:2018-11-15 14:21:15  
    ##  3783940:  2   (Other)     :154     Max.   :2018-11-20 20:07:00  
    ##  (Other):188   NA's        : 36                                  
    ##  Activity Completed Date       Br Area Desc                 Activity Type
    ##  Min.   :2018-12-01 10:00:00   Length:200         Cleaning Only    :  5  
    ##  1st Qu.:2018-12-05 11:58:56   Class :character   Field Repair     :140  
    ##  Median :2018-12-10 14:37:30   Mode  :character   First Line       : 37  
    ##  Mean   :2018-12-11 13:06:45                      Forced Entry     :  1  
    ##  3rd Qu.:2018-12-16 20:35:00                      Installation - BW: 17  
    ##  Max.   :2018-12-31 11:30:00                                             
    ##                                                                          
    ##  Base Call YN
    ##  Y:200       
    ##              
    ##              
    ##              
    ##              
    ##              
    ## 

### 2.2 Free Form Text

The features in our dataset that are free form text are the features
`Billing Notes` and `Call Text`.

Below is a preview of `Call
    Text`

``` r
billing_df$`Call Text` %>% head(3)
```

    ## [1] "ATM head came in damaged in shipping . head is not fixable . lead time on parts 2-3 months to replace AHD Head ordered 00016760000C night drop head that was damaged . job completed"                                                                                                                                                                                                 
    ## [2] "DECAL Generated Call, Customer Product ID 9912V. SEC6*DRIVE THRU*EQUIPMENT - NEW OR REPLACE*Please proceed with the proposal to order and install (2) new headsets.  NTE:$1144  Turned over to Ron Freeman. Proposal submitted to CPG on 9/26/2018. Pending approval. Proposal declined."                                                                                             
    ## [3] "DECAL Generated Call, Customer Product ID 9101L. SEC6*TELLER UNDERCOUNTER STEEL*EQUIPMENT - NEW*Please  Portable Teller Locker and a Teller bus per proposal.NTE:$2161  09/20/18  1:16:28PM - EDT  Michael  per tech threatt, assign to tech kennedy. wjy Per PM Casey Hill. schedule for the evening of 12/4/18. Per PM Casey Hill work completed on 12/4/18. Closing call complete."

Below is a preview of `Billing
    Notes`

``` r
billing_df$`Billing Notes` %>% extract(c(3, 5, 1))
```

    ## [1] "1. PLEASE PROVIDE REPAIR TIME FOR BILLING. THANKS. 2. SENDING TO SPECIAL QUEUE FOR ACCOUNT TEAM REVIEW 3. NON-BILL. WILL BE BILLED ON SO"
    ## [2] "1. ON WHICH PIECE OF EQUIPMENT COMBOS CHANGED?  AND OK TO BILL FOR KEYS. 2. NO BILL SR CONTRACT WORK."                                   
    ## [3] NA

``` r
call_text <-  use_series(billing_df, `Call Text`)
billing_notes <-  use_series(billing_df, `Billing Notes`)
```

``` r
call_text_corpus <- VCorpus(VectorSource(call_text), readerControl = list(language = "en"))
bill_notes_corpus <- VCorpus(VectorSource(billing_notes), readerControl = list(language = "en"))
```

``` r
call_text_corpus %>% extract(1:3) %>% inspect()
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 3
    ## 
    ## [[1]]
    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 180
    ## 
    ## [[2]]
    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 280
    ## 
    ## [[3]]
    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 373

``` r
call_text_corpus %>% head(3) %>% lapply(function (doc) doc$content)
```

    ## $`1`
    ## [1] "ATM head came in damaged in shipping . head is not fixable . lead time on parts 2-3 months to replace AHD Head ordered 00016760000C night drop head that was damaged . job completed"
    ## 
    ## $`2`
    ## [1] "DECAL Generated Call, Customer Product ID 9912V. SEC6*DRIVE THRU*EQUIPMENT - NEW OR REPLACE*Please proceed with the proposal to order and install (2) new headsets.  NTE:$1144  Turned over to Ron Freeman. Proposal submitted to CPG on 9/26/2018. Pending approval. Proposal declined."
    ## 
    ## $`3`
    ## [1] "DECAL Generated Call, Customer Product ID 9101L. SEC6*TELLER UNDERCOUNTER STEEL*EQUIPMENT - NEW*Please  Portable Teller Locker and a Teller bus per proposal.NTE:$2161  09/20/18  1:16:28PM - EDT  Michael  per tech threatt, assign to tech kennedy. wjy Per PM Casey Hill. schedule for the evening of 12/4/18. Per PM Casey Hill work completed on 12/4/18. Closing call complete."

To clean our data set we will have to:

  - Convert the text to lower case, so that words like “write” and
    “Write” are considered the same word
  - Remove numbers
  - Remove English stopwords e.g “the”, “is”, “of”, etc.
  - Remove punctuation e.g “,”, “?”, etc.
  - Eliminate extra white spaces
  - Stemming our text

Using the `tm` package we will apply transformations to each text
document in the `call_text_corpus` to clean the text document.

``` r
replace_asterix <- function(document) {
  gsub(pattern = "\\*", replacement = " ", document)
}

add_space_period <- function(document) {
  gsub(pattern = "\\.", replacement = ". ", document)
}

remove_single_chars <- function(document) {
  gsub(pattern = "\\s[a-z]\\s", replacement = " ", document)
}

clean_up <- function(corpus) {
  corpus %>% 
    # Convert the text to lower case
    tm_map(content_transformer(tolower)) %>%
    # Replace asterics "*" with an empty space
    tm_map(content_transformer(replace_asterix)) %>%
    # Add a space after a period
    tm_map(content_transformer(add_space_period)) %>%
    # Remove numbers
    tm_map(removeNumbers) %>%
    # Remove english common stopwords
    tm_map(removeWords, stopwords("english")) %>%
    # Remove words related to time
    tm_map(removeWords, c("pm", "am", "edt")) %>%
    # Remove punctuations
    tm_map(removePunctuation) %>%
    # Remove orphaned letters
    tm_map(content_transformer(remove_single_chars)) %>%
    # Eliminate extra white spaces
    tm_map(stripWhitespace) %>%
    # strip trailing and leading whitespace
    tm_map(content_transformer(trimws))
}

call_text_cleaned <- clean_up(call_text_corpus)
bill_notes_cleaned <- clean_up(bill_notes_corpus)
```

``` r
call_text_cleaned %>% head(3) %>% lapply(function (doc) doc$content)
```

    ## $`1`
    ## [1] "atm head came damaged shipping head fixable lead time parts months replace ahd head ordered night drop head damaged job completed"
    ## 
    ## $`2`
    ## [1] "decal generated call customer product id sec drive thru equipment new replace please proceed proposal order install new headsets nte turned ron freeman proposal submitted cpg pending approval proposal declined"
    ## 
    ## $`3`
    ## [1] "decal generated call customer product id sec teller undercounter steel equipment new please portable teller locker teller bus per proposal nte michael per tech threatt assign tech kennedy wjy per casey hill schedule evening per casey hill work completed closing call complete"

``` r
bill_notes_cleaned %>% head(3) %>% lapply(function (doc) doc$content)
```

    ## $`1`
    ## [1] NA
    ## 
    ## $`2`
    ## [1] NA
    ## 
    ## $`3`
    ## [1] "please provide repair time billing thanks sending special queue account team review nonbill will billed"

``` r
billing_df$`Call Text` <- call_text_cleaned %>% sapply(function (doc) doc$content)
billing_df$`Billing Notes` <- bill_notes_cleaned %>% sapply(function (doc) doc$content)
```

``` r
save(billing_df, file="data/data_preparation.RData")
```
