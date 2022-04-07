
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConMedClassify

<!-- badges: start -->
<!-- badges: end -->

This package stores sufficient demonstration data to perform a
classification.

``` r
library(ConMedClassify)

# load demo drug basket information
data(drug_baskets)
drug_baskets
#> # A tibble: 1 × 3
#>   drug_basket_name combo_drug_label drug_free_label
#>   <chr>            <chr>            <chr>          
#> 1 basket_demo      demo             demo_FREE

# load demo clinical data
data(demo_clinical_data)
dplyr::glimpse(demo_clinical_data)
#> Rows: 4
#> Columns: 11
#> $ STUDYID    <chr> "trial1", "trial1", "trial1", "trial1"
#> $ USUBJID    <chr> "trial1-site1-subject1", "trial1-site1-subject2", "trial1-s…
#> $ ACTARM     <chr> "arm1", "arm1", "arm2", "arm2"
#> $ SEX        <chr> "M", "F", "M", "F"
#> $ AGE        <dbl> 30, 50, 54, 71
#> $ TRTSDT     <chr> "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01"
#> $ TRTEDT     <chr> "2020-02-20", "2020-02-20", "2020-02-20", "2020-02-20"
#> $ SAFFL      <chr> "Y", "Y", "Y", "Y"
#> $ nested.CM  <list> [<tbl_df[4 x 13]>], [<tbl_df[4 x 13]>], [<tbl_df[4 x 13]>],…
#> $ nested.TTE <list> [<tbl_df[1 x 3]>], [<tbl_df[1 x 3]>], [<tbl_df[1 x 3]>], [<…
#> $ nested.EX  <list> [<tbl_df[1 x 8]>], [<tbl_df[1 x 8]>], [<tbl_df[1 x 8]>], [<…

as.data.frame(unnest_domain(demo_clinical_data[, c("STUDYID", "USUBJID", "nested.CM")], nested.CM))
#>    STUDYID               USUBJID EOSDY     CMDECOD                  CMCAT
#> 1   trial1 trial1-site1-subject1    50       drugA CONCOMITANT MEDICATION
#> 2   trial1 trial1-site1-subject1    50 drugA/miscA CONCOMITANT MEDICATION
#> 3   trial1 trial1-site1-subject1    50       miscC CONCOMITANT MEDICATION
#> 4   trial1 trial1-site1-subject1    50       drugA CONCOMITANT MEDICATION
#> 5   trial1 trial1-site1-subject2    30       drugA CONCOMITANT MEDICATION
#> 6   trial1 trial1-site1-subject2    30 drugA/miscA CONCOMITANT MEDICATION
#> 7   trial1 trial1-site1-subject2    30       miscC CONCOMITANT MEDICATION
#> 8   trial1 trial1-site1-subject2    30       drugB CONCOMITANT MEDICATION
#> 9   trial1 trial1-site1-subject3    30       drugC CONCOMITANT MEDICATION
#> 10  trial1 trial1-site1-subject3    30 miscA/drugC CONCOMITANT MEDICATION
#> 11  trial1 trial1-site1-subject3    30       miscC CONCOMITANT MEDICATION
#> 12  trial1 trial1-site1-subject3    30       drugC CONCOMITANT MEDICATION
#> 13  trial1 trial1-site1-subject4    30       drugD CONCOMITANT MEDICATION
#> 14  trial1 trial1-site1-subject4    30 miscA/drugD CONCOMITANT MEDICATION
#> 15  trial1 trial1-site1-subject4    30       miscC CONCOMITANT MEDICATION
#> 16  trial1 trial1-site1-subject4    30       drugD CONCOMITANT MEDICATION
#>    CMCLAS CMDOSE CMDOSU CMSTDY CMENDY CMENRTPT CMSTRTPT   CMSTTPT CMROUTE
#> 1   clasw     20 TABLET      5     10  ONGOING   BEFORE SCREENING    ORAL
#> 2   clasw     10     mg      6      8  ONGOING   BEFORE SCREENING    ORAL
#> 3   clasw     15     mg     10     15  ONGOING   BEFORE SCREENING    ORAL
#> 4   clasw      1 TABLET     12     20  ONGOING   BEFORE SCREENING    ORAL
#> 5   clasw      1 TABLET      5     10  ONGOING   BEFORE SCREENING    ORAL
#> 6   clasw     10     mg      6      8  ONGOING   BEFORE SCREENING    ORAL
#> 7   clasw     15     mg     10     15  ONGOING   BEFORE SCREENING    ORAL
#> 8   clasw      1 TABLET     12     20  ONGOING   BEFORE SCREENING    ORAL
#> 9   clasw      1 TABLET      5     10  ONGOING   BEFORE SCREENING    ORAL
#> 10  clasw     10     mg      6      8  ONGOING   BEFORE SCREENING    ORAL
#> 11  clasw     15     mg     10     15  ONGOING   BEFORE SCREENING    ORAL
#> 12  clasw      1 TABLET     12     20  ONGOING   BEFORE SCREENING    ORAL
#> 13  clasw      1 TABLET      5     10  ONGOING   BEFORE SCREENING    ORAL
#> 14  clasw     10     mg      6      8  ONGOING   BEFORE SCREENING    ORAL
#> 15  clasw     15     mg     10     15  ONGOING   BEFORE SCREENING    ORAL
#> 16  clasw      1 TABLET     12     20  ONGOING   BEFORE SCREENING    ORAL
#>     CMINDC
#> 1     pain
#> 2   nausea
#> 3  allergy
#> 4     pain
#> 5     pain
#> 6   nausea
#> 7  allergy
#> 8     pain
#> 9     pain
#> 10  nausea
#> 11 allergy
#> 12    pain
#> 13    pain
#> 14  nausea
#> 15 allergy
#> 16    pain

as.data.frame(unnest_domain(demo_clinical_data[, c("STUDYID", "USUBJID", "nested.EX")], nested.EX))
#>   STUDYID               USUBJID EXTRT EXDOSE EXDOSU EXOCCUR    VISIT   AVISIT
#> 1  trial1 trial1-site1-subject1  trt1    100  units       Y BASELINE BASELINE
#> 2  trial1 trial1-site1-subject2  trt1    100  units       Y BASELINE BASELINE
#> 3  trial1 trial1-site1-subject3  trt2    200  units       Y BASELINE BASELINE
#> 4  trial1 trial1-site1-subject4  trt2    200  units       Y BASELINE BASELINE
#>   EXSTDY EXENDY
#> 1      1    100
#> 2      1    100
#> 3      1    100
#> 4      1    100

as.data.frame(unnest_domain(demo_clinical_data[, c("STUDYID", "USUBJID", "nested.TTE")], nested.TTE))
#>   STUDYID               USUBJID PARAMCD AVAL CNSR
#> 1  trial1 trial1-site1-subject1      OS  150    1
#> 2  trial1 trial1-site1-subject2      OS  200    0
#> 3  trial1 trial1-site1-subject3      OS  250    1
#> 4  trial1 trial1-site1-subject4      OS  100    1
```

Performing the classification results in a file of profiles

``` r
# profile subjects into conmed profiles
extract_conmed_profiles_to_tsv(demo_clinical_data, drug_baskets = drug_baskets, drug_basket_data_objects = NULL, treatments = "trt1")
```

``` r
res <- readr::read_tsv("inst/outputs/ConMedProfiles_trial1_basket_demo.tsv", col_types = "c")
dplyr::glimpse(res)
#> Rows: 4
#> Columns: 17
#> $ STUDYID            <chr> "trial1", "trial1", "trial1", "trial1"
#> $ USUBJID            <chr> "trial1-site1-subject1", "trial1-site1-subject2", "…
#> $ isTreatedPt        <lgl> TRUE, TRUE, FALSE, FALSE
#> $ BASKET_CATEGORY    <chr> "classX", "demo_COMBO", "classZ", "demo_FREE"
#> $ NB_CM_RECORDS      <dbl> 3, 3, 2, NA
#> $ NB_CMSTART_IMPUTED <dbl> 0, 0, 0, NA
#> $ CMSTART_FIRST      <dbl> 5, 5, 5, NA
#> $ NB_CMEND_IMPUTED   <dbl> 0, 0, 0, NA
#> $ CMEND_LAST         <dbl> 20, 20, 20, NA
#> $ START_TREATMENT    <dbl> 0, 0, 0, NA
#> $ OS                 <dbl> 150, 200, 250, NA
#> $ OS_CNSR            <dbl> 1, 0, 1, NA
#> $ END_TREATMENT      <dbl> 71, 71, 71, NA
#> $ ndays_drug_os      <dbl> 15, 15, 15, NA
#> $ percent_drug_os    <dbl> 9.933775, 7.462687, 5.976096, NA
#> $ ndays_drug_trt     <dbl> 15, 15, 15, NA
#> $ percent_drug_trt   <dbl> 20.83333, 20.83333, 20.83333, NA
```

When many of these are constructed they can be aggregated together into
a single file per drug basket

``` r
# merge all individual TSV files across studies into a single one for a given drug basket
aggregate_conmed_profile_tsv(input_tsv_path = "outputs/",
                             tsv_pattern = '_basket_demo\\.tsv',
                             basket_name = "DEMO",
                             output_filename_prefix = "outputs/CITDataMart_ConMedProfiles_")
```
