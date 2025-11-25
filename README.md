
<!-- README.md is generated from README.Rmd. Please edit that file -->

# electronco

![](inst/extdata/electronco_logo.png)

**electronco is an R package for extracting patient cohorts and flagging
complex** **phenotypes in electronic health records (EHR) based on
diagnoses, procedures,** **medications, and lab tests. Diagnosis and
procedure cohorts are based on** **versioned ICD9, ICD10, and CPT code
lists. A versioned diagnosis concept set** **and versioned procedure
groups are currently available for breast cancer.**

## Installation

**You can install electronco as follows. If you do not already have the
remotes** **package installed, use the first line of code (after the \#)
before installing** **electronco.**

``` r
# install.packages("remotes")
remotes::install_github("jhaak1/electronco")
```

## 1. Connect to your database or upload files.

**Here is an example using a Postgres SQL database.**

``` r
library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "ehr",
  host     = Sys.getenv("PG_HOST", "localhost"),
  port     = as.integer(Sys.getenv("PG_PORT", 5432)),
  user     = Sys.getenv("PG_USER", Sys.getenv("DB_USER")),
  password = Sys.getenv("PG_PASSWORD", Sys.getenv("DB_PASS"))
)
```

## 2. Make a patient cohort based on diagnoses.

**If you are working with a large database, I recommend doing as much
filtering as** **possible in SQL (e.g. with WHERE statements in the
dbGetQuery() call below).** **If using a local file, you can use
read.csv(), read_csv(), etc.**

``` r
dia1 = dbGetQuery(con, "
  SELECT *
  FROM public.diagnoses
")
```

**Now you can use the diagnosis() function to make your patient cohort.
In this** **example, I set concept to ‘bc’ for breast cancer. This
concept set comes with** **electronco. You can also use a custom concept
set, which should be a dataframe with** **the following columns: code
(e.g. C50.01), system (e.g. icd10), and include (** **TRUE for every row
in the set). If using a custom concept set, you can set concept to the
name of your dataframe.**

``` r
#library(electronco)

d1 = diagnosis(
      data = dia1,
      concept = 'bc',
      lookback_start = '2011-01-01',
      lookback_end = '2023-01-01',
      min_events = 1,
      patient_id_col = "patient_id",
      code_col = "code",
      system = "code_type",
      date_col = "diagnosis_date",
      date_format = "%Y-%m-%d"
)

head(d1$patient_level, 10)
#>    patient_id n_total first_date  last_date meets_min_event diagnosis_flag
#> 1        P001       0       <NA>       <NA>           FALSE          FALSE
#> 2        P002       1 2020-06-01 2020-06-01            TRUE           TRUE
#> 3        P003       0       <NA>       <NA>           FALSE          FALSE
#> 4        P004       0       <NA>       <NA>           FALSE          FALSE
#> 5        P005       0       <NA>       <NA>           FALSE          FALSE
#> 6        P006       1 2022-07-07 2022-07-07            TRUE           TRUE
#> 7        P007       0       <NA>       <NA>           FALSE          FALSE
#> 8        P008       0       <NA>       <NA>           FALSE          FALSE
#> 9        P009       0       <NA>       <NA>           FALSE          FALSE
#> 10       P010       1 2021-04-04 2021-04-04            TRUE           TRUE
head(d1$evidence, 10)
#> # A tibble: 10 × 5
#>    patient_id code    system date       is_canonical
#>    <chr>      <chr>   <chr>  <date>     <lgl>       
#>  1 1          C50.911 ICD10  2019-03-01 TRUE        
#>  2 1          174.9   ICD9   2019-03-01 TRUE        
#>  3 10         C50.212 ICD10  2019-12-05 TRUE        
#>  4 10         174.2   ICD9   2019-12-05 TRUE        
#>  5 11         C50.911 ICD10  2019-03-01 TRUE        
#>  6 11         174.9   ICD9   2019-03-01 TRUE        
#>  7 12         C50.912 ICD10  2020-06-25 TRUE        
#>  8 12         174.9   ICD9   2020-06-25 TRUE        
#>  9 13         C50.411 ICD10  2018-11-12 TRUE        
#> 10 13         174.4   ICD9   2018-11-12 TRUE
```

## 3. Make a patient cohort based on procedures.

**First, select the desired patients from your database.**

``` r
pro1 = dbGetQuery(con, "
  SELECT *
  FROM public.procedures
")
```

**Next, refine your patient cohort using proc().**

``` r
p1 = proc(
      data = pro1,
      proc_group = c("breast cancer screening", "diagnostic mammography",
        "breast ultrasound", "mri of the breast", "needle biopsy",
        "breast specimen radiography", "pathology", "tumor marker testing",
        "breast-conserving surgery", "mastectomy procedure"),
      patient_id_col = "patient_id",
      code_col = "cpt_code",
      date_col = "proc_date",
      date_format = NULL,
      date_range = NULL,
      min_count = 1,
      first_only = FALSE,
      code_list = NULL
)

head(p1, 10)
#> # A tibble: 10 × 7
#>    patient_id cohort_name      cohort_count cohort_start cohort_end sample_codes
#>    <chr>      <chr>                   <int> <date>       <date>     <chr>       
#>  1 1          mastectomy proc…            1 2019-03-10   2019-03-10 19301       
#>  2 10         lymph node proc…            1 2019-12-18   2019-12-18 38525       
#>  3 11         mastectomy proc…            1 2019-03-10   2019-03-10 19301       
#>  4 12         mastectomy proc…            1 2020-07-02   2020-07-02 19303       
#>  5 13         breast-conservi…            1 2018-11-20   2018-11-20 19120       
#>  6 14         lymph node proc…            1 2021-02-12   2021-02-12 38525       
#>  7 15         mastectomy proc…            1 2019-08-28   2019-08-28 19307       
#>  8 17         breast-conservi…            1 2020-04-20   2020-04-20 19120       
#>  9 18         mastectomy proc…            1 2018-06-15   2018-06-15 19301       
#> 10 19         mastectomy proc…            1 2021-09-01   2021-09-01 19303       
#> # ℹ 1 more variable: proc_flag <lgl>
```

## 4. Make a patient cohort based on medications.

**Read in your medication data.**

``` r
med1 = dbGetQuery(con, "
  SELECT *
  FROM public.meds
")
```

**Make your medication-based patient cohort using meds().**

``` r
m1 = meds(
      data = med1,
      drugs = c('tamoxifen', 'trastuzumab', 'doxorubicin'),
      patient_id = "patient_id",
      order_date = "order_date",
      med_name = "med_name",
      route = "route",
      dose = "dose",
      route_filter = NULL,
      date_range = NULL,
      first_only = FALSE,
      inc_original_cols = FALSE
)

head(m1, 10)
#> # A tibble: 10 × 4
#>    patient_id order_date drug_matched cohort_flag
#>    <chr>      <chr>      <chr>        <lgl>      
#>  1 P002       2020-06-10 trastuzumab  TRUE       
#>  2 P010       2021-05-01 trastuzumab  TRUE       
#>  3 P002       2020-06-10 trastuzumab  TRUE       
#>  4 P010       2021-05-01 trastuzumab  TRUE       
#>  5 2          2020-07-08 doxorubicin  TRUE       
#>  6 4          2021-02-17 trastuzumab  TRUE       
#>  7 5          2019-09-02 tamoxifen    TRUE       
#>  8 12         2020-07-08 doxorubicin  TRUE       
#>  9 14         2021-02-17 trastuzumab  TRUE       
#> 10 15         2019-09-02 tamoxifen    TRUE
```

## 5. Make a patient cohort based on lab tests.

**Read in your data.**

``` r
lab1 = dbGetQuery(con, "
  SELECT *
  FROM public.labs
")
```

**Use labs() to make a patient cohort.**

``` r
l1 = labs(
      data = lab1,
      markers = c('CA-125', 'hemoglobin', 'CEA', 'ER', 'PR', 'HER2'),
      match_type = "exact",
      patient_id_col = "patient_id",
      lab_date_col = "lab_date",
      lab_name_col = "lab_name",
      date_range = NULL,
      cohort_type = "all",
      min_tests = 1
)

head(l1, 10)
#>    patient_id   lab_date   lab_name value units
#> 1           1 2019-03-12 Hemoglobin    13  g/dL
#> 2          10 2019-12-19 Hemoglobin    12  g/dL
#> 3          11 2019-03-12 Hemoglobin    13  g/dL
#> 4          12 2020-07-05 Hemoglobin    11  g/dL
#> 5          13 2018-11-20 Hemoglobin    12  g/dL
#> 6          14 2021-02-14 Hemoglobin    10  g/dL
#> 7          15 2019-08-30 Hemoglobin    13  g/dL
#> 8          16 2022-01-10 Hemoglobin    12  g/dL
#> 9          17 2020-04-22 Hemoglobin    11  g/dL
#> 10         18 2018-06-18 Hemoglobin    14  g/dL
```

**Diconnect from your database.**

``` r
dbDisconnect(con)
```

## 6. Flag a desired phenotype based on diagnoses, procedures, medications, and lab tests.

**pheno() uses the output of diagnosis(), proc(), meds(), and labs() or
user** **supplied vectors of diagnoses, procedures, medications, and lab
tests.**

**First, set up your custom specification.**

``` r
spec = list(id = "Best Spec Ever",
            version = '1.0.0',
            date_range = c('2010-01-01', '2025-01-01'),
            rule = 'any',
            min_count = 1)
```

**Then run pheno() to flag your desired phenotype. If you’re using the
output** **of diagnosis() for dx_tbl, make sure you specify the
patient_level dataframe.**

``` r
pheno1 = pheno(
          spec = spec,
          meds_tbl = m1,
          proc_tbl = p1,
          labs_tbl = l1,
          dx_tbl = d1$patient_level,
          return_evidence = TRUE
)

head(pheno1, 10)
#> # A tibble: 10 × 7
#>    patient_id flag  flag_date  evidence_count evidence_sample  spec_id       
#>    <chr>      <lgl> <date>              <int> <list>           <chr>         
#>  1 P002       TRUE  2020-05-20              3 <tibble [3 × 8]> Best Spec Ever
#>  2 P010       TRUE  2021-03-20              3 <tibble [3 × 8]> Best Spec Ever
#>  3 2          TRUE  2020-07-05              2 <tibble [2 × 8]> Best Spec Ever
#>  4 4          TRUE  2021-02-14              2 <tibble [2 × 8]> Best Spec Ever
#>  5 5          TRUE  2019-08-30              2 <tibble [2 × 8]> Best Spec Ever
#>  6 12         TRUE  2020-07-05              2 <tibble [2 × 8]> Best Spec Ever
#>  7 14         TRUE  2021-02-14              2 <tibble [2 × 8]> Best Spec Ever
#>  8 15         TRUE  2019-08-30              2 <tibble [2 × 8]> Best Spec Ever
#>  9 22         TRUE  2020-07-05              2 <tibble [2 × 8]> Best Spec Ever
#> 10 24         TRUE  2021-02-14              2 <tibble [2 × 8]> Best Spec Ever
#> # ℹ 1 more variable: spec_version <chr>
```

**You can look at the evidence_sample column for any patient to see why
that** **patient was flagged.**

``` r
pheno1$evidence_sample[1]
#> [[1]]
#> # A tibble: 3 × 8
#>   patient_id order_date drug_matched cohort_flag lab_date   lab_name value units
#>   <chr>      <date>     <chr>        <lgl>       <date>     <chr>    <int> <chr>
#> 1 P002       2020-06-10 trastuzumab  TRUE        NA         <NA>        NA <NA> 
#> 2 P002       2020-06-10 trastuzumab  TRUE        NA         <NA>        NA <NA> 
#> 3 P002       NA         <NA>         NA          2020-05-20 CA-125      45 U/mL
```

## 7. View the code versions being used with your version of electronco.

``` r
codeversions()
#> # A tibble: 5 × 3
#>   dataset data_version                      retrieved 
#>   <chr>   <chr>                             <chr>     
#> 1 icd10   ICD10 Jan 2025 (combined sources) 2025-10-29
#> 2 icd9    ICD9 Jan 2025 (combined sources)  2025-10-29
#> 3 cpt     CPT 11-26-2024                    2025-10-29
#> 4 bc      PheKB Breast Cancer Phenotype V2  2025-11-10
#> 5 bc_proc March 2, 2024                     2025-11-13
```

## 8. To see more detailed metadata for each code list, use codemetadata().

``` r
codemetadata('icd10')
#> $dataset
#> [1] "icd10"
#> 
#> $data_version
#> [1] "ICD10 Jan 2025 (combined sources)"
#> 
#> $retrieved
#> [1] "2025-10-29"
#> 
#> $sources
#> $sources[[1]]
#> $sources[[1]]$source_name
#> [1] "ICD10 Valid"
#> 
#> $sources[[1]]$source_url
#> [1] "https://www.cms.gov/medicare/coordination-benefits-recovery/overview/icd-code-lists"
#> 
#> $sources[[1]]$original_filename
#> [1] "section111validicd10-jan2025_0.xlsx"
#> 
#> $sources[[1]]$retrieved
#> [1] "2025-10-29"
#> 
#> 
#> $sources[[2]]
#> $sources[[2]]$source_name
#> [1] "ICD10 Excluded"
#> 
#> $sources[[2]]$source_url
#> [1] "https://www.cms.gov/medicare/coordination-benefits-recovery/overview/icd-code-lists"
#> 
#> $sources[[2]]$original_filename
#> [1] "section111excludedicd10-jan2025_0.xlsx"
#> 
#> $sources[[2]]$retrieved
#> [1] "2025-10-29"
#> 
#> 
#> 
#> $`_versions_file`
#> [1] "/home/jeremy/Desktop/electronco/inst/extdata/VERSIONS.yaml"
```
