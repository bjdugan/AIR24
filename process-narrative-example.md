---
title: "Process Narrative Example"
author: "Brendan Dugan"
date: "2024-05-21"
output: 
  html_document:
    theme: paper
    css: styles.css
    code_folding: hide
    keep_md: yes
---



_This template provides a general process narrative framework. Use or discard sections as needed. Only mock data are used. Notes to user are presented in these chunks in italics._


## Purpose
_What is the goal? This section describes the general aims of the task._

To construct statistical weights for use in reporting NSSE survey responses to institutions.

## Background
_Why is this done? This section describes the history or broader context of the task._

As part of each survey administration, NSSE collects population files from institutions containing student email addresses and variables used in statistical weighting to adjust for known differences in engagement and response rates: enrollment status (full- or part-time), gender (male, female, another e.g. non-binary, unknown), and class level (first-year or senior). 

Each student's response to the survey is assigned a disposition code (American Association for Public Opinion Research, 2023) such as "Complete," "Partial," "Explicit refusal," or "Logged on to the survey, did not complete any items," that determine inclusion in reports and in constructing sample weights.

## Scope
_What does this affect? Who should do it? This section describes which individuals or units are involved, and where it may fit into broader processes if not self-contained._

Lead: Brendan Dugan  
Support: ...  

As they feature weights or weighted statistics, the tables processed here affect final dataset delivery and report creation.  

### Timeline
_When should this occur? This section describes timelines, due dates, or durations in short._

Generally this occurs in late May or early June following the closing of the survey and the Report Form.

### Definitions
_What is the meaning of terms used? This section describes terminology and dataset variables as needed._

 - **population**: All first-year and senior bachelor's-degree-seeking students, regardless of age, residence, or enrollment status      
 - **sample**: All census- or randomly-sampled students, often the entire sample (~99.9% are census-sampled)  
 - **respondents**: all sample students with complete or partial dispositions  
 - **dispositions**: type of response to survey based on AAPOR's framework  

## Process
_What does this task consist of?_

Data are extracted from the database table `population` which features response disposition and student demographic information. Weights are computed by class, gender, and enrollment status from their respective representation in the population and respondents. 

### Extract Data
_While copying code isn't ideal, it can sometimes be a useful illlustration to break apart longer pipes in unevaluated chunks, explain each, and then compile the entire pipe later._

Extract, apply common filters, further subset to sample and respondents, then combine in one table.  

Since each level of aggregation (population > sample > respondents) draw from the same table, it can be extracted once, with common filters and recodes applied

```r
dispositions <- select(population, unitid, class, enrollment,
                gender, dispoLabel, sample) |>
  filter( # common criteria
    !is.na(dispoLabel),
    sample != "Ineligible for sampling",
    !is.na(enrollment), !is.na(gender),
    class %in% c("First-year", "Senior"), 
    dispoLabel %in% c("Complete", "Partial", "Explicit Refusal", 
                      "Logged on to survey, did not complete any items", 
                      "Unavailable during field period", # v rare
                      "Break-off", "Not contacted", "Nothing returned") ) |> 
  mutate(class = if_else(class == "First-year" ~ "fy", class == "Senior" ~ "sr"))
```
Then repeated into a list of dataframes

```r
dispositions |> 
  (\(x) replicate(3, x, simplify = FALSE))() |> 
  set_names(c("pop", "sample", "resp"))
```
Then subsequent filters applied

```r
  # apply sample restrictions (census or random)
  modify_at("sample", filter, 
            sample %in% c("Census", "Random sample", "Random oversample")) |>
  # keep only respondents
  modify_at("resp", filter, 
            dispoLabel %in% c("Complete", "Partial"),
            sample %in% c("Census", "Random sample", "Random oversample")) |> 
```
Before summarizing and reducing back into a single table with columns representing counts for each level.

```r
dispositions |> 
  map(group_by, unitid) |> 
  map(count, class, enrollment, gender) |> 
  imap(mutate) |> 
  map(pivot_wider, names_from = last_col(), values_from = n) |> 
  reduce(left_join, by = c("unitid", "class", "enrollment", "gender")) |> 
  replace_na(list(pop = 0, sample = 0, resp = 0)) |> 
  arrange(unitid, class, enrollment, desc(gender)) |> 
  collect()
```


| unitid|class |enrollment |gender | pop| sample| resp|
|------:|:-----|:----------|:------|---:|------:|----:|
| 123456|fy    |Full-time  |Male   | 150|    150|   26|
| 123456|fy    |Full-time  |Female | 288|    288|   71|
| 123456|fy    |Part-time  |Male   |  16|     16|    2|
| 123456|fy    |Part-time  |Female |  34|     34|    9|
| 123456|sr    |Full-time  |Male   | 137|    137|   30|
| 123456|sr    |Full-time  |Female | 303|    303|   78|
| 123456|sr    |Part-time  |Male   |  19|     19|    6|
| 123456|sr    |Part-time  |Female |  41|     41|   11|

### Compute Weights and Response Rates
Weights adjust `resp`ondents to reflect their number in the `pop`ulation and to account for different known response patterns. Weights are given as $w_c = P_c / r_c$. The mean of `weight_scale` is used to adjust `weight_adj`, $adj(w_c) = w_c / mean(w_{c-all})$. If a cell is empty (0), it is treated as missing; between 1 and 4, it's the overall weight; otherwise, just as calculated.


```r
# the main output
weights <- group_by(dispositions, unitid, class) |> 
  mutate(
    sample_temp = if_else(resp < 5, 0L, sample),
    resp_temp = if_else(resp < 5, 0L, resp),
    weight_scale_overall = sum(sample_temp) / sum(resp_temp),
    # only calculate weights for cells with at least 5
    weight_scale = case_when(resp >= 5 ~ sample / resp,
                        resp < 5 & resp > 0 ~ weight_scale_overall,
                        resp == 0 ~ NA_real_),
    weight_adj = weight_scale / weight_scale_overall, 
    rr = resp / sample * 100,
    sample_temp = NULL,
    resp_temp = NULL, 
    weight_scale_overall = NULL
  )

head(weights, 8) |> 
  kable(digits = 2)
```



| unitid|class |enrollment |gender      | pop| sample| resp| weight_scale| weight_adj|    rr|
|------:|:-----|:----------|:-----------|---:|------:|----:|------------:|----------:|-----:|
| 123456|fy    |Full-time  |Unknown     |  21|     21|    4|         4.45|       1.00| 19.05|
| 123456|fy    |Full-time  |Male        | 150|    150|   26|         5.77|       1.30| 17.33|
| 123456|fy    |Full-time  |Female      | 288|    288|   71|         4.06|       0.91| 24.65|
| 123456|fy    |Full-time  |Another sex |  14|     14|    4|         4.45|       1.00| 28.57|
| 123456|fy    |Part-time  |Unknown     |   2|      2|    1|         4.45|       1.00| 50.00|
| 123456|fy    |Part-time  |Male        |  16|     16|    2|         4.45|       1.00| 12.50|
| 123456|fy    |Part-time  |Female      |  34|     34|    9|         3.78|       0.85| 26.47|
| 123456|fy    |Part-time  |Another sex |   4|      4|    1|         4.45|       1.00| 25.00|

### Label and Export Data
Variable labels are added for export in SPSS and coupled with data in an R-native format. This could similarly be maintained within a database environment or Excel workbook.


```r
dictionary <- tibble(
  variable_name = colnames(weights),
  label = c(
    "IPEDS unitid", "Class level", "Enrollment status", "Gender", 
    "Population count", "Sample (subset of pop. within census or random sample) count", 
    "Respondent (subset of sample with partial or complete responses) count", 
    "Scale weight (respondent -> sample)", "Adjusted scale weight", "Response rate"
    )
  )

# add SPSS attributes and export
map2(weights, as.list(dictionary$label), `attr<-`, which = "label") |>
  bind_rows() |>
  write_sav("NSSE DISPOSITIONS.sav")

# in R native format
saveRDS(
  list("dictionary" = dictionary, 
       "dispositions" = dispositions),
  "dispo_info.rds"
)
```

## Resources
_What resources, definitions, would help here?_
 
 - The American Association for Public Opinion Research. _2023 Standard Definitions:
Final Dispositions of Case Codes and Outcome Rates for Surveys. 10th edition._ AAPOR. Retrieved from https://aapor.org/wp-content/uploads/2024/03/Standards-Definitions-10th-edition.pdf  

 - National Survey of Student Engagement. "An Explanation of Weighting in the NSSE Institutional Report." [https://nsse.indiana.edu/nsse/reports-data/weighting.html](https://nsse.indiana.edu/nsse/reports-data/weighting.html)  
 




