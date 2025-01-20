# clean ECEC data

#### Clean census division ECEC ####
cd_ecec_clean <- cd_ecec_raw |>  
  # Join with division codes
  left_join(division_codes, by = "series_id") |> 
  # Filter for specific quarter and datatype
  filter(quarter == 2, datatype_code == 'D') |> 
  # Select relevant columns
  select(-end_period, -end_year, -begin_period, -begin_year) |> 
  # Create area title variable
  mutate(
    area_title = case_when(
      area_code == 99100 ~ "New England",
      area_code == 99120 ~ "Middle Atlantic",
      area_code == 99130 ~ "East South Central",
      area_code == 99140 ~ "South Atlantic",
      area_code == 99150 ~ "East North Central",
      area_code == 99160 ~ "West North Central",
      area_code == 99170 ~ "West South Central",
      area_code == 99180 ~ "Mountain",
      area_code == 99190 ~ "Pacific",
      area_code == 99999 ~ "United States",
      TRUE ~ NA_character_),
    # Make sure value is numeric
      value = as.numeric(value))

cd_ecec_fmt <- cd_ecec_clean |> 

# Aggregate "Insurance benefits and retirement benefits"
  bind_rows(
    cd_ecec_clean |> 
      filter(estimate_code %in% c(13, 18)) |> 
      summarize(
        estimate_code = 99, # New code for aggregated "Insurance benefits and retirement benefits"
        value = sum(value, na.rm = TRUE),
        series_title = "Insurance benefits and retirement benefits",
        .by = area_title
      )
  ) |> 
  # Drop original subcategories for insurance benefits
  filter(!estimate_code %in% c(13, 18)) |> 

# Aggregate "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)"
  bind_rows(
    cd_ecec_clean |> 
      filter(estimate_code %in% c(2, 4, 9)) |> 
      summarize(
        estimate_code = 98, # New code for "Pay (wages, salaries, etc.)"
        value = sum(value, na.rm = TRUE),
        series_title = "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)",
        .by = area_title
      )
  ) |> 

# Filter for relevant estimate codes
  filter(estimate_code %in% c(1, 2, 4, 9, 21, 98, 99)) |> 
  
  # Create a labelled factor for series_title based on estimate_code
  mutate(series_title = factor(estimate_code, levels = c(98, 2, 9, 4, 99, 21, 1), labels = c(
    "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)",
    "Wages and salaries",
    "Supplemental pay (e.g., overtime)",
    "Paid leave (vacation, holiday, sick, personal)",
    "Insurance benefits and retirement benefits",
    "Legally required benefits (Social Security, Medicare, federal and state unemployment insurance, and workers’ compensation)",
    "Total Compensation"
  ))) |>

# Calculate total compensation and share of pay
  mutate(
    pay = sum(value[estimate_code == 98], na.rm = TRUE),
    ratio_of_pay = (value / pay),
    .by = area_title
  ) |> 

# Arrange for presentation and select final columns
  arrange(area_title, series_title) |> 
  select(
    area_title,
    estimate_code,
    compensation_component = series_title,
    cost_per_hour_worked_cd = value,
    ratio_of_pay_cd = ratio_of_pay
  )

#### Clean national ECEC ####

natl_ecec_clean <- natl_ecec_raw |> 
  left_join(national_codes) |> 
  filter(quarter == 2, datatype_code == 'D') |> 
  select(-end_period, -end_year, -begin_period, -begin_year) |> 
  # Ensure value is numeric
  mutate(value = as.numeric(value))

natl_ecec_fmt <- natl_ecec_clean |> 
  
  filter(estimate_code %in% c(13, 18)) |> 
  summarize(
    # New code for aggregated "Insurance benefits and retirement benefits"
    estimate_code = 99, 
    value = sum(value, na.rm = TRUE),
    series_title = "Insurance benefits and retirement benefits") |>
  
  # Aggregate "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)"
  bind_rows(
    natl_ecec_clean |>
      filter(estimate_code %in% c(2, 4, 9)) |>
      # New code for "Pay (wages, salaries, etc.)"
      summarize(
        estimate_code = 98, 
        value = sum(value, na.rm = TRUE),
        series_title = "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)")) |>
  
  # Add aggregated "Pay" to the dataset
  bind_rows(natl_ecec_clean) |>
  
  # Filter for relevant estimate codes
  filter(estimate_code %in% c(1, 2, 4, 9, 21, 98, 99)) |>
  
  # Create a labelled factor for series_title based on estimate_code
  mutate(series_title = factor(estimate_code, levels = c(98, 2, 9, 4, 99, 21, 1), labels = c(
    "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)",
    "Wages and salaries",
    "Supplemental pay (e.g., overtime)",
    "Paid leave (vacation, holiday, sick, personal)",
    "Insurance benefits and retirement benefits",
    "Legally required benefits (Social Security, Medicare, federal and state unemployment insurance, and workers’ compensation)",
    "Total Compensation"
  ))) |>
  
  # Calculate total compensation and share of pay
  mutate(
    pay = sum(value[estimate_code == 98], na.rm = TRUE),
    ratio_of_pay = (value / pay)
  ) |> 

  # Select final columns for output
  arrange(series_title) |> 
  select(
    compensation_component = series_title,
    cost_per_hour_worked_natl = value,
    ratio_of_pay_natl = ratio_of_pay
  )


#### Clean industry ECEC ####

# Initial cleaning and preparation
ind_ecec_clean <- ind_ecec_raw |> 
  left_join(industry_codes) |> 
  filter(quarter == 2, datatype_code == 'D') |> 
  select(-end_period, -end_year, -begin_period, -begin_year) |> 
  mutate(value = as.numeric(value)) # Ensure value is numeric

# Aggregate "Insurance benefits and retirement benefits"
ind_ecec_fmt <- ind_ecec_clean |> 
  bind_rows(
    ind_ecec_clean |> 
      filter(estimate_code %in% c(13, 18)) |> 
      summarize(
        # New code for aggregated "Insurance benefits and retirement benefits"
        estimate_code = 99,
        value = sum(value, na.rm = TRUE),
        series_title = "Insurance benefits and retirement benefits",
        .by = industry_title)) |>
  
  # Add aggregated "Insurance benefits and retirement benefits" and drop original subcategories
  
  filter(!estimate_code %in% c(13, 18)) |> 

# Aggregate "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)"
  bind_rows(
    ind_ecec_clean |>
      filter(estimate_code %in% c(2, 4, 9)) |>
      summarize(
        estimate_code = 98, # New code for "Pay (wages, salaries, etc.)"
        value = sum(value, na.rm = TRUE),
        series_title = "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)",
        .by = industry_title
        )) |>
  
  # Filter for relevant estimate codes
  filter(estimate_code %in% c(1, 2, 4, 9, 21, 98, 99)) |>
  
  # Create a labelled factor for series_title based on estimate_code
  mutate(series_title = factor(estimate_code, levels = c(98, 2, 9, 4, 99, 21, 1), labels = c(
    "Pay (wages, salaries, supplemental pay, vacation, holiday, sick, personal)",
    "Wages and salaries",
    "Supplemental pay (e.g., overtime)",
    "Paid leave (vacation, holiday, sick, personal)",
    "Insurance benefits and retirement benefits",
    "Legally required benefits (Social Security, Medicare, federal and state unemployment insurance, and workers’ compensation)",
    "Total Compensation"
  ))) |>
  
  # Calculate total pay and ratio of pay
  mutate(
    pay = sum(value[estimate_code == 98], na.rm = TRUE),
    ratio_of_pay = (value / pay),
    .by = industry_title) |> 

# Arrange for presentation and select final columns
  arrange(industry_title, series_title) |> 
  select(
    industry_title,
    compensation_component = series_title,
    cost_per_hour_worked_ind = value,
    ratio_of_pay_ind = ratio_of_pay
  )

#### Final ECEC tables ####

ecec_methodology <- cd_ecec_fmt |>
  left_join(ind_ecec_fmt, by = 'compensation_component', relationship = "many-to-many") |>
  left_join(natl_ecec_fmt, by = 'compensation_component', relationship = "many-to-many") |>
  mutate(ratio_of_pay_v1 = (ratio_of_pay_ind / ratio_of_pay_natl) * ratio_of_pay_cd) |>
  
  # Adjust ratio_of_pay_v1 for "Pay (wages, ...)"
  mutate(
    ratio_of_pay_v1 = case_when(
      estimate_code == 98  ~ sum(ratio_of_pay_v1[estimate_code %in% c(2,4,9)],na.rm = TRUE),
      TRUE ~ ratio_of_pay_v1),
    .by = c(area_title, industry_title)) |>
  
  # Create ratio_of_pay_v* which "forces" our comp_components to sum to 100%
  mutate(
    ratio_of_pay_v2 = case_when(
      estimate_code == 2 ~ (ratio_of_pay_v1[estimate_code==2] / ratio_of_pay_v1[estimate_code == 98]),
      estimate_code == 9 ~ (ratio_of_pay_v1 / ratio_of_pay_v1[estimate_code==98]),
      estimate_code == 4 ~ (ratio_of_pay_v1 / ratio_of_pay_v1[estimate_code==98]),
      estimate_code == 99 ~ ratio_of_pay_v1,
      estimate_code == 21 ~ ratio_of_pay_v1),
    .by = c(area_title, industry_title)) |>
  
  # Create ratio_of_pay_final which sums the comp_components to get pay and total compensation
  mutate(
    ratio_of_pay_final = case_when(
      estimate_code == 98 ~ sum(ratio_of_pay_v2[estimate_code %in% c(2,4,9)]),
      estimate_code == 1 ~ sum(ratio_of_pay_v2[estimate_code %in% c(2,4,9,99,21)]),
      TRUE ~ ratio_of_pay_v2),
    .by = c(area_title, industry_title)) |>
  
  # Arrange for readability
  arrange(area_title, industry_title) 

# A subset of the ECEC methodology table
regional_industry_ecec <- ecec_methodology |> 
  select(census_division=area_title, industry_title, compensation_component, ratio_of_pay_final)




