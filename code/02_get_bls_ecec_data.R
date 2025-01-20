# Load BLS data using blsR

## get BLS series codes for ECEC worksheets

## download data ####
# define URL objects
# rawdatafile <- "cm.data.1.AllData"
currentfile <- "cm.data.0.Current"
rawseries <- 'cm.series'

rawsource <- "https://download.bls.gov/pub/time.series/cm/"

# download ecec files with system command
# "wget -N" omitts download if data has not been updated "-P" sets the file destination"
# system(paste0('wget -U "epi@epi.org" -N --progress=bar:force --header="Accept-Encoding: gzip" ',rawsource, rawdatafile, " -P input/"))
system(paste0('wget -U "epi@epi.org" -N --progress=bar:force --header="Accept-Encoding: gzip" ',rawsource, currentfile, " -P input/"))

system(paste0('wget -U "epi@epi.org" -N --progress=bar:force --header="Accept-Encoding: gzip" ',rawsource, rawseries, " -P input/"))


# import ecec data flat file
# cm_data <- fread("gunzip -c input/cm.data.1.AllData") 
cm_current <- fread("gunzip -c input/cm.data.0.Current") 
cm_series <- fread("gunzip -c input/cm.series")

series_filters <- cm_series |> 
  filter(datatype_code %in% c('P','D'))

# National ECEC data
national_codes <- series_filters |> 
  filter(area_code == 99999) |> 
  filter(datatype_code %in% c('P','D')) |> 
  # owner_code = private sector, industry/subcell_code/occupation == all workers
  filter(owner_code==2, industry_code=='000000', subcell_code==0, occupation_code==0) |> 
  filter(estimate_code %in% c(1, 2, 4, 9, 13, 18, 21))


# Census division ECEC data
division_codes <- series_filters |> 
  filter(area_code %in% c(99100:99190)) |> 
  filter(estimate_code %in% c(1, 2, 4, 9, 13, 18, 21))

# Industry ECEC data
industry_codes <- series_filters |> 
  filter(area_code == 99999) |> 
  filter(estimate_code %in% c(1, 2, 4, 9, 13, 18, 21)) |> 
  filter(datatype_code %in% c('P','D')) |> 
  # owner_code = private sector, industry/subcell_code/occupation == all workers
  filter(owner_code==2, subcell_code==0, occupation_code==0) |> 
  # Select industries of interest
  filter(industry_code %in% c(230000, 430000, 560000,
                              810000, 412000, 623000)) |> 
  
  # https://www.bls.gov/ecec/factsheets/ecec-series-id-guide.htm
  mutate(industry_title = case_when(
    industry_code == 230000 ~ "Construction",
    industry_code == 430000	~ "Transportation and Warehousing",
    industry_code == 560000 ~ "Administrative and Waste Services Industry",
    industry_code == 810000 ~ "Other Services Industry",
    industry_code == 412000 ~ "Retail Trade Industry",
    industry_code == 623000 ~ "Nursing and Residential Care Facilities Industry",
    TRUE ~ NA_character_)) |>
  arrange(datatype_code, industry_code)

  
bls_key <- Sys.getenv("BLS_REG_KEY")


#note: this is a random selection of codes used in our Jobs and Unemployment figures
# bls_codes <- read.csv(here("bls_codes.csv"))

# create array of series ids
natl_codes <- national_codes$series_id[]
cd_codes <- division_codes$series_id[]
ind_codes <- industry_codes$series_id[]


# use map to iteratively call blsR api at max number of series id
#note: BLS restricts to max 50 series in a single call
ecec_function <- function(codes) {
  map(
    split(codes, ceiling(seq_along(codes) / 50)),
    ~ get_n_series_table(series_ids = .x, start_year = 2024, end_year = 2024, 
                         api_key = bls_key, tidy = TRUE)
  ) |>
    # Map returns a list, flatten by joining data
    (\(x) reduce(x, function(df1, df2) full_join(df1, df2, by = c("year", "quarter"))))() |>
    # Ensure all CMU columns are treated as character
    mutate(across(starts_with("CMU"), as.character)) |>
    # Pivot longer
    pivot_longer(
      cols = starts_with("CMU"),
      names_to = "series_id",
      values_to = "value"
    )
}

# Call data from series_ids
natl_ecec_raw <- ecec_function(natl_codes)
cd_ecec_raw <- ecec_function(cd_codes)
ind_ecec_raw <- ecec_function(ind_codes)

