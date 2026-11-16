# Tidy the Hypoxia Data ----
## Coder: Aiman Samsudin
## Tidy the hypoixa glider pilot data so that we have 12 cases and 8 columns.

# Step 1: Load Packages ----
## Needed Packages: {tidyverse}
library(tidyverse)

# Step 2: Load Data ----
## Needed data: hypoxia.csv
Hypoxia_Raw <- read.table(
  file = "/Users/manperak/STAT184/hypoxia-1.csv",
  header = TRUE,
  sep = ","
)

# Step 3: Check the Import ----
View(Hypoxia_Raw)
str(Hypoxia_Raw)

# Step 4: Wrangling the Data ----
Hypoxia_Tidy <- Hypoxia_Raw |>
  rename( #S4A: Rename columns
    junk1 = 1,
    altitude = 2,
    airPressure_mmHg = 3,
    pp_O2_mmHg = 4,
    alv_p_O2.21 = 5,
    hemo_O2.21 = 6,
    alv_p_CO2.21 = 7,
    alv_p_O2.100 = 8,
    hemo_O2.100 = 9,
    alv_p_CO2.100 = 10
  ) |>
  filter_out(junk1 == 1) |> # S4B: Drop extra row
  select(-junk1) |>
  pivot_longer( 
    cols = alv_p_O2.21:alv_p_CO2.100,
    names_to = "attributes.oxygenContent",
    values_to = "readings",
    values_transform = list(readings = as.character)
  ) |>
  separate_wider_delim(
    cols = attributes.oxygenContent,
    delim = ".",
    names = c("attributes", "oxygen_content")
  ) |>
  pivot_wider(
    names_from = attributes,
    values_from = readings
  ) |>
  mutate(
    altitude = replace_when(
      x = altitude,
      altitude == "Sea level" ~ "0k/0k"
    )
  ) |>
  separate_wider_delim(
    cols = altitude,
    delim = "/",
    names = c("altitude_ft", "altitude_m")
  ) |>
  mutate(
    altitude_ft = parse_number(altitude_ft) * 1000,
    altitude_m = parse_number(altitude_m) * 1000
  ) |>
  mutate(
    across(
      .cols = contains("mmHg") | contains("O2"),
      .fns = ~parse_number(.x, na = c("", "NA", "NaN"))
    )
  )
  
  
  
 

