# Rushing by Weight and Position Scatter Plot ----
## Original Coder: Neil J. Hatfield
## Create a quickly updatable scatter plot showing PSU rushing leaders
## yardage by weight and position. Include their jersey numbers for 
## quick identification

# Step 0: Install any needed packages that aren't installed ----
# Note: install commands should ALWAYS be commented out
# install.packages(pkgs = c("rvest", "ggrepel"))

# Step 1: Load Packages ----
## Needed Packages: tidyverse, rvest, ggrepel
library(tidyverse) # Tidying and Wrangling; plotting
library(rvest) # Scraping
library(ggrepel) # For plotting

# Step 2: Scrap Data ----
## Step 2A: Scrape PSU Roster Data ----
psu_page_tables <- read_html(
  x = "https://gopsusports.com/sports/football/roster?view=table"
) |>
  html_elements(css = "table") |>
  html_table()

View(psu_page_tables)

## We need the first list item
psu_roster_raw <- psu_page_tables[[1]]

## Step 2B: Scrape ESPN Data ----
espn_page_tables <- read_html(
  x = "https://www.espn.com/college-football/team/stats/_/id/213/penn-state-nittany-lions"
) |>
  html_elements(css = "table") |>
  html_table()

View(espn_page_tables)

## We need the third and fourth items bound together
espn_rushing_raw <- bind_cols(espn_page_tables[[3]], espn_page_tables[[4]])

# Step 3: Tidy and Wrangle the Data Sets ----

## Step 3A: ESPN Data ----
espn_rushing_clean <- espn_rushing_raw |>
  filter_out(Name == "Total") |>
  separate_wider_delim(
    cols = Name,
    delim = " ",
    names = c("name1", "name2", "name3", "position"),
    too_few = "align_end",
    too_many = "merge"
  ) |>
  unite(
    col = "Name",
    contains("name"),
    sep = " ",
    na.rm = TRUE
  )

## Step 3B: PSU Data ----
psu_roster_clean <- psu_roster_raw |>
  rename(weight_lbs = Weight) |>
  mutate(
    weight_lbs = parse_number(weight_lbs)
  ) |>
  select(
    jersey = `# Jersey Number`, Name, Position, weight_lbs
  )

# Step 4: Join Data Frames Together ----
rushing_data <- left_join(
  x = espn_rushing_clean,
  y = psu_roster_clean,
  by = join_by(Name == Name, position == Position)
)


# Step 5: Make the Plot ----
ggplot(
  data = rushing_data,
  mapping = aes(
    x = weight_lbs,
    y = YDS,
    color = position,
    shape = position,
    label = jersey
  )
) +
  geom_point(size = 5) +
  labs(
    x = "Player weight (lbs)",
    y = "Rushing yard (YDS)",
    color = "position",
    shape = "position",
    title = "PSU Rushing Yard by Weight of Each Position"
  ) +
  scale_color_manual(
    values = c("#99CC00", "#BC204B", "#1E407C","")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
  


