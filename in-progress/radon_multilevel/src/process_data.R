require(readr)
load("data/radon.data")

county_df <- data.frame(
    county = 1:J,
    uranium = u
)

write_csv(county_df, "output/data/county_radon.csv")

household_df <- data.frame(
    county = county,
    ff = x,
    radon = y
)

write_csv(household_df, "output/data/household_radon.csv")