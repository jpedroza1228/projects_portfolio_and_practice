url <- "https://docs.google.com/spreadsheets/d/15Cd95xb-OPBUumuqDHKHrZ4SxetR7hy0UnAs04aRn74/edit?gid=0#gid=0"

df <- googlesheets4::read_sheet(url, sheet = 1)
2
df <- data.frame(df)

readr::write_csv(df, here::here("projects/pipeline/stocks.csv"))
