# Global variable declarations to avoid R CMD check NOTEs
# These are column names used in dplyr/tidyr operations

utils::globalVariables(c(

# Chart data columns
"pos", "pos_change", "artist", "artist_id", "title", "track_id",
"days", "peak", "streams_daily", "streams_change",
"streams_7day", "streams_7day_change", "streams_total",
"scrape_date", "country",

# Track history columns
"date", "streams", "position",

# Feature engineering columns
"day_of_week", "day_of_month", "day_of_year", "week_of_year",
"month", "is_weekend", "rolling_7day", "rolling_14day", "rolling_30day",
"lag_1", "lag_7", "pct_change_1", "pct_change_7", "momentum_7",

# Analysis columns
"avg_streams",

# Misc
"."
))

# Package-level constants
.kworb_base_url <- "https://kworb.net/spotify/"
.kworb_user_agent <- "kworb-r-package/0.1.0 (https://github.com/peteowen1/kworb)"
.kworb_rate_limit_seconds <- 1
