# packages ----------------------------------------------------------------
library(purrr)
library(rvest)
library(dplyr)
library(yaml)
library(stringr)

# settings ----------------------------------------------------------------
# version to download
version <- "12.6.0"
hccodeurl <- "http://code.highcharts.com"

path       <- sprintf("inst/htmlwidgets/lib/highcharts")
path_temp  <- sprintf("inst/htmlwidgets/lib/highcharts-temp")

# creating folder structure
folders <- c("", "modules", "plugins", "css", "custom")
try(map(file.path(path_temp, folders), dir.create))

# main files --------------------------------------------------------------
file_temp <- tempfile(fileext = ".zip")

download.file(
  sprintf("https://code.highcharts.com/zips/Highcharts-%s.zip", version),
  file_temp,
  method = "curl"
)

# unzip into an empty folder so other session files cannot be copied
folder_temp <- tempfile("highcharts-zip-")
dir.create(folder_temp)

unzip(file_temp, exdir = folder_temp)

files <- dir(folder_temp, recursive = TRUE, full.names = TRUE) |> 
  str_subset("src.js$", negate = TRUE) |> 
  str_subset("js.map$", negate = TRUE)

basename(files)

# the widget loads only the UMD core files; the 13.x autoloader is a second
# standalone core build that fails when `Highcharts` already exists
main <- str_subset(files, "code/highcharts") |>
  str_subset("highcharts-autoload", negate = TRUE)
main
  
file.copy(
  main,
  file.path(path_temp, basename(main)),
  overwrite = TRUE
)

modules <- str_subset(files, "code/modules")
modules

file.copy(
  modules,
  file.path(path_temp, "modules", basename(modules)),
  overwrite = TRUE
)


# map ---------------------------------------------------------------------
# urlmapmodule <- sprintf("https://code.highcharts.com/maps/%s/modules/map.js", version)

# download.file(
#   urlmapmodule,
#   file.path(path_temp, "modules", basename(urlmapmodule))
# )

# check what modules are missing in yaml ----------------------------------
modules <- dir(file.path(path, "modules")) 
modules

modules_yalm <- readLines("inst/htmlwidgets/highchart.yaml") |> 
  str_subset("modules/") |>
  str_extract("modules/.*") |> 
  str_trim() |> 
  basename()

# copy and paste if this is not empty
setdiff(modules, modules_yalm) |>
  (\(x) str_c("#    - modules/", x, "\n"))() |>
  message()

# sobran
setdiff(modules_yalm, modules)

# repetidos en yalm
tibble(m = modules_yalm) |> 
  count(m, sort = TRUE) |> 
  filter(n > 1)

# plugins -----------------------------------------------------------------
# Each URL names a fixed commit, so a rerun downloads the vendored file
# instead of whatever the plugin's default branch holds that day. Update a
# commit only after reviewing the plugin's changes.
# - motion.js: larsac07/Motion-Highcharts-Plugin master at 036338d0
#   (2019-01-09, v1.0.9; the repository has no tag for it)
# - draggable-legend.js: highcharts/draggable-legend tag v1.3.9 (f59bd38d)
# - grouped-categories.js: blacklabel/grouped_categories tag v1.3.4 (568174d5)
# - highcharts-regression.js: streamlinesocial/highcharts-regression tag v2.2.0
#   (568326a6)
files <- c(
  # "http://blacklabel.github.io/multicolor_series/js/multicolor_series.js",
  "https://raw.githubusercontent.com/larsac07/Motion-Highcharts-Plugin/036338d0a948a832faf71f7163d0f764f19d5aaf/motion.js",
  "https://raw.githubusercontent.com/highcharts/draggable-legend/f59bd38d2781bd5723896bbd8dc73e9bd376391b/draggable-legend.js",
  # "https://raw.githubusercontent.com/rudovjan/highcharts-tooltip-delay/master/tooltip-delay.js",
  "https://raw.githubusercontent.com/blacklabel/grouped_categories/568174d582500b9e32be3e90ce0bde550b9f3603/grouped-categories.js",
  "https://raw.githubusercontent.com/streamlinesocial/highcharts-regression/568326a6e73ca9346deef1a6d194376ef41ea4bd/highcharts-regression.js"
  # "https://raw.githubusercontent.com/highcharts/pattern-fill/master/pattern-fill-v2.js",
  # "https://raw.githubusercontent.com/blacklabel/annotations/master/js/annotations.js",
  # "https://raw.githubusercontent.com/highcharts/export-csv/master/export-csv.js",
  # "https://raw.githubusercontent.com/highcharts/draggable-points/master/draggable-points.js",
  )

walk2(
  files,
  file.path(path_temp, "plugins", basename(files)),
  download.file
)

# customs -----------------------------------------------------------------
file.copy(
  dir(file.path(path, "custom"), full.names = TRUE),
  str_replace(dir(file.path(path, "custom"), full.names = TRUE), path, path_temp),
  overwrite = TRUE
)

file.copy(
  dir(file.path(path, "css"), full.names = TRUE),
  str_replace(dir(file.path(path, "css"), full.names = TRUE), path, path_temp),
  overwrite = TRUE
)

# moving ------------------------------------------------------------------
try(fs::dir_delete(path))

try(fs::dir_create(path))

try(map(file.path(path, folders), fs::dir_create))

file.copy(
  dir(file.path(path_temp), full.names = TRUE, recursive = TRUE),
  str_replace(dir(file.path(path_temp), full.names = TRUE, recursive = TRUE), path_temp, path),
  overwrite = TRUE
)

try(fs::dir_delete(path_temp))

# change the version on yaml ----------------------------------------------
walk(1:5, ~ message("change version on inst/htmlwidgets/highchart.yaml and inst/htmlwidgets/highchartzero.yaml"))
walk(1:5, ~ message("Line 11 in inst/htmlwidgets/highchart.yaml"))
