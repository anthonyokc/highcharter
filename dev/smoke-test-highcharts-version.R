# Smoke test for the bundled Highcharts version -------------------------------
#
# Installs the package source tree into a temporary library, saves seven
# widgets with `htmlwidgets::saveWidget(selfcontained = FALSE)`, opens each page
# in headless Chrome through chromote, and stops with an error when a check
# fails.
#
# Run from the package root:
#
#   CHROMOTE_CHROME=/path/to/chromium \
#     Rscript dev/smoke-test-highcharts-version.R
#
# Optional positional arguments:
#
#   1. path to the package source tree (default ".")
#   2. expected `Highcharts.version` (default: the highcharts `version` in
#      inst/htmlwidgets/highchart.yaml of that source tree)
#
# Each page must report the expected `Highcharts.version`, render its chart, and
# log no console error, uncaught exception, or browser log error. Two pages also
# check the security fixes:
#
# - "credits": `credits.href = "javascript:..."` must not run when the credits
#   text is clicked (GHSA-5793-x2q3-c3q7, fixed in Highcharts 12.6.1). The
#   check fails if the trusted mouse click does not reach the credits element.
# - "font": a theme font name that contains markup must not add elements to
#   the page (the font link in inst/htmlwidgets/highchart.js), and the font
#   name "Open Sans" must still produce the URL `css?family=Open+Sans`.
#
# The browser blocks requests to fonts.googleapis.com, so the test needs no
# network access. A blocked stylesheet request on the font page is reported but
# does not fail the test.
#
# If Chromium comes from Nix, R's LD_LIBRARY_PATH can break the Nix wrapper.
# Point CHROMOTE_CHROME at a script that runs `unset LD_LIBRARY_PATH` and then
# `exec`s the Nix chromium binary.

library(purrr)
library(fs)

# arguments -------------------------------------------------------------------
read_yaml_highcharts_version <- function(pkg_path) {
  yaml_path <- path(pkg_path, "inst", "htmlwidgets", "highchart.yaml")

  versions <- yaml::read_yaml(yaml_path)$dependencies |>
    keep(\(dep) identical(dep$name, "highcharts")) |>
    map_chr(\(dep) as.character(dep$version))

  if (length(versions) != 1) {
    stop("highchart.yaml must list exactly one highcharts dependency.")
  }

  versions
}

smoke_args <- commandArgs(trailingOnly = TRUE)

pkg_path <- smoke_args[1] |>
  dplyr::coalesce(".") |>
  path_real()

yaml_version <- read_yaml_highcharts_version(pkg_path)
expected_version <- dplyr::coalesce(smoke_args[2], yaml_version)

chrome_path <- Sys.getenv("CHROMOTE_CHROME")
if (!nzchar(chrome_path) || !file_exists(chrome_path)) {
  stop("Set CHROMOTE_CHROME to the path of a Chrome or Chromium executable.")
}

# install the source tree -----------------------------------------------------
# The test must load the package from the source tree under test, not from a
# user library, so it installs that tree into a temporary library.
install_source_tree <- function(pkg_path, lib_path) {
  dir_create(lib_path)

  utils::install.packages(
    pkg_path,
    lib = lib_path,
    repos = NULL,
    type = "source",
    quiet = TRUE
  )

  loadNamespace(
    "highcharter",
    lib.loc = lib_path
  )

  loaded_path <- getNamespaceInfo("highcharter", "path") |>
    path_real()
  installed_path <- path(lib_path, "highcharter") |>
    path_real()

  if (!identical(loaded_path, installed_path)) {
    stop("highcharter loaded from ", loaded_path, ", not ", installed_path)
  }

  invisible(lib_path)
}

smoke_dir <- file_temp("highcharter-smoke-")
smoke_lib <- path(smoke_dir, "lib-r")
smoke_html_dir <- path(smoke_dir, "html")
dir_create(smoke_html_dir)

install_source_tree(
  pkg_path = pkg_path,
  lib_path = smoke_lib
)

# widgets ---------------------------------------------------------------------
build_square_feature <- function(x0, code) {
  ring_x <- c(
    x0,
    x0 + 10,
    x0 + 10,
    x0,
    x0
  )
  ring_y <- c(
    0,
    0,
    10,
    10,
    0
  )
  ring_list <- map2(ring_x, ring_y, c)

  list(
    type = "Feature",
    properties = list(
      code = code
    ),
    geometry = list(
      type = "Polygon",
      coordinates = list(
        ring_list
      )
    )
  )
}

build_basic_widget <- function() {
  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = c(
        1,
        4,
        2
      ),
      type = "column",
      name = "Basic"
    )
}

build_stock_widget <- function() {
  start_date <- as.Date("2024-01-01")
  days <- seq(
    start_date,
    by = "day",
    length.out = 60
  )
  day_index <- seq_along(days)
  price_values <- sin(day_index / 5) + 100
  prices <- xts::xts(
    price_values,
    order.by = days
  )

  highcharter::hchart(
    prices,
    name = "Price"
  )
}

build_map_widget <- function() {
  geojson_list <- list(
    type = "FeatureCollection",
    features = list(
      build_square_feature(0, "A"),
      build_square_feature(10, "B")
    )
  )
  map_data <- data.frame(
    code = c(
      "A",
      "B"
    ),
    value = c(
      1,
      2
    )
  )

  highcharter::highchart() |>
    highcharter::hc_add_series_map(
      map = geojson_list,
      df = map_data,
      value = "value",
      joinBy = "code"
    )
}

build_gantt_task <- function(name, start, end) {
  start_ms <- as.Date(start) |>
    highcharter::datetime_to_timestamp()
  end_ms <- as.Date(end) |>
    highcharter::datetime_to_timestamp()

  list(
    name = name,
    start = start_ms,
    end = end_ms
  )
}

build_gantt_widget <- function() {
  task_list <- list(
    build_gantt_task("Plan", "2024-01-01", "2024-01-10"),
    build_gantt_task("Build", "2024-01-08", "2024-01-20")
  )

  highcharter::highchart(type = "gantt") |>
    highcharter::hc_add_series(
      name = "Project",
      data = task_list
    )
}

build_motion_widget <- function() {
  frame_list <- list(
    list(
      sequence = c(
        1,
        1,
        1,
        1
      )
    ),
    list(
      sequence = c(
        NA,
        2,
        2,
        2
      )
    ),
    list(
      sequence = c(
        NA,
        NA,
        5,
        5
      )
    )
  )

  highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_add_series(data = frame_list) |>
    highcharter::hc_motion(
      enabled = TRUE,
      labels = 1:4,
      series = 0
    )
}

build_credits_widget <- function() {
  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = c(
        3,
        1,
        2
      ),
      name = "Credits"
    ) |>
    highcharter::hc_credits(
      enabled = TRUE,
      text = "credits-link",
      href = "javascript:void(window.hcCreditsPwned=1)"
    )
}

build_font_widget <- function() {
  font_payload <- 'Roboto"><img/src="x"/onerror="window.hcFontPwned=1">'
  theme <- highcharter::hc_theme(
    chart = list(
      style = list(
        fontFamily = font_payload
      )
    ),
    title = list(
      style = list(
        fontFamily = "Open Sans"
      )
    )
  )

  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = c(
        2,
        2,
        3
      ),
      name = "Font"
    ) |>
    highcharter::hc_add_theme(theme)
}

save_smoke_widget <- function(widget, name, html_dir) {
  html_path <- path(html_dir, name, ext = "html")

  htmlwidgets::saveWidget(
    widget,
    file = html_path,
    selfcontained = FALSE,
    libdir = "lib"
  )

  html_path
}

# page checks -----------------------------------------------------------------
# Each check is a JavaScript expression about `chart`, the first chart on the
# page. It returns true when the page passes.
page_check_list <- list(
  basic = paste(
    "chart.series[0].type === 'column' &&",
    "chart.series[0].points.length === 3"
  ),
  stock = paste(
    "typeof chart.navigator === 'object' &&",
    "chart.series[0].points.length > 0"
  ),
  map = paste(
    "chart.series[0].type === 'map' &&",
    "chart.series[0].points.length === 2"
  ),
  gantt = paste(
    "chart.series[0].type === 'gantt' &&",
    "chart.series[0].points.length === 2"
  ),
  motion = paste(
    "typeof chart.motion === 'object' &&",
    "document.getElementById('play-controls') !== null"
  ),
  credits = "document.querySelector('.highcharts-credits') !== null",
  font = paste(
    "chart.series[0].points.length === 3 &&",
    "document.querySelector('link[href=\"",
    "https://fonts.googleapis.com/css?family=Open+Sans",
    "\"]') !== null",
    sep = ""
  )
)

evaluate_js <- function(session, expression) {
  result <- session$Runtime$evaluate(
    expression,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )

  if (!is.null(result$exceptionDetails)) {
    stop("JavaScript evaluation failed: ", expression)
  }

  result$result$value
}

wait_for_chart_js <- "
new Promise(function(resolve) {
  var started = Date.now();
  (function poll() {
    var ready = window.Highcharts && Highcharts.charts.some(function(chart) {
      return chart && chart.hasRendered;
    });
    if (ready) return resolve(true);
    if (Date.now() - started > 15000) return resolve(false);
    setTimeout(poll, 100);
  })();
})
"

open_smoke_page <- function(session, html_path) {
  page_url <- paste0("file://", html_path)
  loaded <- session$Page$loadEventFired(wait_ = FALSE)
  session$Page$navigate(
    page_url,
    wait_ = FALSE
  )
  session$wait_for(loaded)

  evaluate_js(session, wait_for_chart_js)
}

# The widget resizes after the first render, which moves the credits text.
# Wait until its position stays the same for three polls, then record whether
# a click reaches the credits element.
credits_center_js <- "
new Promise(function(resolve) {
  var started = Date.now();
  var last = null;
  var stable = 0;
  (function poll() {
    var el = document.querySelector('.highcharts-credits');
    var r = el.getBoundingClientRect();
    var center = { x: r.left + r.width / 2, y: r.top + r.height / 2 };
    var same = last && last.x === center.x && last.y === center.y;
    stable = same ? stable + 1 : 0;
    last = center;
    if (stable >= 3 || Date.now() - started > 5000) {
      window.hcCreditsClicked = false;
      document.addEventListener('click', function(event) {
        if (el.contains(event.target)) window.hcCreditsClicked = true;
      }, true);
      return resolve(center);
    }
    setTimeout(poll, 150);
  })();
})
"

click_credits <- function(session) {
  center <- evaluate_js(session, credits_center_js)

  walk(
    c(
      "mousePressed",
      "mouseReleased"
    ),
    \(event_type) {
      session$Input$dispatchMouseEvent(
        type = event_type,
        x = center$x,
        y = center$y,
        button = "left",
        clickCount = 1
      )
    }
  )

  # A javascript: URL assigned to location.href runs asynchronously.
  evaluate_js(session, "new Promise(function(r) { setTimeout(r, 1000); })")
}

security_flags_js <- "
({
  creditsClicked: window.hcCreditsClicked === true,
  creditsPwned: window.hcCreditsPwned === 1,
  fontPwned: window.hcFontPwned === 1,
  injectedImages: document.querySelectorAll('img').length,
  fontLinks: Array.prototype.map.call(
    document.querySelectorAll('link[href^=\"https://fonts.googleapis.com/\"]'),
    function(link) { return link.getAttribute('href'); }
  )
})
"

collect_security_flags <- function(session, name) {
  if (identical(name, "credits")) {
    click_credits(session)
  }

  evaluate_js(session, security_flags_js)
}

format_console_arg <- function(arg) {
  description <- pluck(arg, "description", .default = "")

  pluck(arg, "value", .default = description) |>
    format()
}

new_error_log <- function(session) {
  error_log <- new.env()
  error_log$entries <- character()

  add_entry <- function(...) {
    error_log$entries <- c(
      error_log$entries,
      paste(...)
    )
  }

  empty_arg_list <- list()

  session$Runtime$consoleAPICalled(callback_ = \(event) {
    event_type <- pluck(event, "type")
    if (identical(event_type, "error")) {
      message_text <- pluck(event, "args", .default = empty_arg_list) |>
        map_chr(format_console_arg) |>
        paste(collapse = " ")
      add_entry("console.error:", message_text)
    }
  })

  session$Runtime$exceptionThrown(callback_ = \(event) {
    details <- pluck(event, "exceptionDetails")
    exception_text <- pluck(details, "text", .default = "")
    add_entry(
      "exception:",
      pluck(details, "exception", "description", .default = exception_text)
    )
  })

  session$Log$entryAdded(callback_ = \(event) {
    entry <- pluck(event, "entry")
    entry_level <- pluck(entry, "level")
    if (identical(entry_level, "error")) {
      add_entry(
        "log:",
        pluck(entry, "text", .default = ""),
        pluck(entry, "url", .default = "")
      )
    }
  })

  error_log
}

is_allowed_error <- function(entry, name) {
  identical(name, "font") &&
    grepl("https://fonts.googleapis.com/", entry, fixed = TRUE)
}

run_page_check <- function(session, check_js) {
  paste0(
    "(function() { var chart = Highcharts.charts.filter(Boolean)[0]; return ",
    check_js,
    "; })()"
  ) |>
    evaluate_js(session = session) |>
    isTRUE()
}

inspect_smoke_page <- function(
  session,
  name,
  html_path,
  check_js,
  expected_version
) {
  blocked_url_list <- list(
    "*fonts.googleapis.com*"
  )

  session$Runtime$enable()
  session$Log$enable()
  session$Network$enable()
  session$Network$setBlockedURLs(urls = blocked_url_list)
  error_log <- new_error_log(session)

  rendered <- open_smoke_page(session, html_path)
  version <- evaluate_js(
    session,
    "window.Highcharts ? Highcharts.version : null"
  )
  check_passed <- run_page_check(session, check_js)
  flags <- collect_security_flags(session, name)

  errors <- error_log$entries
  is_blocking <- !map_lgl(errors, \(entry) is_allowed_error(entry, name))
  font_links <- flags$fontLinks |>
    unlist() |>
    paste(collapse = " ")

  list(
    page = name,
    version = version %||% NA_character_,
    version_ok = identical(version, expected_version),
    rendered = isTRUE(rendered),
    check_passed = check_passed,
    credits_missed = identical(name, "credits") &&
      !isTRUE(flags$creditsClicked),
    credits_pwned = isTRUE(flags$creditsPwned),
    font_pwned = isTRUE(flags$fontPwned),
    injected_images = flags$injectedImages,
    font_links = font_links,
    errors = errors,
    blocking_errors = errors[is_blocking]
  )
}

describe_failures <- function(result, expected_version) {
  version_text <- paste0(
    "Highcharts.version is ",
    result$version,
    ", expected ",
    expected_version
  )

  c(
    if (!result$version_ok) {
      version_text
    },
    if (!result$rendered) {
      "no chart finished rendering"
    },
    if (!result$check_passed) {
      "the page check failed"
    },
    if (result$credits_missed) {
      "the click did not reach the credits element"
    },
    if (result$credits_pwned) {
      "credits.href javascript: URL ran"
    },
    if (result$font_pwned) {
      "font name markup ran script"
    },
    if (result$injected_images > 0) {
      "font name markup added an <img> element"
    },
    result$blocking_errors
  )
}

summarise_result <- function(result) {
  data.frame(
    page = result$page,
    version = result$version,
    rendered = result$rendered,
    check = result$check_passed,
    credits_missed = result$credits_missed,
    credits_pwned = result$credits_pwned,
    font_pwned = result$font_pwned,
    errors = length(result$errors),
    blocking = length(result$blocking_errors)
  )
}

# run -------------------------------------------------------------------------
widget_list <- list(
  basic = build_basic_widget(),
  stock = build_stock_widget(),
  map = build_map_widget(),
  gantt = build_gantt_widget(),
  motion = build_motion_widget(),
  credits = build_credits_widget(),
  font = build_font_widget()
)

html_paths <- imap_chr(
  widget_list,
  \(widget, name) save_smoke_widget(widget, name, smoke_html_dir)
)

chrome <- chromote::Chrome$new(path = chrome_path)
chromote <- chromote::Chromote$new(browser = chrome)

result_list <- imap(
  html_paths,
  \(html_path, name) {
    session <- chromote$new_session()
    page_result_list <- inspect_smoke_page(
      session = session,
      name = name,
      html_path = html_path,
      check_js = page_check_list[[name]],
      expected_version = expected_version
    )
    session$close()
    page_result_list
  }
)

chromote$close()

failure_list <- result_list |>
  map(\(result) describe_failures(result, expected_version)) |>
  compact()

summary_data <- result_list |>
  map(summarise_result) |>
  list_rbind()

message("Source tree: ", pkg_path)
message("Expected Highcharts.version: ", expected_version)
print(summary_data, row.names = FALSE)

result_list |>
  keep(\(result) length(result$errors) > 0) |>
  iwalk(\(result, name) {
    log_text <- paste(result$errors, collapse = "\n  ")
    message(name, " page log:\n  ", log_text)
  })

message("font page link: ", result_list$font$font_links)

if (length(failure_list) > 0) {
  iwalk(
    failure_list,
    \(messages, name) {
      failure_text <- paste(messages, collapse = "; ")
      message("FAIL ", name, ": ", failure_text)
    }
  )
  failed_pages <- failure_list |>
    names() |>
    paste(collapse = ", ")
  stop("Highcharts smoke test failed for: ", failed_pages)
}

page_count <- length(result_list)
message("Highcharts smoke test passed for ", page_count, " pages.")
