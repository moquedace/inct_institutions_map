required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "janitor",
  "leaflet",
  "htmlwidgets",
  "htmltools",
  "scales",
  "tibble",
  "tidyr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

rm(list = ls())
gc()

# Parameters

project_dir <- "D:/usuario_armazenamento/cassio/projects/inct_institutions_map"
setwd(project_dir)

input_dir <- file.path("data", "input")
output_dir <- file.path("output")

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

researcher_file <- file.path(input_dir, "inct_researchers_institutions.csv")

output_html <- file.path(output_dir, "inct_institutions_interactive_map.html")
output_institution_summary <- file.path(output_dir, "inct_institution_summary.csv")
output_region_summary <- file.path(output_dir, "inct_region_summary.csv")
output_biome_summary <- file.path(output_dir, "inct_biome_summary.csv")

cluster_institution_points <- F
show_cluster_legend <- FALSE

initial_longitude <- -54
initial_latitude <- -14
initial_zoom <- 4

institution_radius_min <- 5
institution_radius_max <- 34

max_researchers_in_popup <- 20

regional_layer_name <- "INCT institutions by regional group"
biome_layer_name <- "Brazilian institutions by biome"

expected_columns <- c(
  "researcher_name",
  "proposal_role",
  "institution",
  "institution_abbreviation",
  "institution_scope",
  "country",
  "state_code",
  "city",
  "institution_website",
  "brazilian_biome",
  "brazilian_region",
  "latitude",
  "longitude"
)

region_colors <- c(
  "North" = "#1B9E77",
  "Northeast" = "#D95F02",
  "Central-West" = "#7570B3",
  "Southeast" = "#E7298A",
  "South" = "#66A61E",
  "International" = "#4D4D4D",
  "Not informed" = "#BDBDBD"
)

biome_colors <- c(
  "Amazon" = "#1B9E77",
  "Caatinga" = "#D95F02",
  "Cerrado" = "#7570B3",
  "Atlantic Forest" = "#66A61E",
  "Pampa" = "#E7298A",
  "Pantanal" = "#A6761D",
  "Not informed" = "#BDBDBD"
)

cluster_color <- "#3B3B3B"

# Template file

if (!file.exists(researcher_file)) {
  researcher_template <- tibble::tibble(
    researcher_name = character(),
    proposal_role = character(),
    institution = character(),
    institution_abbreviation = character(),
    institution_scope = character(),
    country = character(),
    state_code = character(),
    city = character(),
    institution_website = character(),
    brazilian_biome = character(),
    brazilian_region = character(),
    latitude = character(),
    longitude = character()
  )
  
  readr::write_csv2(
    x = researcher_template,
    file = researcher_file
  )
  
  stop(
    paste0(
      "Input template created. Fill the file and run again: ",
      normalizePath(researcher_file, winslash = "/")
    )
  )
}

# Helper functions

read_delimited_file <- function(file) {
  first_line <- readLines(
    con = file,
    n = 1,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  delimiter_count <- c(
    semicolon = stringr::str_count(first_line, ";"),
    comma = stringr::str_count(first_line, ","),
    tab = stringr::str_count(first_line, "\t")
  )
  
  delimiter <- names(delimiter_count)[which.max(delimiter_count)]
  
  delimiter_value <- dplyr::case_when(
    delimiter == "semicolon" ~ ";",
    delimiter == "comma" ~ ",",
    delimiter == "tab" ~ "\t",
    TRUE ~ ";"
  )
  
  readr::read_delim(
    file = file,
    delim = delimiter_value,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
}

normalize_empty <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "-")] <- NA_character_
  x
}

normalize_key <- function(x) {
  x <- normalize_empty(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x
}

parse_coordinate_value <- function(x) {
  x <- normalize_empty(x)
  x <- stringr::str_replace_all(x, "\\s+", "")
  x <- stringr::str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

normalize_url <- function(x) {
  x <- normalize_empty(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(x, "^https?://") ~ x,
    TRUE ~ paste0("https://", x)
  )
}

normalize_scope_value <- function(scope, country) {
  scope_key <- normalize_key(scope)
  country_key <- normalize_key(country)
  
  dplyr::case_when(
    stringr::str_detect(scope_key, "brasil|brazil|brazilian") ~ "Brazil",
    country_key %in% c("brasil", "brazil") ~ "Brazil",
    stringr::str_detect(scope_key, "internacional|international") ~ "International",
    !is.na(country_key) & !country_key %in% c("brasil", "brazil") ~ "International",
    TRUE ~ "Not informed"
  )
}

infer_region_from_state <- function(state_code) {
  state_code <- stringr::str_to_upper(normalize_empty(state_code))
  
  dplyr::case_when(
    state_code %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "North",
    state_code %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Northeast",
    state_code %in% c("DF", "GO", "MT", "MS") ~ "Central-West",
    state_code %in% c("ES", "MG", "RJ", "SP") ~ "Southeast",
    state_code %in% c("PR", "RS", "SC") ~ "South",
    TRUE ~ NA_character_
  )
}

normalize_region_value <- function(x) {
  x_original <- normalize_empty(x)
  x_key <- normalize_key(x)
  
  dplyr::case_when(
    x_key %in% c("norte", "north") ~ "North",
    x_key %in% c("nordeste", "northeast") ~ "Northeast",
    x_key %in% c("centro oeste", "centro-oeste", "central west", "central-west", "midwest") ~ "Central-West",
    x_key %in% c("sudeste", "southeast") ~ "Southeast",
    x_key %in% c("sul", "south") ~ "South",
    is.na(x_key) ~ NA_character_,
    TRUE ~ x_original
  )
}

normalize_biome_value <- function(x) {
  x_original <- normalize_empty(x)
  x_key <- normalize_key(x)
  
  dplyr::case_when(
    x_key %in% c("amazonia", "amazon", "amazonas", "amazon biome") ~ "Amazon",
    x_key %in% c("caatinga") ~ "Caatinga",
    x_key %in% c("cerrado") ~ "Cerrado",
    x_key %in% c("mata atlantica", "atlantic forest") ~ "Atlantic Forest",
    x_key %in% c("pampa", "pampas") ~ "Pampa",
    x_key %in% c("pantanal") ~ "Pantanal",
    is.na(x_key) ~ NA_character_,
    TRUE ~ x_original
  )
}

first_non_empty <- function(x) {
  x <- normalize_empty(x)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[1]
}

collapse_preview_values <- function(x, max_items = 20, separator = "; ") {
  x <- normalize_empty(x)
  x <- sort(unique(x[!is.na(x)]))
  
  if (length(x) == 0) {
    return("Not informed")
  }
  
  if (length(x) <= max_items) {
    return(paste(x, collapse = separator))
  }
  
  paste0(
    paste(x[seq_len(max_items)], collapse = separator),
    separator,
    "... +",
    length(x) - max_items,
    " more"
  )
}

summarize_counts <- function(x) {
  x <- normalize_empty(x)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return("Not informed")
  }
  
  count_table <- sort(table(x), decreasing = TRUE)
  
  paste0(
    names(count_table),
    ": ",
    as.integer(count_table),
    collapse = "; "
  )
}

scale_marker_radius <- function(x, min_radius, max_radius) {
  x <- as.numeric(x)
  valid_x <- x[!is.na(x)]
  
  if (length(valid_x) == 0) {
    return(rep(min_radius, length(x)))
  }
  
  if (length(unique(valid_x)) == 1) {
    return(rep(mean(c(min_radius, max_radius)), length(x)))
  }
  
  radius <- scales::rescale(
    x,
    to = c(min_radius, max_radius),
    from = range(valid_x, na.rm = TRUE)
  )
  
  radius[is.na(radius)] <- min_radius
  
  radius
}

safe_text <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Not informed"
  htmltools::htmlEscape(x)
}

safe_number <- function(x) {
  ifelse(
    is.na(x),
    "Not informed",
    scales::comma(as.numeric(x), accuracy = 1)
  )
}

safe_link <- function(x) {
  x <- normalize_url(x)
  
  ifelse(
    is.na(x) | x == "",
    "Not informed",
    paste0(
      "<a href='",
      htmltools::htmlEscape(x),
      "' target='_blank'>",
      htmltools::htmlEscape(x),
      "</a>"
    )
  )
}

make_cluster_options <- function(cluster_color) {
  leaflet::markerClusterOptions(
    showCoverageOnHover = FALSE,
    zoomToBoundsOnClick = TRUE,
    spiderfyOnMaxZoom = TRUE,
    removeOutsideVisibleBounds = TRUE,
    iconCreateFunction = htmlwidgets::JS(
      paste0(
        "function(cluster) {",
        "var count = cluster.getChildCount();",
        "var size = count < 10 ? 34 : count < 100 ? 42 : 50;",
        "return new L.DivIcon({",
        "html: '<div style=\"",
        "background:", cluster_color, ";",
        "color:white;",
        "width:' + size + 'px;",
        "height:' + size + 'px;",
        "line-height:' + size + 'px;",
        "border-radius:50%;",
        "text-align:center;",
        "font-weight:700;",
        "font-family:Arial, sans-serif;",
        "border:2px solid white;",
        "box-shadow:0 1px 6px rgba(0,0,0,0.35);",
        "\">' + count + '</div>',",
        "className: 'custom-marker-cluster',",
        "iconSize: new L.Point(size, size)",
        "});",
        "}"
      )
    )
  )
}

make_color_legend <- function(title, colors) {
  legend_items <- paste0(
    "<div style='display:flex; align-items:center; margin-bottom:4px;'>",
    "<span style='",
    "display:inline-block;",
    "width:12px;",
    "height:12px;",
    "border-radius:50%;",
    "background:",
    unname(colors),
    ";",
    "margin-right:7px;",
    "border:1px solid rgba(0,0,0,0.25);",
    "'></span>",
    "<span>",
    htmltools::htmlEscape(names(colors)),
    "</span>",
    "</div>",
    collapse = ""
  )
  
  paste0(
    "<div style='",
    "background: rgba(255,255,255,0.94);",
    "padding: 10px 12px;",
    "border-radius: 8px;",
    "box-shadow: 0 1px 6px rgba(0,0,0,0.25);",
    "font-family: Arial, sans-serif;",
    "font-size: 13px;",
    "line-height: 1.25;",
    "'>",
    "<div style='font-weight:700; margin-bottom:7px;'>",
    htmltools::htmlEscape(title),
    "</div>",
    legend_items,
    "</div>"
  )
}

# Read input data

researcher_data <- read_delimited_file(researcher_file) %>%
  janitor::clean_names()

missing_columns <- setdiff(
  expected_columns,
  names(researcher_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "Missing columns in researcher file: ",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Clean input data

researcher_data <- researcher_data %>%
  mutate(
    across(
      .cols = all_of(expected_columns),
      .fns = normalize_empty
    ),
    institution_scope = normalize_scope_value(
      scope = institution_scope,
      country = country
    ),
    latitude = parse_coordinate_value(latitude),
    longitude = parse_coordinate_value(longitude),
    institution_website = normalize_url(institution_website),
    brazilian_region = normalize_region_value(brazilian_region),
    inferred_region = infer_region_from_state(state_code),
    brazilian_region = dplyr::case_when(
      institution_scope == "Brazil" & !is.na(brazilian_region) ~ brazilian_region,
      institution_scope == "Brazil" & is.na(brazilian_region) ~ inferred_region,
      TRUE ~ NA_character_
    ),
    brazilian_region = dplyr::case_when(
      institution_scope == "Brazil" & is.na(brazilian_region) ~ "Not informed",
      institution_scope == "Brazil" ~ brazilian_region,
      TRUE ~ NA_character_
    ),
    brazilian_biome = normalize_biome_value(brazilian_biome),
    brazilian_biome = dplyr::case_when(
      institution_scope == "Brazil" & is.na(brazilian_biome) ~ "Not informed",
      institution_scope == "Brazil" ~ brazilian_biome,
      TRUE ~ NA_character_
    ),
    map_region = dplyr::case_when(
      institution_scope == "Brazil" ~ brazilian_region,
      institution_scope == "International" ~ "International",
      TRUE ~ "Not informed"
    )
  ) %>%
  select(
    -inferred_region
  ) %>%
  filter(
    !is.na(researcher_name),
    !is.na(institution),
    !is.na(latitude),
    !is.na(longitude),
    latitude >= -90,
    latitude <= 90,
    longitude >= -180,
    longitude <= 180
  )

if (nrow(researcher_data) == 0) {
  stop("No valid researcher records with institution and coordinates were found.")
}

# Aggregate by institution

institution_data <- researcher_data %>%
  group_by(
    institution,
    institution_abbreviation,
    institution_scope,
    country,
    state_code,
    city,
    latitude,
    longitude
  ) %>%
  summarise(
    institution_website = first_non_empty(institution_website),
    brazilian_region = first_non_empty(brazilian_region),
    brazilian_biome = first_non_empty(brazilian_biome),
    map_region = first_non_empty(map_region),
    researcher_count = n_distinct(researcher_name),
    record_count = dplyr::n(),
    proposal_role_summary = summarize_counts(proposal_role),
    researcher_names = collapse_preview_values(
      researcher_name,
      max_items = max_researchers_in_popup
    ),
    .groups = "drop"
  ) %>%
  mutate(
    institution_abbreviation = ifelse(
      is.na(institution_abbreviation),
      "Not informed",
      institution_abbreviation
    ),
    country = ifelse(
      is.na(country),
      "Not informed",
      country
    ),
    state_code = ifelse(
      is.na(state_code),
      "Not informed",
      state_code
    ),
    city = ifelse(
      is.na(city),
      "Not informed",
      city
    ),
    brazilian_region = ifelse(
      institution_scope == "Brazil" & is.na(brazilian_region),
      "Not informed",
      brazilian_region
    ),
    brazilian_biome = ifelse(
      institution_scope == "Brazil" & is.na(brazilian_biome),
      "Not informed",
      brazilian_biome
    ),
    map_region = ifelse(
      is.na(map_region),
      "Not informed",
      map_region
    ),
    marker_radius = scale_marker_radius(
      x = researcher_count,
      min_radius = institution_radius_min,
      max_radius = institution_radius_max
    ),
    region_color = unname(region_colors[map_region]),
    region_color = ifelse(
      is.na(region_color),
      unname(region_colors["Not informed"]),
      region_color
    )
  )

brazilian_institution_data <- institution_data %>%
  filter(
    institution_scope == "Brazil"
  ) %>%
  mutate(
    map_biome = ifelse(
      is.na(brazilian_biome),
      "Not informed",
      brazilian_biome
    ),
    biome_marker_radius = marker_radius,
    biome_color = unname(biome_colors[map_biome]),
    biome_color = ifelse(
      is.na(biome_color),
      unname(biome_colors["Not informed"]),
      biome_color
    )
  )

# Summaries

institution_summary <- institution_data %>%
  arrange(
    desc(researcher_count),
    institution
  )

region_summary <- institution_data %>%
  group_by(
    map_region
  ) %>%
  summarise(
    institution_count = n_distinct(institution),
    researcher_count = sum(researcher_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(researcher_count)
  )

biome_summary <- brazilian_institution_data %>%
  group_by(
    map_biome
  ) %>%
  summarise(
    institution_count = n_distinct(institution),
    researcher_count = sum(researcher_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(researcher_count)
  )

readr::write_csv2(
  x = institution_summary,
  file = output_institution_summary
)

readr::write_csv2(
  x = region_summary,
  file = output_region_summary
)

readr::write_csv2(
  x = biome_summary,
  file = output_biome_summary
)

# Popups and labels

institution_data <- institution_data %>%
  mutate(
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 13px; line-height:1.35; max-width:360px;'>",
      "<div style='font-size:15px; font-weight:700; margin-bottom:4px;'>",
      safe_text(institution_abbreviation),
      "</div>",
      "<strong>Institution:</strong> ", safe_text(institution), "<br/>",
      "<strong>Scope:</strong> ", safe_text(institution_scope), "<br/>",
      "<strong>Country:</strong> ", safe_text(country), "<br/>",
      "<strong>State:</strong> ", safe_text(state_code), "<br/>",
      "<strong>City:</strong> ", safe_text(city), "<br/>",
      "<strong>Regional group:</strong> ", safe_text(map_region), "<br/>",
      "<strong>Brazilian biome:</strong> ", safe_text(brazilian_biome), "<br/>",
      "<strong>Researchers:</strong> ", safe_number(researcher_count), "<br/>",
      "<strong>Proposal roles:</strong> ", safe_text(proposal_role_summary), "<br/>",
      "<strong>Website:</strong> ", safe_link(institution_website), "<br/>",
      "<strong>Researcher names:</strong><br/>",
      safe_text(researcher_names),
      "</div>"
    ),
    label_text = paste0(
      institution_abbreviation,
      " | ",
      researcher_count,
      " researchers | ",
      map_region
    )
  )

brazilian_institution_data <- brazilian_institution_data %>%
  mutate(
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 13px; line-height:1.35; max-width:360px;'>",
      "<div style='font-size:15px; font-weight:700; margin-bottom:4px;'>",
      safe_text(institution_abbreviation),
      "</div>",
      "<strong>Institution:</strong> ", safe_text(institution), "<br/>",
      "<strong>Brazilian biome:</strong> ", safe_text(map_biome), "<br/>",
      "<strong>Regional group:</strong> ", safe_text(brazilian_region), "<br/>",
      "<strong>State:</strong> ", safe_text(state_code), "<br/>",
      "<strong>City:</strong> ", safe_text(city), "<br/>",
      "<strong>Researchers:</strong> ", safe_number(researcher_count), "<br/>",
      "<strong>Proposal roles:</strong> ", safe_text(proposal_role_summary), "<br/>",
      "<strong>Website:</strong> ", safe_link(institution_website), "<br/>",
      "<strong>Researcher names:</strong><br/>",
      safe_text(researcher_names),
      "</div>"
    ),
    label_text = paste0(
      institution_abbreviation,
      " | ",
      researcher_count,
      " researchers | ",
      map_biome
    )
  )

# Active legends

region_order <- c(
  "North",
  "Northeast",
  "Central-West",
  "Southeast",
  "South",
  "International",
  "Not informed"
)

biome_order <- c(
  "Amazon",
  "Caatinga",
  "Cerrado",
  "Atlantic Forest",
  "Pampa",
  "Pantanal",
  "Not informed"
)

active_region_colors <- region_colors[
  region_order[region_order %in% unique(institution_data$map_region)]
]

active_biome_colors <- biome_colors[
  biome_order[biome_order %in% unique(brazilian_institution_data$map_biome)]
]

region_legend_control <- make_color_legend(
  title = "Regional group",
  colors = active_region_colors
)

biome_legend_control <- make_color_legend(
  title = "Brazilian biome",
  colors = active_biome_colors
)

cluster_legend_html <- if (show_cluster_legend) {
  paste0(
    "<div style='margin-bottom:6px;'>",
    "<span style='display:inline-block; width:18px; height:18px; border-radius:50%; background:",
    cluster_color,
    "; color:white; text-align:center; line-height:18px; font-size:11px; font-weight:700; margin-right:6px;'>n</span>",
    "Cluster number: grouped institutions",
    "</div>",
    "<div style='border-top:1px solid #ddd; margin-top:8px; padding-top:8px;'>"
  )
} else {
  paste0(
    "<div style='border-top:0px solid #ddd; margin-top:0px; padding-top:0px;'>"
  )
}

size_legend_title <- if (show_cluster_legend) {
  "Map symbols"
} else {
  "Circle size"
}

size_legend_control <- paste0(
  "<div style='",
  "background: rgba(255,255,255,0.94);",
  "padding: 10px 12px;",
  "border-radius: 8px;",
  "box-shadow: 0 1px 6px rgba(0,0,0,0.25);",
  "font-family: Arial, sans-serif;",
  "font-size: 13px;",
  "line-height: 1.25;",
  "'>",
  "<div style='font-weight:700; margin-bottom:6px;'>",
  size_legend_title,
  "</div>",
  cluster_legend_html,
  "<div style='font-weight:700; margin-bottom:5px;'>Circle size</div>",
  "<div>Proportional to the number of researchers per institution</div>",
  "</div>",
  "<div style='margin-top:6px;'>",
  "<span style='display:inline-block; width:10px; height:10px; border-radius:50%; background:#666; margin-right:6px;'></span>",
  "Fewer researchers",
  "</div>",
  "<div style='margin-top:4px;'>",
  "<span style='display:inline-block; width:22px; height:22px; border-radius:50%; background:#666; margin-right:6px; vertical-align:middle;'></span>",
  "More researchers",
  "</div>",
  "</div>"
)

# Cluster options

regional_cluster_options <- if (cluster_institution_points) {
  make_cluster_options(cluster_color)
} else {
  NULL
}

biome_cluster_options <- if (cluster_institution_points) {
  make_cluster_options(cluster_color)
} else {
  NULL
}

# Map controls

total_researchers <- sum(institution_data$researcher_count, na.rm = TRUE)
total_institutions <- nrow(institution_data)
total_brazilian_institutions <- sum(institution_data$institution_scope == "Brazil", na.rm = TRUE)
total_international_institutions <- sum(institution_data$institution_scope == "International", na.rm = TRUE)

title_control <- paste0(
  "<div style='",
  "background: rgba(255,255,255,0.94);",
  "padding: 12px 14px;",
  "border-radius: 8px;",
  "box-shadow: 0 1px 6px rgba(0,0,0,0.25);",
  "font-family: Arial, sans-serif;",
  "max-width: 430px;",
  "'>",
  "<div style='font-size: 18px; font-weight: 700; margin-bottom: 4px;'>",
  "INCT Soils of Brazil institutional network",
  "</div>",
  "<div style='font-size: 13px; line-height: 1.35;'>",
  "Participating institutions are grouped by the INCT regional structure. ",
  "Cluster numbers indicate grouped institutions at low zoom. ",
  "Individual circle size is proportional to the number of researchers linked to each institution.",
  "<br/><br/>",
  "<strong>Institutions:</strong> ", safe_number(total_institutions), " | ",
  "<strong>Researchers:</strong> ", safe_number(total_researchers), "<br/>",
  "<strong>Brazilian institutions:</strong> ", safe_number(total_brazilian_institutions), " | ",
  "<strong>International institutions:</strong> ", safe_number(total_international_institutions),
  "</div>",
  "</div>"
)

# Build map

inct_map <- leaflet::leaflet(
  options = leaflet::leafletOptions(
    preferCanvas = TRUE,
    minZoom = 2,
    maxZoom = 18,
    worldCopyJump = FALSE
  )
) %>%
  leaflet::setView(
    lng = initial_longitude,
    lat = initial_latitude,
    zoom = initial_zoom
  ) %>%
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.Positron,
    group = "Light map"
  ) %>%
  leaflet::addProviderTiles(
    provider = leaflet::providers$Esri.WorldImagery,
    group = "Satellite"
  ) %>%
  leaflet::addCircleMarkers(
    data = institution_data,
    lng = ~longitude,
    lat = ~latitude,
    radius = ~marker_radius,
    color = ~region_color,
    fillColor = ~region_color,
    stroke = TRUE,
    weight = 1.2,
    opacity = 1,
    fillOpacity = 0.78,
    popup = ~popup,
    label = ~label_text,
    group = regional_layer_name,
    clusterOptions = regional_cluster_options
  )

if (nrow(brazilian_institution_data) > 0) {
  inct_map <- inct_map %>%
    leaflet::addCircleMarkers(
      data = brazilian_institution_data,
      lng = ~longitude,
      lat = ~latitude,
      radius = ~biome_marker_radius,
      color = ~biome_color,
      fillColor = ~biome_color,
      stroke = TRUE,
      weight = 1.2,
      opacity = 1,
      fillOpacity = 0.78,
      popup = ~popup,
      label = ~label_text,
      group = biome_layer_name,
      clusterOptions = biome_cluster_options
    )
}

inct_map <- inct_map %>%
  leaflet::addLayersControl(
    baseGroups = c(
      "Light map",
      "Satellite"
    ),
    overlayGroups = c(
      regional_layer_name,
      biome_layer_name
    ),
    options = leaflet::layersControlOptions(
      collapsed = FALSE
    )
  ) %>%
  leaflet::addControl(
    html = title_control,
    position = "topleft"
  ) %>%
  leaflet::addControl(
    html = region_legend_control,
    position = "bottomright"
  ) %>%
  leaflet::addControl(
    html = biome_legend_control,
    position = "bottomleft"
  ) %>%
  leaflet::addControl(
    html = size_legend_control,
    position = "topright"
  ) %>%
  leaflet::hideGroup(
    group = biome_layer_name
  )

# Save HTML

save_result <- tryCatch(
  {
    htmlwidgets::saveWidget(
      widget = inct_map,
      file = output_html,
      selfcontained = TRUE,
      title = "INCT Soils of Brazil institutional network"
    )
    
    TRUE
  },
  error = function(e) {
    message("Self-contained HTML failed. Saving with external dependencies.")
    
    htmlwidgets::saveWidget(
      widget = inct_map,
      file = output_html,
      selfcontained = FALSE,
      title = "INCT Soils of Brazil institutional network"
    )
    
    FALSE
  }
)

message(
  paste0(
    "Map successfully created: ",
    normalizePath(output_html, winslash = "/")
  )
)

message(
  paste0(
    "Institution summary created: ",
    normalizePath(output_institution_summary, winslash = "/")
  )
)

message(
  paste0(
    "Region summary created: ",
    normalizePath(output_region_summary, winslash = "/")
  )
)

message(
  paste0(
    "Biome summary created: ",
    normalizePath(output_biome_summary, winslash = "/")
  )
)

inct_map