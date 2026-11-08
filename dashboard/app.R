# ============================================================
# EDUCATION EQUALITY IN INDIA
# Enhanced Shiny Dashboard  (more charts + automatic observations)
# File: dashboard/app.R
# ============================================================

# IMPORTANT:
# Keep this file inside the project's dashboard/ folder.
# Required data files (../data/processed/):
#   cleaned_literacy_2011.csv
#   cleaned_2021_22.csv
#   infrastructure_analysis_2023_24.csv
# Optional (adds a year-on-year growth chart):
#   cleaned_2020_21.csv

# ----------------------------
# 1. Packages
# ----------------------------
packages <- c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "scales")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install these packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# ----------------------------
# 2. Read datasets
# ----------------------------
data_folder <- file.path("..", "data", "processed")

literacy_path <- file.path(data_folder, "cleaned_literacy_2011.csv")
enrolment_path <- file.path(data_folder, "cleaned_2021_22.csv")
enrolment20_path <- file.path(data_folder, "cleaned_2020_21.csv")
infrastructure_path <- file.path(
  data_folder, "infrastructure_analysis_2023_24.csv"
)

paths <- c(literacy_path, enrolment_path, infrastructure_path)
not_found <- paths[!file.exists(paths)]

if (length(not_found) > 0) {
  stop(
    "Data file(s) not found:\n",
    paste(not_found, collapse = "\n"),
    "\nOpen/run app.R from the dashboard folder."
  )
}

literacy_raw <- readr::read_csv(literacy_path, show_col_types = FALSE)
enrolment_raw <- readr::read_csv(enrolment_path, show_col_types = FALSE)
infrastructure_raw <- readr::read_csv(
  infrastructure_path, show_col_types = FALSE
)

# ----------------------------
# 3. Validate expected columns
# ----------------------------
require_columns <- function(data, columns, dataset_label) {
  absent <- setdiff(columns, names(data))
  if (length(absent) > 0) {
    stop(
      dataset_label, " is missing column(s): ",
      paste(absent, collapse = ", ")
    )
  }
}

require_columns(
  literacy_raw,
  c("State", "Male - All", "Female - All", "Persons - All"),
  "Literacy dataset"
)

boys_column <- paste(
  "Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Boys"
)
girls_column <- paste(
  "Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Girls"
)
total_column <- paste(
  "Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Total"
)

require_columns(
  enrolment_raw,
  c("State", boys_column, girls_column, total_column),
  "Enrolment dataset"
)

require_columns(
  infrastructure_raw,
  c(
    "State", "Total_Schools", "Total_Enrolments",
    "Total_Teachers", "PTR", "Zero_Enrolment_Schools",
    "Single_Teacher_Schools", "Zero_Enrolment_Percentage",
    "Single_Teacher_Percentage", "Avg_Enrolments"
  ),
  "Infrastructure dataset"
)

# ----------------------------
# 4. Prepare datasets
# ----------------------------

# Make state names identical across datasets
# (e.g. "Jammu & Kashmir" -> "Jammu and Kashmir")
norm_state <- function(x) {
  x <- stringr::str_trim(gsub("&", "and", as.character(x), fixed = TRUE))
  x <- gsub("\\s+", " ", x)
  x[x == "A and N Islands"] <- "Andaman and Nicobar Islands"
  x
}

literacy_data <- literacy_raw %>%
  transmute(
    State = norm_state(State),
    Male = readr::parse_number(as.character(`Male - All`)),
    Female = readr::parse_number(as.character(`Female - All`)),
    Total = readr::parse_number(as.character(`Persons - All`))
  ) %>%
  filter(
    !is.na(State), State != "",
    toupper(State) != "INDIA"
  ) %>%
  drop_na(Male, Female, Total) %>%
  distinct(State, .keep_all = TRUE) %>%
  mutate(Gender_Gap = Male - Female) %>%
  arrange(State)

# National literacy figures (INDIA row of the census file)
india_lit <- literacy_raw %>%
  filter(toupper(stringr::str_trim(as.character(State))) == "INDIA") %>%
  transmute(
    Male = readr::parse_number(as.character(`Male - All`)),
    Female = readr::parse_number(as.character(`Female - All`)),
    Total = readr::parse_number(as.character(`Persons - All`))
  )
if (nrow(india_lit) == 0) {
  india_lit <- literacy_data %>%
    summarise(Male = mean(Male), Female = mean(Female), Total = mean(Total))
}
india_lit <- india_lit[1, ]

enrolment_data <- enrolment_raw %>%
  transmute(
    State = stringr::str_trim(as.character(State)),
    Boys = readr::parse_number(as.character(.data[[boys_column]])),
    Girls = readr::parse_number(as.character(.data[[girls_column]])),
    Total = readr::parse_number(as.character(.data[[total_column]]))
  ) %>%
  filter(
    !is.na(State), State != "",
    toupper(State) != "INDIA"
  ) %>%
  drop_na(Boys, Girls, Total) %>%
  distinct(State, .keep_all = TRUE) %>%
  mutate(
    Girls_Share = if_else(Total > 0, Girls / Total * 100, NA_real_),
    Gender_Gap = Boys - Girls
  ) %>%
  arrange(State)

# Optional: 2020-21 enrolment (for growth comparison)
enrolment20 <- tryCatch({
  if (!file.exists(enrolment20_path)) {
    NULL
  } else {
    readr::read_csv(enrolment20_path, show_col_types = FALSE) %>%
      transmute(
        State = stringr::str_trim(as.character(State)),
        Boys = readr::parse_number(as.character(.data[[boys_column]])),
        Girls = readr::parse_number(as.character(.data[[girls_column]]))
      ) %>%
      filter(!is.na(State), State != "", toupper(State) != "INDIA") %>%
      drop_na(Boys, Girls) %>%
      distinct(State, .keep_all = TRUE)
  }
}, error = function(e) NULL)

infrastructure_data <- infrastructure_raw %>%
  mutate(State = stringr::str_trim(as.character(State))) %>%
  filter(
    !is.na(State), State != "",
    toupper(State) != "INDIA"
  ) %>%
  distinct(State, .keep_all = TRUE) %>%
  arrange(State)

if (nrow(literacy_data) == 0 ||
    nrow(enrolment_data) == 0 ||
    nrow(infrastructure_data) == 0) {
  stop("At least one dataset has no state-level records after cleaning.")
}

# Literacy + enrolment joined on state name (used for equality analysis)
gender_join <- inner_join(
  literacy_data %>% select(State, Male, Female, Total_Lit = Total, Lit_Gap = Gender_Gap),
  enrolment_data %>% select(State, Girls_Share, Total),
  by = "State"
)

# ----------------------------
# 5. Stage-wise enrolment helpers
# ----------------------------
# Column positions (Boys column; Girls column = position + 1)
stage_cols <- c(
  "Pre-Primary" = 5, "Primary" = 8, "Upper Primary" = 11,
  "Secondary" = 14, "Higher Secondary" = 17
)

enrolment_states_raw <- enrolment_raw %>%
  mutate(State = stringr::str_trim(as.character(State))) %>%
  filter(!is.na(State), State != "", toupper(State) != "INDIA") %>%
  distinct(State, .keep_all = TRUE)

# Boys / girls enrolment for each stage; state = "All" gives the national total
stage_for <- function(state = "All") {
  raw <- enrolment_states_raw
  if (!(state %in% c("All", "All States / UTs"))) {
    raw <- raw %>% filter(State == state)
  }
  boys <- vapply(stage_cols, function(cc) {
    sum(readr::parse_number(as.character(raw[[cc]])), na.rm = TRUE)
  }, numeric(1))
  girls <- vapply(stage_cols, function(cc) {
    sum(readr::parse_number(as.character(raw[[cc + 1]])), na.rm = TRUE)
  }, numeric(1))
  tibble(
    Stage = factor(names(stage_cols), levels = names(stage_cols)),
    Boys = unname(boys),
    Girls = unname(girls)
  ) %>%
    mutate(Girls_Share = if_else(Boys + Girls > 0, Girls / (Boys + Girls) * 100, NA_real_))
}

# State x stage matrix of girls' share (for the heatmap)
stage_share_matrix <- local({
  raw <- enrolment_states_raw
  m <- vapply(stage_cols, function(cc) {
    b <- readr::parse_number(as.character(raw[[cc]]))
    g <- readr::parse_number(as.character(raw[[cc + 1]]))
    ifelse(b + g > 0, g / (b + g) * 100, NA_real_)
  }, numeric(nrow(raw)))
  m <- matrix(m, nrow = nrow(raw), dimnames = list(raw$State, names(stage_cols)))
  m
})

# ------------------------------------------------------------
# 5b. Bollinger Band data: real education indicators per state / UT
# ------------------------------------------------------------
# One row per state / UT with every indicator used in the project.
state_all <- enrolment_data %>%
  select(State, Enrolment = Total, Girls_Share) %>%
  left_join(
    infrastructure_data %>%
      mutate(State = norm_state(State)) %>%
      select(State, Total_Schools, PTR,
             Single_Teacher_Percentage, Zero_Enrolment_Percentage),
    by = "State"
  ) %>%
  left_join(
    literacy_data %>%
      select(State, Male, Female, Total_Lit = Total, Lit_Gap = Gender_Gap),
    by = "State"
  )

bb_indicators <- c(
  "Girls' share of enrolment (%)" = "Girls_Share",
  "Total literacy rate (%)" = "Total_Lit",
  "Female literacy rate (%)" = "Female",
  "Literacy gender gap (points)" = "Lit_Gap",
  "Pupil-teacher ratio" = "PTR",
  "Single-teacher schools (%)" = "Single_Teacher_Percentage",
  "Zero-enrolment schools (%)" = "Zero_Enrolment_Percentage"
)
bb_orders <- c(
  "Total enrolment (largest state first)" = "Enrolment",
  "Number of schools (largest state first)" = "Total_Schools",
  "Alphabetical order" = "State"
)

# Adds mavg / up / dn columns. Uses TTR::BBands when TTR is installed,
# otherwise computes the same bands by hand (population standard deviation).
add_bands <- function(d, n, k) {
  if (requireNamespace("TTR", quietly = TRUE)) {
    bands <- as.data.frame(TTR::BBands(d$Price, n = n, sd = k))
    d$mavg <- bands$mavg
    d$up <- bands$up
    d$dn <- bands$dn
  } else {
    x <- d$Price
    mavg <- rep(NA_real_, length(x))
    sdev <- rep(NA_real_, length(x))
    for (i in seq_along(x)) {
      if (i >= n) {
        w <- x[(i - n + 1):i]
        mavg[i] <- mean(w)
        sdev[i] <- sqrt(mean((w - mean(w))^2))
      }
    }
    d$mavg <- mavg
    d$up <- mavg + k * sdev
    d$dn <- mavg - k * sdev
  }
  d$Width <- (d$up - d$dn) / d$mavg * 100
  d$PctB <- (d$Price - d$dn) / (d$up - d$dn)
  d
}

# ----------------------------
# 6. Summary helpers & choices
# ----------------------------
fmt_big <- function(x) {
  if (is.na(x)) return("N/A")
  if (x >= 1e7) paste0(round(x / 1e7, 2), " Cr")
  else if (x >= 1e5) paste0(round(x / 1e5, 2), " Lakh")
  else format(round(x), big.mark = ",")
}
pct <- function(x, d = 1) paste0(round(x, d), "%")
b <- function(x) paste0("<b>", x, "</b>")
is_all <- function(x) x %in% c("All", "All States / UTs")

number_of_states <- nrow(infrastructure_data)

literacy_choices <- c(
  "All States / UTs" = "All",
  stats::setNames(literacy_data$State, literacy_data$State)
)

enrolment_choices <- c(
  "All States / UTs" = "All",
  stats::setNames(enrolment_data$State, enrolment_data$State)
)

COL <- list(
  boys = "#22d3ee", girls = "#f472b6", purple = "#a78bfa",
  amber = "#fbbf24", rose = "#fb7185", green = "#34d399"
)

# ----------------------------
# 7. Shared chart styling
# ----------------------------
ax <- function(title = "", ...) {
  list(title = title, gridcolor = "#2b384d", zerolinecolor = "#2b384d", ...)
}

dark_plot <- function(p, margin = list(l = 60, r = 25, t = 30, b = 60), ...) {
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#e2e8f0", family = "Poppins, sans-serif"),
      margin = margin,
      ...
    ) %>%
    config(responsive = TRUE, displaylogo = FALSE, displayModeBar = FALSE)
}

obs_list <- function(...) {
  items <- list(...)
  tags$ul(class = "obs", lapply(items, function(x) tags$li(HTML(x))))
}

dt_options <- list(
  pageLength = 10,
  lengthMenu = c(10, 15, 25, 50),
  autoWidth = TRUE,
  scrollX = TRUE,
  dom = "lftip"
)

# ----------------------------
# 8. UI: Header and sidebar
# ----------------------------
header <- dashboardHeader(
  title = "Education Equality India",
  titleWidth = 290
)

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "main_tabs",
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Literacy", tabName = "literacy", icon = icon("book")),
    menuItem("Enrolment", tabName = "enrolment", icon = icon("users")),
    menuItem(
      "Infrastructure",
      tabName = "infrastructure",
      icon = icon("building")
    ),
    menuItem("Gender Equality", tabName = "gender", icon = icon("venus-mars")),
    menuItem("Bollinger Band", tabName = "bollinger", icon = icon("chart-line")),
    menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
  ),
  div(
    class = "side-foot",
    "Sources", br(),
    "Census 2011 • UDISE+ 2020-21", br(),
    "UDISE+ 2021-22 • UDISE+ 2023-24"
  )
)

# ----------------------------
# 9. UI: Dashboard pages
# ----------------------------
page_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap');
body, .content-wrapper, .right-side { background: #0b1220 !important; font-family: 'Poppins', sans-serif; }
h1,h2,h3,h4,h5,.main-header .logo { font-family: 'Poppins', sans-serif !important; }
.main-header .logo, .main-header .navbar { background: #0f172a !important; color: #ffffff !important; }
.main-header .logo { font-weight: 700; letter-spacing: .3px; }
.main-header .navbar .sidebar-toggle:hover { background: #1e293b !important; }
.main-sidebar { background: #111c30 !important; }
.sidebar-menu > li > a { color: #cbd5e1 !important; padding: 13px 18px; font-weight: 500; }
.sidebar-menu > li:hover > a { background: #1c2a44 !important; color: #fff !important; }
.sidebar-menu > li.active > a {
  background: linear-gradient(90deg, #1e3a5f, #1c2a44) !important;
  border-left: 4px solid #22d3ee; color: #ffffff !important;
}
.side-foot { position: absolute; bottom: 16px; left: 18px; font-size: 11px; color: #64748b; line-height: 1.6; }
.content-header > h1, .content h2 { color: #f8fafc !important; font-weight: 700; }
.project-subtitle { color: #94a3b8; font-size: 15px; margin-bottom: 18px; }

/* hero banner */
.hero { border-radius: 18px; padding: 26px 30px; margin-bottom: 20px; color: #fff;
  background: linear-gradient(120deg, #0e7490 0%, #4338ca 55%, #be185d 100%);
  box-shadow: 0 12px 30px rgba(0,0,0,.35); }
.hero h2 { margin: 0 0 6px; color: #fff !important; font-size: 28px; }
.hero p { margin: 0 0 14px; opacity: .92; font-size: 14px; }
.chip { display: inline-block; background: rgba(255,255,255,.18); border-radius: 20px;
  padding: 4px 14px; font-size: 12px; margin: 0 8px 6px 0; }

/* boxes */
.box { background: #16233a !important; color: #f8fafc !important; border-radius: 16px !important;
  border-top: 0 !important; box-shadow: 0 8px 22px rgba(0,0,0,.28) !important; }
.box-header { border-bottom: 1px solid #243350 !important; }
.box-header .box-title { font-size: 15px; font-weight: 600; color: #f1f5f9 !important; }
.box-body, .control-label { color: #e2e8f0 !important; }
.box.box-solid.box-primary > .box-header { background: transparent; }

/* value boxes */
.small-box { border-radius: 16px !important; box-shadow: 0 8px 22px rgba(0,0,0,.3) !important;
  transition: transform .2s; }
.small-box:hover { transform: translateY(-3px); }
.small-box h3 { font-size: 26px !important; font-weight: 700; }
.small-box p { font-size: 12px !important; opacity: .95; }
.small-box .icon { opacity: .25; }
.bg-aqua { background: linear-gradient(135deg, #0891b2, #22d3ee) !important; }
.bg-blue { background: linear-gradient(135deg, #2563eb, #60a5fa) !important; }
.bg-green { background: linear-gradient(135deg, #059669, #34d399) !important; }
.bg-purple { background: linear-gradient(135deg, #7c3aed, #a78bfa) !important; }
.bg-red { background: linear-gradient(135deg, #e11d48, #fb7185) !important; }
.bg-yellow { background: linear-gradient(135deg, #d97706, #fbbf24) !important; }
.bg-maroon { background: linear-gradient(135deg, #be185d, #f472b6) !important; }
.small-box .small-box-footer { display: none; }

/* observations */
ul.obs { list-style: none; padding: 0; margin: 0; display: grid;
  grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); gap: 12px; }
ul.obs li { background: #1b2b47; border-left: 4px solid #22d3ee; border-radius: 12px;
  padding: 12px 16px; font-size: 13.5px; line-height: 1.55; color: #e2e8f0; }
ul.obs li:nth-child(3n+2) { border-left-color: #f472b6; }
ul.obs li:nth-child(3n+3) { border-left-color: #fbbf24; }
ul.obs li b { color: #ffffff; }
.obs-title { font-size: 12px; letter-spacing: 1.4px; text-transform: uppercase; color: #94a3b8; margin-bottom: 10px; }

/* tables & inputs */
.dataTables_wrapper { color: #f8fafc !important; width: 100%; }
table.dataTable { color: #f8fafc !important; width: 100% !important; }
table.dataTable thead th { color: #22d3ee !important; border-bottom: 1px solid #334155 !important; }
table.dataTable tbody tr { background: transparent !important; }
table.dataTable.stripe tbody tr.odd { background: #1a2942 !important; }
table.dataTable.hover tbody tr:hover { background: #24395c !important; }
.dataTables_filter input, .dataTables_length select {
  background: #1e293b !important; color: #ffffff !important;
  border: 1px solid #475569 !important; border-radius: 8px; }
.dataTables_info, .dataTables_paginate, .dataTables_length, .dataTables_filter { color: #f8fafc !important; }
.paginate_button { color: #f8fafc !important; }
.selectize-input, .form-control { background: #1e293b !important; color: #ffffff !important;
  border: 1px solid #475569 !important; border-radius: 10px !important; box-shadow: none !important; }
.selectize-input > .item { color: #fff !important; }
.selectize-dropdown { background: #1e293b !important; color: #ffffff !important; border: 1px solid #475569; }
.selectize-dropdown .active { background: #334155 !important; color: #fff !important; }
.irs--shiny .irs-bar { background: #22d3ee; border-color: #22d3ee; }
.irs--shiny .irs-single, .irs--shiny .irs-handle { background: #22d3ee; border-color: #22d3ee; }
.irs--shiny .irs-line { background: #334155; }
.irs--shiny .irs-min, .irs--shiny .irs-max { background: #334155; color: #cbd5e1; }
.btn-dl { background: linear-gradient(135deg, #0891b2, #22d3ee); color: #fff !important; border: 0;
  border-radius: 10px; padding: 8px 18px; font-weight: 500; }
.read-help p { margin-bottom: 8px; color: #cbd5e1; }
.read-help strong { color: #22d3ee; }
"

body <- dashboardBody(
  tags$head(tags$style(HTML(page_css))),

  tabItems(
    # ======================================================
    # OVERVIEW
    # ======================================================
    tabItem(
      tabName = "overview",
      div(
        class = "hero",
        h2("Education Equality in India"),
        p("Literacy, enrolment and school infrastructure across states and union territories."),
        span(class = "chip", paste(number_of_states, "States / UTs")),
        span(class = "chip", "Census 2011"),
        span(class = "chip", "UDISE+ 2020-21 to 2023-24"),
        span(class = "chip", "Gender focus")
      ),
      fluidRow(
        valueBoxOutput("vb_schools", width = 2),
        valueBoxOutput("vb_students", width = 2),
        valueBoxOutput("vb_teachers", width = 2),
        valueBoxOutput("vb_ptr", width = 2),
        valueBoxOutput("vb_girls", width = 2),
        valueBoxOutput("vb_female_lit", width = 2)
      ),
      fluidRow(
        box(
          title = "Filter Dashboard by State / UT",
          width = 12,
          selectInput(
            inputId = "overview_state",
            label = "Select State / UT (the cards and the first two charts update)",
            choices = c(
              "All States / UTs",
              sort(unique(infrastructure_data$State))
            ),
            selected = "All States / UTs",
            width = "100%"
          )
        )
      ),
      fluidRow(
        box(
          title = "Student Enrolment Gender Composition",
          width = 4,
          plotlyOutput("overview_enrolment_donut", height = "320px")
        ),
        box(
          title = "Enrolment by School Stage",
          width = 4,
          plotlyOutput("overview_stage", height = "320px")
        ),
        box(
          title = "Top 10 States by Schools",
          width = 4,
          plotlyOutput("overview_schools_top10", height = "320px")
        )
      ),
      fluidRow(
        box(
          title = "Male vs Female Literacy (Top 12 States)",
          width = 6,
          plotlyOutput("overview_literacy_comparison", height = "370px")
        ),
        box(
          title = "Widest Literacy Gender Gaps (Top 10)",
          width = 6,
          plotlyOutput("overview_gap_top10", height = "370px")
        )
      ),
      fluidRow(
        box(
          title = "Pupil-Teacher Ratio (Highest 12 States)",
          width = 6,
          plotlyOutput("overview_ptr", height = "370px")
        ),
        box(
          title = "Zero-Enrolment vs Single-Teacher Schools",
          width = 6,
          plotlyOutput("overview_infrastructure_challenges", height = "370px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations (all India)",
          width = 12,
          uiOutput("overview_obs")
        )
      )
    ),

    # ======================================================
    # LITERACY
    # ======================================================
    tabItem(
      tabName = "literacy",
      h2("Literacy Analysis"),
      p(
        "Male, female and overall literacy rates. Source: Census 2011.",
        class = "project-subtitle"
      ),
      fluidRow(
        valueBoxOutput("vb_lit_total", width = 3),
        valueBoxOutput("vb_lit_male", width = 3),
        valueBoxOutput("vb_lit_female", width = 3),
        valueBoxOutput("vb_lit_gap", width = 3)
      ),
      fluidRow(
        box(
          title = "State filter (scatter plot and table)",
          width = 12,
          selectInput(
            "literacy_state",
            "Select State / UT",
            choices = literacy_choices,
            selected = "All",
            width = "100%"
          )
        )
      ),
      fluidRow(
        box(
          title = "Male vs Female Literacy",
          width = 8,
          plotlyOutput("literacy_plot", height = "460px")
        ),
        box(
          title = "Distribution of Literacy Rates",
          width = 4,
          plotlyOutput("literacy_box", height = "460px")
        )
      ),
      fluidRow(
        box(
          title = "Male-Female Literacy Gap by State (dumbbell chart)",
          width = 12,
          plotlyOutput("literacy_dumbbell", height = "820px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations",
          width = 12,
          uiOutput("literacy_obs")
        )
      ),
      fluidRow(
        box(
          title = "How to read the charts",
          width = 12,
          class = "read-help",
          p(strong("State: "), "identifies the state or union territory."),
          p(strong("Male / Female: "), "literacy rate (%) of males / females."),
          p(strong("Total: "), "overall literacy rate for persons (%)."),
          p(strong("Gap: "), "Male minus Female, in percentage points. Larger = more inequality."),
          p(strong("Dashed diagonal: "), "equal male and female literacy. Points below it have higher male literacy."),
          p(strong("Hover: "), "move the pointer over a point to see the state and values.")
        )
      ),
      fluidRow(
        box(
          title = "State-wise literacy data",
          width = 12,
          DTOutput("literacy_table")
        )
      )
    ),

    # ======================================================
    # ENROLMENT
    # ======================================================
    tabItem(
      tabName = "enrolment",
      h2("Enrolment Analysis"),
      p(
        "Boys' and girls' enrolment, Pre-Primary to Higher Secondary. Source: UDISE+ 2021-22.",
        class = "project-subtitle"
      ),
      fluidRow(
        box(
          title = "State filter",
          width = 12,
          selectInput(
            "enrolment_state",
            "Select State / UT",
            choices = enrolment_choices,
            selected = "All",
            width = "100%"
          )
        )
      ),
      fluidRow(
        valueBoxOutput("vb_en_total", width = 3),
        valueBoxOutput("vb_en_boys", width = 3),
        valueBoxOutput("vb_en_girls", width = 3),
        valueBoxOutput("vb_en_share", width = 3)
      ),
      fluidRow(
        box(
          title = "Boys and Girls Enrolment by State",
          width = 12,
          plotlyOutput("enrolment_plot", height = "460px")
        )
      ),
      fluidRow(
        box(
          title = "Enrolment by School Stage",
          width = 6,
          plotlyOutput("enrolment_stage_plot", height = "380px")
        ),
        box(
          title = "Girls' Share at Each Stage (%)",
          width = 6,
          plotlyOutput("enrolment_stage_share", height = "380px")
        )
      ),
      fluidRow(
        box(
          title = "Growth in Enrolment, 2020-21 to 2021-22 (%)",
          width = 12,
          plotlyOutput("enrolment_change_plot", height = "420px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations",
          width = 12,
          uiOutput("enrolment_obs")
        )
      ),
      fluidRow(
        box(
          title = "Enrolment data",
          width = 12,
          DTOutput("enrolment_table")
        )
      )
    ),

    # ======================================================
    # INFRASTRUCTURE
    # ======================================================
    tabItem(
      tabName = "infrastructure",
      h2("School Infrastructure"),
      p(
        "School availability, teachers and pupil-teacher ratio. Source: UDISE+ 2023-24.",
        class = "project-subtitle"
      ),
      fluidRow(
        valueBoxOutput("vb_in_schools", width = 3),
        valueBoxOutput("vb_in_ptr", width = 3),
        valueBoxOutput("vb_in_single", width = 3),
        valueBoxOutput("vb_in_zero", width = 3)
      ),
      fluidRow(
        box(
          title = "Rank states by an indicator",
          width = 12,
          fluidRow(
            column(
              6,
              selectInput(
                "infra_metric", "Indicator",
                choices = c(
                  "Pupil-Teacher Ratio" = "PTR",
                  "Single-teacher schools (%)" = "Single_Teacher_Percentage",
                  "Zero-enrolment schools (%)" = "Zero_Enrolment_Percentage",
                  "Average enrolment per school" = "Avg_Enrolments",
                  "Total schools" = "Total_Schools",
                  "Total teachers" = "Total_Teachers"
                ),
                width = "100%"
              )
            ),
            column(
              6,
              sliderInput(
                "infra_n", "Number of states to show",
                min = 5, max = nrow(infrastructure_data),
                value = 15, step = 1, width = "100%"
              )
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "States ranked by selected indicator",
          width = 12,
          plotlyOutput("infra_rank_plot", height = "480px")
        )
      ),
      fluidRow(
        box(
          title = "Pupil-Teacher Ratio by State / UT (dashed line = 30)",
          width = 6,
          plotlyOutput("ptr_plot", height = "700px")
        ),
        box(
          title = "Single-Teacher vs Zero-Enrolment Schools (%)",
          width = 6,
          plotlyOutput("infra_risk_scatter", height = "700px")
        )
      ),
      fluidRow(
        box(
          title = "Total Schools by State / UT",
          width = 12,
          plotlyOutput("schools_plot", height = "700px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations",
          width = 12,
          uiOutput("infra_obs")
        )
      ),
      fluidRow(
        box(
          title = "Infrastructure data",
          width = 12,
          DTOutput("infrastructure_table")
        )
      )
    ),

    # ======================================================
    # GENDER EQUALITY
    # ======================================================
    tabItem(
      tabName = "gender",
      h2("Gender Equality"),
      p(
        "Girls' share of enrolment, stage-wise participation and the literacy gap.",
        class = "project-subtitle"
      ),
      fluidRow(
        box(
          title = "Multi-Variable Pearson Correlation Matrix",
          width = 12,
          plotOutput("girls_heatmap", height = "680px")
        )
      ),
      fluidRow(
        box(
          title = "Girls' Share of Total Enrolment (dashed line = 50% parity)",
          width = 6,
          plotlyOutput("girls_share_plot", height = "760px")
        ),
        column(
          6,
          box(
            title = "Female Literacy (2011) vs Girls' Enrolment Share (2021-22)",
            width = 12,
            plotlyOutput("gender_scatter", height = "340px")
          ),
          box(
            title = "Female Literacy vs Literacy Gap",
            width = 12,
            plotlyOutput("gender_gap_scatter", height = "340px")
          )
        )
      ),
      fluidRow(
        box(
          title = "Male and Female Literacy by State / UT",
          width = 12,
          plotlyOutput("gender_literacy_plot", height = "500px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations",
          width = 12,
          uiOutput("gender_obs")
        )
      ),
      fluidRow(
        box(
          title = "Gender literacy data",
          width = 12,
          DTOutput("gender_table")
        )
      )
    ),

    # ======================================================
    # BOLLINGER BAND
    # ======================================================
    tabItem(
      tabName = "bollinger",
      h2("Bollinger Band Analysis"),
      p(
        "Bollinger Bands applied to education indicators: states are lined up in a chosen order and a moving average with upper / lower bands shows which states stand out from their neighbours.",
        class = "project-subtitle"
      ),
      fluidRow(
        box(
          title = "Band settings",
          width = 12,
          fluidRow(
            column(
              3,
              selectInput("bb_ind", "Indicator", choices = bb_indicators,
                          selected = "Girls_Share", width = "100%")
            ),
            column(
              3,
              selectInput("bb_order", "Order states by", choices = bb_orders,
                          selected = "Enrolment", width = "100%")
            ),
            column(
              3,
              sliderInput("bb_n", "Moving-average window (states)",
                          min = 3, max = 10, value = 5, step = 1, width = "100%")
            ),
            column(
              3,
              sliderInput("bb_sd", "Standard deviations (band width)",
                          min = 1, max = 3, value = 1.5, step = 0.5, width = "100%")
            )
          )
        )
      ),
      fluidRow(
        valueBoxOutput("vb_bb_last", width = 3),
        valueBoxOutput("vb_bb_above", width = 3),
        valueBoxOutput("vb_bb_below", width = 3),
        valueBoxOutput("vb_bb_width", width = 3)
      ),
      fluidRow(
        box(
          title = "Bollinger Band Visualization",
          width = 12,
          plotlyOutput("bb_plot", height = "560px")
        )
      ),
      fluidRow(
        box(
          title = "Band width along the state order",
          width = 6,
          plotlyOutput("bb_width_plot", height = "360px")
        ),
        box(
          title = "%B: where each state sits inside the bands",
          width = 6,
          plotlyOutput("bb_pctb_plot", height = "360px")
        )
      ),
      fluidRow(
        box(
          title = "Key Observations",
          width = 12,
          uiOutput("bb_obs")
        )
      )
    ),

    # ======================================================
    # DATA EXPLORER
    # ======================================================
    tabItem(
      tabName = "explorer",
      h2("Data Explorer"),
      p(
        "Choose a dataset. Search, sort, browse and download its records.",
        class = "project-subtitle"
      ),
      fluidRow(
        box(
          title = "Choose dataset",
          width = 12,
          fluidRow(
            column(
              8,
              selectInput(
                "dataset_choice",
                "Dataset",
                choices = c(
                  "Literacy - Census 2011" = "literacy",
                  "Enrolment - UDISE 2021-22" = "enrolment",
                  "Infrastructure - UDISE 2023-24" = "infrastructure",
                  "Literacy + Enrolment (joined)" = "joined"
                ),
                selected = "literacy",
                width = "100%"
              )
            ),
            column(
              4,
              br(),
              downloadButton("download_data", "Download CSV", class = "btn-dl")
            )
          ),
          DTOutput("explorer_table")
        )
      )
    )
  )
)

# ----------------------------
# 10. Assemble UI
# ----------------------------
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "black",
  title = "Education Equality India"
)

# ----------------------------
# 11. Server
# ----------------------------
server <- function(input, output, session) {

  # --------------------------------------------------------
  # Overview filters
  # --------------------------------------------------------
  filtered_infrastructure <- reactive({
    selected <- input$overview_state
    if (is.null(selected) || is_all(selected)) return(infrastructure_data)
    infrastructure_data %>% filter(State == selected)
  })

  filtered_literacy <- reactive({
    selected <- input$overview_state
    if (is.null(selected) || is_all(selected)) return(literacy_data)
    literacy_data %>% filter(State == selected)
  })

  filtered_enrolment <- reactive({
    selected <- input$overview_state
    if (is.null(selected) || is_all(selected)) return(enrolment_data)
    enrolment_data %>% filter(State == selected)
  })

  # --------------------------------------------------------
  # OVERVIEW: value boxes
  # --------------------------------------------------------
  output$vb_schools <- renderValueBox({
    valueBox(fmt_big(sum(filtered_infrastructure()$Total_Schools, na.rm = TRUE)),
             "Schools (2023-24)", icon = icon("building"), color = "aqua")
  })

  output$vb_students <- renderValueBox({
    valueBox(fmt_big(sum(filtered_infrastructure()$Total_Enrolments, na.rm = TRUE)),
             "Students (2023-24)", icon = icon("users"), color = "green")
  })

  output$vb_teachers <- renderValueBox({
    valueBox(fmt_big(sum(filtered_infrastructure()$Total_Teachers, na.rm = TRUE)),
             "Teachers (2023-24)", icon = icon("chalkboard-teacher"), color = "purple")
  })

  output$vb_ptr <- renderValueBox({
    d <- filtered_infrastructure()
    v <- sum(d$Total_Enrolments, na.rm = TRUE) / sum(d$Total_Teachers, na.rm = TRUE)
    valueBox(round(v, 1), "Pupils per teacher", icon = icon("balance-scale"), color = "yellow")
  })

  output$vb_girls <- renderValueBox({
    d <- filtered_enrolment()
    v <- sum(d$Girls, na.rm = TRUE) / sum(d$Total, na.rm = TRUE) * 100
    valueBox(pct(v), "Girls' share of enrolment", icon = icon("female"), color = "maroon")
  })

  output$vb_female_lit <- renderValueBox({
    d <- filtered_literacy()
    v <- if (is.null(input$overview_state) || is_all(input$overview_state)) {
      india_lit$Female
    } else if (nrow(d) == 1) {
      d$Female
    } else {
      NA_real_
    }
    valueBox(if (is.na(v)) "N/A" else pct(v), "Female literacy (2011)",
             icon = icon("book"), color = "red")
  })

  # --------------------------------------------------------
  # OVERVIEW: charts
  # --------------------------------------------------------
  output$overview_enrolment_donut <- renderPlotly({
    totals <- filtered_enrolment() %>%
      summarise(
        Boys = sum(Boys, na.rm = TRUE),
        Girls = sum(Girls, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Gender", values_to = "Students")

    plot_ly(
      totals,
      labels = ~Gender,
      values = ~Students,
      type = "pie",
      hole = 0.62,
      marker = list(colors = c(COL$boys, COL$girls)),
      textinfo = "label+percent",
      hovertemplate = "%{label}: %{value:,.0f}<extra></extra>"
    ) %>%
      dark_plot(margin = list(l = 15, r = 15, t = 25, b = 15), showlegend = FALSE)
  })

  output$overview_stage <- renderPlotly({
    req(input$overview_state)
    s <- stage_for(input$overview_state)
    plot_ly(s, x = ~Stage, y = ~Boys, type = "bar", name = "Boys",
            marker = list(color = COL$boys),
            hovertemplate = "%{x}<br>Boys: %{y:,.0f}<extra></extra>") %>%
      add_trace(y = ~Girls, name = "Girls", marker = list(color = COL$girls),
                hovertemplate = "%{x}<br>Girls: %{y:,.0f}<extra></extra>") %>%
      dark_plot(
        margin = list(l = 60, r = 15, t = 25, b = 70),
        barmode = "group",
        xaxis = ax("", tickangle = -25),
        yaxis = ax("Students", tickformat = ".2s"),
        legend = list(orientation = "h", y = -0.3)
      )
  })

  output$overview_schools_top10 <- renderPlotly({
    d <- infrastructure_data %>%
      arrange(desc(Total_Schools)) %>%
      slice_head(n = 10) %>%
      arrange(Total_Schools) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      d, x = ~Total_Schools, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$boys),
      hovertemplate = "<b>%{y}</b><br>Schools: %{x:,.0f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 105, r = 15, t = 25, b = 45),
        xaxis = ax("Schools", tickformat = ",.2s"),
        yaxis = ax("")
      )
  })

  output$overview_literacy_comparison <- renderPlotly({
    d <- literacy_data %>%
      arrange(desc(Total)) %>%
      slice_head(n = 12) %>%
      select(State, Male, Female) %>%
      pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Literacy")

    plot_ly(
      d, x = ~State, y = ~Literacy, color = ~Gender, type = "scatter",
      mode = "lines+markers",
      colors = c(Male = COL$boys, Female = COL$girls),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 55, r = 15, t = 25, b = 110),
        xaxis = ax("", tickangle = -45),
        yaxis = ax("Literacy (%)", range = c(0, 100)),
        legend = list(orientation = "h", y = -0.4)
      )
  })

  output$overview_gap_top10 <- renderPlotly({
    d <- literacy_data %>%
      arrange(desc(Gender_Gap)) %>%
      slice_head(n = 10) %>%
      arrange(Gender_Gap) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      d, x = ~Gender_Gap, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$amber),
      hovertemplate = "<b>%{y}</b><br>Gap: %{x:.1f} pts<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 120, r = 15, t = 25, b = 45),
        xaxis = ax("Male minus female literacy (percentage points)"),
        yaxis = ax("")
      )
  })

  output$overview_ptr <- renderPlotly({
    d <- infrastructure_data %>%
      arrange(desc(PTR)) %>%
      slice_head(n = 12) %>%
      arrange(PTR) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      d, x = ~PTR, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$purple),
      hovertemplate = "<b>%{y}</b><br>Pupil-teacher ratio: %{x:.1f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 105, r = 15, t = 25, b = 45),
        xaxis = ax("Pupils per teacher"),
        yaxis = ax("")
      )
  })

  output$overview_infrastructure_challenges <- renderPlotly({
    d <- infrastructure_data %>%
      arrange(desc(Zero_Enrolment_Schools)) %>%
      slice_head(n = 12) %>%
      select(State, Zero_Enrolment_Schools, Single_Teacher_Schools) %>%
      pivot_longer(
        cols = c(Zero_Enrolment_Schools, Single_Teacher_Schools),
        names_to = "Indicator", values_to = "Schools"
      ) %>%
      mutate(Indicator = recode(
        Indicator,
        Zero_Enrolment_Schools = "Zero enrolment",
        Single_Teacher_Schools = "Single teacher"
      ))

    plot_ly(
      d, x = ~State, y = ~Schools, color = ~Indicator, type = "bar",
      colors = c("Zero enrolment" = COL$rose, "Single teacher" = COL$amber),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 65, r = 15, t = 25, b = 110),
        barmode = "group",
        xaxis = ax("", tickangle = -45),
        yaxis = ax("Number of schools", tickformat = ","),
        legend = list(orientation = "h", y = -0.4)
      )
  })

  # --------------------------------------------------------
  # OVERVIEW: observations (all India, auto-generated)
  # --------------------------------------------------------
  output$overview_obs <- renderUI({
    e <- enrolment_data
    l <- literacy_data
    i <- infrastructure_data

    top <- e %>% slice_max(Total, n = 1, with_ties = FALSE)
    top_share <- top$Total / sum(e$Total) * 100
    nat_girls <- sum(e$Girls) / sum(e$Total) * 100
    gs_hi <- e %>% slice_max(Girls_Share, n = 1, with_ties = FALSE)
    gs_lo <- e %>% slice_min(Girls_Share, n = 1, with_ties = FALSE)
    lit_hi <- l %>% slice_max(Total, n = 1, with_ties = FALSE)
    lit_lo <- l %>% slice_min(Total, n = 1, with_ties = FALSE)
    gap_hi <- l %>% slice_max(Gender_Gap, n = 1, with_ties = FALSE)
    ptr_hi <- i %>% slice_max(PTR, n = 1, with_ties = FALSE)
    sing_hi <- i %>% slice_max(Single_Teacher_Percentage, n = 1, with_ties = FALSE)
    zero_hi <- i %>% slice_max(Zero_Enrolment_Schools, n = 1, with_ties = FALSE)
    n_ptr30 <- sum(i$PTR > 30, na.rm = TRUE)

    r <- if (nrow(gender_join) > 3) {
      cor(gender_join$Female, gender_join$Girls_Share, use = "complete.obs")
    } else NA_real_
    r_txt <- if (is.na(r)) {
      "Not enough matching states to compare female literacy with girls' enrolment."
    } else {
      strength <- if (abs(r) >= 0.6) "strong" else if (abs(r) >= 0.3) "moderate" else "weak"
      direction <- if (r >= 0) "positive" else "negative"
      paste0("Female literacy (2011) and girls' enrolment share show a ", b(paste(strength, direction)),
             " relationship (r = ", round(r, 2), ").")
    }

    obs_list(
      paste0(b(top$State), " has the largest enrolment: ", fmt_big(top$Total),
             " students, about ", pct(top_share), " of the national total."),
      paste0("Girls make up ", b(pct(nat_girls)), " of enrolment nationally. The highest share is in ",
             b(gs_hi$State), " (", pct(gs_hi$Girls_Share), ") and the lowest in ",
             b(gs_lo$State), " (", pct(gs_lo$Girls_Share), ")."),
      paste0("Literacy ranges from ", b(lit_lo$State), " (", pct(lit_lo$Total), ") to ",
             b(lit_hi$State), " (", pct(lit_hi$Total), "), a spread of ",
             round(lit_hi$Total - lit_lo$Total, 1), " points."),
      paste0("The widest male-female literacy gap is in ", b(gap_hi$State), ": ",
             round(gap_hi$Gender_Gap, 1), " points. National gap: ",
             round(india_lit$Male - india_lit$Female, 1), " points."),
      paste0(b(n_ptr30), " of ", nrow(i), " states / UTs have a pupil-teacher ratio above 30. The highest is ",
             b(ptr_hi$State), " (", ptr_hi$PTR, " pupils per teacher)."),
      paste0(b(sing_hi$State), " has the largest share of single-teacher schools (",
             pct(sing_hi$Single_Teacher_Percentage), "); ", b(zero_hi$State),
             " has the most zero-enrolment schools (", format(zero_hi$Zero_Enrolment_Schools, big.mark = ","), ")."),
      r_txt
    )
  })

  # --------------------------------------------------------
  # LITERACY
  # --------------------------------------------------------
  output$vb_lit_total <- renderValueBox({
    valueBox(pct(india_lit$Total), "India literacy rate", icon = icon("book"), color = "aqua")
  })
  output$vb_lit_male <- renderValueBox({
    valueBox(pct(india_lit$Male), "Male literacy", icon = icon("male"), color = "blue")
  })
  output$vb_lit_female <- renderValueBox({
    valueBox(pct(india_lit$Female), "Female literacy", icon = icon("female"), color = "maroon")
  })
  output$vb_lit_gap <- renderValueBox({
    valueBox(paste(round(india_lit$Male - india_lit$Female, 1), "pts"),
             "National gender gap", icon = icon("balance-scale"), color = "yellow")
  })

  selected_literacy <- reactive({
    req(input$literacy_state)
    if (input$literacy_state == "All") {
      literacy_data
    } else {
      literacy_data %>% filter(State == input$literacy_state)
    }
  })

  output$literacy_plot <- renderPlotly({
    d <- selected_literacy()
    validate(need(nrow(d) > 0, "No literacy data for this selection."))

    # x = Male, y = Female, text = State (state name appears on hover)
    p <- plot_ly(
      data = d, x = ~Male, y = ~Female, type = "scatter", mode = "markers",
      text = ~State, customdata = ~Total,
      marker = list(
        size = 12, color = ~Gender_Gap, showscale = TRUE,
        colorscale = list(c(0, COL$boys), c(1, COL$girls)),
        colorbar = list(title = "Gap", thickness = 10),
        line = list(color = "#ffffff", width = 1)
      ),
      hovertemplate = paste0(
        "<b>%{text}</b>",
        "<br>Male literacy: %{x:.1f}%",
        "<br>Female literacy: %{y:.1f}%",
        "<br>Overall literacy: %{customdata:.1f}%",
        "<extra></extra>"
      )
    )

    if (input$literacy_state == "All") {
      p <- p %>%
        add_trace(
          x = c(40, 100), y = c(40, 100), type = "scatter", mode = "lines",
          name = "Equal literacy", inherit = FALSE,
          line = list(color = COL$amber, dash = "dash", width = 2),
          hoverinfo = "skip"
        )
    }

    dark_plot(
      p,
      xaxis = ax("Male Literacy (%)", range = c(60, 100)),
      yaxis = ax("Female Literacy (%)", range = c(40, 100)),
      legend = list(orientation = "h", y = -0.2)
    )
  })

  output$literacy_box <- renderPlotly({
    d <- literacy_data %>%
      select(State, Male, Female, Total) %>%
      pivot_longer(cols = c(Male, Female, Total), names_to = "Group", values_to = "Literacy") %>%
      mutate(Group = factor(Group, levels = c("Male", "Female", "Total")))

    plot_ly(
      d, y = ~Literacy, color = ~Group, type = "box", text = ~State,
      colors = c(Male = COL$boys, Female = COL$girls, Total = COL$purple),
      boxpoints = "all", jitter = 0.4, pointpos = 0,
      marker = list(size = 5, opacity = 0.7),
      hovertemplate = "<b>%{text}</b><br>%{y:.1f}%<extra></extra>"
    ) %>%
      dark_plot(
        yaxis = ax("Literacy (%)"),
        xaxis = ax(""),
        showlegend = FALSE
      )
  })

  output$literacy_dumbbell <- renderPlotly({
    d <- literacy_data %>%
      arrange(Gender_Gap) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(d) %>%
      add_segments(
        x = ~Female, xend = ~Male, y = ~State, yend = ~State,
        line = list(color = "#64748b", width = 3),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_markers(
        x = ~Male, y = ~State, name = "Male",
        marker = list(color = COL$boys, size = 10),
        hovertemplate = "<b>%{y}</b><br>Male: %{x:.1f}%<extra></extra>"
      ) %>%
      add_markers(
        x = ~Female, y = ~State, name = "Female",
        marker = list(color = COL$girls, size = 10),
        hovertemplate = "<b>%{y}</b><br>Female: %{x:.1f}%<extra></extra>"
      ) %>%
      dark_plot(
        margin = list(l = 150, r = 25, t = 30, b = 60),
        xaxis = ax("Literacy rate (%)", range = c(40, 100)),
        yaxis = ax("", tickfont = list(size = 11)),
        legend = list(orientation = "h", y = -0.06)
      )
  })

  output$literacy_obs <- renderUI({
    l <- literacy_data
    hi <- l %>% slice_max(Total, n = 1, with_ties = FALSE)
    lo <- l %>% slice_min(Total, n = 1, with_ties = FALSE)
    fem_lo <- l %>% slice_min(Female, n = 1, with_ties = FALSE)
    gap_hi <- l %>% slice_max(Gender_Gap, n = 1, with_ties = FALSE)
    gap_lo <- l %>% slice_min(Gender_Gap, n = 1, with_ties = FALSE)
    n_below60 <- sum(l$Female < 60)
    n_gap15 <- sum(l$Gender_Gap > 15)
    near <- l %>% filter(Gender_Gap < 5) %>% arrange(Gender_Gap)
    near_txt <- if (nrow(near) == 0) "No state is within 5 points of parity." else
      paste0(b(nrow(near)), " states / UTs are within 5 points of parity: ",
             paste(head(near$State, 6), collapse = ", "), if (nrow(near) > 6) ", ..." else ".")

    obs_list(
      paste0("Overall literacy is highest in ", b(hi$State), " (", pct(hi$Total),
             ") and lowest in ", b(lo$State), " (", pct(lo$Total), ")."),
      paste0("Female literacy is lowest in ", b(fem_lo$State), " (", pct(fem_lo$Female),
             "); ", b(n_below60), " states / UTs have female literacy below 60%."),
      paste0("The gender gap is widest in ", b(gap_hi$State), " (", round(gap_hi$Gender_Gap, 1),
             " points) and narrowest in ", b(gap_lo$State), " (", round(gap_lo$Gender_Gap, 1), " points)."),
      paste0(b(n_gap15), " states / UTs have a gap larger than 15 points."),
      near_txt,
      paste0("Female literacy trails male literacy in ",
             b(sum(l$Gender_Gap > 0)), " of ", nrow(l), " states / UTs.")
    )
  })

  output$literacy_table <- renderDT({
    d <- selected_literacy() %>%
      transmute(
        `State / UT` = State,
        `Male Literacy (%)` = Male,
        `Female Literacy (%)` = Female,
        `Overall Literacy (%)` = Total,
        `Gender Gap (percentage points)` = Gender_Gap
      )

    datatable(d, rownames = FALSE, filter = "none", options = dt_options,
              class = "compact stripe hover") %>%
      formatRound(columns = 2:5, digits = 1)
  })

  # --------------------------------------------------------
  # ENROLMENT
  # --------------------------------------------------------
  selected_enrolment <- reactive({
    req(input$enrolment_state)
    if (input$enrolment_state == "All") {
      enrolment_data
    } else {
      enrolment_data %>% filter(State == input$enrolment_state)
    }
  })

  output$vb_en_total <- renderValueBox({
    valueBox(fmt_big(sum(selected_enrolment()$Total)), "Total enrolment",
             icon = icon("users"), color = "aqua")
  })
  output$vb_en_boys <- renderValueBox({
    valueBox(fmt_big(sum(selected_enrolment()$Boys)), "Boys enrolled",
             icon = icon("male"), color = "blue")
  })
  output$vb_en_girls <- renderValueBox({
    valueBox(fmt_big(sum(selected_enrolment()$Girls)), "Girls enrolled",
             icon = icon("female"), color = "maroon")
  })
  output$vb_en_share <- renderValueBox({
    d <- selected_enrolment()
    valueBox(pct(sum(d$Girls) / sum(d$Total) * 100), "Girls' share",
             icon = icon("percent"), color = "purple")
  })

  output$enrolment_plot <- renderPlotly({
    d0 <- selected_enrolment() %>% arrange(desc(Total))
    validate(need(nrow(d0) > 0, "No enrolment data for this selection."))
    lv <- d0$State

    d <- d0 %>%
      select(State, Boys, Girls) %>%
      pivot_longer(cols = c(Boys, Girls), names_to = "Gender", values_to = "Students") %>%
      mutate(State = factor(State, levels = lv))

    plot_ly(
      data = d, x = ~State, y = ~Students, color = ~Gender, type = "bar",
      colors = c(Boys = COL$boys, Girls = COL$girls),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 70, r = 25, t = 30, b = 120),
        barmode = "group",
        xaxis = ax("", tickangle = -45),
        yaxis = ax("Enrolment", tickformat = ",.2s"),
        legend = list(orientation = "h", y = -0.35)
      )
  })

  output$enrolment_stage_plot <- renderPlotly({
    req(input$enrolment_state)
    s <- stage_for(input$enrolment_state)
    plot_ly(s, x = ~Stage, y = ~Boys, type = "bar", name = "Boys",
            marker = list(color = COL$boys),
            hovertemplate = "%{x}<br>Boys: %{y:,.0f}<extra></extra>") %>%
      add_trace(y = ~Girls, name = "Girls", marker = list(color = COL$girls),
                hovertemplate = "%{x}<br>Girls: %{y:,.0f}<extra></extra>") %>%
      dark_plot(
        barmode = "group",
        xaxis = ax(""),
        yaxis = ax("Students", tickformat = ",.2s"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$enrolment_stage_share <- renderPlotly({
    req(input$enrolment_state)
    s <- stage_for(input$enrolment_state)
    plot_ly(s, x = ~Stage, y = ~Girls_Share, type = "scatter", mode = "lines+markers",
            line = list(color = COL$girls, width = 3),
            marker = list(color = COL$girls, size = 11, line = list(color = "#fff", width = 1)),
            hovertemplate = "%{x}<br>Girls' share: %{y:.1f}%<extra></extra>") %>%
      dark_plot(
        xaxis = ax(""),
        yaxis = ax("Girls' share (%)", range = c(35, 60)),
        showlegend = FALSE,
        shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50,
                           line = list(color = COL$amber, dash = "dash", width = 2)))
      )
  })

  output$enrolment_change_plot <- renderPlotly({
    validate(need(
      !is.null(enrolment20),
      "Add cleaned_2020_21.csv to data/processed/ to see the year-on-year growth chart."
    ))

    d0 <- inner_join(
      enrolment20 %>% select(State, Boys20 = Boys, Girls20 = Girls),
      enrolment_data %>% select(State, Boys, Girls),
      by = "State"
    ) %>%
      filter(Boys20 > 0, Girls20 > 0) %>%
      mutate(
        Boys_Change = (Boys - Boys20) / Boys20 * 100,
        Girls_Change = (Girls - Girls20) / Girls20 * 100
      ) %>%
      arrange(desc(Girls_Change))
    lv <- d0$State

    d <- d0 %>%
      select(State, Boys_Change, Girls_Change) %>%
      pivot_longer(cols = c(Boys_Change, Girls_Change), names_to = "Gender", values_to = "Change") %>%
      mutate(
        Gender = recode(Gender, Boys_Change = "Boys", Girls_Change = "Girls"),
        State = factor(State, levels = lv)
      )

    plot_ly(
      d, x = ~State, y = ~Change, color = ~Gender, type = "bar",
      colors = c(Boys = COL$boys, Girls = COL$girls),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 60, r = 25, t = 30, b = 120),
        barmode = "group",
        xaxis = ax("", tickangle = -45),
        yaxis = ax("Change in enrolment (%)"),
        legend = list(orientation = "h", y = -0.35)
      )
  })

  output$enrolment_obs <- renderUI({
    req(input$enrolment_state)
    st <- input$enrolment_state
    label <- if (st == "All") "India" else st
    d <- selected_enrolment()
    s <- stage_for(st) %>% filter(!is.na(Girls_Share))

    tot <- sum(d$Total)
    diff <- sum(d$Boys) - sum(d$Girls)
    low_stage <- s %>% slice_min(Girls_Share, n = 1, with_ties = FALSE)
    high_stage <- s %>% slice_max(Girls_Share, n = 1, with_ties = FALSE)

    items <- list(
      paste0(b(label), " has ", fmt_big(tot), " students enrolled; girls are ",
             b(pct(sum(d$Girls) / tot * 100)), " of the total."),
      paste0("There are ", b(fmt_big(abs(diff))),
             if (diff >= 0) " more boys than girls." else " more girls than boys."),
      paste0("Girls' share is lowest at the ", b(low_stage$Stage), " stage (", pct(low_stage$Girls_Share),
             ") and highest at the ", b(high_stage$Stage), " stage (", pct(high_stage$Girls_Share), ").")
    )

    if (st == "All") {
      top3 <- enrolment_data %>% slice_max(Total, n = 3, with_ties = FALSE)
      items <- c(items, list(
        paste0("The three largest systems are ", b(paste(top3$State, collapse = ", ")),
               ", together ", pct(sum(top3$Total) / sum(enrolment_data$Total) * 100),
               " of national enrolment."),
        paste0("Girls outnumber boys in ",
               b(sum(enrolment_data$Girls > enrolment_data$Boys)), " states / UTs: ",
               paste(head(enrolment_data$State[enrolment_data$Girls > enrolment_data$Boys], 6), collapse = ", "),
               ".")
      ))
    }

    if (!is.null(enrolment20)) {
      ch <- inner_join(
        enrolment20 %>% select(State, B0 = Boys, G0 = Girls),
        enrolment_data %>% select(State, B1 = Boys, G1 = Girls), by = "State"
      ) %>%
        filter(B0 > 0, G0 > 0) %>%
        mutate(Gg = (G1 - G0) / G0 * 100, Bg = (B1 - B0) / B0 * 100)
      if (nrow(ch) > 0) {
        up <- ch %>% slice_max(Gg, n = 1, with_ties = FALSE)
        items <- c(items, list(
          paste0("Between 2020-21 and 2021-22, girls' enrolment grew faster than boys' in ",
                 b(sum(ch$Gg > ch$Bg)), " of ", nrow(ch), " states / UTs. The biggest rise for girls: ",
                 b(up$State), " (", pct(up$Gg), ").")
        ))
      }
    }

    do.call(obs_list, items)
  })

  output$enrolment_table <- renderDT({
    d <- selected_enrolment() %>%
      transmute(
        `State / UT` = State,
        `Boys Enrolment` = Boys,
        `Girls Enrolment` = Girls,
        `Total Enrolment` = Total,
        `Girls Share (%)` = Girls_Share,
        `Boys - Girls` = Gender_Gap
      )

    datatable(d, rownames = FALSE, options = dt_options, class = "compact stripe hover") %>%
      formatRound(columns = 2:4, digits = 0, mark = ",") %>%
      formatRound(columns = 5, digits = 1) %>%
      formatRound(columns = 6, digits = 0, mark = ",")
  })

  # --------------------------------------------------------
  # INFRASTRUCTURE
  # --------------------------------------------------------
  output$vb_in_schools <- renderValueBox({
    valueBox(fmt_big(sum(infrastructure_data$Total_Schools, na.rm = TRUE)), "Total schools",
             icon = icon("building"), color = "aqua")
  })
  output$vb_in_ptr <- renderValueBox({
    v <- sum(infrastructure_data$Total_Enrolments, na.rm = TRUE) /
      sum(infrastructure_data$Total_Teachers, na.rm = TRUE)
    valueBox(round(v, 1), "Pupils per teacher (national)", icon = icon("balance-scale"), color = "purple")
  })
  output$vb_in_single <- renderValueBox({
    valueBox(format(sum(infrastructure_data$Single_Teacher_Schools, na.rm = TRUE), big.mark = ","),
             "Single-teacher schools", icon = icon("user"), color = "yellow")
  })
  output$vb_in_zero <- renderValueBox({
    valueBox(format(sum(infrastructure_data$Zero_Enrolment_Schools, na.rm = TRUE), big.mark = ","),
             "Zero-enrolment schools", icon = icon("exclamation-triangle"), color = "red")
  })

  metric_labels <- c(
    PTR = "Pupil-teacher ratio",
    Single_Teacher_Percentage = "Single-teacher schools (%)",
    Zero_Enrolment_Percentage = "Zero-enrolment schools (%)",
    Avg_Enrolments = "Average enrolment per school",
    Total_Schools = "Total schools",
    Total_Teachers = "Total teachers"
  )

  output$infra_rank_plot <- renderPlotly({
    m <- input$infra_metric
    req(m, input$infra_n)
    d <- infrastructure_data %>%
      mutate(Value = .data[[m]]) %>%
      arrange(desc(Value)) %>%
      slice_head(n = input$infra_n) %>%
      arrange(Value) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      d, x = ~Value, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$green),
      hovertemplate = paste0("<b>%{y}</b><br>", metric_labels[[m]], ": %{x:,.2f}<extra></extra>")
    ) %>%
      dark_plot(
        margin = list(l = 130, r = 25, t = 30, b = 55),
        xaxis = ax(metric_labels[[m]]),
        yaxis = ax("")
      )
  })

  output$ptr_plot <- renderPlotly({
    d <- infrastructure_data %>%
      arrange(PTR) %>%
      mutate(State = factor(State, levels = State),
             Colour = if_else(PTR > 30, COL$rose, COL$purple))

    plot_ly(
      data = d, x = ~PTR, y = ~State, type = "bar", orientation = "h",
      marker = list(color = ~Colour),
      hovertemplate = "<b>%{y}</b><br>Pupil-teacher ratio: %{x:.1f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 140, r = 25, t = 30, b = 55),
        xaxis = ax("Pupils per teacher"),
        yaxis = ax("", tickfont = list(size = 11)),
        shapes = list(list(type = "line", x0 = 30, x1 = 30, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = COL$amber, dash = "dash", width = 2)))
      )
  })

  output$infra_risk_scatter <- renderPlotly({
    plot_ly(
      infrastructure_data,
      x = ~Single_Teacher_Percentage, y = ~Zero_Enrolment_Percentage, text = ~State,
      type = "scatter", mode = "markers",
      marker = list(size = 12, color = COL$amber, opacity = 0.85,
                    line = list(color = "#ffffff", width = 1)),
      hovertemplate = paste0(
        "<b>%{text}</b><br>Single-teacher: %{x:.1f}%<br>Zero-enrolment: %{y:.1f}%<extra></extra>"
      )
    ) %>%
      dark_plot(
        xaxis = ax("Single-teacher schools (%)"),
        yaxis = ax("Zero-enrolment schools (%)"),
        showlegend = FALSE
      )
  })

  output$schools_plot <- renderPlotly({
    d <- infrastructure_data %>%
      arrange(Total_Schools) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      data = d, x = ~Total_Schools, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$boys),
      hovertemplate = "<b>%{y}</b><br>Schools: %{x:,.0f}<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 140, r = 25, t = 30, b = 55),
        xaxis = ax("Number of schools", tickformat = ","),
        yaxis = ax("", tickfont = list(size = 11))
      )
  })

  output$infra_obs <- renderUI({
    i <- infrastructure_data
    ptr_hi <- i %>% slice_max(PTR, n = 1, with_ties = FALSE)
    ptr_lo <- i %>% slice_min(PTR, n = 1, with_ties = FALSE)
    sing_hi <- i %>% slice_max(Single_Teacher_Percentage, n = 1, with_ties = FALSE)
    zero_hi <- i %>% slice_max(Zero_Enrolment_Percentage, n = 1, with_ties = FALSE)
    avg_hi <- i %>% slice_max(Avg_Enrolments, n = 1, with_ties = FALSE)
    avg_lo <- i %>% slice_min(Avg_Enrolments, n = 1, with_ties = FALSE)
    over30 <- i %>% filter(PTR > 30) %>% arrange(desc(PTR))
    r <- cor(i$PTR, i$Single_Teacher_Percentage, use = "complete.obs")

    obs_list(
      paste0("Pupil-teacher ratio ranges from ", b(ptr_lo$State), " (", ptr_lo$PTR, ") to ",
             b(ptr_hi$State), " (", ptr_hi$PTR, ")."),
      paste0(b(nrow(over30)), " states / UTs exceed 30 pupils per teacher",
             if (nrow(over30) > 0) paste0(": ", paste(head(over30$State, 5), collapse = ", "),
                                           if (nrow(over30) > 5) ", ..." else "") else "", "."),
      paste0(b(sing_hi$State), " has the highest share of single-teacher schools (",
             pct(sing_hi$Single_Teacher_Percentage), ")."),
      paste0(b(zero_hi$State), " has the highest share of schools with zero enrolment (",
             pct(zero_hi$Zero_Enrolment_Percentage), ")."),
      paste0("Average school size is largest in ", b(avg_hi$State), " (", avg_hi$Avg_Enrolments,
             " students) and smallest in ", b(avg_lo$State), " (", avg_lo$Avg_Enrolments, ")."),
      paste0("PTR and the share of single-teacher schools have a ",
             b(if (abs(r) >= 0.6) "strong" else if (abs(r) >= 0.3) "moderate" else "weak"),
             " link (r = ", round(r, 2), "), so crowded classrooms and thinly staffed schools are ",
             if (abs(r) >= 0.3) "partly" else "largely not", " the same problem.")
    )
  })

  output$infrastructure_table <- renderDT({
    d <- infrastructure_data %>%
      select(
        State, Total_Schools, Total_Enrolments, Total_Teachers,
        PTR, Zero_Enrolment_Schools, Single_Teacher_Schools
      ) %>%
      rename(`State / UT` = State)

    datatable(d, rownames = FALSE, options = dt_options, class = "compact stripe hover") %>%
      formatRound(columns = 2:7, digits = 0, mark = ",")
  })

  # --------------------------------------------------------
  # GENDER EQUALITY
  # --------------------------------------------------------
  output$girls_share_plot <- renderPlotly({
    d <- enrolment_data %>%
      filter(!is.na(Girls_Share)) %>%
      arrange(Girls_Share) %>%
      mutate(State = factor(State, levels = State))

    plot_ly(
      data = d, x = ~Girls_Share, y = ~State, type = "bar", orientation = "h",
      marker = list(color = COL$girls),
      hovertemplate = "<b>%{y}</b><br>Girls' share: %{x:.1f}%<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 140, r = 25, t = 30, b = 55),
        xaxis = ax("Girls' share of total enrolment (%)", range = c(40, 55)),
        yaxis = ax("", tickfont = list(size = 11)),
        shapes = list(list(type = "line", x0 = 50, x1 = 50, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = COL$amber, dash = "dash", width = 2)))
      )
  })

  # Correlation heat map: x and y are the same set of state-level variables,
  # every cell shows the Pearson r written inside it.
  output$girls_heatmap <- renderPlot({
    infra_part <- infrastructure_data %>%
      mutate(State = norm_state(State)) %>%
      select(State, PTR, Single_Teacher_Percentage, Zero_Enrolment_Percentage)

    cor_data <- gender_join %>%
      inner_join(infra_part, by = "State") %>%
      transmute(
        `Male Lit.` = Male,
        `Female Lit.` = Female,
        `Total Lit.` = Total_Lit,
        `Lit. Gap` = Lit_Gap,
        `Girls Share` = Girls_Share,
        `Enrolment` = Total,
        `PTR` = PTR,
        `Single-Teacher %` = Single_Teacher_Percentage,
        `Zero-Enrol. %` = Zero_Enrolment_Percentage
      )

    cm <- cor(cor_data, use = "pairwise.complete.obs")
    lv <- colnames(cm)
    cor_long <- as.data.frame(as.table(cm), stringsAsFactors = FALSE)
    names(cor_long) <- c("X", "Y", "r")
    cor_long$X <- factor(cor_long$X, levels = lv)
    cor_long$Y <- factor(cor_long$Y, levels = lv)

    ggplot(cor_long, aes(x = X, y = Y, fill = r)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.2f", r)), size = 4.2, fontface = "bold") +
      scale_fill_gradient2(
        low = "#4f7df3", mid = "white", high = "#e8383d",
        midpoint = 0, limits = c(-1, 1), name = "Correlation"
      ) +
      labs(
        title = "Multi-Variable Pearson Correlation Matrix",
        subtitle = "Literacy (Census 2011), girls' share of enrolment (2021-22) and school infrastructure (2023-24) across states / UTs",
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "bottom"
      ) +
      guides(fill = guide_colourbar(barwidth = 12, barheight = 0.8, title.position = "top"))
  }, res = 96)

  output$gender_scatter <- renderPlotly({
    validate(need(nrow(gender_join) > 3, "Not enough matching states to plot."))
    j <- gender_join %>% arrange(Female)
    fit <- lm(Girls_Share ~ Female, data = j)
    j$Fit <- fitted(fit)

    plot_ly(j, x = ~Female, y = ~Girls_Share, text = ~State, type = "scatter", mode = "markers",
            marker = list(size = 12, color = COL$purple, opacity = 0.85,
                          line = list(color = "#fff", width = 1)),
            name = "State / UT",
            hovertemplate = "<b>%{text}</b><br>Female literacy: %{x:.1f}%<br>Girls' share: %{y:.1f}%<extra></extra>") %>%
      add_lines(data = j, x = ~Female, y = ~Fit, name = "Trend", inherit = FALSE,
                line = list(color = COL$amber, dash = "dash", width = 2),
                hoverinfo = "skip") %>%
      dark_plot(
        xaxis = ax("Female literacy, Census 2011 (%)"),
        yaxis = ax("Girls' share of enrolment, 2021-22 (%)"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$gender_gap_scatter <- renderPlotly({
    validate(need(nrow(gender_join) > 3, "Not enough matching states to plot."))
    plot_ly(gender_join, x = ~Female, y = ~Lit_Gap, text = ~State, type = "scatter", mode = "markers",
            marker = list(size = 12, color = ~Girls_Share, showscale = TRUE,
                          colorscale = "Plasma", colorbar = list(title = "Girls %", thickness = 10),
                          line = list(color = "#fff", width = 1)),
            hovertemplate = "<b>%{text}</b><br>Female literacy: %{x:.1f}%<br>Gap: %{y:.1f} pts<extra></extra>") %>%
      dark_plot(
        xaxis = ax("Female literacy (%)"),
        yaxis = ax("Male minus female literacy (points)"),
        showlegend = FALSE
      )
  })

  output$gender_literacy_plot <- renderPlotly({
    d <- literacy_data %>%
      arrange(desc(Total)) %>%
      select(State, Male, Female) %>%
      mutate(State = factor(State, levels = State)) %>%
      pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Literacy")

    plot_ly(
      data = d, x = ~State, y = ~Literacy, color = ~Gender, type = "bar",
      colors = c(Male = COL$boys, Female = COL$girls),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_plot(
        margin = list(l = 60, r = 25, t = 30, b = 120),
        barmode = "group",
        xaxis = ax("", tickangle = -45),
        yaxis = ax("Literacy rate (%)", range = c(0, 100)),
        legend = list(orientation = "h", y = -0.35)
      )
  })

  output$gender_obs <- renderUI({
    e <- enrolment_data
    l <- literacy_data
    par <- e %>% filter(Girls_Share >= 49.5)
    lo <- e %>% slice_min(Girls_Share, n = 3, with_ties = FALSE)
    hi <- e %>% slice_max(Girls_Share, n = 3, with_ties = FALSE)
    gap_hi <- l %>% slice_max(Gender_Gap, n = 3, with_ties = FALSE)

    # which stage has the lowest girls' share nationally
    s <- stage_for("All") %>% filter(!is.na(Girls_Share))
    s_lo <- s %>% slice_min(Girls_Share, n = 1, with_ties = FALSE)
    s_hi <- s %>% slice_max(Girls_Share, n = 1, with_ties = FALSE)

    r <- if (nrow(gender_join) > 3) cor(gender_join$Female, gender_join$Girls_Share, use = "complete.obs") else NA_real_
    r_txt <- if (is.na(r)) {
      "Female literacy and girls' enrolment could not be compared."
    } else {
      paste0("States with higher female literacy tend to enrol ",
             if (r >= 0) b("more") else b("fewer"),
             " girls (r = ", round(r, 2), "). Treat this as a pattern, not proof of cause: the two datasets are from different years.")
    }

    obs_list(
      paste0(b(nrow(par)), " of ", nrow(e), " states / UTs are within 0.5 points of girls' enrolment parity (49.5% or more)."),
      paste0("Highest girls' share: ", b(paste(hi$State, collapse = ", ")),
             ". Lowest: ", b(paste(lo$State, collapse = ", ")), "."),
      paste0("Nationally, girls' share is lowest at the ", b(s_lo$Stage), " stage (", pct(s_lo$Girls_Share),
             ") and highest at the ", b(s_hi$Stage), " stage (", pct(s_hi$Girls_Share), ")."),
      paste0("Widest literacy gaps: ", b(paste(gap_hi$State, collapse = ", ")), "."),
      r_txt
    )
  })

  output$gender_table <- renderDT({
    d <- gender_join %>%
      transmute(
        `State / UT` = State,
        `Male Literacy (%)` = Male,
        `Female Literacy (%)` = Female,
        `Gender Gap (percentage points)` = Lit_Gap,
        `Girls Share of Enrolment (%)` = Girls_Share
      )

    datatable(d, rownames = FALSE, options = dt_options, class = "compact stripe hover") %>%
      formatRound(columns = 2:5, digits = 1)
  })

  # --------------------------------------------------------
  # BOLLINGER BAND
  # --------------------------------------------------------
  bb_label <- reactive({
    names(bb_indicators)[match(input$bb_ind, bb_indicators)]
  })

  bb_data <- reactive({
    req(input$bb_ind, input$bb_order, input$bb_n, input$bb_sd)
    d <- state_all
    d$Price <- d[[input$bb_ind]]
    d <- d[!is.na(d$Price), ]
    d <- if (input$bb_order == "State") {
      d[order(d$State), ]
    } else {
      d[order(-d[[input$bb_order]]), ]
    }
    validate(need(nrow(d) > input$bb_n,
                  "Window is larger than the number of states available."))
    d$Pos <- seq_len(nrow(d))
    add_bands(d, n = input$bb_n, k = input$bb_sd)
  })

  # x axis: state names instead of numbers
  bb_xaxis <- function(d) {
    ax("", tickmode = "array", tickvals = d$Pos, ticktext = d$State,
       tickangle = -60, tickfont = list(size = 10))
  }

  output$vb_bb_last <- renderValueBox({
    d <- bb_data()
    valueBox(nrow(d), "States / UTs in the series",
             icon = icon("chart-line"), color = "aqua")
  })
  output$vb_bb_above <- renderValueBox({
    d <- bb_data()
    valueBox(sum(d$Price > d$up, na.rm = TRUE), "States above upper band",
             icon = icon("arrow-up"), color = "red")
  })
  output$vb_bb_below <- renderValueBox({
    d <- bb_data()
    valueBox(sum(d$Price < d$dn, na.rm = TRUE), "States below lower band",
             icon = icon("arrow-down"), color = "green")
  })
  output$vb_bb_width <- renderValueBox({
    d <- bb_data()
    valueBox(pct(mean(d$Width, na.rm = TRUE)), "Average band width",
             icon = icon("expand"), color = "purple")
  })

  output$bb_plot <- renderPlotly({
    d <- bb_data()
    out <- d %>% filter(!is.na(up), Price > up | Price < dn)

    plot_ly(d, x = ~Pos, text = ~State) %>%
      add_ribbons(ymin = ~dn, ymax = ~up, name = "Band range",
                  fillcolor = "rgba(34,211,238,0.15)",
                  line = list(color = "transparent"), hoverinfo = "skip") %>%
      add_lines(y = ~up, name = "Upper band",
                line = list(color = COL$rose, dash = "dash", width = 1.5),
                hovertemplate = "Upper: %{y:.2f}<extra></extra>") %>%
      add_lines(y = ~dn, name = "Lower band",
                line = list(color = COL$green, dash = "dash", width = 1.5),
                hovertemplate = "Lower: %{y:.2f}<extra></extra>") %>%
      add_lines(y = ~mavg, name = "Moving average",
                line = list(color = "#60a5fa", width = 2.5),
                hovertemplate = "Average: %{y:.2f}<extra></extra>") %>%
      add_trace(y = ~Price, type = "scatter", mode = "lines+markers", name = "State value",
                line = list(color = "#f8fafc", width = 2),
                marker = list(color = "#f8fafc", size = 5),
                hovertemplate = "<b>%{text}</b><br>Value: %{y:.2f}<extra></extra>") %>%
      add_markers(data = out, x = ~Pos, y = ~Price, text = ~State, name = "Outside bands",
                  marker = list(color = COL$amber, size = 11, line = list(color = "#fff", width = 1)),
                  hovertemplate = "<b>%{text}</b><br>Value: %{y:.2f} (outside bands)<extra></extra>") %>%
      dark_plot(
        margin = list(l = 60, r = 25, t = 30, b = 130),
        xaxis = bb_xaxis(d),
        yaxis = ax(bb_label()),
        legend = list(orientation = "h", y = -0.35)
      )
  })

  output$bb_width_plot <- renderPlotly({
    d <- bb_data() %>% filter(!is.na(Width))
    plot_ly(d, x = ~Pos, y = ~Width, text = ~State, type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(167,139,250,0.25)",
            line = list(color = COL$purple, width = 2),
            hovertemplate = "<b>%{text}</b><br>Width: %{y:.1f}%<extra></extra>") %>%
      dark_plot(margin = list(l = 60, r = 25, t = 30, b = 120),
                xaxis = bb_xaxis(d), yaxis = ax("Band width (% of average)"),
                showlegend = FALSE)
  })

  output$bb_pctb_plot <- renderPlotly({
    d <- bb_data() %>% filter(!is.na(PctB))
    plot_ly(d, x = ~Pos, y = ~PctB, text = ~State, type = "scatter", mode = "lines+markers",
            line = list(color = COL$boys, width = 2),
            marker = list(color = COL$boys, size = 5),
            hovertemplate = "<b>%{text}</b><br>%B: %{y:.2f}<extra></extra>") %>%
      dark_plot(
        margin = list(l = 60, r = 25, t = 30, b = 120),
        xaxis = bb_xaxis(d), yaxis = ax("%B (0 = lower band, 1 = upper band)"),
        showlegend = FALSE,
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 1, y1 = 1,
               line = list(color = COL$rose, dash = "dash")),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
               line = list(color = COL$green, dash = "dash"))
        )
      )
  })

  output$bb_obs <- renderUI({
    d <- bb_data()
    valid <- d %>% filter(!is.na(up))
    hi <- valid %>% filter(Price > up)
    lo <- valid %>% filter(Price < dn)
    wide <- valid %>% slice_max(Width, n = 1, with_ties = FALSE)
    narrow <- valid %>% slice_min(Width, n = 1, with_ties = FALSE)
    first_v <- d$Price[1]
    last_v <- tail(d$Price, 1)
    nm <- function(x) if (nrow(x) == 0) "none" else paste(b(x$State), collapse = ", ")

    obs_list(
      paste0("Indicator: ", b(bb_label()), ". States are ordered by ",
             b(tolower(names(bb_orders)[match(input$bb_order, bb_orders)])), ". Bands use a ",
             b(input$bb_n), "-state moving average with ", b(input$bb_sd),
             " standard deviations, so the first ", input$bb_n - 1,
             " states have no band."),
      paste0("Above the upper band (unusually high for their neighbours): ", nm(hi), "."),
      paste0("Below the lower band (unusually low for their neighbours): ", nm(lo), "."),
      paste0("The series moves from ", b(round(first_v, 1)), " (", d$State[1], ") to ",
             b(round(last_v, 1)), " (", tail(d$State, 1), ")."),
      paste0("Bands are widest at ", b(wide$State), " (", pct(wide$Width),
             " of the average: the neighbouring states differ a lot) and narrowest at ",
             b(narrow$State), " (", pct(narrow$Width), ": neighbours are very similar)."),
      "A state outside the bands is a candidate for closer study in the Literacy, Enrolment and Infrastructure pages."
    )
  })

  # --------------------------------------------------------
  # DATA EXPLORER
  # --------------------------------------------------------
  current_dataset <- reactive({
    req(input$dataset_choice)
    switch(
      input$dataset_choice,
      literacy = literacy_data,
      enrolment = enrolment_data,
      infrastructure = infrastructure_data,
      joined = gender_join
    )
  })

  output$explorer_table <- renderDT({
    datatable(current_dataset(), rownames = FALSE, options = dt_options,
              class = "compact stripe hover")
  })

  output$download_data <- downloadHandler(
    filename = function() paste0(input$dataset_choice, "_data.csv"),
    content = function(file) readr::write_csv(current_dataset(), file)
  )
}

# ----------------------------
# 12. Launch dashboard
# ----------------------------
shinyApp(ui = ui, server = server)
