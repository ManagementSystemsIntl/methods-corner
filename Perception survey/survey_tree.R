# survey_tree.R
# Palestinian Perception Survey — Interactive Survey Tree
#
# Requires wbg_full to be loaded in the R environment.
# Run with: shiny::runApp("Perception survey/survey_tree.R")

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)

usaid_blue <- "#0067B9"
light_blue <- "#A7C6ED"

# =============================================================================
# Question Definitions
# =============================================================================
qs <- list(

  # ── USAID Perception ────────────────────────────────────────────────────────
  list(id = "q1", section = "USAID Perception",
       label = "Q1. Donor logo recognition",
       q     = "In the past year, have you ever seen any of these donor logos?",
       type  = "multi_binary",
       vars  = c("seen_UNRWA","seen_UNDP","seen_UNICEF","seen_RedC",
                 "seen_WFP","seen_EU","seen_USAID","seen_Action"),
       vlabs = c("UNRWA","UNDP","UNICEF","Red Cross/Crescent",
                 "WFP","EU","USAID","ActionAid"),
       cond = NULL, clab = NULL),

  list(id = "q2", section = "USAID Perception",
       label = "Q2. USAID familiarity",
       q     = "How familiar are you with the US Agency for International Development (USAID)?",
       type  = "single",
       vars  = "usaid_famil",
       vlabs = c("Not at all familiar" = 0, "Somewhat familiar" = 1, "Very familiar" = 2),
       cond = NULL, clab = NULL),

  list(id = "q4", section = "USAID Perception",
       label = "Q4. Impression of USAID",
       q     = "Would you say your overall impression of USAID is positive or negative?",
       type  = "scale",
       vars  = "usaid_percep",
       vlabs = c("Very negative" = 1, "Somewhat negative" = 2, "Neutral" = 3,
                 "Somewhat positive" = 4, "Very positive" = 5),
       cond = "usaid_famil %in% c(1, 2)",
       clab = "Asked only of respondents familiar with USAID (Q2)"),

  list(id = "q5", section = "USAID Perception",
       label = "Q5. Why negative impression",
       q     = "Why do you have a negative impression of USAID? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q5", letters[1:7]),
       vlabs = c("Haven't seen projects in my community",
                 "Haven't seen results in my community",
                 "The US doesn't support Palestine",
                 "Foreign interference is always bad",
                 "The projects are not relevant",
                 "The projects are harmful to my community",
                 "The projects are unfair in who participates"),
       cond = "usaid_famil %in% c(1, 2) & usaid_percep %in% c(1, 2)",
       clab = "Asked if familiar with USAID and has negative impression (Q2, Q4)"),

  list(id = "q6", section = "USAID Perception",
       label = "Q6. Why positive impression",
       q     = "Why do you have a positive impression of USAID? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q6", letters[1:7]),
       vlabs = c("They help the needy/poor/vulnerable",
                 "They build infrastructure",
                 "They provide hope",
                 "Because of what I've seen or read",
                 "Because they are from the US",
                 "The projects are fair",
                 "They care about Palestine"),
       cond = "usaid_famil %in% c(1, 2) & usaid_percep %in% c(4, 5)",
       clab = "Asked if familiar with USAID and has positive impression (Q2, Q4)"),

  list(id = "q7", section = "USAID Perception",
       label = "Q7. US Government impression",
       q     = "Would you say your overall impression of the U.S. Government is positive or negative?",
       type  = "scale",
       vars  = "usg_percep",
       vlabs = c("Very negative" = 1, "Somewhat negative" = 2, "Neutral" = 3,
                 "Somewhat positive" = 4, "Very positive" = 5),
       cond = NULL, clab = NULL),

  list(id = "q7a", section = "USAID Perception",
       label = "Q7a. Ordinary Americans",
       q     = "Agree or disagree: \"Irrespective of US foreign policies, most ordinary Americans are good people.\"",
       type  = "single",
       vars  = "q7a",
       vlabs = c("Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither" = 3,
                 "Somewhat agree" = 4, "Strongly agree" = 5),
       cond = NULL, clab = NULL),

  list(id = "q8", section = "USAID Perception",
       label = "Q8. USAID performance",
       q     = "Overall, do you think USAID is doing a bad job or a good job?",
       type  = "scale",
       vars  = "usaid_perf",
       vlabs = c("Very bad job" = 1, "Somewhat bad job" = 2, "Neither" = 3,
                 "Somewhat good job" = 4, "Very good job" = 5),
       cond = NULL, clab = NULL),

  list(id = "q9", section = "USAID Perception",
       label = "Q9. Noticed USAID activities",
       q     = "Have you noticed the implementation of any USAID activities in your community?",
       type  = "single",
       vars  = "q9",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q10", section = "USAID Perception",
       label = "Q10. Types of USAID activities noticed",
       q     = "What types of USAID activities have you noticed? (Select all that apply)",
       type  = "multi_binary",
       vars  = c(paste0("q10", letters[1:10], "_bin"), "q10j_a_bin", "q10k_bin", "q10l_bin"),
       vlabs = c("Food security & nutrition", "Job training & support",
                 "Local community groups", "Economic growth / small businesses",
                 "Water & wastewater", "Transportation", "Youth activities",
                 "Health activities", "Cross-border peacebuilding", "Tourism",
                 "Other", "Educational activities", "Agriculture"),
       cond = "q9 == 1",
       clab = "Asked only of those who noticed USAID activities (Q9)"),

  list(id = "q11", section = "USAID Perception",
       label = "Q11. Participated in USAID activities",
       q     = "Have you participated in any USAID-funded activities in the past year?",
       type  = "single",
       vars  = "q11",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q12", section = "USAID Perception",
       label = "Q12. Types of USAID activities participated in",
       q     = "What type of USAID activity did you participate in? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q12", letters[1:12], "_bin"),
       vlabs = c("Food security & nutrition", "Job training & support",
                 "Local community groups", "Health activities",
                 "Educational activities", "Agriculture",
                 "Economic growth / small businesses", "Water & wastewater",
                 "Cross-border peacebuilding", "Tourism", "Other (1)", "Other (2)"),
       cond = "q11 == 1",
       clab = "Asked only of those who participated in USAID activities (Q11)"),

  # ── Donor Perception ────────────────────────────────────────────────────────
  list(id = "q13", section = "Donor Perception",
       label = "Q13. Familiarity with other donors",
       q     = "How familiar would you consider yourself with other foreign donor activities (not from the US)?",
       type  = "single",
       vars  = "donor_famil",
       vlabs = c("Not at all familiar" = 0, "Somewhat familiar" = 1, "Very familiar" = 2),
       cond = NULL, clab = NULL),

  list(id = "q14", section = "Donor Perception",
       label = "Q14. Which donors (familiar with)",
       q     = "Which donors are you familiar with? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q14", letters[1:12], "_bin"),
       vlabs = paste0("Donor ", LETTERS[1:12]),  # placeholder — update with real labels
       cond = "donor_famil %in% c(1, 2)",
       clab = "Asked only of those familiar with other donors (Q13)"),

  list(id = "q15", section = "Donor Perception",
       label = "Q15. Participated in non-USAID activities",
       q     = "In the past 12 months, have you participated in any donor-funded activities that were NOT USAID?",
       type  = "single",
       vars  = "q15",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q16", section = "Donor Perception",
       label = "Q16. Which donors funded non-USAID activity",
       q     = "Which donors funded the activity? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q16", letters[1:14], "_bin"),
       vlabs = paste0("Donor ", LETTERS[1:14]),  # placeholder
       cond = "q15 == 1",
       clab = "Asked only of those who participated in non-USAID activities (Q15)"),

  # ── Media Use ───────────────────────────────────────────────────────────────
  list(id = "q18", section = "Media Use",
       label = "Q18. Radio listenership",
       q     = "Do you listen to radio programs?",
       type  = "single",
       vars  = "q18_bin",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q19", section = "Media Use",
       label = "Q19. Hours of radio per day",
       q     = "How many hours a day do you generally listen to radio?",
       type  = "continuous",
       vars  = "radio_hrs",
       vlabs = NULL,
       cond = "q18_bin == 1",
       clab = "Asked only of radio listeners (Q18)"),

  list(id = "q20", section = "Media Use",
       label = "Q20. Radio stations listened to",
       q     = "Which radio stations do you listen to most often? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q20", letters[1:16]),
       vlabs = paste0("Station ", LETTERS[1:16]),  # placeholder
       cond = "q18_bin == 1",
       clab = "Asked only of radio listeners (Q18)"),

  list(id = "q21", section = "Media Use",
       label = "Q21. TV viewership",
       q     = "Do you watch television?",
       type  = "single",
       vars  = "q21_bin",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q22", section = "Media Use",
       label = "Q22. Hours of TV per day",
       q     = "How many hours a day do you generally watch television?",
       type  = "continuous",
       vars  = "tv_hrs",
       vlabs = NULL,
       cond = "q21_bin == 1",
       clab = "Asked only of TV viewers (Q21)"),

  list(id = "q23", section = "Media Use",
       label = "Q23. TV stations watched",
       q     = "Which television stations do you watch most often? (Select all that apply)",
       type  = "multi_binary",
       vars  = paste0("q23", letters[1:17]),
       vlabs = paste0("Station ", LETTERS[1:17]),  # placeholder
       cond = "q21_bin == 1",
       clab = "Asked only of TV viewers (Q21)"),

  list(id = "q24", section = "Media Use",
       label = "Q24. Internet access",
       q     = "Does this household have access to internet?",
       type  = "single",
       vars  = "internet_access",
       vlabs = c("No" = 0, "Yes" = 1),
       cond = NULL, clab = NULL),

  list(id = "q25", section = "Media Use",
       label = "Q25. Social media engagement",
       q     = "About how often do you engage on social networking sites?",
       type  = "single",
       vars  = "social_media_engage",
       vlabs = c("Non/infrequent user" = 0, "Regular user" = 1),
       cond = "internet_access == 1",
       clab = "Asked only of those with internet access (Q24)"),

  list(id = "q26", section = "Media Use",
       label = "Q26. Social media platforms used",
       q     = "Which social media platforms do you use? (Select all that apply)",
       type  = "multi_binary",
       vars  = c("Facebook","Twitter","Instagram","WhatsApp","SnapChat","Viber",
                 "Telegram","BOTIM","TikTok","LinkedIn","Pinterest","VChat","Clubhouse","Reddit"),
       vlabs = c("Facebook","Twitter","Instagram","WhatsApp","Snapchat","Viber",
                 "Telegram","BOTIM","TikTok","LinkedIn","Pinterest","VChat","Clubhouse","Reddit"),
       cond = "internet_access == 1",
       clab = "Asked only of those with internet access (Q24)"),

  # ── Demographics ─────────────────────────────────────────────────────────────
  list(id = "d_sex", section = "Demographics",
       label = "Sex",
       q     = "Gender of respondent (recorded by interviewer)",
       type  = "single",
       vars  = "sex",
       vlabs = c("Male" = 1, "Female" = 2),
       cond = NULL, clab = NULL),

  list(id = "d_age", section = "Demographics",
       label = "Age group",
       q     = "Age group of respondent",
       type  = "single",
       vars  = "age_grp",
       vlabs = NULL,
       cond = NULL, clab = NULL),

  list(id = "d_educ", section = "Demographics",
       label = "Education",
       q     = "Highest level of education completed",
       type  = "single",
       vars  = "educ_cat",
       vlabs = NULL,
       cond = NULL, clab = NULL),

  list(id = "d_area", section = "Demographics",
       label = "Area type",
       q     = "Area of residence",
       type  = "single",
       vars  = "area",
       vlabs = c("Urban" = 1, "Village" = 2, "Refugee Camp" = 3),
       cond = NULL, clab = NULL),

  list(id = "d_region", section = "Demographics",
       label = "Region",
       q     = "West Bank or Gaza",
       type  = "single",
       vars  = "region",
       vlabs = c("West Bank" = 1, "Gaza" = 2),
       cond = NULL, clab = NULL),

  list(id = "d_marital", section = "Demographics",
       label = "Marital status",
       q     = "Marital status of respondent",
       type  = "single",
       vars  = "marital",
       vlabs = NULL,
       cond = NULL, clab = NULL)
)

# =============================================================================
# Helpers
# =============================================================================
q_by_id <- setNames(qs, sapply(qs, `[[`, "id"))
sections <- unique(sapply(qs, `[[`, "section"))

# Filter to respondents who were asked this question
asked_df <- function(df, q) {
  if (is.null(q$cond)) return(df)
  df |> filter(eval(parse(text = q$cond)))
}

# Frequency table for single/scale questions
freq_data <- function(df, q) {
  var   <- q$vars
  vlabs <- q$vlabs

  if (!is.null(vlabs)) {
    valid_vals  <- unname(vlabs)
    label_names <- names(vlabs)
    df |>
      filter(.data[[var]] %in% valid_vals) |>
      count(.data[[var]]) |>
      mutate(
        label = label_names[match(.data[[var]], valid_vals)],
        label = factor(label, levels = label_names),
        pct   = n / sum(n)
      )
  } else {
    df |>
      filter(!is.na(.data[[var]]), .data[[var]] < 97) |>
      count(.data[[var]]) |>
      mutate(
        label = as.character(.data[[var]]),
        label = factor(label, levels = unique(label)),
        pct   = n / sum(n)
      )
  }
}

# % selecting each item for multi-binary questions
multi_data <- function(df, q) {
  n_total <- nrow(df)
  data.frame(
    label = q$vlabs,
    pct   = sapply(q$vars, function(v) {
      if (v %in% names(df)) mean(df[[v]] == 1, na.rm = TRUE) else NA_real_
    })
  ) |>
    filter(!is.na(pct)) |>
    mutate(label = factor(label, levels = label[order(pct)]))  # ascending → top of chart = highest
}

# Horizontal bar chart
make_bar <- function(fd, n_asked) {
  x_max <- min(max(fd$pct, na.rm = TRUE) * 1.18 + 0.02, 1)
  ggplot(fd, aes(x = pct, y = label)) +
    geom_col(fill = usaid_blue) +
    geom_text(aes(label = percent(pct, accuracy = 1)),
              hjust = -0.15, size = 3.8, color = "grey30") +
    scale_x_continuous(
      labels  = percent_format(accuracy = 1),
      limits  = c(0, x_max),
      expand  = expansion(0)
    ) +
    labs(x = NULL, y = NULL,
         caption = paste0("n = ", comma(n_asked), " respondents asked this question")) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.caption       = element_text(hjust = 0, color = "grey50", size = 10)
    )
}

# Donut chart (for single/scale questions with ≤ 7 categories)
make_donut <- function(fd, n_asked) {
  n_cats <- nrow(fd)
  pal    <- colorRampPalette(c(usaid_blue, light_blue))(n_cats)

  ggplot(fd, aes(x = 2, y = pct, fill = label)) +
    geom_col(width = 1, color = "white", linewidth = 0.6) +
    geom_text(
      aes(label = paste0(percent(pct, accuracy = 1))),
      position = position_stack(vjust = 0.5),
      size = 4, color = "white", fontface = "bold"
    ) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = pal, name = NULL) +
    labs(caption = paste0("n = ", comma(n_asked), " respondents asked this question")) +
    theme_void(base_size = 13) +
    theme(
      legend.position = "right",
      plot.caption    = element_text(hjust = 0.5, color = "grey50", size = 10)
    )
}

# =============================================================================
# UI
# =============================================================================
ui <- page_sidebar(
  title = tags$span(
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/USAID-Identity.svg/200px-USAID-Identity.svg.png",
             height = "24px", style = "margin-right:8px; vertical-align:middle;"),
    "Palestinian Perception Survey"
  ),
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Source Sans 3")),

  sidebar = sidebar(
    width  = 285,
    style  = "overflow-y: auto;",
    tags$style(HTML("
      .survey-btn {
        display: block; width: 100%; padding: 5px 8px; margin-bottom: 3px;
        border: none; border-radius: 5px; background: transparent;
        text-align: left; font-size: 0.82rem; color: #333; cursor: pointer;
        transition: background 0.15s;
      }
      .survey-btn:hover { background: #e8f0fb; }
      .survey-btn.selected { background: #0067B9; color: white; font-weight: 600; }
      .cond-indent { padding-left: 14px; border-left: 2px solid #dee2e6; margin-left: 6px; }
    ")),
    uiOutput("sidebar_tree")
  ),

  # Main display
  uiOutput("main_panel")
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  selected_q <- reactiveVal("q1")

  # One observer per question button
  lapply(qs, function(q) {
    btn_id <- paste0("btn_", q$id)
    observeEvent(input[[btn_id]], {
      selected_q(q$id)
    }, ignoreInit = TRUE)
  })

  # ── Sidebar tree ──────────────────────────────────────────────────────────
  output$sidebar_tree <- renderUI({
    sel <- selected_q()

    panels <- lapply(sections, function(sec) {
      sec_qs <- Filter(function(q) q$section == sec, qs)

      items <- lapply(sec_qs, function(q) {
        is_sel  <- q$id == sel
        is_cond <- !is.null(q$cond)
        btn_cls <- paste("survey-btn", if (is_sel) "selected" else "")

        item <- tags$button(
          id      = paste0("btn_", q$id),
          class   = btn_cls,
          onclick = sprintf("Shiny.setInputValue('%s', Math.random());", paste0("btn_", q$id)),
          q$label
        )

        if (is_cond) div(class = "cond-indent", item) else item
      })

      do.call(accordion_panel, c(list(title = sec), items))
    })

    do.call(accordion, c(list(open = TRUE, multiple = TRUE), panels))
  })

  # ── Reactive objects ──────────────────────────────────────────────────────
  current_q  <- reactive(q_by_id[[selected_q()]])
  df_asked   <- reactive(asked_df(wbg_full, current_q()))
  n_asked    <- reactive(nrow(df_asked()))

  # ── Main panel ────────────────────────────────────────────────────────────
  output$main_panel <- renderUI({
    q  <- current_q()
    is_multi <- q$type == "multi_binary"

    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$strong(q$label),
        if (!is_multi) {
          div(
            class = "d-flex gap-2 align-items-center",
            tags$small(class = "text-muted", "Chart:"),
            radioButtons(
              "chart_type", NULL,
              choices  = c("Bar" = "bar", "Donut" = "donut"),
              selected = {ct <- isolate(input$chart_type); if (is.null(ct)) "bar" else ct},
              inline   = TRUE
            )
          )
        }
      ),
      card_body(
        tags$p(class = "text-muted fst-italic mb-2", q$q),
        if (!is.null(q$clab)) {
          div(
            class = "alert alert-info py-1 px-3 mb-3",
            style = "font-size:0.82rem;",
            tags$i(class = "bi bi-funnel-fill me-1"),
            q$clab
          )
        },
        plotOutput("chart", height = "430px")
      )
    )
  })

  # ── Chart ─────────────────────────────────────────────────────────────────
  output$chart <- renderPlot({
    q  <- current_q()
    df <- df_asked()
    n  <- n_asked()

    if (n == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No respondents reached this question.",
                   size = 5.5, color = "grey50") +
          theme_void()
      )
    }

    if (q$type %in% c("single", "scale")) {
      fd <- freq_data(df, q)
      ct <- if (is.null(input$chart_type)) "bar" else input$chart_type
      if (ct == "donut") make_donut(fd, n) else make_bar(fd, n)

    } else if (q$type == "multi_binary") {
      fd <- multi_data(df, q)
      make_bar(fd, n)

    } else if (q$type == "continuous") {
      var  <- q$vars
      df_c <- df |> filter(!is.na(.data[[var]]), .data[[var]] < 97)
      ggplot(df_c, aes(x = .data[[var]])) +
        geom_histogram(fill = usaid_blue, color = "white", bins = 20) +
        scale_x_continuous(expand = expansion(0.01)) +
        labs(x = "Hours", y = "Count",
             caption = paste0("n = ", comma(n), " respondents asked this question")) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.caption     = element_text(hjust = 0, color = "grey50", size = 10)
        )
    }
  }, res = 110)
}

shinyApp(ui, server)
