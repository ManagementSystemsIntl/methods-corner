# Methods Corner — Session Memory

## Active project: Survey Tree App
**Goal:** Build an interactive Shiny app that displays the Palestinian Perception Survey
as a navigable *tree* — not just a question browser. The tree should reflect the
survey's conversational flow and branching/skip logic. Clicking nodes shows response
distributions (bar or donut).

**Key distinction (user feedback):** The current app (`Perception survey/survey_tree.R`)
shows frequencies and has sidebar navigation, but does not show the actual *tree
structure* visually. That is the next step — user will demonstrate what they mean.

## Data
- `wbg_full` — West Bank/Gaza perception survey, 2568 obs × 403 vars
  - Loaded in the R session; source project: `Palestinian Perception Study/`
- `irq_full` — Iraq perception survey (tbl_df)
- `afg_full` — Afghanistan perception survey (data.frame)

## Survey instrument
- File: `C:/Users/dkill/OneDrive/Documents/methods corner/meta-analysis/Perception surveys/Palestinian Perception Survey_public (2022).docx`
- Read into session as `content` (docx_summary) and `instrument_text` (character)

## Key branching logic (WBG instrument)
- Q2 (usaid_famil): Not familiar → skip to Q7
  - usaid_famil: 0=Not familiar, 1=Somewhat, 2=Very; 98=Refused
- Q4 (usaid_percep): impression 1–5
  - Q5 (q5a–q5g): why negative → if usaid_famil %in% c(1,2) & usaid_percep %in% c(1,2)
  - Q6 (q6a–q6g): why positive → if usaid_famil %in% c(1,2) & usaid_percep %in% c(4,5)
- Q9 (q9): noticed USAID activities → Q10 if q9==1
- Q11 (q11): participated in USAID → Q12 if q11==1
- Q13 (donor_famil): familiar with other donors → Q14 if donor_famil %in% c(1,2)
- Q15 (q15): participated non-USAID → Q16 if q15==1
- Q18 (q18_bin): radio → Q19 (radio_hrs), Q20 (stations) if q18_bin==1
- Q21 (q21_bin): TV → Q22 (tv_hrs), Q23 (stations) if q21_bin==1
- Q24 (internet_access): → Q25 (social_media_engage), Q26 (platforms) if internet_access==1

## Known data issues / TODOs
- Q14 (q14a–q14l_bin), Q16 (q16a–q16n_bin): donor labels are placeholders
- Q20 (q20a–q20p), Q23 (q23a–q23q): station labels are placeholders
- Q17 (overall donor performance): variable not found in data (no col between q16n_bin and q18_bin)
- q10j_a_bin: purpose unclear; labeled "Other" for now

## USAID colors (in session)
- usaid_blue = "#0067B9"
- usaid_red  = "#BA0C2F"
- light_blue = "#A7C6ED"
- USAID_palette: data.frame with named colors
