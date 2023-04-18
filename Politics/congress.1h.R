#!/usr/local/bin/Rscript

key_congress <- # Your API key for ProPublica Congress API.
  ""

# BitBar Metadata ----
# <xbar.title>US Congress</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jacob Sowder</xbar.author>
# <xbar.author.github>jsowder</xbar.author.github>
# <xbar.desc>Track congressional members & bills</xbar.desc>
# <xbar.image></xbar.image>
# <xbar.dependencies>R</xbar.dependencies>


# Initialize ----
options(tidyverse.quiet = TRUE)
if (!require("tidyverse", quietly = TRUE, warn.conflicts = FALSE)) install.packages("tidyverse")
if (!require("magrittr", quietly = TRUE, warn.conflicts = FALSE)) install.packages("magrittr")
if (!require("devtools", quietly = TRUE, warn.conflicts = FALSE)) install.packages("devtools")
if (!require("ProPublicaR", quietly = TRUE, warn.conflicts = FALSE)) install.packages("ProPublicaR")
if (!require("bitbaR", quietly = TRUE, warn.conflicts = FALSE)) devtools::install_github("jsowder/bitbaR")

bb_head(icon = "\U0001f3db", refresh = TRUE) # Classical Building

# Check Credentials ----

if (key_congress == "") stop("Register for an API Key | href=https://www.propublica.org/datastore/api/propublica-congress-api")

# Helper Functions ----

pp_tibble <- # Parse return from ProPublica Congress API into tibbles.
  function(pp_result) {
    pp_result %>%
      pluck("results", 1) %>%
      enframe() %>%
      pivot_wider() %>%
      unnest(cols = everything()) %>%
      unnest_wider(last_col())
  }

# Recent Bills ----
#  List bills from the House and Senate with actions recently taken on them.
bills_recent <-
  bind_rows(
    ProPublicaR::recent_bills_by_type(116, chamber = "house", type = "updated", myAPI_Key = key_congress) %>% pp_tibble(),
    ProPublicaR::recent_bills_by_type(116, chamber = "senate", type = "updated", myAPI_Key = key_congress) %>% pp_tibble(),
  )
%>%
  bb_print("Bills:")

Recent <-
  bills_recent %>%
  transmute(
    menu_title = "Recent Bill Actions"
    , chamber
    , bill = str_glue("{number}: {short_title}",  bb_attributes(length = 40))
    , info = str_glue(
      .sep = "\n"
      , "{latest_major_action} ({latest_major_action_date})"
      , "{sponsor_title} {sponsor_name} ({sponsor_party}-{sponsor_state}) (Sponsor)"
      , "{committees}"
      , "More... | href={congressdotgov_url}"
    )
  ) %>%
  separate_rows(info, sep = "\n") %>%
  bb_nest(menu_title, chamber, bill, info) %T>%
  bb_print()


# Upcoming Bills ----
#  List bills from the House and Senate with actions recently taken on them.
bills_upcoming <-
  bind_rows(
    ProPublicaR::get_upcoming_bills("house", myAPI_Key = key_congress) %>%
      pp_tibble() %>%
      { if (dim(.)[1] == 0) tribble(~chamber, ~bill, ~info, "House", "No bills to show.", "") },
    ProPublicaR::get_upcoming_bills("senate", myAPI_Key = key_congress) %>%
      pp_tibble() %>%
      { if (dim(.)[1] == 0) tribble(~chamber, ~bill, ~info, "Senate", "No bills to show.", "") },
  )

Upcoming <- tryCatch(
  { bills_upcoming %>%
      transmute(
        menu_title = "Upcoming Bills"
        , chamber
        , bill = str_glue("{number}: {short_title}",  bb_attributes(length = 40))
        , info = str_glue(
          .sep = "\n"
          , "{latest_major_action} ({latest_major_action_date})"
          , "{sponsor_title} {sponsor_name} ({sponsor_party}-{sponsor_state}) (Sponsor)"
          , "{committees}"
          , "More... | href={congressdotgov_url}"
        )
      ) %>%
      separate_rows(info, sep = "\n") %>%
      bb_nest(menu_title, chamber, bill, info)
  }
  , error = function(cond) {
    "Upcoming Bills\n-- No upcoming bills.\n"
  }
) %T>%
  bb_print()

# Members ----

members_all <-
  bind_rows(
    ProPublicaR::list_members_chamber_congress(116, "house", myAPI_Key = key_congress) %>% pp_tibble(),
    ProPublicaR::list_members_chamber_congress(116, "senate", myAPI_Key = key_congress) %>% pp_tibble(),
  ) %>%
  na_if("") %>%
  filter(in_office == TRUE) %>%
  arrange(chamber, state, last_name) %>%
  mutate(
    full_name = str_glue("{short_title} {first_name} {last_name} ({party}-{state})")
    , last_first = str_glue("{last_name}, {first_name}")
    , leadership = if_else(!is.na(leadership_role), "Leadership", NULL)
    , info = str_glue(
      .sep = "\n"
      , "{full_name}"
      , "---"
      , "House.gov | href={url}"
      , "Twitter | href=https://www.twitter.com/{twitter_account}"
      , "---"
      , "{office}"
      , "{phone}"
      , "---"
      , "{votes_with_party_pct}% of votes with their party"
      , "{missed_votes_pct}% of votes missed"
      , "Cook PVI: {cook_pvi}"
    )
  ) %>%
  separate_rows(info, sep = "\n")

bb_print("---")
bb_print("Members:")

Members <-
  members_all %>%
  mutate(menu_title = "All") %>%
  bb_nest(menu_title, chamber, state, last_first, info) %T>%
  bb_print()

Leadership <-
  members_all %>%
  filter(!is.na(leadership)) %>%
  arrange(desc(seniority)) %>%
  mutate(menu_title = "Leadership") %>%
  bb_nest(menu_title, chamber, leadership_role, info) %T>%
  bb_print()
