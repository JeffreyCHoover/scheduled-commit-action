# data from
# https://www.hockey-reference.com/leagues/NHL_2021_games.html

url <- "https://www.hockey-reference.com/leagues/NHL_2021_games.html"

dat <- xml2::read_html(url) %>%
  rvest::html_nodes("table") %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::rename(date = Date, away = Visitor, goals_away = G, home = Home,
                goals_home = `G.1`, extra = `Var.6`) %>%
  dplyr::select(-`Att.`, -LOG, -Notes)

away <- dat %>%
  dplyr::rename(team1 = away, team2 = home) %>%
  dplyr::mutate(outcome = dplyr::case_when(goals_away > goals_home &
                                             is.na(extra) ~ 1,
                                           goals_away > goals_home &
                                             !is.na(extra) ~ .75,
                                           goals_away < goals_home &
                                             is.na(extra) ~ 0,
                                           goals_away < goals_home &
                                             !is.na(extra) ~ .25)) %>%
  dplyr::select(date, team1, team2, outcome)

home <- dat %>%
  dplyr::rename(team1 = home, team2 = away) %>%
  dplyr::mutate(outcome = dplyr::case_when(goals_home > goals_away &
                                             is.na(extra) ~ 1,
                                           goals_home > goals_away &
                                             !is.na(extra) ~ .75,
                                           goals_home < goals_away &
                                             is.na(extra) ~ 0,
                                           goals_home < goals_away &
                                             !is.na(extra) ~ .25)) %>%
  dplyr::select(date, team1, team2, outcome)

full_dat <- dplyr::bind_rows(home, away) %>%
  dplyr::rename(team = team1, opp = team2)

strength <- full_dat %>%
  dplyr::filter(!is.na(outcome)) %>%
  dplyr::mutate(pts = dplyr::case_when(outcome == 1 ~ 2L,
                                       outcome == .75 ~ 2L,
                                       outcome == .25 ~ 1L,
                                       outcome == 0 ~ 0L)) %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(team) %>%
  dplyr::mutate(game_number = dplyr::row_number()) %>%
  dplyr::summarize(pts = sum(pts),
                   pts_perc = pts / (max(game_number) * 2),
                   .groups = 'keep') %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(pts_perc)) %>%
  dplyr::select(-pts)

jh_power_rankings <- full_dat %>%
  dplyr::filter(!is.na(outcome)) %>%
  dplyr::left_join(strength, by = c("opp" = "team")) %>%
  dplyr::rename(strength = pts_perc) %>%
  dplyr::mutate(weighted_outcome = outcome * strength) %>%
  dplyr::group_by(team) %>%
  dplyr::mutate(game_number = dplyr::row_number(),
                weight = game_number / max(game_number),
                time_weighted_outcome = weight * weighted_outcome) %>%
  dplyr::summarize(weighted_outcome = sum(weighted_outcome),
                   time_weighted_outcome = sum(time_weighted_outcome),
                   .groups = 'keep') %>%
  dplyr::ungroup()

rankings <- jh_power_rankings %>%
  dplyr::arrange(desc(weighted_outcome)) %>%
  dplyr::mutate(rank = dplyr::row_number())

time_rankings <- jh_power_rankings %>%
  dplyr::arrange(desc(time_weighted_outcome)) %>%
  dplyr::mutate(rank = dplyr::row_number())

remaining <- full_dat %>%
  dplyr::filter(is.na(outcome)) %>%
  dplyr::left_join(strength, by = "team") %>%
  dplyr::rename(strength = pts_perc) %>%
  dplyr::left_join(strength, by = c("opp" = "team")) %>%
  dplyr::rename(opp_strength = pts_perc) %>%
  dplyr::mutate(predicted_outcome = strength / (strength + opp_strength)) %>%
  dplyr::group_by(team) %>%
  dplyr::summarize(performance = sum(predicted_outcome),
                   .groups = 'keep') %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(performance)) %>%
  dplyr::mutate(rank = dplyr::row_number())

pts_ranking <- full_dat %>%
  dplyr::filter(!is.na(outcome)) %>%
  dplyr::mutate(pts = dplyr::case_when(outcome == 1 ~ 2L,
                                       outcome == .75 ~ 2L,
                                       outcome == .25 ~ 1L,
                                       outcome == 0 ~ 0L)) %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(team) %>%
  dplyr::mutate(game_number = dplyr::row_number()) %>%
  dplyr::summarize(pts = sum(pts),
                   pts_perc = pts / (max(game_number) * 2),
                   .groups = 'keep') %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(pts)) %>%
  dplyr::select(-pts_perc) %>%
  dplyr::mutate(rank = dplyr::row_number())

final_rankings <- pts_ranking %>%
  dplyr::select(-pts) %>%
  dplyr::rename(pts_rank = rank) %>%
  dplyr::left_join(rankings %>%
                     dplyr::select(team, w_rank = rank),
                   by = "team") %>%
  dplyr::left_join(time_rankings %>%
                     dplyr::select(team, time_rank = rank),
                   by = "team") %>%
  dplyr::left_join(remaining %>%
                     dplyr::select(team, remaining_rank = rank),
                   by = "team") %>%
  dplyr::mutate(rank_avg = (pts_rank + w_rank + time_rank + remaining_rank) / 4,
                tiebreak = (pts_rank + w_rank + time_rank) / 3) %>%
  dplyr::select(team, rank_avg, tiebreak) %>%
  dplyr::arrange(rank_avg, tiebreak) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::select(team, rank)

if(!fs::dir_exists(here::here("rankings"))) {
  fs::dir_create(here::here("rankings"))
}

cur_date <- Sys.Date()
writexl::write_xlsx(final_rankings,
                    path =
                      here::here(glue::glue("rankings/rankings_{cur_date}.xlsx")))
