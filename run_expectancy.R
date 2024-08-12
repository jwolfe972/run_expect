library(tidyverse)
install.packages("baseballr")
library(baseballr)

pbp_2024_mlb <- read.csv('full_2024_pbp.csv')


pbp_2024_mlb %>%
  mutate(RUNS= post_home_score + post_away_score,
         HALF.INNING = paste(game_pk, inning, inning_topbot),
         RUNS.SCORED= ( (post_home_score+post_away_score)- (home_score+away_score))) -> pbp_2024_mlb_runs

pbp_2024_mlb_runs %>%
  group_by(HALF.INNING) %>%
  summarize(RUNS.INNING= sum(RUNS.SCORED),
            RUNS.START = first(RUNS),
            MAX.RUNS = RUNS.INNING + RUNS.START,
            OUTS.INNING = max(outs_when_up)) -> half_innings

pbp_2024_mlb_runs %>%
  inner_join(half_innings, by="HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> pbp_2024_mlb_runs

pbp_2024_mlb_runs %>%
  mutate(BASES = paste(
    ifelse(on_1b == 1, 1,0),
    ifelse(on_2b == 1, 1,0),
    ifelse(on_3b == 1, 1,0)
  ) ,
  STATE = paste(BASES, outs_when_up)) -> pbp_2024_mlb_runs

pbp_2024_mlb_runs %>%
  group_by(HALF.INNING) %>%
  mutate(on_1b_after_play = lead(on_1b, order_by = at_bat_number) ,
         on_2b_after_play = lead(on_2b, order_by = at_bat_number),
          on_3b_after_play = lead(on_3b, order_by = at_bat_number),
         outs_made = case_when(
          events %in% c("pickoff_caught_stealing_home","pickoff_caught_stealing_3b", "pickoff_3b" ,"pickoff_1b" ,"sac_fly" ,"fielders_choice","other_out","field_out", "strikeout", "double_play", "triple_play", "force_out", "sac_bunt", "caught_stealing_2b","caught_stealing_3b" ,"caught_stealing_home"    ) ~ 1,
          events %in% c("double_play", "grounded_into_double_play", "strikeout_double_play", "sac_fly_double_play" ) ~ 2,
        events %in% c("triple_play") ~ 3,
         TRUE ~ 0
          ),
          NEW.BASES= paste(on_1b_after_play, on_2b_after_play, on_3b_after_play),
          NEW.TOTAL.OUTS = outs_when_up+outs_made,
          NEW.STATE = paste(NEW.BASES, NEW.TOTAL.OUTS)) -> pbp_2024_mlb_runs


pbp_2024_mlb_runs %>%
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) -> pbp_2024_mlb_runs_changes

pbp_2024_mlb_runs_changes %>%
  filter(OUTS.INNING  == 2) -> pbp_2024_mlb_runs_changes_no_full


pbp_2024_mlb_runs_changes_no_full %>%
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE,7,7)) %>%
arrange(Outs)-> RUNS

RUNS_out <- matrix(round(RUNS$Mean,2), 8,3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 outs", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
RUNS_out


pbp_2024_mlb_runs_changes %>%
  left_join(select(RUNS, -Outs), by="STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS,-Outs),  by=c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State+ RUNS.SCORED) -> pbp_2024_mlb_runs_changes_no_full_run_value


pbp_2024_mlb_runs_changes_no_full_run_value %>%
  group_by(batter) %>%
  summarize(Total_Run_Value = sum(run_value)) -> mlb_2024_run_value

player_id <- playerid_lookup("Seager", "Corey")$mlbam_id
player_id

pbp_2024_mlb_runs_changes_no_full_run_value %>%
  filter(batter == player_id) -> seager_bats


seager_bats %>%
  group_by(BASES) %>%
  summarize(Total_Run_Value = sum(run_value)) -> seager_bats_sum

total_run_value <- sum(seager_bats_sum$Total_Run_Value)

ggplot(seager_bats, aes(BASES, run_value)) +
  geom_jitter(width = .25, alpha=.5)+
  geom_hline(yintercept = 0, color='red') +
  xlab("Bases") +
  ylab("Run Value")+
  theme_classic() +
  theme( panel.grid.minor = element_line(color = "gray") ,  panel.grid.major = element_line(color = "gray")) +
  labs(title = paste("Aaron Judges's Run Value based on Bases Total Run Value: " , total_run_value))


pbp_2024_mlb_runs_changes_no_full_run_value %>%
  group_by(batter) %>%
  summarize(Total_Run_Value = sum(run_value),
            PA = length(run_value),
            Runs.Start = sum(Runs.State)
            ) %>%
  arrange(desc(Total_Run_Value)) -> mlb_total_run_value
mlb_total_run_value %>%
  filter(PA > 350) -> mlb_total_run_value_350
View(mlb_total_run_value_350)

ggplot(mlb_total_run_value_350, aes(Runs.Start, Total_Run_Value)) +
  geom_point() +
  geom_smooth() +
  xlab("Start Runs") +
  ylab("Total Run Value") +
  geom_hline(yintercept = 0, color='red')+ theme_classic()


pbp_2024_mlb_runs_changes_no_full_run_value %>%
  filter(batter == 665742) %>%
select(STATE, BASES, NEW.STATE, run_value)-> aaron_judge
