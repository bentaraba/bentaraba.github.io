library("tidyverse")
dat=RWInProbData1

#GAME OF PROB SLIDE RED GRAPH 
usfdat=case_ucf_data
ggplot(usfdat, aes(x = `Time Left`, y = TuProb)) +
  geom_line(color = "#cc0000", size = 1.1) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_reverse() +
  labs(
    title = "Temple Vs USF Win Probability",
    x = "Time Remaining (minutes)",
    y = "Win Probability"
  ) +
  theme_minimal(base_size = 14)





#GAME OF PROB SLIDE DRIVE GRAPH


usfdat$DRIVE = as.numeric(usfdat$DRIVE)
usfdat$DRIVE = factor(usfdat$DRIVE, levels = sort(unique(usfdat$DRIVE)))

ggplot(usfdat, aes(x = `Time Left`, y = TuProb)) +
geom_line(aes(color = factor(DRIVE), group = DRIVE),size = 2, na.rm = TRUE) +
geom_line(color = "black",alpha=.4, size = 1.1,linetype="dashed", na.rm = TRUE) +
scale_color_discrete(name = "Drive", na.translate = FALSE) + scale_x_reverse() +
scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
labs(title = "Temple vs USF Win Probability by Drive",x = "Time Remaining (minutes)",y = "Win Probability") +
theme_minimal(base_size = 14)


#DRIVE PROB ADDED SLIDE DATA 

WeekDrv = group_by(dat, WEEK, DRIVE)

varrsum = summarise(WeekDrv,DriveProbAdded = first(DriveProbAdded),PlaysInDrive = n(),StartFieldPos=first(DRIVE_START_FIELD_POSITION),
                    EndFieldPos= first(DRIVE_END_FIELD_POSITION),StartQuarter= first(QUARTER),StartScoreDiff= first(SCORE_DIFFERENTIAL),.groups = "drop")

drive_dat = mutate(varrsum,StartFieldPos = as.numeric(gsub("\\+", "", as.character(StartFieldPos))),EndFieldPos   = as.numeric(gsub("\\+", "", as.character(EndFieldPos))),
                   StartQuarter= as.numeric(as.character(StartQuarter)),Success = DriveProbAdded > 0)

bs1 = group_by(dat, WEEK, DRIVE)

bs2 = summarise(bs1,DriveProbAdded = first(DriveProbAdded),PlaysInDrive   = n(),.groups = "drop")

drive_dat = mutate(bs2,Success = DriveProbAdded > 0)

A = group_by(drive_dat, PlaysInDrive)

B = summarise(A,n_drives = n(),mean_DPA = mean(DriveProbAdded, na.rm = TRUE),success_rt = mean(Success, na.rm = TRUE),.groups = "drop")

plydrv = arrange(B, PlaysInDrive)

plydrv


# DWPA LINEAR REG

DWPAreg =lm(DriveProbAdded ~ PlaysInDrive, data = drive_dat)

summary(DWPAreg)



#AVG DRIVE PROB BY LENGTHH CHART 

drive_sum = aggregate(
DriveProbAdded ~ PlaysInDrive,
data = drive_dat,
FUN = function(x) mean(x, na.rm = TRUE))

names(drive_sum)[2] = "MeanDriveProbAdded"


ggplot(drive_sum,aes(x = PlaysInDrive, y = MeanDriveProbAdded)) +
geom_col(fill = "#cc0000") +   # or geom_bar(stat = "identity", fill = "blue")
labs( title = "Average Drive Win Probability Added by Number of Plays", x = "Plays in Drive",y = "Mean Drive Prob Added") +theme_minimal(base_size = 18)



#MONTE CARLO

set.seed(123)


temple_spread = c(9.5,-13.5,18, -10,18.5,23.5,13.5,14.5,3.5,20,17,9.5)

H_A = c("A","H","H","H","A","A","H","A","H","A","H","H") 

length(temple_spread) 

temple_line = -temple_spread


wp_from_line = function(line) {
  sigma = 18.4126
  pnorm((line + 0.5) / sigma)
}

game_wp = wp_from_line(temple_line)


cbind( game= 1:12,H_A = H_A,Temple_spread = temple_spread,Temple_line= round(temple_line, 1),win_prob = round(game_wp, 3))


n_sims  = 10000
n_games = length(game_wp)
season_wins = numeric(n_sims)

for (s in 1:n_sims) {
  game_results = rbinom(n_games, size = 1, prob = game_wp)
  season_wins[s] = sum(game_results)
}



mean(season_wins)          

table(season_wins)         

quantile(season_wins,
         c(0.1, 0.25, 0.5, 0.75, 0.9))


hist(season_wins,breaks = -0.5:(n_games + 0.5),main = "Monte Carlo Distribution of TU Season Wins\n(corrected spreads)",xlab = "Wins in 12-game season")


p_at_least_0  = mean(season_wins == 0)   
p_at_least_1  = mean(season_wins >= 1)
p_at_least_2  = mean(season_wins >= 2)
p_at_least_3  = mean(season_wins >= 3)
p_at_least_4  = mean(season_wins >= 4)
p_at_least_5  = mean(season_wins >= 5)
p_at_least_6  = mean(season_wins >= 6)
p_at_least_7  = mean(season_wins >= 7)
p_at_least_8  = mean(season_wins >= 8)
p_at_least_9  = mean(season_wins >= 9)
p_at_least_10 = mean(season_wins >= 10)

p_at_least_0;p_at_least_1;p_at_least_2;p_at_least_3;p_at_least_4;p_at_least_5;p_at_least_6;p_at_least_7;p_at_least_8;p_at_least_9;p_at_least_10



#LONG DRIVE LM

lm_len2 = lm(DRIVE_PLAY ~  RUN_PASS + TIME_OUTS_REMAINING_OFFENSE + DEF_PACKAGE ,data = dat)

summary(lm_len2)


  