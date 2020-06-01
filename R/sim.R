setwd("~/Thesis")
library(MASS)

#import data
last.stats = read.csv("sim last stats.csv")
this.stats = read.csv("sim md29 stats.csv")
matches = read.csv("sim matches.csv")

master = read.csv("stats.csv")
results = read.csv("results.csv")

#create input data frame
inputs = data.frame("home" = results$home_team, "away" = results$away_team, "season" = results$season,
                    "result" = results$result, "goals" = NA, "shots.on.target" = NA, 
                    "clean.sheets" = NA, "goals.conceded" = NA)
features = c("goals", "ontarget_scoring_att", "clean_sheet", "goals_conceded")
for (game in 1:nrow(inputs)) {
  home.team = inputs$home[game]
  season = inputs$season[game]
  home.stat = master[master$team == home.team & master$season == season, features]
  away.team = inputs$away[game]
  away.stat = master[master$team == away.team & master$season == season, features]
  inputs[game, 5:ncol(inputs)] = home.stat - away.stat
}

#create ordinal logistic model
unnecessary.vars = c("home", "away", "season")
inputs.train = inputs[ , !names(inputs) %in% unnecessary.vars]
ord.logit = polr(result ~ ., inputs.train)

#create prediction data frame
matches$goals = NA
matches$shots.on.target = NA
matches$clean.sheets = NA
matches$goals.conceded = NA

for (game in 1:nrow(matches)) {
  home.team = matches$home[game]
  home.this.stat = this.stats[this.stats$team == home.team, names(this.stats) != "team"]
  home.last.stat = last.stats[last.stats$team == home.team, names(last.stats) != "team"]
  
  away.team = matches$away[game]
  away.this.stat = this.stats[this.stats$team == away.team, names(this.stats) != "team"]
  away.last.stat = last.stats[last.stats$team == away.team, names(last.stats) != "team"]
  
  this.stat = home.this.stat - away.this.stat
  last.stat = home.last.stat - away.last.stat
  matches[game, 7:ncol(matches)] = (this.stat + last.stat) / 2
}

#predict and compare
predicted.probs = predict(ord.logit, matches, "probs")

matches$away.prob = 1 / matches$away.odds
matches$draw.prob = 1 / matches$draw.odds
matches$home.prob = 1 / matches$home.odds

wager = function(budget, edge, return) {
  proportion = (min(0.1, edge) * (return + 1)) / return
  change = min(0.1, proportion * 0.3)
  bet = max(1, round(budget * change, 2))
  return(bet)
}

threshold = 0.025
budget = 223.41
bets = matrix(NA, nrow(matches), 5)
for (n in 1:nrow(matches)) {
  bets[n, 1] = as.character(matches$date[n])
  bets[n, 2] = as.character(matches$home[n])
  bets[n, 3] = as.character(matches$away[n])
  away.edge = predicted.probs[n, 1] - matches$away.prob[n]
  draw.edge = predicted.probs[n, 2] - matches$draw.prob[n]
  home.edge = predicted.probs[n, 3] - matches$home.prob[n]
  best.bet = max(home.edge, away.edge)
  if (best.bet < threshold) {
    bets[n, 4] = "N"
    bets[n, 5] = 0
  }
  else if(best.bet == home.edge) {
    bets[n, 4] = "H"
    home.return = matches$home.odds[n] - 1
    bets[n, 5] = wager(budget, home.edge, home.return)
  }
  else if(best.bet == away.edge) {
    bets[n, 4] = "A"
    away.return = matches$away.odds[n] - 1
    bets[n, 5] = wager(budget, away.edge, away.return)
  }
  else {
    bets[n, 4] = "D"
    draw.return = matches$draw.odds[n] - 1
    bets[n, 5] = wager(budget, draw.edge, draw.return)
  }
}

tail(bets, 10)