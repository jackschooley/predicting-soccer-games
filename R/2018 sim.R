library(MASS)

master = read.csv("./data/stats.csv")
odds = read.csv("./data/2018_2019 betting.csv")
results = read.csv("./data/results.csv")

vars = c("team", "goals", "ontarget_scoring_att", "clean_sheet", "goals_conceded")
stats = master[master$season == "2017-2018", vars]
input.vars = c("goals", "shots.on.target", "clean.sheets", "goals.conceded")
names(stats)[2:5] = input.vars

#create data for teams that weren't in the prem (mean - half a standard deviation)
createStats = function(i) {
  row = stats[, i]
  if (i == 5) {
    return(ceiling(mean(row) - sd(row) / 2))
  } else {
    return(floor(mean(row) - sd(row) / 2))
  }
}

cardiff = c("Cardiff", createStats(2), createStats(3), createStats(4), createStats(5))
fulham = c("Fulham", createStats(2), createStats(3), createStats(4), createStats(5))
wolves = c("Wolves", createStats(2), createStats(3), createStats(4), createStats(5))
for (team in list(cardiff, fulham, wolves)) {
  stats = rbind(stats, team)
}

#delete the teams that aren't in the prem anymore and refactor
relegated = c(17, 19, 20)
stats = stats[-relegated, ]
stats$team = factor(stats$team, levels = levels(odds$HomeTeam))

#reconstruct the input data frame so it only has data available from that time
inputs = data.frame("date" = odds$Date, "home" = odds$HomeTeam, "away" = odds$AwayTeam, 
                    "home.odds" = odds$B365H, "away.odds" = odds$B365A, "draw.odds" = odds$B365D,
                    "goals" = NA, "shots.on.target" = NA, "clean.sheets" = NA, "goals.conceded" = NA)

inputs$date = as.Date(inputs$date, format = "%d/%m/%Y")
for (i in 1:nrow(inputs)) {
  date = inputs$date[i]
  home.team = inputs$home[i]
  home.stats = rep(0, 4)
  away.team = inputs$away[i]
  away.stats = rep(0, 4)
  for (j in 1:nrow(inputs)) {
    if (date == inputs$date[j]) {
      break
    }
    if (inputs$home[j] == home.team) {
      home.stats[1] = home.stats[1] + odds$FTHG[j]
      home.stats[2] = home.stats[2] + odds$HST[j]
      if (odds$FTAG[j] == 0) {
        home.stats[3] = home.stats[3] + 1
      }
      home.stats[4] = home.stats[4] + odds$FTAG[j]
    }
    else if (inputs$away[j] == home.team) {
      home.stats[1] = home.stats[1] + odds$FTAG[j]
      home.stats[2] = home.stats[2] + odds$AST[j]
      if (odds$FTHG[j] == 0) {
        home.stats[3] = home.stats[3] + 1
      }
      home.stats[4] = home.stats[4] + odds$FTHG[j]
    }
    if (inputs$home[j] == away.team) {
      away.stats[1] = away.stats[1] + odds$FTHG[j]
      away.stats[2] = away.stats[2] + odds$HST[j]
      if (odds$FTAG[j] == 0) {
        away.stats[3] = away.stats[3] + 1
      }
      away.stats[4] = away.stats[4] + odds$FTAG[j]
    }
    else if (inputs$away[j] == away.team) {
      away.stats[1] = away.stats[1] + odds$FTAG[j]
      away.stats[2] = away.stats[2] + odds$AST[j]
      if (odds$FTHG[j] == 0) {
        away.stats[3] = away.stats[3] + 1
      }
      away.stats[4] = away.stats[4] + odds$FTHG[j]
    }
  }
  last.stats = as.numeric(unname(unlist(stats[stats$team == home.team, input.vars]))) - 
               as.numeric(unname(unlist(stats[stats$team == away.team, input.vars])))
  this.stats = home.stats - away.stats
  if (date == "2018-08-10" | date == "2018-08-11" | date == "2018-08-12") {
    inputs[i, input.vars] = last.stats
  }
  else {
    inputs[i, input.vars] = (last.stats + this.stats) / 2
  }
}

inputs$home.prob = 1 / inputs$home.odds
inputs$away.prob = 1 / inputs$away.odds
inputs$draw.prob = 1 / inputs$draw.odds

#run the model using the training data from until 2016-17
train.results = results[results$season != "2017-2018", ]
train.stats = master[master$season != "2017-2018", append(vars, "season")]
names(train.stats)[2:5] = input.vars
train = data.frame("home" = train.results$home_team, "away" = train.results$away_team, 
                   "season" = train.results$season, "result" = train.results$result, 
                   "goals" = NA, "shots.on.target" = NA, "clean.sheets" = NA, "goals.conceded" = NA)
for (game in 1:nrow(train)) {
  home.team = train$home[game]
  season = train$season[game]
  home.stat = train.stats[train.stats$team == home.team & train.stats$season == season, input.vars]
  away.team = train$away[game]
  away.stat = train.stats[train.stats$team == away.team & train.stats$season == season, input.vars]
  train[game, 5:ncol(train)] = home.stat - away.stat
}

#create ordinal logistic model
unnecessary.vars = c("home", "away", "season")
inputs.train = train[ , !names(train) %in% unnecessary.vars]
ord.logit = polr(result ~ ., inputs.train)

#simulate the betting results
predicted.probs = predict(ord.logit, inputs, "probs")
away.probs = predicted.probs[, 1]
draw.probs = predicted.probs[, 2]
home.probs = predicted.probs[, 3]

bets = data.frame("date" = factor(inputs$date), "true.home.prob" = inputs$home.prob, 
                  "true.away.prob" = inputs$away.prob, "true.draw.prob" = inputs$draw.prob, 
                  "pred.home.prob" = home.probs, "pred.away.prob" = away.probs, 
                  "pred.draw.prob" = draw.probs, "result" = odds$FTR)
bets$home.return = inputs$home.odds - 1
bets$away.return = inputs$away.odds - 1
bets$draw.return = inputs$draw.odds - 1
bets$bet = NA
bets$wager = NA

k = 0.3
wager = function(budget, edge, return) {
  proportion = (min(0.1, edge) * (return + 1)) / return
  change = min(0.1, proportion * k)
  bet = max(1, round(budget * change, 2))
  return(bet)
}

budgets = c()
thresholds = seq(0, 0.1, by = 0.001)
for (threshold in thresholds) {
  budget = 200
  for (n in 1:nrow(bets)) {
    home.edge = bets$pred.home.prob[n] - bets$true.home.prob[n]
    away.edge = bets$pred.away.prob[n] - bets$true.away.prob[n]
    draw.edge = bets$pred.draw.prob[n] - bets$true.draw.prob[n]
    best.bet = max(home.edge, away.edge)
    if (best.bet < threshold) {
      bets$bet[n] = "N"
    }
    else if(best.bet == home.edge) {
      bets$bet[n] = "H"
    }
    else if(best.bet == away.edge) {
      bets$bet[n] = "A"
    }
  }
  
  #check result of bet and alter budget
  for (date in levels(bets$date)) {
    games = bets[bets$date == date, ]
    for (n in 1:nrow(games)) {
      if (games$bet[n] == "H") {
        home.edge = games$pred.home.prob[n] - games$true.home.prob[n]
        games$wager[n] = wager(budget, home.edge, games$home.return[n])
      }
      else if (games$bet[n] == "A") {
        away.edge = games$pred.away.prob[n] - games$true.away.prob[n]
        games$wager[n] = wager(budget, away.edge, games$away.return[n])
      }
      else {
        games$wager[n] = 0
      }
    }
    bets[bets$date == date, ] = games
    
    for(n in 1:nrow(games)) {
      if (games$bet[n] == games$result[n]) {
        if (games$bet[n] == "H") {
          budget = budget + games$home.return[n] * games$wager[n]
        }
        else {
          budget = budget + games$away.return[n] * games$wager[n]
        }
      }
      else if (games$bet[n] != "N") {
        budget = budget - games$wager[n]
      }
    }
  }
  budgets = append(budgets, budget)
}

plot(thresholds, budgets)

k = 0.3
wager = function(budget, edge, return) {
  proportion = (min(0.1, edge) * (return + 1)) / return
  change = min(0.1, proportion * k)
  bet = max(1, round(budget * change, 2))
  return(bet)
}

budgets2 = c()
thresholds = seq(0, 0.1, by = 0.001)
for (threshold in thresholds) {
  budget = 200
  for (n in 1:nrow(bets)) {
    home.edge = bets$pred.home.prob[n] - bets$true.home.prob[n]
    away.edge = bets$pred.away.prob[n] - bets$true.away.prob[n]
    draw.edge = bets$pred.draw.prob[n] - bets$true.draw.prob[n]
    best.bet = max(home.edge, away.edge)
    if (best.bet < threshold) {
      bets$bet[n] = "N"
    }
    else if(best.bet == home.edge) {
      bets$bet[n] = "H"
    }
    else if(best.bet == away.edge) {
      bets$bet[n] = "A"
    }
  }
  
#check result of bet and alter budget
  for (date in levels(bets$date)) {
    games = bets[bets$date == date, ]
    for (n in 1:nrow(games)) {
      if (games$bet[n] == "H") {
        home.edge = games$pred.home.prob[n] - games$true.home.prob[n]
        games$wager[n] = wager(budget, home.edge, games$home.return[n])
      }
      else if (games$bet[n] == "A") {
        away.edge = games$pred.away.prob[n] - games$true.away.prob[n]
        games$wager[n] = wager(budget, away.edge, games$away.return[n])
      }
      else {
        games$wager[n] = 0
      }
    }
    bets[bets$date == date, ] = games
  
    for(n in 1:nrow(games)) {
      if (games$bet[n] == games$result[n]) {
        if (games$bet[n] == "H") {
          budget = budget + games$home.return[n] * games$wager[n]
        }
        else {
          budget = budget + games$away.return[n] * games$wager[n]
        }
      }
      else if (games$bet[n] != "N") {
        budget = budget - games$wager[n]
      }
    }
  }
  budgets2 = append(budgets2, budget)
}

plot(thresholds, budgets)
plot(thresholds, budgets2)