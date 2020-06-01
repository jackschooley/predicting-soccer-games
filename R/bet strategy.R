library(ordinalNet)
library(glmnet)
library(MASS)

#import data
master = read.csv("./data/stats.csv")
results = read.csv("./data/results.csv")

bets = read.csv("./data/2006_2007 Betting.csv")
bets$season = "2006-2007"
for (i in 2007:2017) {
  title = paste("./data", i, "_", i + 1, " Betting.csv", sep = "")
  bets.temp = read.csv(title)
  bets.temp$season = paste(i, "-", i + 1, sep = "")
  bets = rbind(bets, bets.temp)
}
rm(bets.temp)

#convert odds to probabilities
bets$home.prob = 1 - ((bets$B365H - 1) / bets$B365H)
bets$away.prob = 1 - ((bets$B365A - 1) / bets$B365A)
bets$draw.prob = 1 - ((bets$B365D - 1) / bets$B365D)

#create input data frame
inputs = data.frame("home" = results$home_team, "away" = results$away_team,
                    "season" = results$season, "result" = results$result, "h.goals" = NA,
                    "h.yellows" = NA, "h.reds" = NA, "h.shots" = NA, "h.shots.on.target" = NA, 
                    "h.hit.woodwork" = NA, "h.header.goals" = NA, "h.penalty.goals" = NA, 
                    "h.free.kick.goals" = NA, "h.inside.box.goals" = NA, "h.outside.box.goals" = NA, 
                    "h.counterattack.goals" = NA, "h.offsides" = NA, "h.clean.sheets" = NA, 
                    "h.goals.conceded" = NA, "h.saves" = NA, "h.blocks" = NA, 
                    "h.interceptions" = NA, "h.tackles" = NA, "h.last.man.tackles" = NA,
                    "h.clearances" = NA, "h.headed.clearances" = NA, "h.own.goals" = NA,
                    "h.penalties.conceded" = NA, "h.penalty.goals.conceded" = NA, "h.passes" = NA, 
                    "h.through.balls" = NA, "h.long.balls" = NA, "h.backwards.passes" = NA, 
                    "h.crosses" = NA, "h.corners" = NA, "h.touches" = NA, 
                    "h.big.chances.missed" = NA, "h.goal.line.clearances" = NA, 
                    "h.dispossessed" = NA, "h.penalties.saved" = NA, "h.high.claims" = NA, 
                    "h.punches" = NA, "d.goals" = NA, "d.yellows" = NA, "d.reds" = NA, 
                    "d.shots" = NA, "d.shots.on.target" = NA, "d.hit.woodwork" = NA, 
                    "d.header.goals" = NA, "d.penalty.goals" = NA, "d.free.kick.goals" = NA,
                    "d.inside.box.goals" = NA, "d.outside.box.goals" = NA, 
                    "d.counterattack.goals" = NA, "d.offsides" = NA, "d.clean.sheets" = NA, 
                    "d.goals.conceded" = NA, "d.saves" = NA, "d.blocks" = NA,
                    "d.interceptions" = NA, "d.tackles" = NA, "d.last.man.tackles" = NA,
                    "d.clearances" = NA, "d.headed.clearances" = NA, "d.own.goals" = NA,
                    "d.penalties.conceded" = NA, "d.penalty.goals.conceded" = NA,
                    "d.passes" = NA, "d.through.balls" = NA, "d.long.balls" = NA,
                    "d.backwards.passes" = NA, "d.crosses" = NA, "d.corners" = NA,
                    "d.touches" = NA, "d.big.chances.missed" = NA, "d.goal.line.clearances" = NA, 
                    "d.dispossessed" = NA, "d.penalties.saved" = NA, "d.high.claims" = NA, 
                    "d.punches" = NA, "home.prob" = NA, "away.prob" = NA, "draw.prob" = NA,
                    "home.return" = NA, "away.return" = NA, "draw.return" = NA)

#generate two-team inputs from one-team inputs
unnecessary.features = c(1:3, ncol(master))
h.start = 5
h.stop = 42
d.start = 43
d.stop = 80
for (game in 1:nrow(inputs)) {
      home.team = inputs$home[game]
      season =inputs$season[game]
      home.stat = master[master$team == home.team & master$season == season, -unnecessary.features]
      away.team = inputs$away[game]
      away.stat = master[master$team == away.team & master$season == season, -unnecessary.features]
      inputs[game, h.start:h.stop] = home.stat
      inputs[game, d.start:d.stop] = home.stat - away.stat
      inputs$home.prob[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "home.prob"]
      inputs$away.prob[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "away.prob"]
      inputs$draw.prob[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "draw.prob"]
      inputs$home.return[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "B365H"]
      inputs$away.return[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "B365A"]
      inputs$draw.return[game] = bets[bets$HomeTeam == home.team & bets$AwayTeam == away.team & 
                                      bets$season == season, "B365D"]
}

#format data
incomplete.vars = c("home", "away", "season", "h.saves", "h.headed.clearances", 
                    "h.through.balls", "h.backwards.passes", "h.big.chances.missed", 
                    "h.dispossessed", "d.saves", "d.headed.clearances", "d.through.balls", 
                    "d.backwards.passes", "d.big.chances.missed", "d.dispossessed", "home.prob",
                    "away.prob", "draw.prob", "home.return", "away.return", "draw.return")
set.seed(12)
train = sample(1:nrow(inputs), 1000)

avgs = rep(0, 10)
threshold = 0
for (season in levels(inputs$season)) {
  #inputs.train = inputs[train, !names(inputs) %in% incomplete.vars]
  inputs.train = inputs[inputs$season != season, !names(inputs) %in% incomplete.vars]
  #inputs.test = inputs[-train, !names(inputs) %in% incomplete.vars]
  inputs.test = inputs[inputs$season == season, !names(inputs) %in% incomplete.vars]

  #ordinal logistic model
  #inputs.x = model.matrix(result ~ ., inputs.train)[ , -ncol(inputs.train)]
  #lasso.cv = ordinalNetCV(inputs.x, inputs.train$result, alpha = 1)
  #lambda = lasso.cv$lambdaVals[9] #use 12 for just simming latest season
  #lasso = ordinalNet(inputs.x, inputs.train$result, 1, lambdaVals = lambda)

  ord.logit = polr(result ~ d.goals + d.shots.on.target + d.free.kick.goals + d.clean.sheets + 
                   d.goals.conceded, inputs.train)
  fitted.probs = predict(ord.logit, inputs.test, "probs")
  win.fitted.probs = fitted.probs[, 3]
  lose.fitted.probs = fitted.probs[, 1]
  draw.fitted.probs = fitted.probs[, 2]

  #compare to bet365 betting
  win.probs = inputs$home.prob[inputs$season == season]
  lose.probs = inputs$away.prob[inputs$season == season]
  draw.probs = inputs$draw.prob[inputs$season == season]

  #win.probs = inputs$home.prob[-train]
  #lose.probs = inputs$away.prob[-train]
  #draw.probs = inputs$draw.prob[-train]

  #create df for hypothetical bets
  home.return = inputs$home.return[inputs$season == season] - 1
  away.return = inputs$away.return[inputs$season == season] - 1
  draw.return = inputs$draw.return[inputs$season == season] - 1
  #home.return = inputs$home.return[-train] - 1
  #away.return = inputs$away.return[-train] - 1
  #draw.return = inputs$draw.return[-train] - 1
  bets.hyp = data.frame("true.home.prob" = win.probs, "true.away.prob" = lose.probs, 
                      "true.draw.prob" = draw.probs, "pred.home.prob" = win.fitted.probs,
                      "pred.away.prob" = lose.fitted.probs, "pred.draw.prob" = draw.fitted.probs,
                      "result" = inputs.test$result, home.return, away.return, draw.return)

  #make hypothetical bets
  bets.hyp$bet = NA
  final.balances = c()
  home.profits = c()
  away.profits = c()
  tot.games = c()
  for (k in seq(0.1, 1, 0.1)) {
    for (n in 1:nrow(bets.hyp)) {
      home.edge = bets.hyp$pred.home.prob[n] - bets.hyp$true.home.prob[n]
      away.edge = bets.hyp$pred.away.prob[n] - bets.hyp$true.away.prob[n]
      draw.edge = bets.hyp$pred.draw.prob[n] - bets.hyp$true.draw.prob[n]
      best.bet = max(home.edge, away.edge)
      if (best.bet < threshold) {
        bets.hyp$bet[n] = "N"
      }
      else if(best.bet == home.edge) {
        bets.hyp$bet[n] = "H"
      }
      else if(best.bet == away.edge) {
        bets.hyp$bet[n] = "A"
      }
    }
    
    wager = function(budget, edge, return) {
      proportion = (min(0.1, edge) * (return + 1)) / return
      change = proportion * k
      bet = round(budget * change, 2)
      return(bet)
    }

    #calculate returns
    budget = 200
    home.profit = 0
    away.profit = 0
    for (n in 1:nrow(bets.hyp)) {
      if (bets.hyp$bet[n] == "H") {
        home.edge = bets.hyp$pred.home.prob[n] - bets.hyp$true.home.prob[n]
        home.return = bets.hyp$home.return[n]
        bet = wager(budget, home.edge, home.return)
        if(bets.hyp$result[n] == "H") {
          return = bet * bets.hyp$home.return[n]
          budget = budget + round(return, 2)
          home.profit = home.profit + bet * bets.hyp$home.return[n]
        }
        else {
          budget = budget - bet
          home.profit = home.profit - bet
        }
      }
      else if(bets.hyp$bet[n] == "A") {
        away.edge = bets.hyp$pred.away.prob[n] - bets.hyp$true.away.prob[n]
        away.return = bets.hyp$away.return[n]
        bet = wager(budget, away.edge, away.return)
        if (bets.hyp$result[n] == "A") {
          return = bet * bets.hyp$away.return[n]
          budget = budget + round(return, 2)
          away.profit = away.profit + bet * bets.hyp$away.return[n]
        }
        else {
          budget = budget - bet
          away.profit = away.profit - bet
        }
      }
    }
  
    final.balances = append(final.balances, budget)
    home.profits = append(home.profits, home.profit)
    away.profits = append(away.profits, away.profit)
  
    tab = table(bets.hyp$bet)
    games = as.integer(tab[1] + tab[2])
    tot.games = append(tot.games, games)
  }
  avgs = avgs + final.balances
  vals = seq(0.1, 1, 0.1)
  #plot(vals, final.balances)
}

avgs = avgs / length(levels(inputs$season))
k = seq(0.1, 1, 0.1)
plot(k, avgs)