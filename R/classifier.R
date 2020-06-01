suppressMessages(library(ordinalNet))
suppressMessages(library(glmnet))
suppressMessages(library(MASS))
suppressMessages(library(ROCR))
library(ggplot2)

#import data
master = read.csv("./data/stats.csv")
results = read.csv("./data/results.csv")

bets = read.csv("./data/2006_2007 Betting.csv")
bets$season = "2006-2007"
for (i in 2007:2017) {
  title = paste("./data/", i, "_", i + 1, " Betting.csv", sep = "")
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
                    "d.punches" = NA, "home.prob" = NA, "away.prob" = NA, "draw.prob" = NA)

#generate two-team inputs from one-team inputs
unnecessary.features = c(1:3, ncol(master))
seasons = levels(inputs$season)
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
}

#format data
incomplete.vars = c("home", "away", "season", "h.saves", "h.headed.clearances", 
                    "h.through.balls", "h.backwards.passes", "h.big.chances.missed", 
                    "h.dispossessed", "d.saves", "d.headed.clearances", "d.through.balls", 
                    "d.backwards.passes", "d.big.chances.missed", "d.dispossessed", "home.prob",
                    "away.prob", "draw.prob")
set.seed(12)
train = sample(1:nrow(inputs), 1000)
inputs.train = inputs[train, !names(inputs) %in% incomplete.vars]
inputs.test = inputs[-train, !names(inputs) %in% incomplete.vars]

#ordinal logistic model
inputs.x = model.matrix(result ~ ., inputs.train)[ , -ncol(inputs.train)]
lasso.cv = ordinalNetCV(inputs.x, inputs.train$result, alpha = 1, printProgress = FALSE)
lambda = lasso.cv$lambdaVals[9]
lasso = ordinalNet(inputs.x, inputs.train$result, 1, lambdaVals = lambda)

ord.logit = polr(result ~ d.goals + d.shots.on.target + d.free.kick.goals + d.clean.sheets + 
                   d.goals.conceded, inputs.train)
fitted.probs = predict(ord.logit, inputs.test, "probs")

#create separate into train/test for binary logistic models
incomplete.vars = append(incomplete.vars, "result")

win.data = inputs[, 4:ncol(inputs)]
win.data$home.win[win.data$result == "H"] = 1
win.data$home.win[win.data$result == "D" | win.data$result == "A"] = 0
win.data = win.data[ , !names(win.data) %in% incomplete.vars]
win.train = win.data[train, ]
win.test = win.data[-train, ]

lose.data = inputs[, 4:ncol(inputs)]
lose.data$home.lose[lose.data$result == "A"] = 1
lose.data$home.lose[lose.data$result == "D" | lose.data$result == "H"] = 0
lose.data = lose.data[ , !names(lose.data) %in% incomplete.vars]
lose.train = lose.data[train, ]
lose.test = lose.data[-train, ]

d.inputs = inputs
d.inputs[, d.start:d.stop] = abs(d.inputs[, d.start:d.stop])
draw.data = d.inputs[, 4:ncol(d.inputs)]
draw.data$draw[draw.data$result == "D"] = 1
draw.data$draw[draw.data$result == "H" | draw.data$result == "A"] = 0
draw.data = draw.data[ , !names(draw.data) %in% incomplete.vars]
draw.train = draw.data[train, ]
draw.test = draw.data[-train, ]

#create lasso models
win.x = model.matrix(home.win ~ ., win.train)[ , -ncol(win.train)]
win.cv = cv.glmnet(win.x, win.train$home.win, alpha = 1)
win.lasso = glmnet(win.x, win.train$home.win, "binomial", alpha = 1)
win.lasso.coef = predict(win.lasso, s = win.cv$lambda.min, type = "coefficients")

lose.x = model.matrix(home.lose ~ ., lose.train)[ , -ncol(lose.train)]
lose.cv = cv.glmnet(lose.x, lose.train$home.lose, alpha = 1)
lose.lasso = glmnet(lose.x, lose.train$home.lose, "binomial", alpha = 1)
lose.lasso.coef = predict(lose.lasso, s = lose.cv$lambda.min, type = "coefficients")

draw.x = model.matrix(draw ~ ., draw.train)[ , -ncol(draw.train)]
draw.cv = cv.glmnet(draw.x, draw.train$draw, alpha = 1)
draw.lasso = glmnet(draw.x, draw.train$draw, "binomial", alpha = 1)
draw.lasso.coef = predict(draw.lasso, s = draw.cv$lambda.min, type = "coefficients")

#win logistic regression
win.logit = glm(home.win ~ h.goals + d.goals + d.free.kick.goals + d.inside.box.goals + d.offsides +
                  d.goals.conceded + d.touches, "binomial", win.train)
win.logit.probs = predict(win.logit, win.test, "response")

#loss logistic regression
lose.logit = glm(home.lose ~ h.header.goals + h.penalty.goals + h.outside.box.goals + 
                   h.counterattack.goals + h.offsides + h.tackles + h.own.goals + h.long.balls +
                   h.corners + h.high.claims + d.goals + d.shots.on.target + d.header.goals +
                   d.free.kick.goals + d.outside.box.goals + d.clean.sheets + d.goals.conceded + 
                   d.tackles + d.touches + d.goal.line.clearances, "binomial", lose.train)
lose.logit.probs = predict(lose.logit, lose.test, "response")

#draw logistic regression
draw.logit = glm(draw ~ h.inside.box.goals + h.counterattack.goals + h.interceptions + h.tackles +
                   d.goals + d.penalty.goals + d.counterattack.goals + d.clean.sheets + 
                   d.goals.conceded, "binomial", draw.train)
draw.logit.probs = predict(draw.logit, draw.test, "response")

#win roc curves
win.fitted.probs = fitted.probs[, 3]
win.roc.pred = prediction(win.fitted.probs, win.test$home.win)
win.roc = performance(win.roc.pred, "tpr", "fpr")
win.logit.roc.pred = prediction(win.logit.probs, win.test$home.win)
win.logit.roc = performance(win.logit.roc.pred, "tpr", "fpr")

plot(win.roc, col = "blue")
plot(win.logit.roc, add = TRUE, col = "red")
abline(0, 1, lty = 2)
legend(0, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))

#lose roc curves
lose.fitted.probs = fitted.probs[, 1]
lose.roc.pred = prediction(lose.fitted.probs, lose.test$home.lose)
lose.roc = performance(lose.roc.pred, "tpr", "fpr")
lose.logit.roc.pred = prediction(lose.logit.probs, lose.test$home.lose)
lose.logit.roc = performance(lose.logit.roc.pred, "tpr", "fpr")

plot(lose.roc, col = "blue")
plot(lose.logit.roc, add = TRUE, col = "red")
abline(0, 1, lty = 2)
legend(0, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))

#draw roc curves
draw.fitted.probs = fitted.probs[, 2]
draw.roc.pred = prediction(draw.fitted.probs, draw.test$draw)
draw.roc = performance(draw.roc.pred, "tpr", "fpr")
draw.logit.roc.pred = prediction(draw.logit.probs, draw.test$draw)
draw.logit.roc = performance(draw.logit.roc.pred, "tpr", "fpr")

plot(draw.roc, col = "blue")
plot(draw.logit.roc, add = TRUE, col = "red")
abline(0, 1, lty = 2)
legend(0, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))

#compare to bet365 betting
win.probs = inputs$home.prob[-train]
lose.probs = inputs$away.prob[-train]
draw.probs = inputs$draw.prob[-train]

ggplot(mapping = aes(win.fitted.probs, win.probs)) + geom_point(col = "blue") + 
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1.5) +
  labs(x = "Predicted Probability", y = "Betting Odds Implied Probability") + xlim(c(0, 1)) +
  ylim(c(0, 1))

ggplot(mapping = aes(lose.fitted.probs, lose.probs)) + geom_point(col = "blue") +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1.5) +
  labs(x = "Predicted Probability", y = "Betting Odds Implied Probability") + xlim(c(0, 1)) +
  ylim(c(0, 1))

ggplot(mapping = aes(draw.fitted.probs, draw.probs)) + geom_point(col = "blue") +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1.5) +
  labs(x = "Predicted Probability", y = "Betting Odds Implied Probability") + xlim(c(0, 0.4)) +
  ylim(c(0, 0.4))