setwd("~/Thesis")
library(ordinalNet)
library(glmnet)
library(MASS)
library(pROC)

#import data
master = read.csv("stats.csv")
results = read.csv("results.csv")

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
                    "d.punches" = NA)
inputs = inputs[inputs$season != "2006-2007", ]

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
}

#format data
incomplete.vars = c("home", "away", "season", "h.saves", "h.headed.clearances", 
                    "h.through.balls", "h.backwards.passes", "h.big.chances.missed", 
                    "h.dispossessed", "d.saves", "d.headed.clearances", "d.through.balls", 
                    "d.backwards.passes", "d.big.chances.missed", "d.dispossessed")
set.seed(12)
train = sample(1:nrow(inputs), 1000)
inputs.train = inputs[train, !names(inputs) %in% incomplete.vars]
inputs.test = inputs[-train, !names(inputs) %in% incomplete.vars]

#ordinal logistic model
inputs.x = model.matrix(result ~ ., inputs.train)[ , -ncol(inputs.train)]
lasso.cv = ordinalNetCV(inputs.x, inputs.train$result, alpha = 1)
lambda = lasso.cv$lambdaVals[9]
lasso = ordinalNet(inputs.x, inputs.train$result, 1, lambdaVals = lambda)
print(lasso$coefs)

ord.logit = polr(result ~ h.shots.on.target + d.goals + d.outside.box.goals + d.clean.sheets + 
                   d.goals.conceded + d.penalties.saved, inputs.train)
fitted.probs = predict(ord.logit, inputs.test, "probs")
fitted.vals = predict(ord.logit, inputs.test)
confusion = table(inputs.test$result, fitted.vals, dnn = c("Actual", "Predicted"))

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

draw.data = inputs[, 4:ncol(inputs)]
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

print(win.lasso.coef)
print(lose.lasso.coef)
print(draw.lasso.coef)

#win logistic regression
win.logit = glm(home.win ~ h.goals + h.shots.on.target + d.goals + d.outside.box.goals + 
                  d.goals.conceded + d.penalties.saved, "binomial", win.train)
win.logit.probs = predict(win.logit, win.test, "response")
win.logit.preds = factor(win.logit.probs > 0.5, levels = c(FALSE, TRUE), 
                         labels = c("Don't Win", "Win"))
win.test$home.win = factor(win.test$home.win, c(0, 1), c("Don't Win", "Win"))
win.logit.table = table(win.test$home.win, win.logit.preds, dnn = c("Actual", "Predicted"))

#loss logistic regression
lose.logit = glm(home.lose ~ h.hit.woodwork + d.goals + d.yellows +d.outside.box.goals + 
                   d.clean.sheets + d.goals.conceded + d.penalties.saved, "binomial",
                 lose.train)
lose.logit.probs = predict(lose.logit, lose.test, "response")
lose.logit.preds = factor(lose.logit.probs > 0.5, levels = c(FALSE, TRUE),
                          labels = c("Don't Lose", "Lose"))
lose.test$home.lose = factor(lose.test$home.lose, c(0, 1), c("Don't Lose", "Lose"))
lose.logit.table = table(lose.test$home.lose, lose.logit.preds, dnn = c("Actual", "Predicted"))

#draw logistic regression
draw.logit = glm(draw ~ h.goals, "binomial", draw.train)
draw.logit.probs = predict(draw.logit, draw.test, "response")
draw.logit.preds = factor(draw.logit.probs > 0.5, levels = c(FALSE, TRUE),
                          labels = c("Don't Draw", "Draw"))
draw.test$draw = factor(draw.test$draw, c(0, 1), c("Don't Draw", "Draw"))
draw.logit.table = table(draw.test$draw, draw.logit.preds, dnn = c("Actual", "Predicted"))

#win roc curves
win.fitted.vals = fitted.vals
win.fitted.vals[win.fitted.vals == "D"] = "A"
win.fitted.vals = factor(win.fitted.vals, level = c("A", "H"), labels = c("Don't Win", "Win"))
win.roc = roc(win.test$home.win, c(win.fitted.vals))
win.logit.roc = roc(win.test$home.win, c(win.logit.preds))
plot(win.roc, main = "Win ROC Curve Comparison", col = "blue")
lines(win.logit.roc, col = "red")
legend(1.1, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))
png("win roc curves.png")

#lose roc curves
lose.fitted.vals = fitted.vals
lose.fitted.vals[win.fitted.vals == "D"] = "H"
lose.roc = roc(lose.test$home.lose, c(lose.fitted.vals))
lose.logit.roc = roc(lose.test$home.lose, c(lose.logit.preds))
plot(lose.roc, main = "Lose ROC Curve Comparison", col = "blue")
lines(lose.logit.roc, col = "red")
legend(1.1, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))
png("lose roc curves.png")

#draw roc curves
draw.fitted.vals = fitted.vals
draw.fitted.vals[draw.fitted.vals == "H"] = "A"
draw.roc = roc(draw.test$draw, c(draw.fitted.vals))
draw.logit.roc = roc(draw.test$draw, c(draw.logit.preds))
plot(draw.roc, main = "Draw ROC Curve Comparison", col = "blue")
lines(draw.logit.roc, col = "red", lty = 2)
legend(1.1, 1, c("Ordinal Logistic", "Simple Logistic"), c("blue", "red"))
png("draw roc curves.png")