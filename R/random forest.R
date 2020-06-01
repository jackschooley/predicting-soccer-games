#win random forest
win.train$home.win = factor(win.train$home.win, c(0, 1), c("Don't Win", "Win"))
set.seed(6)
win.rf = randomForest(home.win ~ ., win.train)
win.rf.preds = predict(win.rf, win.test)

#loss random forest
lose.train$home.lose = factor(lose.train$home.lose, c(0, 1), c("Don't Lose", "Lose"))
set.seed(21)
lose.rf = randomForest(home.lose ~ ., lose.train)
lose.rf.preds = predict(lose.rf, lose.test)