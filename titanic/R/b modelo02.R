load("RData/titanic.RData")


train_control_model02 <- trainControl(
  
)

tune_grid_model02 <- expand.grid(
  
)

model02 <- train(
  
)

save(model02, file = "RData/model02.RData")
