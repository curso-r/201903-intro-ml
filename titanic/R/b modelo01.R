load("RData/titanic.RData")


train_control_model01 <- trainControl(
  
)

tune_grid_model01 <- expand.grid(
  
)

model01 <- train(
  
)

save(model01, file = "RData/model01.RData")
