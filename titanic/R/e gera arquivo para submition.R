load("RData/titanic_predicted.RData")

submition_model01 <- titanic_predicted %>%
  dplyr::filter(base %in% "test") %>%
  dplyr::mutate(Survived = if_else(score_model01 > 0.465, 1, 0)) %>%
  dplyr::select(PassengerId,
                Survived) 

submition_model01 %>% write_csv(path = "submition_model01.csv")


submition_model02 <- titanic_predicted %>%
  dplyr::filter(base %in% "test") %>%
  dplyr::mutate(Survived = if_else(score_model02 > 0.422, 1, 0)) %>%
  dplyr::select(PassengerId,
                Survived) 

submition_model02 %>% write_csv(path = "submition_model02.csv")


submition_modelXX <- titanic_predicted %>%
  dplyr::filter(base %in% "test") %>%
  dplyr::mutate(Survived = if_else(score_modelXX > 0.367, 1, 0)) %>%
  dplyr::select(PassengerId,
                Survived) 

submition_modelXX %>% write_csv(path = "submition_modelXX.csv")

