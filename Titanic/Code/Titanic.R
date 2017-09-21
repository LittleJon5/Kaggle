require(tidyverse)

titanic <- read.table("E:\\Document\\KaggleProjects\\Kaggle\\Titanic\\Data\\train.csv", sep = ',', header = TRUE)


# Survival Barchar on Sex -------------------------------------------------

ggplot() +
    geom_bar(data = titanic, aes(x = Sex), fill = 'blue') +
    geom_bar(data = titanic[titanic$Survived == 0,], aes(x = Sex), fill = 'red')


# Survival Barchar on Pclass ----------------------------------------------

ggplot() +
  geom_bar(data = titanic, aes(x = Pclass), fill = 'blue') +
  geom_bar(data = titanic[titanic$Survived == 0,], aes(x = Pclass), fill = 'red')


# basic logit age vs survivors-------------------------------------------------------------

ageMod <- glm(factor(Survived) ~ Age, data = titanic, family = binomial(link = 'logit'))

ageMod %>% summary

ageMod %>% plot

curve(predict(ageMod, data.frame(Age = x), type = "response", add = TRUE))
points(Age, fitted(ageMod), pch = 20)


# More Complex Logit ------------------------------------------------------

ageModII <- glm(Survived ~ Pclass + Sex + Age + Parch + Fare,
                data = titanic,
                family = binomial(link = 'logit'))

# Density curves ----------------------------------------------------------

ggplot() +
  geom_density(data = titanic, aes(x = Age), size = 1.75) +
  geom_density(data = titanic[titanic$Survived == 1,], aes(x = Age), color = 'blue', size = 1.75) +
  geom_density(data = titanic[titanic$Survived == 0,], aes(x = Age), color = 'red', size = 1.75) +
  geom_density(data = titanic[titanic$Sex == "male",], aes(x = Age), color = 'green', size = 1.75) +
  geom_density(data = titanic[titanic$Sex == "female",], aes(x = Age), color = 'pink', size = 1.75) 

# 

# Box Plot age diagrams ---------------------------------------------------

ggplot(data = titanic, aes(x = factor(Survived), y = Age, color = Sex)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  coord_flip()
