
install.packages("lme4")
install.packages("reshape")
install.packages("effects")

library('ggplot2')
library ('reshape')
library('lme4')
library('effects')
library('lmerTest')



Experiment_data<-read.csv("plausability - Foaie1.csv", header=TRUE, fill=TRUE)

Experiment_data$new <- with(Experiment_data, ifelse(condition=='condition 1'| condition=='condition 3', 1, 0))
Experiment_data$morpheme <- with(Experiment_data, ifelse(condition=='condition 1'| condition=='condition 4', 1, 0))
Experiment_data
Experiment_data$new <- as.factor(Experiment_data$new)
Experiment_data$morpheme <-as.factor(Experiment_data$morpheme)

contrasts(Experiment_data$new) = c(-0.5, +0.5)
contrasts(Experiment_data$morpheme) = c(-0.5, +0.5)

Experiment_data$productive <- factor(Experiment_data$productive)

contrasts(Experiment_data$productive) = c(-0.5, +0.5)

model <- glmer(response_plausable ~ morpheme*new + (1|type_template), data = Experiment_data)

# Print the model summary
summary(model)

#fails to converge 
model2 <- glmer(response_plausable ~ new*morpheme*productive + (1|type_template), data = Experiment_data, family = binomial)

linear_model<-glm(response_plausable ~ morpheme*new*productive, data = Experiment_data, family = binomial)
summary(linear_model)
# Print the model summary
summary(model2)
#anovatype_template
Experiment_data$type_template <- factor(Experiment_data$type_template)
ggplot(Finaldata, aes(x=type, y=total_successes)) + geom_boxplot() +
  guides(fill=FALSE)
plot(effect(term="new*morpheme*productive",mod=model2), 
     main = 'Interaction between "new", "morpheme",\nand the productivity of the morpheme in the test word', 
     multiline=TRUE)
library(dplyr)

install.packages("MASS")
# Assuming your existing data frame is named 'data' and has columns: Word, Morpheme, Condition, Definition
library('MASS')
# Group the data by Text and Condition, and calculate the count of correct responses
Experiment_data <- Experiment_data %>%
  group_by(type_template, condition) %>%
  mutate(CorrectCount = sum(response_plausable == 1))

# Print the updated data frame
print(data)

ggplot(Experiment_data, aes(x=type_template, y=CorrectCount)) + geom_boxplot() +
  xlab("Prompt name") +
  ylab("Number of plausible definitions") +
  ggtitle("Boxplot showing number of plausible definitions provided following each of the prompting strategies") +
  labs(colour="new") +
  scale_x_discrete(labels=c('Context\nManager', 'Define...', 'Infinite\nGenerator', 'Lexicographer\nPersona', 'Template', 'Word Generator\nPersona')) +
  guides(fill=FALSE)
