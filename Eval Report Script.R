# Peer Assessments for Fall 2016 Capstone Projects
setwd("C:/Users/nholt2/Desktop/peerdata/")
library(dplyr)
library(car)

# read data
peer <- read.csv("peerevals.csv")
self <- read.csv("selfevals.csv")

# clean up and recode likert questions to numeric
colnames(peer)[5:7] <- c("quality", "quantity", "team_eff")
colnames(self)[5:7] <- c("quality", "quantity", "team_eff")

peer$quality <- recode(peer$quality, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")
peer$quantity <- recode(peer$quantity, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")
peer$team_eff <- recode(peer$team_eff, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")

self$quality <- recode(self$quality, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")
self$quantity <- recode(self$quantity, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")
self$team_eff <- recode(self$team_eff, "'well below average'='1'; 'below average'='2'; 'average'='3'; 'above average'='4'; 'well above average'='5'")

# create full data frame
self$assess_type <- "self"
peer$assess_type <- "peer"
all <- rbind(peer, self)

all[,5] <- as.numeric(all[,5])
all[,6] <- as.numeric(all[,6])
all[,7] <- as.numeric(all[,7])
all[,7] <- all[,7]-1

# set up team data frames
team1 <- subset(all, name == "Derrick Devane" | name == "Sean Dockery" | name == "Joseph George" | name == "Darryl Pardon" | name == "John Sieboldt" | name == "Michael VonSick")
team2 <- subset(all, name == "McKenna Edgett" | name == "Kevin Finney" | name == "James Gandenberger" | name == "Ben Graft" | name == "Joel Kleyer" | name == "Kris Stevens" | name == "Carson Wheeless")
team3 <- subset(all, name == "Sean Ayres" | name == "Carder Labrake" | name == "Jacob Marburger" | name == "Jonathan Marks" | name == "Brian VanVactor" | name == "Erin Yepis")
team4 <- subset(all, name == "David Casale" | name == "William George" | name == "Sonny Hydes" | name == "Steven Lavelle" | name == "Ingrid Martinez" | name == "Jayesh Rai")

team1$team <- "team1"
team2$team <- "team2"
team3$team <- "team3"
team4$team <- "team4"

all <- rbind(team1, team2, team3, team4)

peer <- subset(all, assess_type == "peer")
self <- subset(all, assess_type == "self")

# create summary table for measures (quality, quantity, team_effectiveness, hire, bonus_amt)
table <- peer %>%
        group_by(assessee, team) %>%
        summarize( 
                  num_reviews = n(), 
                  quality = mean(quality), 
                  quantity = mean(quantity), 
                  team_effectiveness = mean(team_eff), 
                  hire_yes = sum(as.numeric((hire[hire == "Yes"]))/2),
                  hire_no = sum(as.numeric((hire[hire == "No"]))),
                  bonus_avg = mean(bonus_amt),
                  min_bonus = min(bonus_amt),
                  max_bonus = max(bonus_amt)
                  ) %>%
                        as.data.frame()

# create table to summarize team statistics
team.table <- table %>% 
        group_by(team) %>%
        summarize(
                team_avg_quality = mean(quality),
                team_avg_quantity = mean(quantity),
                team_avg_effectiveness = mean(team_effectiveness),
                team_avg_hire_yes = mean(hire_yes),
                team_avg_hire_no = mean(hire_no),
                team_avg_bonus = mean(bonus_avg),
                team_min_bonus = min(min_bonus),
                team_max_bonus = max(max_bonus)
                ) %>%
                        as.data.frame()

# join team data to individual table
# table <- left_join(table, team.table, by = "team")


# export to csv
# write.csv(table, "Peer Eval Summary.csv", row.names = F)
# write.csv(team.table, "Peer Eval Summary by Team.csv", row.names = F)

