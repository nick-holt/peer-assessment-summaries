# ANLY 590 Capstone Peer Assessment Summarization 

setwd("C:/Users/nholt2/Desktop/peerdata/student_summaries")

# This function depends on the dataframes that were generated from the Eval Report Script.R file. The function
# should take three data frames (peer, team.table, and self) and generate a file for each individual 
# enrolled in the capstone course if type = "individual". Each file should contain three rows of data: peer assessment, 
# team averages, and self assessment. If type = "all", a single .csv file will be generated that will include summary
# data for all students in the course.

evalsummary <- function(type, df1 = table, df2 = peer, df3 = self) {
        studentnames <- as.vector(unique(self[,3]))
        alldata <- as.data.frame(matrix(nrow = 1, ncol = 12))
        firstpass <- 0
        # for loop to create summary data for each student in the class
        for(i in studentnames) {
                studentteam <- subset(df3, name == i)
                studentteam <- studentteam[1,12]
                # team data should exclude data for the individual
                t <- df2 %>%
                        subset(team == studentteam) %>% 
                        subset(assessee != i) %>%
                        group_by(team) %>%
                        summarize(
                                team_avg_quality = mean(quality),
                                team_avg_quantity = mean(quantity),
                                team_avg_effectiveness = mean(team_eff),
                                hire_yes = sum(as.numeric((hire[hire == "Yes"]))/2)/n(),
                                hire_no = sum(as.numeric((hire[hire == "No"])))/n(),
                                bonus_avg = mean(bonus_amt),
                                min_bonus = min(bonus_amt),
                                max_bonus = max(bonus_amt)
                        ) %>%
                        as.data.frame()
                # peer data
                p <- as.data.frame(subset(df1, assessee == i))
                # self data
                s <- as.data.frame(subset(df3, name == i))
                # need to make the frames identical
                        # p edits
                                p <- p[-c(1)]
                                p <- p[-c(2)]
                                colnames(p)[1] <- "assessment_type"
                                p[1,1] <- "peer"
                                hiresum <- sum(p$hire_yes, p$hire_no)
                                p$hire_yes <- p$hire_yes/hiresum
                                p$hire_no <- p$hire_no/hiresum
                        # t edits
                                colnames(t) <- colnames(p)
                                t[1,1] <- "team"
                        # s edits
                                s <- s[-c(1:3)]
                                s <- s[-c(7:9)]
                                s$hire_yes <- sum(as.numeric((s$hire[s$hire == "Yes"]))/2)
                                s$hire_no <- sum(as.numeric((s$hire[s$hire == "No"])))
                                s$bonus_avg <- s$bonus_amt
                                s$bonus_max <- "NA"
                                s$bonus_min <- "NA"
                                s <- s[-c(5:6)]
                                colnames(s) <- colnames(p)
                                s$assessment_type <- "self"
                # bind rows into dataframe and add columns for student name, team_grade, and individual grade
                summary <- rbind(p, t, s)
                summary$student <- i
                summary$team_grade <- NA
                summary$individual_grade <- NA
                # add colnames and delete first row of alldata if first pass = 0
                if(firstpass == 0){
                        colnames(alldata) <- colnames(summary)
                        alldata <- alldata[-1,]
                        firstpass <- 1
                } else {
                }
                # write files: two options specified by the type argument
                if(type == "individual" | type != "all") {
                        # write individual files for each student
                        write.csv(summary, paste0(i, "_peer_eval_summary.csv"), row.names = F)
                } else if(type == "all") {
                        alldata <- rbind(alldata, summary)
                } else {
                }
        }
        # if type = all, write alldata to csv after the for loop is finished
        if(type == "all"){
                write.csv(alldata, "Peer Eval Data - Master.csv", row.names = F)
        } else {
        }
}
