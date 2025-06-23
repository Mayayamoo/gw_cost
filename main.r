library(dplyr)
student_bill <- data.frame(
  type = c(
    "student_gov_fee",
    "tuition", 
    "transit",
    "student_activities_fee",
    "matriculation_fee",
    "health_insurance"
    ),
  code = c("ESA", "T110", "UPAS", "ESTA", "EMAT", "SHIN"),
  sem_cost = c(21.00, 16310.00, 100.00, 50.00, 350.00, 1273.00),
  stringsAsFactors = FALSE
)


semester_cost <- sum(student_bill$sem_cost)
semester_cost_noinsurance <- sum(student_bill$sem_cost[-6])

housing <- data.frame(
    type = c("student", "apartment", "house"),
    sem_cost = c(7100, 8000, -2400)
)

food <- data.frame(
  type = c("all_access", "block90", "block120", "commuter50", "commuter25", "hellofresh"),
  sem_cost = c(3150.00, 2345.00, 2545.00, 950.00, 475.00, 1600.00)
)

aid <- data.frame(
    type = c("grant","scholarship","subsidized","unsubsidized"),
    sem_cost = c(3700,0,1750,3000)
)

all_data <- list(housing, food, aid, student_bill)

monthly <- function(x) {
    for (df in all_data) {
        if (x %in% df$type) {
            monthly <- df$sem_cost[df$type == x]
            monthly <- monthly/4
            return(monthly)
        }
    }
    monthly <- x/4
    return(monthly)
}

yearly <- function(x) {
    for (df in all_data) {
        if (x %in% df$type) {
            yearly <- df$sem_cost[df$type == x]
            yearly <- yearly/4
            return(yearly)
        }
    }
    yearly <- x*2
    return(yearly)
}

summer <- function(y) {
    summer <- y*2330
    return(summer)
}

#all aid
AA <- function() {
    all_aid <- sum(aid$sem_cost)
    return(all_aid)
}

#Loans only
LO <- function() {
    all_aid <- sum(aid$sem_cost[3:4])
    return(all_aid)
}

#Subsidized loan only
SLO <- function() {
    all_aid <- aid$sem_cost[3]
    return(all_aid)
}

# Grants only
GO <- function() {
    all_aid <- aid$sem_cost[1]
    return(all_aid)
}

#Grants and Subsidized loans only
GSL <- function() {
    all_aid <- sum(aid$sem_cost[c(1,3)])
    return(all_aid)
}

aid_plans <- list(
    "Grants and Subsidized Loan" = GSL,
    "Grants Only" = GO,
    "Subsidized Loan Only" = SLO,
    "Loans Only" = LO,
    "All Aid" = AA)

sem_cost_waid <- function(x) {
    aid_amount <- x
    sem.cost.aid <- (semester_cost_noinsurance - aid_amount)
    return(sem.cost.aid)
}

aid_cost_list <- function(x) {
    aid_amount <- x
    sem.cost.aid <- (semester_cost_noinsurance - aid_amount)
    ysca <- yearly(sem.cost.aid)
    msca <- monthly(sem.cost.aid)
    costs <- c(ysca, sem.cost.aid, msca)
    names <- c("yearly", "semester", "monthly")
    aid_cost_list <- data.frame(names, costs)
    return(aid_cost_list)
}
catchemall <- function() {
    lapply(aid_plans, function(aid_func) {
        aid_cost_list(aid_func())
    })
}   