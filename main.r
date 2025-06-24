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
    type = c("grant","scholarship","subsidized","unsubsidized", "institutional"),
    sem_cost = c(3700,0,1750,3000,29500)
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

IG <- function() {
    all_aid <- sum(aid$sem_cost[5])
    return(all_aid)
}

aid_plans <- list(
    "Grants and Subsidized Loan" = GSL,
    "Grants Only" = GO,
    "Subsidized Loan Only" = SLO,
    "Loans Only" = LO,
    "Institutional Grant" = IG,
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

loan_interest <- function(aid_plan_name, years_in_school = 3) {
    sub_rate <- 0.0505 
    unsub_rate <- 0.0505 
    
    subsidized_per_sem <- 0
    unsubsidized_per_sem <- 0
    
    if (aid_plan_name %in% c("Subsidized Loan Only", "Grants and Subsidized Loan", "All Aid", "Loans Only")) {
        subsidized_per_sem <- aid$sem_cost[3]
    }
    if (aid_plan_name %in% c("Loans Only", "All Aid")) {
        unsubsidized_per_sem <- aid$sem_cost[4]
    }
    
    subsidized_total <- subsidized_per_sem * 6
    unsubsidized_total <- unsubsidized_per_sem * 6
    
    monthly_payment <- 0
    total_interest <- 0
    total_debt <- 0
    
    if (subsidized_total > 0) {
        n <- 120
        r <- sub_rate / 12
        monthly_payment <- monthly_payment + (subsidized_total * r * (1 + r)^n) / ((1 + r)^n - 1)
        sub_total_paid <- monthly_payment * n
        sub_interest <- sub_total_paid - subsidized_total
        total_interest <- total_interest + sub_interest
        total_debt <- total_debt + subsidized_total
    }
    
    if (unsubsidized_total > 0) {
        principal_at_grad <- unsubsidized_total * (1 + unsub_rate)^years_in_school
        n <- 120
        r <- unsub_rate / 12
        unsub_monthly <- (principal_at_grad * r * (1 + r)^n) / ((1 + r)^n - 1)
        monthly_payment <- monthly_payment + unsub_monthly
        unsub_total_paid <- unsub_monthly * n
        unsub_interest <- unsub_total_paid - unsubsidized_total
        total_interest <- total_interest + unsub_interest
        total_debt <- total_debt + principal_at_grad  
    }
    
    return(list(
        monthly_payment = monthly_payment, 
        total_interest = total_interest,
        post_school_debt = total_debt  
    ))
}

run_simulation <- function(include_summer = TRUE) {
    results <- data.frame()
    
    for (h in housing$type) {
        for (f in food$type) {
            if ((f %in% c("commuter50", "commuter25")) && h == "student") {
                next
            }
            h_cost <- housing$sem_cost[housing$type == h]
            f_cost <- food$sem_cost[food$type == f]

            all_aid_plans <- c(list("No Aid" = function() 0), aid_plans)
            
            for (aid_name in names(all_aid_plans)) {
                aid_amount <- all_aid_plans[[aid_name]]()
                base_sem_cost <- sem_cost_waid(aid_amount)
                
                total_sem <- base_sem_cost + h_cost + f_cost
                
                total_year_no_summer <- total_sem * 2
                
                loan_info <- loan_interest(aid_name, years_in_school = 3)
                
                if (include_summer) {
                    credit_options <- c(0, 2, 3, 4, 5, 6, 7, 8)
                } else {
                    credit_options <- c(0)
                }
                
                for (summer_credits in credit_options) {
                    if (summer_credits > 0) {
                        summer_tuition <- summer(summer_credits)
                        summer_housing_food <- (h_cost + f_cost) * 0.5
                        summer_cost_total <- summer_tuition + summer_housing_food
                        total_year <- total_year_no_summer + summer_cost_total
                        total_month <- total_year / 12
                        summer_included <- TRUE
                    } else {
                        total_year <- total_year_no_summer
                        total_month <- total_year / 8
                        summer_included <- FALSE
                    }
                    
                    row <- data.frame(
                        student = (h == "student"),
                        apartment = (h == "apartment"),
                        house = (h == "house"),
                        all_access = (f == "all_access"),
                        block90 = (f == "block90"),
                        block120 = (f == "block120"),
                        commuter50 = (f == "commuter50"),
                        commuter25 = (f == "commuter25"),
                        hellofresh = (f == "hellofresh"),
                        no_aid = (aid_name == "No Aid"),
                        grants_only = (aid_name == "Grants Only"),
                        subsidized_only = (aid_name == "Subsidized Loan Only"),
                        loans_only = (aid_name == "Loans Only"),
                        grants_subsidized = (aid_name == "Grants and Subsidized Loan"),
                        institutional_grant = (aid_name == "Institutional Grant"),
                        all_aid = (aid_name == "All Aid"),
                        summer_classes = summer_included,
                        summer_credits = summer_credits,
                        total_aid = aid_amount,
                        cost_per_semester = total_sem,
                        cost_per_year = total_year,
                        cost_per_month = total_month,
                        total_3_years = total_year * 3,
                        post_school_debt = loan_info$post_school_debt,
                        post_schooling_monthly = loan_info$monthly_payment,
                        post_schooling_total_interest = loan_info$total_interest
                    )
                    
                    results <- rbind(results, row)
                }
            }
        }
    }
    
    return(results)
}


results_df <- run_simulation()


filtered_possibilities <- results_df %>%
  filter(!(institutional_grant | all_aid)) %>%
  select(-all_aid, -institutional_grant)

f2iltered_possibilities <- filtered_possibilities %>%
    filter(!no_aid) %>%
    select(-no_aid)

filter_likely <- f2iltered_possibilities %>%
    filter(!loans_only) %>%
    filter(!summer_credits > 6) %>%
    filter(!block120) %>%
    filter(!cost_per_year > 62000) %>%
    select(-block120, -loans_only)

fl.likely.noloan <- filter_likely %>%
    filter(!subsidized_only) %>%
    filter(!grants_subsidized) %>%
    filter(!all_access) %>%
    filter(summer_credits == 4 | summer_credits == 0) %>%
    select(-subsidized_only, -grants_subsidized, -all_access)