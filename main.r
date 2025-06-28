library(dplyr)
library(tidyverse)

# Set working directory to rental model location and source functions
rental_model_path <- "C:/Users/mayay/.vscode/Rentcost"
current_wd <- getwd()
setwd(rental_model_path)
source("data_compiling.r")
source("calculations.r")
setwd(current_wd)  # Return to original directory

# Configuration Constants
MONTHS_PER_SEMESTER <- 4
MONTHS_PER_YEAR_NO_SUMMER <- 8  # Fall + Spring semesters
MONTHS_PER_YEAR_WITH_SUMMER <- 12  # Fall + Spring + Summer
LOAN_TERM_MONTHS <- 120
SUBSIDIZED_RATE <- 0.0505
UNSUBSIDIZED_RATE <- 0.0505
DEFAULT_YEARS_IN_SCHOOL <- 3
SUMMER_HOUSING_FOOD_MULTIPLIER <- 0.5

# Student house cost calculation using rental model
#' Calculate realistic house costs for students using rental market data
#' @param property_value Property value in dollars (default DC median)
#' @param num_roommates Number of roommates to split rental income (0-3)
#' @param semester_length Length of semester in months
#' @return List with semester cost, down payment, and monthly breakdown
student_house_cost <- function(property_value = 400000, num_roommates = 2, semester_length = MONTHS_PER_SEMESTER) {
    county <- "District Of Columbia County"
    state <- "District Of Columbia"
    
    # Calculate monthly housing costs
    monthly_mortgage <- mortgage(county, state, 30, property_value)
    monthly_expenses_cost <- monthly_expenses(county, state, property_value)
    
    # Calculate potential rental income (based on roommates)
    # Using rental model's market data for full rental potential
    full_rental_income <- monthly_income(county, state)
    
    # Adjust rental income based on number of roommates
    # Assume student occupies 1 room, rents out others
    rooms_available <- 3  # Typical house has 3-4 bedrooms
    rental_income <- full_rental_income * (num_roommates / rooms_available)
    
    # Net monthly cost to student
    net_monthly <- monthly_mortgage + monthly_expenses_cost - rental_income
    
    # Convert to semester cost
    semester_cost <- net_monthly * semester_length
    
    # Calculate required down payment and closing costs
    down_payment_required <- property_value * down_payment  # Using rental model's down_payment variable
    closing_costs <- property_value * 0.03  # Typical 3% closing costs
    total_upfront <- down_payment_required + closing_costs
    
    return(list(
        semester_cost = semester_cost,
        down_payment_required = down_payment_required,
        closing_costs = closing_costs,
        total_upfront = total_upfront,
        monthly_breakdown = list(
            mortgage = monthly_mortgage,
            expenses = monthly_expenses_cost,
            rental_income = rental_income,
            net_monthly = net_monthly
        ),
        property_details = list(
            property_value = property_value,
            num_roommates = num_roommates
        )
    ))
}

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
semester_cost_web <- 

# Calculate realistic house costs using rental model
house_solo_cost <- student_house_cost(property_value = 400000, num_roommates = 0)
house_with_roommates_cost <- student_house_cost(property_value = 400000, num_roommates = 2)

housing <- data.frame(
    type = c("student", "apartment", "house_solo", "house_with_roommates"),
    sem_cost = c(7100, 8000, 
                house_solo_cost$semester_cost,
                house_with_roommates_cost$semester_cost)
)

food <- data.frame(
  type = c("all_access", "block90", "block120", "commuter50", "commuter25", "hellofresh"),
  sem_cost = c(3150.00, 2345.00, 2545.00, 950.00, 475.00, 1600.00)
)

aid <- data.frame(
    type = c("grant","scholarship","subsidized","unsubsidized", "institutional"),
    sem_cost = c(3700,0,1750,3000,29500)
)

# Helper function to get house cost breakdown
get_house_breakdown <- function(house_type = "house_with_roommates") {
    if (house_type == "house_solo") {
        return(house_solo_cost)
    } else if (house_type == "house_with_roommates") {
        return(house_with_roommates_cost)
    } else {
        stop("Invalid house type. Use 'house_solo' or 'house_with_roommates'")
    }
}

# Helper function to display house costs
display_house_costs <- function() {
    cat("=== HOUSE COST BREAKDOWN ===\n")
    cat("\nHouse Solo (No Roommates):\n")
    cat("  Semester Cost: $", format(house_solo_cost$semester_cost, big.mark=","), "\n")
    cat("  Monthly Cost: $", format(house_solo_cost$monthly_breakdown$net_monthly, big.mark=","), "\n")
    cat("  Down Payment: $", format(house_solo_cost$down_payment_required, big.mark=","), "\n")
    cat("  Total Upfront: $", format(house_solo_cost$total_upfront, big.mark=","), "\n")
    
    cat("\nHouse with 2 Roommates:\n")
    cat("  Semester Cost: $", format(house_with_roommates_cost$semester_cost, big.mark=","), "\n")
    cat("  Monthly Cost: $", format(house_with_roommates_cost$monthly_breakdown$net_monthly, big.mark=","), "\n")
    cat("  Rental Income: $", format(house_with_roommates_cost$monthly_breakdown$rental_income, big.mark=","), "/month\n")
    cat("  Down Payment: $", format(house_with_roommates_cost$down_payment_required, big.mark=","), "\n")
    cat("  Total Upfront: $", format(house_with_roommates_cost$total_upfront, big.mark=","), "\n")
    
    cat("\nComparison to Alternatives:\n")
    cat("  Student Housing: $7,100/semester\n")
    cat("  Apartment: $8,000/semester\n")
}

all_data <- list(housing, food, aid, student_bill)

monthly <- function(x, months_divisor = MONTHS_PER_SEMESTER) {
    for (df in all_data) {
        if (x %in% df$type) {
            monthly_cost <- df$sem_cost[df$type == x]
            return(monthly_cost / months_divisor)
        }
    }
    return(x / months_divisor)
}

yearly <- function(x) {
    yearly <- x*2 
    return(yearly)
}

# Calculate summer tuition cost
summer <- function(credits) {
    SUMMER_COST_PER_CREDIT <- 2330  # Keep existing rate as constant
    return(credits * SUMMER_COST_PER_CREDIT)
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

#' Calculate loan payments and interest over repayment period
#' @param aid_plan_name Character string matching aid plan names
#' @param years_in_school Number of years loans accrue interest (default 3)
#' @return List with monthly_payment, total_interest, post_school_debt
loan_interest <- function(aid_plan_name, years_in_school = DEFAULT_YEARS_IN_SCHOOL) {
    # Input validation
    if (!is.character(aid_plan_name) || length(aid_plan_name) != 1) {
        stop("aid_plan_name must be a single character string")
    }
    if (!is.numeric(years_in_school) || years_in_school <= 0) {
        stop("years_in_school must be a positive number")
    }
    
    sub_rate <- SUBSIDIZED_RATE
    unsub_rate <- UNSUBSIDIZED_RATE 
    
    subsidized_per_sem <- 0
    unsubsidized_per_sem <- 0
    
    if (aid_plan_name %in% c("Subsidized Loan Only", "Grants and Subsidized Loan", "All Aid", "Loans Only")) {
        subsidized_per_sem <- aid$sem_cost[3]
    }
    if (aid_plan_name %in% c("Loans Only", "All Aid")) {
        unsubsidized_per_sem <- aid$sem_cost[4]
        subsidized_per_sem <- aid$sem_cost[3]
    }

    subsidized_total <- subsidized_per_sem * 6
    unsubsidized_total <- unsubsidized_per_sem * 6
    
    monthly_payment <- 0
    total_interest <- 0
    total_debt <- 0
    
    if (subsidized_total > 0) {
        n <- LOAN_TERM_MONTHS
        r <- sub_rate / 12
        monthly_payment <- monthly_payment + (subsidized_total * r * (1 + r)^n) / ((1 + r)^n - 1)
        sub_total_paid <- monthly_payment * n
        sub_interest <- sub_total_paid - subsidized_total
        total_interest <- total_interest + sub_interest
        total_debt <- total_debt + subsidized_total
    }
    
    if (unsubsidized_total > 0) {
        principal_at_grad <- unsubsidized_total * (1 + unsub_rate)^years_in_school
        n <- LOAN_TERM_MONTHS
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
                        summer_housing_food <- (h_cost + f_cost) * SUMMER_HOUSING_FOOD_MULTIPLIER
                        summer_cost_total <- summer_tuition + summer_housing_food
                        total_year <- total_year_no_summer + summer_cost_total
                        total_month <- total_year / MONTHS_PER_YEAR_WITH_SUMMER
                        summer_included <- TRUE
                    } else {
                        total_year <- total_year_no_summer
                        total_month <- total_year / MONTHS_PER_YEAR_NO_SUMMER
                        summer_included <- FALSE
                    }
                    
                    # Calculate down payment cost if house option
                    down_payment_cost <- 0
                    if (h == "house_solo") {
                        down_payment_cost <- house_solo_cost$total_upfront
                    } else if (h == "house_with_roommates") {
                        down_payment_cost <- house_with_roommates_cost$total_upfront
                    }
                    
                    row <- data.frame(
                        student = (h == "student"),
                        apartment = (h == "apartment"),
                        house_solo = (h == "house_solo"),
                        house_with_roommates = (h == "house_with_roommates"),
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
                        down_payment_required = down_payment_cost,
                        total_cash_needed = total_year * 3 + down_payment_cost,
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

#' Apply multiple filters to a dataframe
#' @param df Input dataframe
#' @param exclude_cols Vector of column names to filter out (set to FALSE)
#' @param remove_cols Vector of column names to remove from result
#' @return Filtered dataframe
apply_standard_filters <- function(df, exclude_cols = c(), remove_cols = c()) {
    result <- df
    
    # Apply exclusion filters
    for (col in exclude_cols) {
        if (col %in% names(result)) {
            result <- result %>% filter(!get(col))
        }
    }
    
    # Remove specified columns
    cols_to_remove <- intersect(remove_cols, names(result))
    if (length(cols_to_remove) > 0) {
        result <- result %>% select(-all_of(cols_to_remove))
    }
    
    return(result)
}

filtered_possibilities <- results_df %>%
  filter(!(institutional_grant | all_aid)) %>%
  select(-all_aid, -institutional_grant)

f2iltered_possibilities <- filtered_possibilities %>%
    filter(!no_aid) %>%
    select(-no_aid)

filter_likely <- f2iltered_possibilities %>%
    filter(!loans_only) %>%
    filter(summer_credits <= 6) %>%
    filter(!block120) %>%
    filter(cost_per_year <= 80000) %>%
    select(-block120, -loans_only)

fl.likely.noloan <- filter_likely %>%
    filter(!subsidized_only) %>%
    filter(!grants_subsidized) %>%
    filter(!all_access) %>%
    filter(summer_credits %in% c(0, 4)) %>%
    select(-subsidized_only, -grants_subsidized, -all_access)

filter_likely_no_house <- filter_likely %>%
    filter(!house_solo & !house_with_roommates) %>%
    select(-house_solo, -house_with_roommates)

fl.likely.noloan.nohouse <- fl.likely.noloan %>%
    filter(!house_solo & !house_with_roommates) %>%
    select(-house_solo, -house_with_roommates)
