student_bill <- data.frame(
  name = c(
    "student_gov_fee",
    "tuition", 
    "transit",
    "student_activities_fee",
    "matriculation_fee",
    "health_insurance"
    ),
  code = c("ESA", "T110", "UPAS", "ESTA", "EMAT", "SHIN"),
  cost = c(21.00, 16310.00, 100.00, 50.00, 350.00, 1273.00),
  stringsAsFactors = FALSE
)


semester_cost <- sum(student_bill$cost)
semester_cost_noinsurance <- sum(student_bill$cost[-6])