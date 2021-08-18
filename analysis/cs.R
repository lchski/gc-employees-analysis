source("load.R")

pop_by_classification_by_organization %>%
  mutate(
    employee_type = if_else(
      classification_groups %in% c("CS - Computer Systems"),
      "cs",
      "not_cs"
    )
  ) %>%
  group_by(organization, fy_end, employee_type) %>%
  summarize(number_of_employees = sum(number_of_employees)) %>%
  mutate(
    total_employees = sum(number_of_employees),
    prop = round(number_of_employees / total_employees, 2)
  ) %>%
  filter(fy_end == 2021) %>%
  filter(total_employees >= 50) %>%
  arrange(employee_type, -prop)
