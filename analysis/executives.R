source("load.R")

pop_by_classification_by_organization %>%
  filter(fy_end == 2019) %>%
  mutate(employee_type = if_else(classification_groups %in% c("EX - Executive", "DM - Deputy Ministers"), "executive", "non_executive")) %>%
  group_by(organization, employee_type) %>%
  summarize(number_of_employees = sum(number_of_employees)) %>%
  pivot_wider(names_from = employee_type, values_from = number_of_employees) %>%
  mutate(
    total = non_executive + executive,
    non_exec_per_exec = round(non_executive / executive)
  )

exec_levels_by_dept <- pop_by_dep_ex_level %>%
  pivot_wider(id_cols = c(organization, date), names_from = ex_type, values_from = employees) %>%
  clean_names %>%
  mutate_at(vars(contains("ex")), ~ replace_na(.x, 0)) %>%
  mutate(
    total = select(., ex_01:non_executive) %>%
      rowSums(na.rm = TRUE)
  ) %>%
  mutate(
    executive = total - non_executive,
    non_exec_per_exec = round(non_executive / executive)
  ) %>%
  group_by(organization) %>%
  top_n(1, wt = date) # select just the latest year of reported data

exec_levels_by_dept %>%
  filter(total >= 50) %>%
  arrange(non_exec_per_exec) %>%
  View("2")
