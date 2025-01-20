# Job value to a worker analysis
# Analysis comparing the value of a job between payroll employees and independent contractors

#### Value to w2 employee ####
value_to_w2_emp <- ecec_oews_merge |> 
  filter(compensation_component %in% c(
    "Wages and salaries",
    "Supplemental pay (e.g., overtime)",
    "Paid leave (vacation, holiday, sick, personal)",
    "Insurance benefits and retirement benefits",
    "Legally required benefits (Social Security, Medicare, federal and state unemployment insurance, and workers’ compensation)"
  )) |>
  
  # Calculate net value to worker
  summarize(
    wages_and_salaries = sum(share_of_pay[compensation_component == "Wages and salaries"]),
    supplemental_pay = sum(share_of_pay[compensation_component == "Supplemental pay (e.g., overtime)"]),
    paid_leave = sum(share_of_pay[compensation_component == "Paid leave (vacation, holiday, sick, personal)"]),
    insurance_and_retirement = sum(share_of_pay[compensation_component == "Insurance benefits and retirement benefits"]),
    # Social security + medicare contributions
    ss_mc_contrib_w2 = (wages_and_salaries + supplemental_pay + paid_leave) * 0.0765,
    # Workers comp, which is the legally required benefits minus SS and medicare contributions
    workers_comp = sum(share_of_pay[compensation_component == "Legally required benefits (Social Security, Medicare, federal and state unemployment insurance, and workers’ compensation)"])-ss_mc_contrib_w2,  
    value_to_worker_w2 = wages_and_salaries + supplemental_pay + paid_leave + insurance_and_retirement - ss_mc_contrib_w2,
    .by = c(state, occupation)
  )


#### Value of job to indep. contractors ####

# paperwork_cost:
# bookkeeping software	114
# tax filing software federal	129
# tax filing software state	64
# paperwork: 39 hours or 39/2080 = 0.01875

# Value of job to IC with no other benefits

value_to_ic <- ecec_oews_merge |> 
  filter(compensation_component %in% c(
    "Wages and salaries"
  )) |>
  
  # Calculate net value to worker
  summarize(
    wages_and_salaries = sum(share_of_pay[compensation_component == "Wages and salaries"]),
    ss_mc_contrib_ic = (wages_and_salaries) * (0.0765*2),
    paperwork_cost = (114 + 129 + 64) + wages_and_salaries*(39/2080),
    value_to_worker_ic = wages_and_salaries -paperwork_cost - ss_mc_contrib_ic,
    .by = c(state, occupation)
  )

#### Value of job to indep. contractors w/health and retirement ####


value_to_ic_hrcomp <- ecec_oews_merge |> 
  filter(compensation_component %in% c(
    "Wages and salaries",
    "Insurance benefits and retirement benefits"
  )) |>
  
  # Calculate net value to worker
  summarize(
    wages_and_salaries = sum(share_of_pay[compensation_component == "Wages and salaries"]),
    insurance_and_retirement = sum(share_of_pay[compensation_component == "Insurance benefits and retirement benefits"]),
    ss_mc_contrib_ic_hrcomp = (wages_and_salaries + insurance_and_retirement) * (0.0765*2),
    paperwork_cost = (114 + 129 + 64) + wages_and_salaries*(39/2080),
    value_to_worker_ic_hrcomp = wages_and_salaries + insurance_and_retirement - paperwork_cost - ss_mc_contrib_ic_hrcomp,
    .by = c(state, occupation)
    )

median_oews_pay <- oews_state_xwalk |> 
  select(state, occupation=occ_title, a_median)
  
w2_ic_comparison <- value_to_w2_emp |> 
  left_join(value_to_ic, by=c('state', 'occupation', 'wages_and_salaries')) |> 
  left_join(value_to_ic_hrcomp, by=c('state', 'occupation', 'wages_and_salaries'), suffix = c('_ic', '_ic_hrcomp')) |> 
  arrange(state, occupation)


# Estimate cost of independent contractor status

cost_to_employees <- median_oews_pay |> 
  left_join(w2_ic_comparison, by=c('state', 'occupation')) |> 
  # Cost of ic status 
  mutate(cost_of_ic_hrcomp = value_to_worker_w2 - value_to_worker_ic_hrcomp,
         cost_of_ic = value_to_worker_w2 - value_to_worker_ic) |> 
  # Cost of ic status w/ full compensation for health and retirement
  mutate(
    cost_of_ic_hrcomp_pct = value_to_worker_ic_hrcomp/value_to_worker_w2-1,
    cost_of_ic_pct = value_to_worker_ic/value_to_worker_w2-1) |> 
  # Rearrange table for readability
  select(state, occupation, a_median, value_to_worker_w2, value_to_worker_ic, value_to_worker_ic_hrcomp,
         cost_of_ic_hrcomp, cost_of_ic, cost_of_ic_hrcomp_pct, cost_of_ic_pct) |> 
  rename(`OEWS median` = a_median) |> 
  arrange(state, occupation)


# Estimate cost of independent contractor status to social insurance funds
cost_to_social_insurance <- w2_ic_comparison |>
  # Calculate social insurance contributions of w2 employees
  mutate(si_contribs_w2 = (ss_mc_contrib_w2*2) + workers_comp) |> 
  select(state, occupation, si_contribs_w2, ss_mc_contrib_ic, ss_mc_contrib_ic_hrcomp) |> 
    # net cost to social insurance funds by ic status
  mutate(socins_ic = si_contribs_w2 - ss_mc_contrib_ic,
         socins_ic_hrcomp = si_contribs_w2 - ss_mc_contrib_ic_hrcomp) |> 
    # Cost to social insurance funds by ic status %
  mutate(socins_ic_hrcomp_pct = ss_mc_contrib_ic_hrcomp/si_contribs_w2-1,
         socins_ic_pct = ss_mc_contrib_ic/si_contribs_w2-1) |> 
  select(state, occupation, si_contribs_w2,
         ss_mc_contrib_ic, ss_mc_contrib_ic_hrcomp,
         socins_ic_hrcomp, socins_ic, socins_ic_hrcomp_pct, socins_ic_pct) |> 
  arrange(state)
  

  
