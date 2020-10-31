monthly_usage <- hydro_observations %>%
  index_by(ymonth = yearmonth(time)) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  arrange(ymonth)

monthly_usage_by_rate_type <- hydro_observations %>%
  index_by(ymonth = yearmonth(time)) %>%
  group_by(rate_type) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  arrange(ymonth, rate_type)



## Q1: Have we ever used more hydro than the lowest tier?
(monthly_usage %>%
  pull(usage) %>%
  max) > (tiered_rates %>%
  distinct(threshold) %>%
  pull() %>%
  min)


## Q2: How do TOU rates compare to the off-peak price?
tou_vs_tiered_rates <- tou_rates %>%
  # filter(effective_date < "2020-01-01") %>% ## filter COVID rates out
  left_join(tiered_rates %>%
              # filter(effective_date < "2020-01-01") %>%
              select(-threshold, -higher_tier_price))

tou_vs_tiered_rates %>%
  pivot_longer(cols = off_peak_price:lower_tier_price, names_to = "price_type", values_to = "price") %>%
  mutate(price_type = str_remove(price_type, "_price")) %>%
  ggplot(aes(x = effective_date, y = price, colour = price_type)) +
  geom_line()

tou_vs_tiered_rates %>%
  mutate(
    off_peak_ratio = off_peak_price / lower_tier_price,
    mid_peak_ratio = mid_peak_price / lower_tier_price,
    on_peak_ratio = on_peak_price / lower_tier_price
  ) %>%
  select(effective_date, contains("_ratio")) %>%
  pivot_longer(cols = off_peak_ratio:on_peak_ratio, names_to = "rate_type", values_to = "ratio") %>%
  ggplot(aes(x = effective_date, y = ratio, colour = rate_type)) +
  geom_line()

tou_vs_tiered_costs_for_usage <- monthly_usage_by_rate_type %>%
  pivot_wider(id_cols = ymonth, names_from = rate_type, values_from = usage) %>%
  clean_names %>%
  left_join(monthly_usage %>% rename(total = usage)) %>%
  left_join(tou_vs_tiered_rates %>% mutate(ymonth = yearmonth(effective_date))) %>%
  fill(off_peak_price:lower_tier_price) %>%
  filter(ymonth >= "2018-05-01") %>%
  mutate_at(vars(mid_peak:on_peak), ~ replace_na(.x, 0)) %>%
  mutate(
    tou_cost = round((off_peak * off_peak_price + mid_peak * mid_peak_price + on_peak * on_peak_price) / 100, 2),
    tiered_cost = round((total * lower_tier_price) / 100, 2)
  ) %>%
  mutate(
    tiered_cheaper = tiered_cost < tou_cost,
    tiered_savings = tou_cost - tiered_cost
  )

## Q3: Better off with tiered or TOU rates?
tou_vs_tiered_costs_for_usage %>%
  count_group(tiered_cheaper)

tou_vs_tiered_costs_for_usage %>%
  select(ymonth, tou_cost, tiered_cost) %>%
  pivot_longer(tou_cost:tiered_cost, names_to = "billing_type", values_to = "cost") %>%
  mutate(billing_type = str_remove(billing_type, "_cost")) %>%
  ggplot(aes(x = ymonth, y = cost, colour = billing_type)) +
  geom_line()

## Q4: Potential savings from switching to TOU?
tou_vs_tiered_costs_for_usage %>% summarize(sum(tiered_savings))


