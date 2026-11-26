leefbaar_2024 <- leefbaarheid %>%
  filter(jaar == 2024,
         str_detect(bu_code, "^BU0363|^BU0599")) %>%
  select(bu_code, bu_naam, lbm)
leefbaar_2024 <- leefbaarheid %>%
  filter(jaar == 2024,
         str_detect(bu_code, "^BU0363|^BU0599")) %>%
  select(bu_code, bu_naam, lbm, afw, fys, onv, soc, vrz, won)
sum(drank_2024$bu_code %in% leefbaar_2024$bu_code)
View(anti_join(drank_2024, leefbaar_2024, by = "bu_code"))
buurten_2024 <- leefbaar_2024 %>%
  inner_join(drank_2024, by = "bu_code")
samengevoegd <- leefbaar_2024 %>%
  inner_join(drank_2024, by = "bu_code")
write_csv(samengevoegd, "data/samengevoegd_2024.csv")