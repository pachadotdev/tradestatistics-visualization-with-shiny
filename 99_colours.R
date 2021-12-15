groups <- ots_commodities %>% 
  select(group_code, group_fullname_english) %>% 
  distinct() %>% 
  arrange(group_code) %>% 
  drop_na() %>% 
  select(group_fullname_english) %>% 
  bind_rows(tibble(group_fullname_english = "Others"))

colors <- c(
  "#ab0033","#2de48e","#9b0f87","#79ec88","#552797",
  "#9dc73e","#241070","#77c042","#922698","#40951a",
  "#8a47ba","#bfc230","#456de5","#e3b02a","#6770e9",
  "#ffcd4f","#001f70","#a7e57b","#330d64","#82ea9c",
  "#b352c3","#007202","#bc72e9","#019439","#cd3ea3",
  "#00e8b4","#c90b51","#00b972","#ff61b1","#009f4e",
  "#7e5dd3","#779200","#f193ff","#1e6800","#d593ff",
  "#557e00","#0067cc","#ee942c","#003388","#fcce63",
  "#00327c","#fe953b","#017cda","#bf7300","#02aefc",
  "#d16318","#004b9d","#ccdd78","#450055","#77eaa7",
  "#b80057","#77e9bd","#74005c","#b6e18e","#57004e",
  "#aee29a","#91004f","#02aa84","#ff5597","#005304",
  "#ff70c2","#004903","#f9aaff","#576d00","#86a2ff",
  "#968700","#ff94e1","#00844e","#eb4655","#005c27",
  "#ce3435","#368c5f","#e25638","#83b473","#54003c",
  "#f0d180","#5f3c79","#807d00","#8f5b98","#8b6900",
  "#62003a","#ffb775","#84003f","#8a843b","#ff85ae",
  "#4d5200","#b9699a","#8f5d00","#a74e6e","#ffa055",
  "#670027","#ffa881","#5e0800","#ff946c","#830027",
  "#dc6427","#782000","#ff8278","#921000","#9b4800"
)

groups_colors <- groups %>% 
  bind_cols(tibble(group_color = colors[1:98]))

readr::write_csv(groups_colors, "groups_colors.csv")
