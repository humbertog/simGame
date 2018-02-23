library(xml2)

sym_in <- read_xml("/Users/humberto.gonzalez/Dropbox/simulation_game/exp_20180215/experiment_design/tripsets/L63V_027_SG_Calage1.xml")

# Parse the routes
routes <- xml_find_all(sym_in, ".//RESEAU/ROUTES/ROUTE")
route_ids <- xml_attr(routes, "id")

route_names <- c("R_N1", "R_N2", "R_N3", 
                 "R_test1", "R_test1_2", "R_test1_3",
                 "R_test2", "R_test2_2", "R_test2_3",
                 "R_04D3_1", "R_04D3_2", "R_04D3_3",
                 "R_05D4_1", "R_05D4_2", "R_05D4_3",
                 "R_06D5_1", "R_06D5_2", "R_06D5_3",
                 "R_07D6_1", "R_07D6_2", "R_07D6_3",
                 "R_08D7_1", "R_08D7_2", "R_08D7_3",
                 "R_09D8_1", "R_09D8_2", "R_09D8_3",
                 "R_010D2_1", "R_010D2_2", "R_010D2_3",
                 "R_011D11_1", "R_011D11_2", "R_011D11_3",
                 "R_012D12_1", "R_012D12_2", "R_012D12_3",
                 "R_013D13_1", "R_013D13_2", "R_013D13_3",
                 "R_014D14_1", "R_014D14_2", "R_014D14_3",
                 "R_015D15_1", "R_015D15_2", "R_015D15_3"
                 )


# Select the routes that we want
idx <- which(route_ids %in% route_names)
length(idx)

selected_routes <- routes[idx]


# Obtain the path of each route
route_list_201802 <- list()

for(r in selected_routes) {
  rname <- xml_attr(r, "id")
  links <- xml_find_all(r, ".//TRONCON_")
  link_ids <- xml_attr(links, "id")
  
  route_list_201802[[rname]] <- link_ids
}

selected_routes_201802 <-  selected_routes
save(route_list_201802, selected_routes_201802, file = "./R_data/routes_201802.RData")






