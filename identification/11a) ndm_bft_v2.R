#--------------------------------------------------------------------------------------------
# stakeholder_network.R         Network analysis of selected stakeholders
#--------------------------------------------------------------------------------------------
# This script makes a social network analysis among the selected stakeholders or countries
# Each stakeholder/country is represent with a node in the network.
# Links between stakeholders are computed according to the number of times they have been
# selected together for each simulated MPA.
# Once created the network, we calculate several network metrics to assess the connectivity
# and relevance of the stakeholders.

library(stringr)
library(dplyr)
library(data.table)
library(viridis)
library(igraph)
library(ggraph)
library(scales)

#-------------------------------------------------
# Step 1. Import data with selected stakeholders
#-------------------------------------------------

# set paths
#input_dir <- "../../OneDrive - University of Exeter/Manuscripts/DOM_paper/data/output/selected_stakeholders/"
#input_dir <- "C:/Users/dm620/OneDrive - University of Exeter/Manuscripts/DOM_paper/data/output/selected_stakeholders"
#selected_csv <- paste0(input_dir, "/bft_countries_intersection.csv")
#selected_csv <- paste0(input_dir, "/fish_admins_BFT_each-month.csv")
# set paths
input_dir <- "C:/DOM/output/"
#input_dir <- "C:/Users/dm620/OneDrive - University of Exeter/Manuscripts/DOM_paper/data"
#selected_csv <- paste0(input_dir, "/output/selected_stakeholders/allorgs_fishery-bft_100km_multypoligon.csv")
#selected_csv <- paste0(input_dir, "/fish_admins_BFT_each-month.csv")
## bluefin tuna
selected_csv <- "C:/DOM/output/allorgs_fishery-bft_100km_multypoligon.csv"
## finwhale
#selected_csv <- "C:/DOM/output/allorgs_traffic-fw_100km_multypoligon.csv"


#input_dir <- "D:/OneDrive/OneDrive - University of Exeter/Manuscripts/DOM_paper/data"
#selected_csv <- paste0(input_dir, "/output/selected_stakeholders/allorgs_fishery-bft_100km_multypoligon.csv")

# import data
df <- read.csv(selected_csv)

# create ID for each month-year combination
df$polID <- paste0(str_pad(df$month, 2, pad = "0"), df$year)

# Create new stakeholder ID
# there is a problem with database, so create a new ID to avoid duplicates
df$ID2 <- paste(df$ID, substr(df$org_type, 1, 3), substr(df$country, 1, 3), sep="_")

## filter for non-decision makers bft

df <- df %>% filter(org_type!="Administration")

### eliminate org sector = recreational fishery

df <- df %>% filter(org_sector!="Recreational fishery")

#-------------------------------------------------
# Step 2. Calculate number of common link for each pair
#-------------------------------------------------

# table of stakeholders
stak <- df %>%
  group_by(ID2, org_name, org_type, org_sector, country) %>%
  summarise(n = n())

data_list <- list()
for (i in 1:nrow(stak)){
  
  # select all polygons where the stakeholder appears
  id <- stak$ID2[i]
  pols <- df %>% filter(ID2 == id) %>% select(polID)
  
  # select the other stakeholders/countries that appear in these pols
  others <- df %>% 
    filter(polID %in% pols$polID, ID2 != id) %>%
    select(ID2)
  
  # create data.frame
  out_df <- data.frame(from = id, to = others$ID2)
  data_list[[i]] <- out_df
}



#########
# combine
relations <- rbindlist(data_list)



#-----------------------------------------------------
# Step 3. Create network
#-----------------------------------------------------

# co-memberships
# check graph from incidence matrix and consider a bipartite network (MPA vs stakeholders)
# https://kateto.net/netscix2016.html

rel <- relations %>%
  mutate(from = as.character(from), to = as.character(to)) %>%
  group_by(from, to) %>%
  summarize("weight" = n()) %>%
  filter(weight > 1) # filter to those relationships with a minimum number of coincidences



# filter stakeholders that appear in the network # 
stak <- dplyr::filter(stak, ID2 %in% c(rel$from, rel$to))

# create graph. Undirected and weighted
# add stakeholder data.frame to incorporate country and type of stakeholder
g <- graph_from_data_frame(rel, directed=FALSE, vertices=stak)


# plot with different layouts
plot(g)
plot(g, vertex.size=10, vertex.label=NA) 
plot(g, layout=layout_with_lgl, vertex.size=10, vertex.label=NA)

# add some color

## for bf
### all orgs
#colrs <- c('#d8b365','#ffbb00','#af8dc3','#67a9cf','#d9ef8b')
### non-decision makers
colrs <- c('#ffbb00','#af8dc3','#67a9cf','#d9ef8b')

V(g)$color <- colrs[as.factor(V(g)$country)]


# centrality measures
b <- centralization.betweenness (g, directed = FALSE, nobigint = TRUE, normalized = TRUE)
d.in <- centralization.degree (g, mode = "in", loops = FALSE, normalized = TRUE)
d.out <- centralization.degree (g, mode = "out",loops = FALSE, normalized = TRUE)

# community detection
# check out the function no.clusters for the number of connected components
clu <- components(g, mode = c("strong"))

# incorporate measures as an attribute of each node
V(g)$betweenness <- b$res
V(g)$degree.in <- d.in$res
V(g)$degree.out <- d.out$res
#V(g)$cut.point <- cut
V(g)$group <- clu$membership

## set circle size based on rescaled betweenness (to reduce size difference between circles)
V(g)$size <- rescale(log1p(b$res), to = c(5, 10)) 

# Set edge width based on weight:
E(g)$width <- E(g)$weight/1000
E(g)$edge.color <- "light gray"

# plot graph
plot(g,layout=layout_with_fr, vertex.label=NA, vertex.frame.color="#BEBEBE66", edge.color="#BEBEBE33") 



# legend bluefin (no monaco selection)
legend(x=-1.5, y=-1.1, c("France", "Italy","Spain","Tunisia"), pch=21,
       col=colrs, pt.bg=colrs, pt.cex=1, cex=.3, bty="n", ncol=1)

## add network centralization value to the graph (rounded at 2nd decimal)
c_value <- paste("Centralization score = ", round(b$centralization, digits = 3))
mtext(c_value, side=1)
## add number of nodes value to the graph
n_value <- paste("n = ", length(b$res))
mtext(n_value, side=1, line= 1)