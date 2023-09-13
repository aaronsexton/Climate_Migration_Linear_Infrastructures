
# Linear Infrastructures for Climate Migration (LICM!)

# First step is to investigate the data coverage of common species in a uniform
# dataset. 
#
# First look will be into the dataset collected by Christian Wolter
# Data covers Central & Eastern Europe - Danube, Rhine, Elbe, & Vistula watersheds
# 1996 - 2010


# Year column fix ----
# Read in biodiversity file
wolter_biod <- read.csv(here::here("LICM/Wolter_Full.csv"))
head(wolter_biod)
str(wolter_biod)

# Read in year file
wolter_year <- read.csv(here::here("LICM/Wolter_Year_occ.csv"))

# Join them based on the Occ_laufnr and operatn columns
wolter_biod <- dplyr::left_join(wolter_biod, wolter_year,
                                by = c("operatn" = "Occ_laufnr"))

# Drop extra columns we don't need
wolter_biod <- dplyr::select(wolter_biod, 
                             -f_rich, -rel_rich, -ab, -n_rich, -i_rich,
                             -i_ab, -prop_i, -Site_code_LR)

# Rename year
wolter_biod <- dplyr::rename(wolter_biod, year = Yearocc)
wolter_biod <- dplyr::select(wolter_biod, study, operatn, site, year,
                             X_WGS84, Y_WGS84, dplyr::everything())

# Save this and reload in
write.csv(wolter_biod, here::here("LICM/Wolter_Full_new.csv"), row.names = F)
rm(list = ls())




# Common Abundances ----
wolter_biod <- read.csv(here::here("LICM/Wolter_Full_new.csv"))
str(wolter_biod)

# Calculate both the abundances and number of occurrences of each species
wolter_biod[2566, 7:92] <- colSums(wolter_biod[7:92])
wolter_biod[2567, 7:92] <- colSums(wolter_biod[7:92] != 0)
wolter_biod[2567, 7:92] <- (wolter_biod[2567, 7:92] - 1)
wolter_biod[2566, 2] <- "Abundances"
wolter_biod[2567, 2] <- "Occurrences"

# Identify the species in the top 25% of occurrences
wolter_occs <- dplyr::filter(wolter_biod, operatn == "Occurrences")
wolter_occs <- dplyr::select(wolter_occs, 
                             -study, -site, -year, -X_WGS84, -Y_WGS84)
wolter_occs <- as.data.frame(t(wolter_occs))
colnames(wolter_occs) <- wolter_occs[1,]
wolter_occs$Species <- rownames(wolter_occs)
rownames(wolter_occs) <- 1:nrow(wolter_occs)
wolter_occs <- wolter_occs[-1,]
str(wolter_occs)
wolter_occs$Occurrences <- as.numeric(wolter_occs$Occurrences)

# Okay actually, lets just keep those wither over 500 occurrences
wolter_occs_common <- dplyr::filter(wolter_occs, Occurrences >= 500)
# Gives 14 species

# Now trim the main biodiversity file to only those 14 species
wolter_biod_common <- dplyr::select(wolter_biod, study, operatn, site, year,
                                    X_WGS84, Y_WGS84, 
                                    wolter_occs_common$Species)
# Now calculate number of occurrences per year for these species

# Create a list of dfs seperated by year
dplyr::count(wolter_biod_common, year)
year_list <- split(wolter_biod_common, f = wolter_biod_common$year)

# Get occs in each df
occ96 <- as.data.frame(colSums(year_list$`1996`[7:20] != 0))
occ97 <- as.data.frame(colSums(year_list$`1997`[7:20] != 0))
occ98 <- as.data.frame(colSums(year_list$`1998`[7:20] != 0))
occ99 <- as.data.frame(colSums(year_list$`1999`[7:20] != 0))
occ00 <- as.data.frame(colSums(year_list$`2000`[7:20] != 0))
occ01 <- as.data.frame(colSums(year_list$`2001`[7:20] != 0))
occ02 <- as.data.frame(colSums(year_list$`2002`[7:20] != 0))
occ03 <- as.data.frame(colSums(year_list$`2003`[7:20] != 0))
occ04 <- as.data.frame(colSums(year_list$`2004`[7:20] != 0))
occ05 <- as.data.frame(colSums(year_list$`2005`[7:20] != 0))
occ06 <- as.data.frame(colSums(year_list$`2006`[7:20] != 0))
occ07 <- as.data.frame(colSums(year_list$`2007`[7:20] != 0))
occ08 <- as.data.frame(colSums(year_list$`2008`[7:20] != 0))
occ09 <- as.data.frame(colSums(year_list$`2009`[7:20] != 0))
occ10 <- as.data.frame(colSums(year_list$`2010`[7:20] != 0))

# Combine them
# First, fix column names
occ96$species <- rownames(occ96)
occ97$species <- rownames(occ97)
occ98$species <- rownames(occ98)
occ99$species <- rownames(occ99)
occ00$species <- rownames(occ00)
occ01$species <- rownames(occ01)
occ02$species <- rownames(occ02)
occ03$species <- rownames(occ03)
occ04$species <- rownames(occ04)
occ05$species <- rownames(occ05)
occ06$species <- rownames(occ06)
occ07$species <- rownames(occ07)
occ08$species <- rownames(occ08)
occ09$species <- rownames(occ09)
occ10$species <- rownames(occ10)

rownames(occ96) <- 1:nrow(occ96)
rownames(occ97) <- 1:nrow(occ97)
rownames(occ98) <- 1:nrow(occ98)
rownames(occ99) <- 1:nrow(occ99)
rownames(occ00) <- 1:nrow(occ00)
rownames(occ01) <- 1:nrow(occ01)
rownames(occ02) <- 1:nrow(occ02)
rownames(occ03) <- 1:nrow(occ03)
rownames(occ04) <- 1:nrow(occ04)
rownames(occ05) <- 1:nrow(occ05)
rownames(occ06) <- 1:nrow(occ06)
rownames(occ07) <- 1:nrow(occ07)
rownames(occ08) <- 1:nrow(occ08)
rownames(occ09) <- 1:nrow(occ09)
rownames(occ10) <- 1:nrow(occ10)

names(occ96)[names(occ96) == "colSums(year_list$`1996`[7:20] != 0)"] <- "y_1996"
names(occ97)[names(occ97) == "colSums(year_list$`1997`[7:20] != 0)"] <- "y_1997"
names(occ98)[names(occ98) == "colSums(year_list$`1998`[7:20] != 0)"] <- "y_1998"
names(occ99)[names(occ99) == "colSums(year_list$`1999`[7:20] != 0)"] <- "y_1999"
names(occ00)[names(occ00) == "colSums(year_list$`2000`[7:20] != 0)"] <- "y_2000"
names(occ01)[names(occ01) == "colSums(year_list$`2001`[7:20] != 0)"] <- "y_2001"
names(occ02)[names(occ02) == "colSums(year_list$`2002`[7:20] != 0)"] <- "y_2002"
names(occ03)[names(occ03) == "colSums(year_list$`2003`[7:20] != 0)"] <- "y_2003"
names(occ04)[names(occ04) == "colSums(year_list$`2004`[7:20] != 0)"] <- "y_2004"
names(occ05)[names(occ05) == "colSums(year_list$`2005`[7:20] != 0)"] <- "y_2005"
names(occ06)[names(occ06) == "colSums(year_list$`2006`[7:20] != 0)"] <- "y_2006"
names(occ07)[names(occ07) == "colSums(year_list$`2007`[7:20] != 0)"] <- "y_2007"
names(occ08)[names(occ08) == "colSums(year_list$`2008`[7:20] != 0)"] <- "y_2008"
names(occ09)[names(occ09) == "colSums(year_list$`2009`[7:20] != 0)"] <- "y_2009"
names(occ10)[names(occ10) == "colSums(year_list$`2010`[7:20] != 0)"] <- "y_2010"

# Now join
yearly_occurrences <- dplyr::full_join(occ96, occ97)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ98)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ99)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ00)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ01)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ02)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ03)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ04)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ05)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ06)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ07)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ08)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ09)
yearly_occurrences <- dplyr::full_join(yearly_occurrences, occ10)
yearly_occurrences <- dplyr::select(yearly_occurrences, species, 
                                    dplyr::everything())

# Add a row for total number of observations per year
observations_peryear <- as.data.frame(dplyr::count(wolter_biod_common, year))
observations_peryear <- observations_peryear[-16,]
yearly_occurrences[15,2:16] <- observations_peryear[1:15, 2]
yearly_occurrences[15,1] <- "Total Observations"


# Get these as proportion values
yearly_occurrences_props <- yearly_occurrences
yearly_occurrences_props[15,]
yearly_occurrences_props$y_1996 <- yearly_occurrences_props$y_1996 / 9
yearly_occurrences_props$y_1997 <- yearly_occurrences_props$y_1997 / 186
yearly_occurrences_props$y_1998 <- yearly_occurrences_props$y_1998 / 216
yearly_occurrences_props$y_1999 <- yearly_occurrences_props$y_1999 / 242
yearly_occurrences_props$y_2000 <- yearly_occurrences_props$y_2000 / 254
yearly_occurrences_props$y_2001 <- yearly_occurrences_props$y_2001 / 171
yearly_occurrences_props$y_2002 <- yearly_occurrences_props$y_2002 / 216
yearly_occurrences_props$y_2003 <- yearly_occurrences_props$y_2003 / 199
yearly_occurrences_props$y_2004 <- yearly_occurrences_props$y_2004 / 189
yearly_occurrences_props$y_2005 <- yearly_occurrences_props$y_2005 / 151
yearly_occurrences_props$y_2006 <- yearly_occurrences_props$y_2006 / 168
yearly_occurrences_props$y_2007 <- yearly_occurrences_props$y_2007 / 528
yearly_occurrences_props$y_2008 <- yearly_occurrences_props$y_2008 / 20
yearly_occurrences_props$y_2009 <- yearly_occurrences_props$y_2009 / 10
yearly_occurrences_props$y_2010 <- yearly_occurrences_props$y_2010 / 6
yearly_occurrences_props <- yearly_occurrences_props[-15, ]
options(digits = 4)
str(yearly_occurrences_props)
head(yearly_occurrences_props)





# Save these dfs
# write.csv(wolter_biod_common, here::here("LICM/Wolter_common_species.csv"), row.names = F)
# write.csv(yearly_occurrences, here::here("LICM/Wolter_common_occurences_byyear.csv"), row.names = F)
# write.csv(yearly_occurrences_props, here::here("LICM/Wolter_common_occurences_proportions_byyear.csv"), row.names = F)












