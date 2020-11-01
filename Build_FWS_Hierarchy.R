# Turn CMT Export into a collapsible dendrotree
# e.g., https://www.r-graph-gallery.com/336-interactive-dendrogram-with-collapsibletree.html

# Load libraries --------------------
# install.packages(c("dplyr", "data.tree", "collapsibleTree",
#       "htmlwidgets", "purrr", "zip", "RCurl", "askpass"))
library(dplyr)
library(data.tree)
library(collapsibleTree)
library(htmlwidgets)
library(purrr)
library(zip)
library(jsonlite)
library(sf)

# NOte: htmlwidgets requires full file paths, not relative paths
# so outpath formatted for this.
outpath = "/output/"
if (!dir.exists(paste0(".", outpath))) {
    dir.create(paste0(".", outpath))
}

# Load CMT data from a CSV file, identify bad_rows -----------------

# Get geo data, then merge with org data that includes rptcode
cmt_data = fromJSON(
    "https://systems.fws.gov/cmt/getCMTBasic.do?REGION=0,1,2,3,4,5,6,7,8,9") %>%

    # Start pathString chain with current orgcode
    mutate(., pathString = if_else(ORGCODE == 90100, "90100",
                                   paste0(as.character(RPTORGCODE), "/",
                                          as.character(ORGCODE))))  %>%

    # Used to cut off chain of pathString
    mutate(., loop_orgcode = RPTORGCODE) %>%

    # Display field for final tree
    mutate(., display = paste0(ORGNAME, " (", CCCODE, ")")) %>%

    # Drop a bunch of fields we'll never use
    select(., -c("IFWS", "MAILADD1", "DIRECTIONS", "MAILCITY",
                 "MAILSTATEABBR", "MAILZIP", "MAILZIP4",
                 "PHYSADD1", "PHYSSTATEABBR", "PHYSCITY",
                 "PHYSZIP", "PHYSZIP4", "PHONE", "FAX", "PHYSADD2",
                 "MAILADD2"))

# Write a csv file for diagnostic purposes
write.csv(cmt_data, "full_raw_export.csv", row.names = F)

# Remove any rows from consideration if the RPTORGCODE does not exist.
# write diagnostic file and warn user.
nonexistent_rptorgcode <- subset(cmt_data, !(RPTORGCODE %in% ORGCODE), select = RPTORGCODE) %>% distinct(.)
if (file.exists("nonexistent_rptorgcode.csv")) {unlink("nonexistent_rptorgcode.csv")}
if (nrow(nonexistent_rptorgcode) > 0) {
    warning(paste("The following orgcodes are listed to in RPTORGCODE but not ORGCODE.",
        "Likely cause, incomplete updates to CMT. Any ORGCODEs below these in the hierarchy will",
        "be disregarded in constructing the trees. Written to nonexistent_rptorgcodes.csv",
        "Missing RPTORGCODES:", paste(nonexistent_rptorgcode, sep = ", ")))
    write.csv(nonexistent_rptorgcode, "nonexistent_rptorgcode.csv", row.names = F)
} else {
    # Otherwise create nonsense value to use in later function
    nonexistent_rptorgcode = -9999
}

# Look for any immediate infinite loops (i.e., one record's RPTORGCODE points
# right back at it). Warn user, write diagnostic file, and remove those rows from
# CMT data. Note that it's OK for 90100, ignore that one.
infinite_loops <- select(cmt_data, c(ORGCODE, RPTORGCODE)) %>%
    left_join(., ., by = c("RPTORGCODE" = "ORGCODE"), suffix = c("_org", "_above")) %>%
    subset(., ORGCODE == RPTORGCODE_above & ORGCODE != 90100, select = ORGCODE) %>%
    distinct(.)
if (file.exists("infinite_loops.csv")) {unlink("infinite_loops.csv")}
if (nrow(nonexistent_rptorgcode) > 0) {
    warning(paste("The following orgcodes and rptorgcodes point at each other and form",
                  "an infinite loop. These rows and any offices under them will be removed",
                  "from consideration. Written to infinite_loops.csv",
                  "Looping ORGCODEs:", paste(infinite_loops, sep = ", ")))
    write.csv(infinite_loops, "infinite_loops.csv", row.names = F)
} else {
    # Otherwise create nonsense value to use in later function
    infinite_loops = -9999
}

# Combine infinite_loops and into "bad_orgcodes", filter out any records right
# off the bat
bad_orgcodes = unlist(c(infinite_loops, nonexistent_rptorgcode))
attributes(bad_orgcodes) = NULL
rm(infinite_loops, nonexistent_rptorgcode)
cmt_data <- mutate(cmt_data, bad_row = ORGCODE %in% bad_orgcodes | RPTORGCODE %in% bad_orgcodes)

# Loop through to build pathString, remove bad_rows -----------

# Function to sub in higher string
# See here on having only  "..." as a parameter
# Warning - uses global variable bad_orgcodes
update_row <- function(...) {
    row <- tibble(...)

    #print(paste(row$ORGCODE, row$RPTORGCODE, row$pathSting))
    # Just return the row if you're already at directors office
    if (row$loop_orgcode == 90100 | row$bad_row) {return(row)}

    # If loop_orgcode is in bad_orgcodes, return row with
    # flag to delete this row
    if (row$loop_orgcode %in% bad_orgcodes) {
        row$bad_row = TRUE; return(row)
    }

    # Find corresponding row in cmt_data
    ind = which(cmt_data$ORGCODE == row$loop_orgcode)

    if (length(ind)==0) {print(row)}
    # Update pathString
    row$pathString = paste0(cmt_data$pathString[ind],
               sub(as.character(row$loop_orgcode), "", row$pathString))

    # Update loop_orgcode
    row$loop_orgcode = cmt_data$loop_orgcode[ind]

    # Return row
    return(row)
}

# For my own interest, how many loops does it take?
loop_ct = 0
while(any(cmt_data$loop_orgcode != 90100 & !cmt_data$bad_row)) {
    loop_ct <- loop_ct + 1
    print(sprintf("Loop # %d", loop_ct))

    cmt_data <- pmap_dfr(cmt_data, update_row)
    head(cmt_data)

    if (loop_ct > 10) {
        stop("Process stopped. Likely infinite loop error in CMT that needs to be fixed.")
    }
}
print(sprintf("Processing took %i loops", loop_ct))
rm(loop_ct, update_row)

# Warn how many rows are being removed, write to csv
if (file.exists("removed_orgcodes.csv")) {unlink("removed_orgcodes.csv")}
if (sum(cmt_data$bad_row) > 0) {
    warning(sprintf(paste("Removing %d offices that lead to infinite loops or non-existing RPTORGCODEs.",
                        "These are written to removed_orgcodes.csv."), sum(cmt_data$bad_row)))
    subset(cmt_data, bad_row) %>% write.csv(., "removed_orgcodes.csv", row.names = F)
    # delete any rows flagged as bad
    cmt_data <- subset(cmt_data, !bad_row)
}
cmt_data <- select(cmt_data, -bad_row)

# Build overall data.tree and collapsibleTree, takes a bit. Also write an HQ only version --------------

# Full FWS hierarchy
full_tree = FromDataFrameTable(cmt_data)
full_collapse_tree = collapsibleTree(full_tree, tooltip = TRUE, attribute = "display")
saveWidget(full_collapse_tree, file=paste0(getwd(), outpath,"full_cmt_interactive.html"))

# HQ only
HQ_tree <- Clone(full_tree)
Prune(HQ_tree, function(x) x$ORGCODE > 90000)
HQ_collapse_tree = collapsibleTree(HQ_tree, tooltip = TRUE, attribute = "display")
saveWidget(HQ_collapse_tree, file=paste0(getwd(), outpath,"HQ_cmt_interactive.html"))
rm(HQ_tree)

# Loop through to make Legacy Region trees, and Legacy Region NWRS trees -------------

# Nodes as numeric names makes it a bit tricky. Must be a better way to handle it.
region_data <- data.frame(legacy_region = 1:8,
                            region_orgcodes = c(10100, 20100, 30100, 40100, 50100, 60100, 70100, 80200),
                            nwrs_orgcodes = c(10130, 20130, 30130, 40130, 50130, 60130, 70130, 80230))

# Note, you will get a message for each "collapseTree", 16 in total.
#  collapsibleTree needs an update to keep up with data.tree
for (i in 1:nrow(region_data)) {

    # Regional collapse tree
    txt = paste0("full_tree$", "`", region_data$region_orgcodes[i], "`")
    regional_collapse_tree = collapsibleTree(eval(parse(text = txt)),
                        tooltip = TRUE, attribute = "display")
    saveWidget(regional_collapse_tree, file=paste0(getwd(), outpath, "R", i, "_cmt_interactive.html"))

    # Regional NWRS collapse tree
    txt = paste0(txt, "$`", region_data$nwrs_orgcodes[i], "`")
    regional_collapse_tree = collapsibleTree(eval(parse(text = txt)),
                                             tooltip = TRUE, attribute = "display")
    saveWidget(regional_collapse_tree, file=paste0(getwd(), outpath, "R", i, "NWRS_cmt_interactive.html"))

}
# rm(txt, regional_collapse_tree, region_data)

# Zip output folder ---------

# Clean up unnecessary folders in output path, they confuse Git
garbage <- lapply(list.dirs("./output"), function(x) {
    if (x != "./output") { unlink(x, recursive=TRUE)}})
rm(garbage)

zip(zipfile = "output.zip", files = file.path("output/"))

# Create map -------------
cmt_data <- st_as_sf(cmt_data, coords = c("LONG", "LAT")) %>% st_set_crs(., 4326)
plot(cmt_data["REGION"])

