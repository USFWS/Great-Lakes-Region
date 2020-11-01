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

# Load CMT data from a CSV file -----------------

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


# There is currently an infinite loop in the ORGCODE -> RPTORGCODE
# between 93000 and 93430. To fix, cheat and change 93000
# (Assistant Director-National Wildlife Refuge System)
# to report to 90100 (Director-U.S. Fish and Wildlife Service
if (cmt_data$RPTORGCODE[cmt_data$ORGCODE==93000]==93430) {
    ind = which(cmt_data$ORGCODE==93000)
    cmt_data$RPTORGCODE[ind] = 90100
    cmt_data$loop_orgcode[ind] = 90100
    cmt_data$pathString[ind] = "90100/93000"
    warning(paste("There is currently an infinite loop in the ORGCODE -> RPTORGCODE",
                  "between 93000 and 93430. To fix, cheat and change 93000",
                  "(Assistant Director-National Wildlife Refuge System)",
                  "to report to 90100 (Director-U.S. Fish and Wildlife Service).",
                  "Flagged for update to JAO.", sep = "\n"))
    rm(ind)
}

# ROW 99660 has been deleted but still exists in RPTORGCODE,
# Flagged for update to IT. And add in here manually for now.
# Cheat for now and change it to 99661
if (any(cmt_data$RPTORGCODE == 99660) & !any(cmt_data$ORGCODE == 99660)) {
    inds = which(cmt_data$RPTORGCODE==99660)
    cmt_data$RPTORGCODE[inds] = 99661
    cmt_data$loop_orgcode[inds] = 99661
    cmt_data$pathString[inds] = paste0("99661/", cmt_data$ORGCODE[inds])
    warning(paste("ORGCODE = 99660 has been deleted but still exists in RPTORGCODE",
                  "Flagged for update to JAO And add in here manually for now.",
                  "Cheat for now and change it to 99661",sep = "\n"))
    rm(inds)
}




# Loop through to build pathString -----------

# Function to sub in higher string
# See here on having only  "..." as a parameter
update_row <- function(...) {
    row <- tibble(...)
    # Just return the row if you're already at directors office
    if (row$loop_orgcode == 90100) {return(row)}

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
while(any(cmt_data$loop_orgcode != 90100)) {
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
#write.csv(cmt_data, "cmt_data_pathString.csv", row.names = F)

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

