# Display USFWS Corporate Master Table Hierarchy

## Description
Create a series of collapsible trees in html widgets that display the current USFWS organizational hierarchy. Intended use: check for corrections that are needed.

## Background
Each winter the USFWS updates the Corporate Master Table mainly by examining the web interface. While this tends to keep contact information up to date, it is very difficult to look at the hierarchy itself for any corrections that are needed. This is a common problem for the National Wildlife Refuge System (NWRS) as stations are frequently 'complexed' or 'decomplexed' for administrative reasons.

## A little detail
This R script downloads the full USFWS Corporate Master Table (CMT) from the API described [here](https://intranet.fws.gov/cmt/HelpFiles/14_Web_Services.htm). It then creates a data tree and exports a series of collapseableTree html widgets into a local folder. The first exported trees shows the full organization, but this can be hard to look at given HQ's large number of offices. Therefore, the script also produces a hierarchy for each region alone, and then for each legacy region's NWRS by itself. The nodes are labeled by orgcode, but if you hover over any node it will display the name and cost center code for that node.

You must be on the USFWS intranet for this script to run. 

## Contact
Andy Allstadt <andrew_allstadt@fws.gov>
_____________________________

The United States Fish and Wildlife Service (FWS) GitHub project code is provided on 
an "as is" basis and the user assumes responsibility for its use. FWS has relinquished 
control of the information and no longer has responsibility to protect the integrity, 
confidentiality, or availability of the information. Any reference to specific 
commercial products, processes, or services by service mark, trademark, manufacturer, 
or otherwise, does not constitute or imply their endorsement, recommendation or 
favoring by FWS. The FWS seal and logo shall not be used in any manner to imply 
endorsement of any commercial product or activity by FWS or the United States 
Government.
