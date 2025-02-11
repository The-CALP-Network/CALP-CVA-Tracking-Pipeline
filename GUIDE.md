# Tracking humanitarian cash and voucher assistance

# Overview and purpose

This methodology and step-by-step guide outlines the process formerly followed by Development Initiatives (DI) and the CALP Network to produce a best possible estimate on global financial volumes of humanitarian cash and voucher assistance. It explains the technical steps alongside their reasoning from data extraction to identifying CVA data, combining it from different datasets and finally analysing the output.

The guide is intended to capture the methodological knowledge and methods by former DI staff, following the insolvency of DI, and thereby ensure that the work can be adapted and continued if the demand for it continues to exist. It therefore also highlights the different aspects of the methodology that need to be reviewed on an ongoing basis and concludes with suggestions for further improvement.

# Data extraction

There are two different groups of CVA data sources included in this guide. Those required to calculate a global estimate on the financial volume of humanitarian CVA (i.e., the survey data, FTS and projects module), and those that contain CVA data that is or could in future be relevant to other CVA analyses (i.e., IATI or WFP CASHboard).

## CVA survey

In recent years, at least 90% of the total value of the global CVA estimate was based on data collected via surveys directly from agencies implementing/delivering humanitarian CVA that are members of the CALP Network. The survey was largely modelled after the [minimum agreements on tracking CVA](https://www.calpnetwork.org/publication/tracking-cash-and-voucher-assistance-agreements-recommendations-and-minimum-requirements-from-the-grand-bargain-cash-workstream/) endorsed by the Grand Bargain cash workstream to collect the required data with a minimal reporting burden. Data is usually collected on the most recently completed financial year from each implementing agency, except for agencies with missing data, for whom multiple years of data might be requested. The survey structure has varied slightly year-on-year but the usual data categories are:

| Data field | Explanation |
| :---- | :---- |
| Organisation name |  |
| Organisation type | Select from: International NGO; Local/national NGO; UN agency; Red Cross and Red Crescent Movement; Private sector; Other |
| Currency | The currency used for all financial values in the survey. |
| Overall cash & voucher programming costs | If the respondent does not have the specific value of overall programming cost, then they are asked to provide an estimation in the 'Comments' of its size relative to what is transferred to the recipients (for example: "An average of around 80% of the CVA programme budgets are transferred to beneficiaries") .  |
| Transfers to recipients | The value of CVA transfers to recipients. |
| Cash assistance | The value of transfers to recipients as cash. |
| Vouchers | The value of transfers to recipients as vouchers. |
| Value of sub-grants received for CVA | The value captured under other provided data on CVA in the survey that was received as sub-grant from another implementing agency. |
| Value of sub-grants provided for CVA | The value captured under other provided data on CVA in the survey that was provided as sub-grant to another implementing agency. |
| Comments | Any relevant caveats to or comments on the provided data, including on possible reasons for increases or decreases in volumes. We also request the breakdown of provided or received sub-grants for CVA so that we can avoid double-counting across different survey respondents. |

The main difference between the minimum agreements on tracking CVA and our survey is the inclusion of sub-grant data. This is because it is common practice for a large recipient of funding for CVA from government donors (e.g., WFP, or UNHCR) to then sub-grant all or aspects of the delivery of that CVA to recipients to another implementing agency. Given we request data from those large actors as well as smaller agencies that receive funding from them, we need the sub-grant data to ensure that there is no double-counting across survey data from both ([see below](#heading=h.elvmhtxbd1pc)).


## Financial Tracking Service

The [Financial Tracking Service](https://fts.unocha.org/) (FTS) by UN OCHA is the most comprehensive source of global humanitarian financing flows in close to real time. It was originally set up to track progress on the funding requirements of UN-coordinated humanitarian response plans, but has since expanded its ambition to track all international humanitarian funding flows whether in- or outside of those plans.

The central data element of the FTS data structure is the financial flow between organisations. These financial flows have characteristics assigned to them at the source and destination (e.g., organisation, location, cluster, or year) on where there are from or to and more, and in addition have a set of characteristics that are central to that flow (e.g., the flow status on whether it is pledged, committed or paid, whether it is a financial flow or in-kind support, or what aid modality the financial flow supports). Some of those characteristics are more comprehensively reported on than others.

Before identifying the FTS data that is relevant to CVA, we first have to extract the financial flows data from FTS. 

The first step of that process is to retrieve the flow data for the year of interest from the FTS API. The request to the API can also be tailored for specific plans, emergencies, global clusters, recipient countries or more. The following code accesses the FTS API to get flows data:

```R
fts_get_flows <- function(year = NULL, planid = NULL, emergencyid = NULL, globalclusterid = NULL, destinationlocationid = NULL, unnest = T){
  lapply(c("data.table", "jsonlite", "httr"), require, character.only=T)
  if(!is.null(year)){
    year <- paste0("year=", paste0(year, collapse=","))
  }
  if(!is.null(planid)){
    planid <- paste0("planid=", paste0(planid, collapse=","))
  }
  if(!is.null(emergencyid)){
    emergencyid <- paste0("emergencyid=", paste0(emergencyid, collapse=","))
  }
  if(!is.null(globalclusterid)){
    globalclusterid <- paste0("globalclusterid=", paste0(globalclusterid, collapse=","))
  }
  if(!is.null(destinationlocationid)){
    destinationlocationid <- paste0("destinationlocationid:", paste0(destinationlocationid, collapse=","))
  }
  
  call.filter <- NULL
  if(!is.null(destinationlocationid)){
    call.filter <- paste0("&filterby=", destinationlocationid)
  }
  
  hpc <- "https://api.hpc.tools/v1/public/fts/flow?"
  call.param <- paste(year, planid, emergencyid, globalclusterid, call.filter, "format=json&limit=1000", sep="&")
  call <- paste0(hpc, call.param)
  fts <- fromJSON(content(GET(call), type = "text", encoding = "UTF-8"), flatten = T)
  
  flowslist <- list()
  flowslist[[1]] <- (fts$data$flows)
  i <- 2
  while (!is.null(fts$meta$nextLink)){
    nextLink <- fts$meta$nextLink
    fts <- fromJSON(content(GET(nextLink), type = "text", encoding = "UTF-8"), flatten = T)
    flowslist[[i]] <- (fts$data$flows)
    i <- i + 1
  }
  
  flows <- rbindlist(flowslist, fill=T, use.names = T)
  
  if(unnest){
    message("Un-nesting output. This may take some time.")
    fts_unnest_flows <- function(fts, cols = c("sourceObjects", "destinationObjects"), splits = "type", remove.nested = T, group.same = T){
      require(data.table)
      if(length(cols) != length(splits) & length(splits) != 1) stop("There must be one split for each nested col, or a single common split for all nested cols.", call.=F)
      fts <- as.data.table(fts)
      expand.splits <- data.table(cols = cols, splits = splits)
      for(i in 1:nrow(expand.splits)){
        col <- expand.splits[i]$cols
        split <- expand.splits[i]$splits
        if(group.same){
          expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(t(unlist(split(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")), as.data.table(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")))[, ..split]))))), fill=T)
        } else {
          expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(unlist(split(x, as.data.table(x)[, ..split])))), fill=T)
        }
        names(expanded) <- paste(col, names(expanded), sep="_")
        split.cols <- unique(names(expanded)[grepl(paste0("[.]", split, "\\d*$"), names(expanded))])
        expanded[, (split.cols) := NULL]
        expanded[, (split.cols) := NULL]
        expanded <- expanded[,which(unlist(lapply(expanded, function(x)!(all(is.na(x))|all(is.null(x)))))),with=F]
        fts <- cbind(fts, expanded)
        if(remove.nested) fts[, (col) := NULL][]
      }
      return(fts)
    }
    
    flows <- fts_unnest_flows(flows)
    
  }
  
  return(flows)
}
```

The following function will be required to split FTS flows that run across different years by each year, assuming an even distribution over time:

\[Add code for 02\_fts\_split\_rows.R\]

The splitting of rows by year was necessary to deflate funding amounts by year later in the next step:

\[Add code for 03\_deflators.R\]

The following code then executes the three steps that were defined above, on top of further manipulations to tidy up the downloaded FTS data (e.g., removing duplicates across API requests across multiple years):

\[Add code for 04\_fts\_curated\_flows.R\]

The final bit of code relating to the FTS data creates as ‘master’ dataset of all the extracted FTS data across all years in this guide’s case for further analysis:

\[Add code for 05\_fts\_curated\_master.R\]

## Projects module

The [Projects module](https://projects.hpc.tools/) by UN OCHA is part of the Humanitarian Programme Cycle and it facilitates the project submission, review and approval cycle in countries with project-based Humanitarian Response Plans. This is to ensure that the needs and populations that are planned to be addressed by different agencies in a specific context are reviewed by the respective clusters to enhance coordination and coherence across those projects. A large amount of data on each project is collected as part of this process, including data that is relevant to CVA.

It is important to recognise that this data reflects the planning stage and may not be updated retrospectively if project aspects change during the implementation. [Below](#heading=h.hmsi8ftbjsat), we will combine it with FTS data on funding flows to be able to identify how much funding went to projects that planned for the delivery of CVA.

\[Add code: 06\_fetch\_projects.R\]

## International Aid Transparency Initiative

The [International Aid Transparency Initiative](https://iatistandard.org/en/) (IATI) provides a reporting standard for donors and implementing agencies to publish data relating to aid projects, budgets and transactions in close to real-time (or even forward-looking of planned activities or budgets). It originated in the development sector and many of the largest humanitarian donors and agencies publish some data to this standard.

A crucial difference to other aid reporting platforms like FTS or the OECD DAC Creditor Reporting System is that IATI is a data standard and not a database. This means that through the [IATI datastore](https://datastore.iatistandard.org/) it is possible to download open source aid data published by donors or implementers that meet certain search criteria, but it is up to the user on whether or how to aggregate this data to avoid double-counting. For instance, donors publish data on outgoing disbursements relating to their aid projects and implementers publish data on incoming commitments or disbursements, meaning that close attention needs to be paid when aggregating financial volumes to what type of transaction is used for what group of actors. In addition, the data quality varies significantly across different IATI publishers and sometimes even for the same publisher across different time periods. This means that data quality checks might be required before using the data for analysis or advocacy messaging.

In terms of CVA data, one critical advantage of the IATI standard is that it is designed to enable implementing agencies to publish data on their project expenditure. This is an advantage compared to FTS, which seeks to capture transactions between organisations, given the delivery of CVA represents project expenditure to the organisation providing it to the end recipients.

In 2019, the IATI standard [introduced](https://www.iaticonnect.org/group/standard-management-consultations-0/discussion/added-proposal-add-cash-transfer-and-voucher) the option for publishers using the 2.03 version (or later) of the standard to publish data on cash and voucher assistance by adding it as an optional aid type. This is possible for all their aid activities and not limited to humanitarian assistance. It allows for two ways of publishing CVA data to IATI:

1. As an aid type to an IATI activity, which would allow IATI publishers to flag whether an activity/project includes cash transfers, vouchers or neither.  
2. As an aid type assigned to IATI transactions, including project-related expenditure. This would allow IATI publishers to specify the financial value of any of their project expenditure as cash transfers or vouchers provided to recipients.

If agencies delivering humanitarian (or any other) CVA to recipients used the second way of publishing CVA data to IATI, this would provide the most accurate representation of transfers to recipients as cash or vouchers as part of routine reporting on the overall project characteristics (including total budgets, donors, cluster/sector, etc.). However, hardly any implementing agency currently uses this option of reporting to IATI on CVA in that way. Most current uses of this IATI codelist seem to represent misreporting, where this codelist was used to represent other forms of cash transfers (between organisations or other cash expenditure).

The easiest way to access and check this data would be through the [IATI datastore](https://datastore.iatistandard.org/) advanced search. See the screenshot below for the appropriate filters:

![IATI datastore advanced search filter](/assets/iati_datastore.png)

Running this search would yield all IATI activities where publishers have included either the CVA aid type as flag for the activity or a CVA aid type as characteristic of any transaction related to that activity.

## WFP CASHboard Analytics {#wfp-cashboard-analytics}

The World Food Programme (WFP) maintains its own online dashboard of what it describes as ‘cash-based transfers and commodity vouchers’ with data on WFP’s CVA operations from 2018 to, at the time of writing, 2024\. This crucially includes a breakdown of data by country, which is not evident from the global CVA data collected from WFP (and all other CALP Network members) via survey. It can therefore be useful to incorporate for analysis disaggregated by country, but is not currently used in the global calculation of humanitarian CVA volumes.

The [WFP CASHboard](https://unwfp.maps.arcgis.com/apps/dashboards/5e403a8944104b328117c67ae4afa11e) data can be extracted through the following lines of code:

\[Add code:  
library(jsonlite)  
json\_response \= fromJSON("https://services3.arcgis.com/t6lYS2Pmd8iVx1fy/arcgis/rest/services/global\_CBT\_operations\_by\_country/FeatureServer/4/query?f=json\&where=1%3D1\&returnGeometry=false\&spatialRel=esriSpatialRelIntersects\&outFields=\*\&orderByFields=OBJECTID%20ASC\&resultOffset=0\&resultRecordCount=1000\&cacheHint=true\&quantizationParameters=%7B%22mode%22%3A%22edit%22%7D")  
wfp\_cash\_map\_data \= json\_response$features$attributes  
\]

## Supplementary data

There are a few supplementary datasets that are not directly related to CVA but required to convert aspects of the financial data for better comparability.

### Exchange rates

All currency units tend to be converted to US$ to have a uniform currency across different financial amounts. There are a number of possible data sources to use for this conversion. In previous iterations of this methodology, DI chose to align the exchange rates for this analysis with that used in other parts of DI’s [Global Humanitarian Assistance Reports](https://devinit.org/resources/falling-short-humanitarian-funding-reform/) for consistent currency conversions across analyses. This might be relevant when trying to estimate the share of total international humanitarian assistance (IHA) made up by CVA ([see below](#relative-share-of-cva-as-%-of-iha)).

If following that approach, the exchanges used in different years to convert different currencies into US$ would be primarily sourced from the OECD DAC, and supplemented by the IMF and World Bank for currencies missing from the OECD DAC, as executed by the following code chunk:

\[Add code on exchange rates\]

### Deflators {#deflators}

Currently, the global volumes of humanitarian CVA are presented in current prices, i.e., without adjusting for inflation/rising costs. The main reasons for this are that it would be a slightly arbitrary choice of which set of deflators to use for the implementing agencies’ data (the price level in donor countries or in recipient locations (if known)?) and that adjusting for inflation would require manipulating the implementers’ data so that they potentially do not recognise themselves in the trend anymore.

However, this means that the increase in global volumes of humanitarian CVA is likely inflated by increasing costs/price levels in both donor and recipient countries. It therefore does not represent an increase of x% across years (depending on the years of comparison) all else being equal.

For time-series analysis of financial data over a long time period, it can make sense to adjust financial data in each year for inflation to have better comparability over time. For example, in current prices, the total bilateral official development assistance from OECD DAC countries increased by 324% between 2002 and 2022, but when adjusting for inflation by deflating both to constant 2022 prices, this changes to an increase by only 189%.

Deflators are mostly relevant for this guide as consideration for future methodological adjustments ([see below](#suggestions-for-future-improvements)) and for calculating CVA as % of IHA, the latter previously calculated by DI in constant prices ([see below](#relative-share-of-cva-as-%-of-iha)). The following code chunk calculates the deflators by donor, which are required to change the IHA figures back to current prices so that they are more comparable with the CVA figures:  
\[Add code 03\_deflators.R\]

# Parsing CVA data

Following the procurement of the required source data in the previous steps of this guide, this section lays out how to isolate the data relevant to CVA from those datasets.

## Projects module CVA fields {#projects-module-cva-fields}

First, the relevant CVA fields from the projects module need to be identified so that the CVA information on those projects can be merged with the FTS funding data, given the projects module at this stage only represents planning figures.

Former DI staff already went through all the unique project questions from all response plans in English, French and Spanish up to 2023 to identify those relevant to CVA. These are saved in the CSV file ‘cva\_project\_questions’. The relevant questions were also already classified into whether their answers represent a yes/no flag of whether CVA is part of the project (flagCVA), whether they provide a quantitative indicator of the planned project budget share of cash, vouchers or both (quantC/V/CVA), or whether they contain other information related to CVA, for instance on conditionality or relating to the recipients (otherCVA). This list of relevant questions would need to be maintained and reviewed every year for possible additions. A line of code was added to the script to automate this question review process each year by searching for keywords relevant to CVA in the set of questions, identifying any newly added questions with those keywords that were not previously marked as CVA.

The following script processes the project data fetched from the projects API by merging project data from multiple years, identifying projects related to CVA based on relevant questions, and standardizes the answers for further analysis:

\[Add code 07\_process\_project\_data.R\]

## Combining FTS and projects module CVA data

Now that we have isolated the **planned** project budget percentage for cash/vouchers/CVA for projects with available data, we can use the unique project IDs to merge this information with FTS funding flow data to the same projects. We thereby make the assumption that the delivered share of CVA of the received funding for each project matches that of the planned CVA project budget share.

We also import the project text for projects that received funding on FTS and combine it with the FTS description, which tends to be brief. This will provide more text data for the machine learning algorithm later on to classify the CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

The following code reads in the FTS and the project data, merges the CVA project budget percentages by project ID and combines project text fields:

\[Add code from 08\_fts\_keyword\_searching\_cash.R lines 1 to 51\]

## Identifying CVA relevance of funding

There are several possible ways to identify financial flows on FTS relevant to CVA, now that we have enriched it with projects data:

1. The ‘method’ column: this can contain either ‘Traditional aid’ as default value or, if reported to FTS, ‘Cash transfer programming (CTP)’. There are however a number of flows that evidently support CVA as per the other methods of identifying relevant FTS data, indicating that this reporting field is unfortunately not in consistent use.  
2. The ‘destinationObjects\_Cluster.name’ column: this is a free-text field that represents the field cluster. It can be in English, French or Spanish. A number of response-plans include a multi-purpose cash cluster, which would be listed in this field, though in a number of different spellings or languages (though always the same spelling and language for the same response plan in the same year). Relevant clusters identified up until 2023 are listed in the 08\_fts\_keyword\_searching\_cash.R code chunk from lines 98 up to 119 and need to be maintained every year to check for updates.  
3. The ‘project\_cva\_percentage’ column: This has been added from the projects dataset and represents the planned budget percentage of CVA for the project supported by this financial flow.  
4. The ‘all\_text’ column: this is a free-text field that often contains a description of the activity supported by the financial flow (merged from the flow description and project text). This can be scanned for CVA keywords and then classified by a machine learning algorithm for its CVA relevance ([see below](#machine-learning-to-classify-cva-flow-descriptions)).

In the existing methodology, the choice was made to distinguish for each FTS financial flow whether its CVA relevance is full, partial or nonexistent. This was in recognition of a number of large financial flows, especially from the US, that as per their description supported a range of activities including CVA alongside other modalities. It would therefore be an overestimate to count the full value of those flows towards CVA and they are marked as partial. Financial flows with no identifiable CVA characteristics as per the three criteria listed above were marked as not relevant.

Otherwise, the categorisation into full/partial/none for the three categories works as follows:

1. ‘Method’: Marked as ‘Full’ if reported as ‘Cash transfer programming (CTP)’ and ‘None’ otherwise.  
2. ‘destinationObjects\_Cluster.name’: Marked as ‘Full’ if a relevant CVA field cluster is reported as the only destination cluster. Marked as ‘Partial’ if a CVA field cluster is reported as one of multiple destination clusters for the same flow. Marked as ‘None’ otherwise.  
3. ‘project\_cva\_percentage’: Marked as ‘Full’ if greater than 75%, marked as ‘Partial’ if between 0 and 75%, marked as ‘None’ if equal to zero or blank.  
4. [See below](#machine-learning-to-classify-cva-flow-descriptions) on machine-learning to classify flow and project descriptions.

The following code chunk executes this classification process with FTS flow data for steps 1 to 3:

\[Add code from ‘08\_fts\_keyword\_searching\_cash.R’ line 52 to line 148\]

### Machine-learning to classify CVA flow descriptions {#machine-learning-to-classify-cva-flow-descriptions}

After applying the three above steps to classify the CVA relevance of FTS funding flows, this still leaves the possibility of identifying funding for CVA through unstructured text. The former DI team trained a machine-learning classifier on a manually classified dataset of several hundred FTS flows for their CVA relevance. This algorithm is then applied to text for FTS flows that either contain a keyword relevant to CVA in their flow or project text (the existing keyword list is in lines 52 to 76 and can be adapted as required) or those that represent funding to projects flagged as CVA in the projects data ([see above](#projects-module-cva-fields)), but without data on the planned budget share of CVA.

The following code chunk compiles the relevant text data before we process that with the classifier:

\[Add code from ‘08\_fts\_keyword\_searching\_cash.R’ line 149 to 157\]

The CSV data from this code serves as input for the classifier, which is run in Python. The following script uses a pre-trained machine learning model to infer the relevance of FTS flows to CVA by processing the text descriptions of flows and predicts for their CVA relevance based on the model's classification:

\[Add code from flow\_inference.py\]

The algorithm predicts with a percentage chance for each prediction whether the relevant financial flows from FTS have full or partial relevance to CVA based on the text input. This resulting prediction data can then be merged into the FTS flow dataset through the following code chunk:

\[Add code from ‘08\_fts\_keyword\_searching\_cash.R’ lines 159 to end\]

### Calculating the CVA-relevant funding amounts

Finally, what remains is to calculated the estimated CVA US$ amount in terms of total programming costs supported by each financial flow in the dataset. Given, as described above, FTS captures financial flows between organisations (i.e., from donors to implementers, or more rarely sub-grants from one implementer to another), instances of funding to CVA projects that are classified as ‘Full’ and thereby fully included in terms of their CVA amounts also include programming costs. For financial flows with planning information on the share of the project budget for CVA, it is possible that those shares represent transfer values only. However, it is not straightforward to ascertain this, given the CVA project questions are often ambiguously worded and do not explicitly ask for the share of CVA in terms of transfer value or programming costs. The many instances of projects indicating CVA project budget shares of 90% or higher suggest that at least some organisations interpret this question to also refer to programming costs. This methodology therefore assumes that instances of funding on FTS to CVA include transfers and overall CVA programming costs, whether counted fully or partially according to the logic laid out below.

The logical steps for calculating CVA amounts are laid out as follows:

1. Including the full current USD amount if ‘method’ is reported as ‘Cash transfer programming (CTP)’, or if there is only one destination field cluster and that is relevant to CVA (usually multi-purpose cash)  
2. For remaining flows with relevant field clusters, including a proportion of the current USD amount if there are multiple destination field clusters, one of which is relevant to CVA. The proportion is estimated by taking the fraction of one divided by the number of total field clusters reported for that financial flow.  
3. For remaining flows with CVA project budget shares, including the current USD amount multiplied by those budget shares.  
4. For remaining flows with a predicted CVA relevance from the machine learning model, including all of the current USD amount if the predicted confidence for CVA relevance is 80% or higher and if the text field includes keywords such as cash/voucher/cva.  
5. Remaining flows with a predicted CVA relevance from the machine learning model of 50% or higher that do not meet the criteria of the previous step are compiled as list of financial flows that require a manual review of their text field for whether they seem to fully or partially support CVA, or whether they represent false positives, or whether the text is insufficient to make that assessment (then also excluded).

To save time when executing this guide and methodology, the following script already joins the manual review decisions from former DI staff up to 2023 to the file. The remaining flows that have not yet coded and that remain after step 5 need to be then reviewed and classified manually. The following code chunk executes those steps:

\[Add code 09\_calculate\_cva.R from beginning to line 73\]

The following code chunk uses the output of the manual review to enhance the training data for the machine learning model:

\[Add code 09\_calculate\_cva.R from from line 74 to line 87\]

Once the flows have been manually reviewed, they can be read into the R environment and merged with the fts\_cva dataset:

\[Add code 09\_calculate\_cva.R from from line 88 to 92\]

This then completes the process for compiling a fully coded dataset of estimated funding amounts to humanitarian CVA based on FTS flow and project planning data.

# CVA data analysis

## Global estimated volumes of humanitarian CVA  {#global-estimated-volumes-of-humanitarian-cva}

The primary use of the above data analysis, adapted from its early iteration from the [‘Counting Cash’ paper](https://odi.org/en/publications/counting-cash-tracking-humanitarian-expenditure-on-cash-based-programming/) in 2016, is to calculate an estimate of the global value of humanitarian cash and voucher assistance delivered in any given year. There are usually two sets of figures:

1. The overall programming costs, including transfer values, for delivering CVA. The rationale behind calculating this is that funding is required for more than just the CVA transfer value to facilitate those transfers.   
2. The transfer values of the delivered CVA, disaggregated if possible by cash and vouchers.

The CVA survey requests both from implementing agencies, though only a small share of respondents provides both and most only provide data on the transfer values. FTS data, as mentioned above, is assumed to provide an indication of funding amounts to overall CVA programming. Given the need to therefore be able to convert CVA programming costs into transfer values and vice versa, every year the methodology was executed in the past, a percentage was calculated based on data from organisations that provided both in their surveys (programming costs and transfer values) of what the ratio was from the latter to the former for the entire sample. This is included as a third tab in the survey data file. For organisations that did not provide CVA programming costs in the survey data, this set of percentages is used to calculate the estimated programming costs for each provided transfer value.

The sub-grant data in the survey file includes a column ‘Take out’, which indicates whether the recipient organisation of each sub-grant has also provided survey data and therefore should be taken into consideration to avoid double-counting when aggregating that survey data. Filling in this column has in the past been a manual process of first reviewing all survey submissions (in the ‘Survey\_data’ tab) and then going through the received data on sub-grants line by line to highlight which of the sub-grant recipients also provided survey data.

The following code reads in the survey data into the R environment so that it can be analysed and combined with the fts\_cva data:

\[Add code 10\_global\_cva\_analysis.R up to line 37\]

To be able to analyse the FTS data along the survey data, we also need to add another reference file that identifies which FTS organisations have provided CVA survey data in any of the years and another file that aligns the implementing organisation types on FTS with the categories used in the final CVA analysis output:

\[Add code 10\_global\_cva\_analysis.R up to line 52\]

What then remains is to follow the steps below to aggregate the CVA data in both datasets while avoiding double-counting:

1. Starting with the survey data, we aggregate all of the programming costs and the transfer values by organisation type and years.  
2. From the calculated program costs, we subtract all the aggregate sub-grant values by source organisation type and year that were highlighted to be taken out to avoid double-counting. We remove double-counted funding by source organisation type to ensure that they are allocated to the organisation type that does the last mile of CVA delivery. For the calculated transfer values in step 1, we do the same but multiply the sub-grant amounts by the corresponding percentage of the transfer value/programming cost estimate for each year.  
3. For FTS data, we aggregate all the CVA amounts by cva\_org\_type (to match survey data) and year, ensuring we only include data for destination organisations in years for which those organisations did not also submit a survey. This provides the programming costs CVA estimate from FTS data additional to the CVA survey. We multiple those values by the corresponding percentage of the transfer value/programming cost estimate for each year to obtain the FTS estimates on CVA transfer values supported by this funding.  
4. We add the values calculated from survey and FTS data for both programming costs and transfer values by organisation type and year to obtain the final set of global CVA estimates.

\[Add remaining code 10\_global\_CVA\_analaysis\]

## CVA data by cluster

There are two main avenues to explore CVA data by cluster with the data compiled in this guide. 

The first involves the FTS CVA dataset generated above. Within that, we could analyse funding for CVA by cluster across a large number of contexts. The ‘destinationObjects\_GlobalCluster.name’ column standardises the field clusters into a set of global clusters and allows for easier analysis (though currently there is no designated ‘multi-purpose cash’ category within that column). However, given none of the possible ways of reporting on CVA to FTS are used comprehensively or consistently, this data will inevitably be partial. It might therefore be harder to use as a basis for advocacy with clusters to change the share of CVA within each cluster if that true share is only partially known.

The second possible way involves using the project planned budget data ([see above](#projects-module-cva-fields)). Thie has the advantage that it provides a complete account of planning figures for each HRP with available data (usually over 20 plans each year). This should therefore provide information with fairly high confidence about the planned significance of CVA by cluster in those response plans. This data would be available alongside information on which organisations or organisation types plan for smaller or greater shares of CVA within their activities, and what those activities are. It also has the advantage for being planning data that it can be used for forward-looking advocacy on activities that are yet to be implemented, unlike most other CVA data, which is mostly retrospective.

## CVA data by country

The dataset compiled above from FTS and projects data allows for a partial analysis of global CVA volumes by country. The comprehensiveness can be improved by also incorporating data from the WFP CASHboard ([see above](#wfp-cashboard-analytics)) to get a complete representation of WFP’s data. This would then require excluding WFP from the FTS data to avoid double-counting. The disadvantage for the WFP data is however that it does not include donor information.

## Relative share of CVA as % of IHA {#relative-share-of-cva-as-%-of-iha}

There tends to be interest, especially from within the CALP Network, on what share of international humanitarian assistance (IHA) is delivered as CVA. Calculating this is currently flawed and only a best estimate given it involves comparing financial inputs to the humanitarian system with its outputs (with CVA a delivery modality). Given the lack of comprehensive public reporting on how much and when humanitarian activities funding from international funding deliver CVA, there is no sufficient data to connect the two. There can also likely be a mismatches (both ways) of funding being disbursed by a donor in one year and it being delivered as CVA in another, making it harder to compare data on financial inputs and outputs in the same year.

Still, given the demand for this calculation, the method used so far to do it was to take the global CVA estimate for programming costs produced in this guide ([see above](#global-estimated-volumes-of-humanitarian-cva)) and to divide it by the best possible estimate of IHA available for each year. This also requires first excluding some CVA survey data that would not be included by IHA funding data (e.g. CVA delivered domestically by RCRC national societies in donor countries).

The IHA figures used are those calculated by DI, which are partly based on a labour-intensive process to compile data on private humanitarian funding alongside an analysis of donor funding amounts based on mostly DAC data for DAC member donors and otherwise FTS data for other donors. IHA figures need to be converted to current prices to be comparable to CVA figures, which have not been deflated in this methodology ([see above](#deflators)). The historical IHA data in current prices calculated by DI is provided alongside this guide to aid calculating this percentage.

# Methodological limitations

Survey data provides very few data points to further investigate CVA data (e.g., lack of donor, country or cluster data).

FTS and project data as main alternative data sources with much more contextual data are inconsistently reported in terms of CVA data and therefore only provides a partial picture across all dimensions. 

Further, FTS represents transfers between organisations (unable to easily capture transfer values in its current state) and project data represents planning figures. When project data merged with FTS, only works for plans with the relevant questions, and only around 60% of funding to those plans reported with project IDs.

# Suggestions for future improvements {#suggestions-for-future-improvements}

Better reporting by implementers \- actually follow the minimum agreements as agreed, especially by publishing comprehensive and timely CVA data to 

Point to the CVA collected by cash working groups and how that could be improved through better interoperability with, e.g., FTS data

Social protection payments in crisis countries \- no comparable global data source, but might be possible to incorporate in specific contexts with available data

Technically:

* Automate the de-duplication of subgrants (Alex already did most of the coding work on this)  
* Change sequence of logical steps for CVA amount calculation  
* Can think about deflating CVA given the time horizon is extending so far but not sure how to best do that \- CVA as % of IHA estimate gets around that somewhat but also a flawed calculation ([see above](#relative-share-of-cva-as-%-of-iha))  
* How to handle anonymous reporting on FTS \- e.g., international NGO’s (confidential)
