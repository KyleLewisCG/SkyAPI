# Packages and Options --------------------
library(httr2)

conflicted::conflict_prefer("filter", "dplyr")

# Setup Auth Functions-------

bb_auth_app_cred_func <- function(new_args = F){

    if(Sys.getenv("bb_ap_id") == "" | Sys.getenv("bb_ap_secret") == "" | Sys.getenv("bb_sub_key") == "" | new_args){

    stop("Must enter 'bb_ap_id', 'bb_ap_secret', and 'bb_sub_key in project level .Renviron file can use usethis::edit_r_environ(scope = 'project')")

    }

    app_id <- Sys.getenv("bb_ap_id")

    app_secret <- Sys.getenv("bb_ap_secret")

        sub_key <- Sys.getenv("bb_sub_key")
    
    a <- list(app_id = app_id, app_secret = app_secret, sub_key = sub_key)

    return(a)

}

ap_ids <- bb_auth_app_cred_func()

# API Details --------------------

bb_ap_id <- ap_ids$app_id
bb_ap_secret <- ap_ids$app_secret
bb_auth_url <- "https://app.blackbaud.com/oauth/authorize"
bb_token_url <- "https://oauth2.sky.blackbaud.com/token"
bb_redirect <- "http://localhost:1410/"
bb_sub_key <- ap_ids$sub_key

# URLS --------------------

## API Docs ------
bb_api_docs_urls <- list(
    "school" = "https://developer.sky.blackbaud.com/api#api=school&operation=V1AcademicsSectionsBySection_idAssignmentsGet",
    "gl" = "https://developer.sky.blackbaud.com/api#api=56eb17a0a9db9516c46bff6f&operation=GetAccount",
    "ap" = "https://developer.sky.blackbaud.com/api#api=57a342f1d7dcde1c28a749fe&operation=ListAccountsPayableDepartments",
    "t" = "https://developer.sky.blackbaud.com/api#api=58e3b2880528ab71c796492e&operation=CreateBankAccountAdjustment",
    "data" = "https://developer.sky.blackbaud.com/api#api=nxt-data-integration&operation=CreateCampaign"
)

## Base URL ------
bb_url_base <- "https://api.sky.blackbaud.com/"

## Base API URLs ------
bb_url_list <- list(
    "gl" = "generalledger/v1/",
    "school" = "school/v1/",
    "treasury" = "treasury/v1/",
    "ap" = "accountspayable/v1/",
    "event" = "event/v1/",
    "query" = "query"
)

## GL URL -------
bb_url_gl_list <- list(
    "funds" = "funds",
    "account" = "accounts",
    "codes" = "codes",
    "budget" = "budgets",
    "fiscal_year" = "fiscalyears",
    "segments" = "segments",
    "structure" = "structure",
    "journalcodes" = "journalcodes",
    "journalentries" = "journalentries",
    "journalentry_batch" = "journalentrybatches",
    "transaction_distribution" = "transactiondistributions",
    "projects" = "projects",
    "projects_types" = "projects/types",
    "customfields" = "customfields",
    "details" = "details",
    "lines" = "lines",
    "scenarios" = "scenarios",
    "validate" = "validate",
    "summary" = "summary",
    "periodsummary" = "periodsummary",
    "transactioncodes" = "transactioncodes",
    "values" = "values",
    "processinginfo" = "processinginfo",
    "summarize" = "summarize",
    "summaries" = "summaries"
)

## School URL ------
bb_url_school <- list(
    "lists" = "lists",
    "advanced" = "advanced",
    "roles" = "roles",
    "academics" = "academics",
    "teachers" = "teachers",
    "users" = "users",
    "extended" = "extended",
    "relationships" = "relationships",
    "events" = "events",
    "categories" = "categories",
    "calendar" = "calendar",
    "schedules" = "schedules",
    "meetings" = "meetings",
    "customfields" = "customfields",
    "gradelevels" = "gradelevels",
    "offeringtypes" = "offeringtypes",
    "levels" = "levels",
    "sessions" = "sessions",
    "terms" = "terms",
    "timezone" = "timezone",
    "years" = "years",
    "sections" = "sections",
    "activities" = "activities",
    "academics" = "academics",
    "schedules" = "schedules",
    "master" = "master",
    "sets" = "sets",
    "courses" = "courses",
    "cycles" = "cycles",
    "departments" = "departments",
    "students" = "students",
    "enrollments" = "enrollments",
    "changes" = "changes",
    "content" = "content",
    "assignments" = "assignments",
    "changed" = "changed",
    "rosters" = "rosters"
)

## Events URL -------
bb_url_events <- list(
    "eventcategories" = "eventcategories",
    "eventlist" = "eventlist",
    "events" = "events"
    
)


## Treasury URL ------
bb_url_treasury_list <- list(
    "bankaccounts" = "bankaccounts",
    "payments" = "payments",
    "checks" = "checks",
    "register" = "register"
    
)

## AP URL ------
bb_url_ap_list <- list(
    "invoices" = "invoices",
    "vendors" = "vendors",
    "addresses" = "addresses",
    "attachments" = "attachments"
)

## Query URL ------

bb_url_query_list <- list(
    "categories" = "categories",
    "querytypes" = "querytypes",
    "summaryfields" = "summaryfields",
    "queryfields" = "queryfields",
    "queries" = "queries",
    "jobs" = "jobs",
    "lookupvalues" = "lookupvalues",
    "executebyid" = "executebyid",
    "queries" = "queries"
)

# API App --------------------

bb_client <- oauth_client(
    id = bb_ap_id,
    token_url = bb_token_url, 
    secret = bb_ap_secret, 
    name = "R Dashboard", 
    auth = "header"
    )

# Parameters --------------------

bb_par_gl <- list(
    "from_date" = NULL, 
    "to_date" = NULL, 
    "account_number" = NULL, 
    "batch_id" = NULL, 
    "ui_batch_id" = NULL,
    "ui_project_id" = NULL,
    "journal" = NULL,
    "type_code" = NULL,
    "last_modified" = NULL,
    "post_status" = NULL,
    "search_text" = NULL,
    "from_account" = NULL,
    "to_account" = NULL,
    "from_account_code" = NULL,
    "to_account_code" = NULL,
    "status" = NULL,
    "scenarioId" = NULL,
    "fiscal_year_id" = NULL,
    "fiscal_period_id" = NULL,
    "ui_project_id" = NULL,
    "transaction_code_1_value" = NULL,
    "transaction_code_2_value" = NULL,
    "transaction_code_3_value" = NULL,
    "transaction_code_4_value" = NULL,
    "encumbrance" = NULL,
    "include_tax_entity" = NULL
)

bb_par_school <- list(
    "user_id" = NULL,
    "roles" = NULL,
    "base_role_ids" = NULL,
    "first_name" = NULL,
    "last_name" = NULL,
    "email" = NULL,
    "maiden_name" = NULL,
    "grad_year" = NULL,
    "start_date" = NULL,
    "end_date" = NULL,
    "end_grad_year" = NULL,
    "offering_types" = NULL,
    "section_ids" = NULL,
    "last_modified" = NULL,
    "school_level" = NULL,
    "show_time_for_current_date" = NULL,
    "school_year" = NULL,
    "level_num" = NULL,
    "level_id" = NULL,
    "department_id" = NULL,
    "types" = NULL,
    "status" = NULL,
    "persona_id" = NULL,
    "filter" = NULL,
    "search" = NULL,
    "include_dropped" = NULL
)

bb_par_events <- list(
    "include_inactive" = NULL,
    "name" = NULL,
    "lookup_id" = NULL,
    "category" = NULL,
    "event_id" = NULL,
    "start_date_from" = NULL,
    "start_date_to" = NULL,
    "date_added" = NULL,
    "last_modified" = NULL,
    "fields" = NULL,
    "sort" = NULL
)

bb_par_treasury <- list(
    "bank_account_id" = NULL,
    "transaction_type" = NULL,
    "transaction_status" = NULL,
    "post_status" = NULL,
    "from_post_date" = NULL,
    "to_post_date" = NULL,
    "check_type" = NULL,
    "starting_check_number" = NULL,
    "ending_check_number" = NULL,
    "from_transaction_status_date" = NULL,
    "to_transaction_status_date" = NULL
)

bb_par_ap <- list(
    "to_date" = NULL,
    "from_date" = NULL,
    "invoice_status" = NULL,
    "vendor_name" = NULL,
    "status" = NULL,
    "ui_vendor_id" = NULL,
    "customer_number" = NULL,
    "last_modified" = NULL,
    "date_added" = NULL,
    "added_by" = NULL,
    "last_modified_by" = NULL,
    "payment_method" = NULL,
    "post_status" = NULL,
    "search_text" = NULL
)

bb_par_query <- list(
    "product" = NULL,
    "module" = NULL,
    "category" = NULL, 
    "search_text" = NULL, 
    "merged_queries_only" = NULL,
    "my_queries_only" = NULL,
    "query_type_id" = NULL,
    "query_type_ids" = NULL,
    "query_format" = NULL,
    "sort_descending" = NULL,
    "sort_column" = NULL,
    "list_queries" = NULL,
    "my_fav_queries_only" = NULL,
    "date_added" = NULL,
    "added_by" = NULL,
    "include_read_url" = NULL
)

# Helper Lists -----

bb_q_helper_list <- list(
    product = list(
        "RE" = "RE", 
        "FE" = "FE"
        ),
    module = list(
        "None" = "None",
        "GeneralLedger" = "GeneralLedger",
        "AccountsPayable" = "AccountsPayable",
        "AccountsReceivable" = "AccountsReceivable",
        "FixedAssets" = "FixedAssets",
        "CashReceipts" = "CashReceipts"
        ),
    query_format = list(
        "Dynamic" = "Dynamic",
        "Static" = "Static"
    ),
    sort_column = list(
        "Name" = "Name",
        "DateLastRun" = "DateLastRun",
        "DateChanged" = "DateChanged",
        "ElapsedMs" = "ElapsedMs",
        "DateAdded" = "DateAdded",
        "AddedBy" = "AddedBy",
        "LastChangedBy" = "LastChangedBy",
        "Records" = "Records"
    ),
    list_queries = list(
        "Unset" = "Unset",
        "NoListQueries" = "NoListQueries"
    ),
    include_read_url = list(
        "Never" = "Never",
        "OnceRunning" = "OnceRunning",
        "OnceCompleted" = "OnceCompleted"
    ),
    content_disposition = list(
        "Inline" = "Inline",
        "Attachment" = "Attachment"
    ),
    ux_mode = list(
        "Synchronous" = "Synchronous",
        "Asynchronous" = "Asynchronous"
    ),
    output_format = list(
        "Csv" = "Csv",
        "Json" = "Json",
        "Jsonl" = "Jsonl",
        "Xlsx" = "Xlsx"
    ),
    formatting_mode = list(
        "None" = "None",
        "UI" = "UI",
        "Export" = "Export"
    ),
    sql_generation_mode = list(
        "Query" = "Query",
        "Export" = "Export",
        "Report" = "Report"
    )
)

bb_q_body_list <- list(
    id = NULL,
    ux_mode = NULL,
    output_format = NULL,
    formatting_mode = NULL,
    sql_generation_mode = NULL,
    use_static_query_id_set = NULL,
    results_file_name = NULL
)

# Functions --------------------

find_sky_api_docs <- function(api = "school") {
    
    browseURL(bb_api_docs_urls[[api]])
    
    
}

# Main Function --------------------

bb_get_func <- function(
        api_url = bb_url_list$school, 
        url1 = NULL,
        url2 = NULL, 
        url3 = NULL, 
        url4 = NULL,
        url5 = NULL,
        limit = 500,
        offset = 0,
        page = 1,
        marker = 0,
        params = NULL,
        testing = F,
        no_format = F,
        no_page = F,
        method = "GET",
        body = NULL,
        ...
    ){

# Setup Variables ----

params <- params

params2 <- list(...)

count = 1

times = 0

offset = offset

page = page

page_helper = 1

marker = marker

tib_list <- list()

# While Loop to handling paging ------
    
while ((offset < count) & page_helper != 0 & marker != "") {

## Base Function that makes the API Call -------

    #httr2::oauth_cache_path()

base_data <- 
request(base_url = bb_url_base) |> 
    req_oauth_auth_code(
        client = bb_client, 
        auth_url = bb_auth_url, 
        redirect_uri = bb_redirect, 
        cache_disk = T, 
        cache_key = "abc"
    ) |> 
    req_throttle(rate = 58/60) |> 
    req_retry(
        max_tries = 5,
        is_transient = ~ resp_status(.x) %in% c(429, 500, 503),
        backoff = ~5
        ) |> 
    req_url_path(api_url) |>
    req_url_path_append(url1) |>
    req_url_path_append(url2) |>
    req_url_path_append(url3) |>
    req_url_path_append(url4) |>
    req_url_path_append(url5) |>
    req_url_query(
        limit = limit,
        offset = offset
        ) |>
    req_url_query(page = page) |>
    req_url_query(marker = marker) |>
    req_url_query(!!!params, .multi = "comma") |>
    req_url_query(!!!params2, .multi = "comma") |>
    req_headers(
        "Bb-Api-Subscription-Key" = bb_sub_key
        ) |> 
    req_method(method)

## Print for Testing Purposes ------

        if (testing) {
            
            print(base_data)
            
            }

## If no body call -------
    
    if (!is.null(body)) {
        
        base_data <-
            base_data |>
            req_progress(type = "up") |>
            req_body_json(data = body) |> 
            req_perform()

        return(base_data)
        
    }
    
    base_data <- base_data |> req_perform()

## Loop ------

    base_data <- base_data |> 
        resp_body_json(simplifyVector = F)

    times <- times + 1

# Paging Types
# Offset
# Next
# Page

### Paging for returned data -----
    
if (no_page) {
    
    return(base_data)
    
} else {
    
    if(is.null(names(base_data))){

        page_type <- "unnamed_list"

    } else if (names(base_data)[1] == "count" & names(base_data)[2] == "value") {
        
        page_type <- "offset"

    } else if (names(base_data)[1] == "count" & 
            names(base_data)[2] == "page" & 
            names(base_data)[3] == "results") {

        page_type <- "page"

    } else if (names(base_data)[1] == "count" & 
            names(base_data)[2] == "next_link" & 
            names(base_data)[3] == "value") {

        page_type <- "next"

    }
    
    else {

        page_type <- "none"

    }
    
    if (page_type == "offset") {
        
        count <- base_data$count
        
        tib_list[[times]] <- base_data
        
        offset <- length(base_data$value) + offset
        
    } else if (page_type == "page") {
        
        page <- page + 1
        
        count <- base_data$count
        
        tib_list[[times]] <- base_data
        
        page_helper <- (page * count)
        
    } else if (page_type == "next") {
        
        count <- base_data$count
        
        marker <- ifelse(
            base_data$next_link == "",
            "", 
            stringr::str_remove(base_data$next_link, ".*marker=")
            )
        
        tib_list[[times]] <- base_data
        
    } else if (page_type == "unnamed_list"){

        count <- offset

        return(base_data)

    } else {
        
        count <- offset
        
        tib_list[[times]] <- base_data |> 
            purrr::modify_tree(leaf = ~(.x %||% NA)) |>  
            purrr::modify_if(.p = \(x) is.list(x) & length(x) != 1, .f = list)
    }
    
    #count

    #tib_list
    
    #offset
    
}
}

## Combine all data ------
    
data <- dplyr::bind_rows(tib_list)

## Format combined data ------
    
if (no_format) {
    
    return(data)
    
} else {

if (page_type == "offset" | page_type == "next") {
    
    data <- data |> tidyr::unnest_wider(value)

    } else if (page_type == "page") {
        
    data <- data |> tidyr::unnest_wider(results)
        
    } else {
        
    data
        
    }

return(data)

    }

}
