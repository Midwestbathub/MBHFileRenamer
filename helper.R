# import extra libraries needed for these functions
library('httr')
library('jsonlite')


#' @title NABat login to NABAt Database GQL
#'
#' @description
#' Get a NABat GQL token to use for queries
#' @param username (optional) String your NABat username from
#' https://sciencebase.usgs.gov/nabat/#/home
#' @param password (optional) String it will prompt you for your password
#' @param branch (optional) String that defaults to 'prod' but can also be
#' 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' nabat_gql_token = get_nabat_gql_token(username = 'NABat_Username')
#' # Prompts for password
#' }
#'
#' @export
#'
get_nabat_gql_token = function(
  username = NULL,
  password = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  
  # Prompts password input incase password isn't included in function call
  if (is.null(username)){
    username = rstudioapi::showPrompt(title = "Username",
      message = "Username", default = "")
  }
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }
  
  out = tryCatch({
    # Returns a message with username
    message(paste0("Logging into the NABat database as ", username))

    
    # Set URL based on branch
    if (is.null(url)) url = get_gql_url(branch)
    
    # Set headers for login
    if (docker){
      if(!is.null(aws_gql)){
        url = paste0(aws_alb, '/graphql')
        headers = httr::add_headers(host = aws_gql)
      }else {
        headers = httr::add_headers(Accept = "")
      }
    } else{
      headers = httr::add_headers(Accept = "")
    }
    
    # Username and password
    variables = paste0('{"l":{"userName" : "',username,'", "password" : "',
      password,'"}}')
    # Mutation to get token
    query = 'mutation RRlogin($l:LoginInput!){
    login(input:$l){
    access_token,
    refresh_token,
    expires_in
    }
  }'
    # Finalize json request
    pbody = list(query = query, variables = variables,
      operationName = 'RRlogin')
    # Query GQL API
    res = POST(url, headers, body = pbody, encode="json")
    # Remove variables with Password
    rm(password, variables, pbody)
    # Extract token
    content = content(res)
    error  = content$data$login$error
    bearer = content$data$login$access_token
    refresh_token = content$data$login$refresh_token
    
    if (res$status_code != 200){
      message(paste0('Status code: ', res$status_code))
      return(NULL)
      }
    if (is.null(refresh_token)){
      message('Error on login. Check Password/Username ')
      return(NULL)
      }
    
    access_token = strsplit(bearer, 'Bearer ')[[1]][2]
    message("Returning a GQL token for NABat.")
    expires = content$data$login$expires_in - (60 * 10)
    refresh_at_this = Sys.time() + expires
    return (list(refresh_token = refresh_token, access_token = access_token,
      refresh_at = refresh_at_this))
    },
    #If it errors or refresh_token = NULL then function will fail
    error = function(cond) {
      message(cond)
      return(NULL)
    })
  return (out)
}


#' @title NABat login to NABAt Database GQL and get access_token
#'
#' @description
#' Get a NABat GQL token to use for queries
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but can
#' also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' #' \dontrun{
#' nabat_gql_token = get_refresh_token(token)
#' -- Prompts for password
#' }
#'
#' @export
#'
get_refresh_token = function(
  token = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE,
  force = FALSE){
  
  # Get URL for branch
  if (is.null(url)) url = get_gql_url(branch)
  
  if (docker){
    if(!is.null(aws_gql)){
      url = paste0(aws_alb, '/graphql')
      headers = httr::add_headers(host = aws_gql)
    }else {
      headers = httr::add_headers(Accept = "")
    }
  } else{
    headers = httr::add_headers(Accept = "")
  }
  
  if (is.null(token)){
    message('Run get_nabat_gql_token() to log back in.')
  }
  
  expires_in = token$refresh_at - Sys.time()
  # If the token has expired than refresh the access_token and
  ## use this new one
  
  out = tryCatch({
    if (expires_in < 0 | force){
      # Username and password
      variables = paste0('{"l":{"userName" : "", "password" : "", "refreshToken": "',
        token$refresh_token,'"}}')
      # Mutation to get token
      query = 'mutation RRlogin($l:LoginInput!){
      login(input:$l){
      access_token,
      refresh_token,
      expires_in
      }
    }'
      # Finalize json request0
      pbody = list(query = query, variables = variables, operationName = 'RRlogin')
      # Query GQL API
      res = httr::POST(url, headers, body = pbody, encode="json")
      
      # Extract token
      content = content(res)
      error  = content$data$login$error
      bearer = content$data$login$access_token
      refresh_token  = content$data$login$refresh_token
      
      if (res$status_code != 200){stop(paste0('Status code: ', res$status_code))}
      #if (is.null(refresh_token)){stop('Error on login. Check Password/Username ')}
      if (is.null(refresh_token)){stop('Error on login. Check Password/Username ')}
      
      
      access_token = strsplit(bearer, 'Bearer ')[[1]][2]
      message("Returning a GQL token for NABat.")
      expires = content$data$login$expires_in - (60 * 10)
      refresh_at_this = Sys.time() + expires
      return (list(refresh_token = refresh_token, access_token = access_token,
        refresh_at = refresh_at_this))
      
}else{
  # If the access token has not expired, then use the original one
  ## from token$access_token
  refresh_at_this = token$refresh_at
  refresh_token = token$refresh_token
  access_token = token$access_token
  return (list(refresh_token = refresh_token, access_token = access_token,
    refresh_at = refresh_at_this))
}
    },
    # If it errors or refresh_token = NULL then function will fail
    error = function(cond) {
      message(cond)
      return(NULL)
    })
  return (out)
  }



#' @title Get User Id from username
#'
#' @description
#' Uses GQL to query username for user ID
#'
#' @param username String NABat username (email)
#' @param token List token created from get_nabat_gql_token()
#' or get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker
#' container or not
#'
#' @keywords species, bats, NABat
#'
#' @export
#'
get_user_id_by_email = function(
  username,
  token,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  # Get headers for token
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  # GQL Query
  query =   'query RRuserByEmailQuery ($email: String!) {
  userByEmail (email: $email) {
  id
  }
}'
  variables = paste0('{"email": "',username,'"}')
  pbody = list(query = query, variables = variables,
    operationName = 'RRuserByEmailQuery')
  # Post to nabat GQL
  res       = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res)
  user_id =  content$data$userByEmail$id
  
  return(user_id)
}


#' @title Get Upload file preview
#'
#' @description Returns a template to be uploaded with the processing function
#'
#' @param file_path String full path to CSV file for preview
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param survey_type (optional) String 'bulk_sae' | 'bulk_mae' | 'bulk_hib' | 'bulk_mat'
#' @param branch (optional) String that defaults to 'prod' but can also be
#' 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
#'

get_upload_file_preview = function(
  file_path,
  token,
  survey_type = 'bulk_sae',
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  # Get headers for token
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  if (survey_type == 'bulk_hib' || survey_type == 'bulk_mat') survey_type = 'bulk_cc'
  
  data_type_ = 'full'
  operation_name = paste0('RR',survey_type,'Preview')
  # GQL Query
  preview_query = paste0('query ',operation_name,'(
    $surveyType: String
    $data: String
    $transactionUuid: String
    $dataType: String
    $requiredFields: [String]
    $template: JSON
  ) {
    bulkPreview(
    surveyType: $surveyType
    data: $data
    transactionUuid: $transactionUuid
    dataType: $dataType
    requiredFields: $requiredFields
    template: $template
    ) {
    headersDetected
    missing
    matched
    preview
    }
  }')

  # parse file and only upload headers, so that preview takes less time
  tmp_df = read.csv(file_path, stringsAsFactors = FALSE)[1,][-1,]
  tmp_file = tempfile()
  write.csv(tmp_df, tmp_file, row.names = FALSE)
  
  # Remove characters that break
  upload_data = readChar(tmp_file, file.info(tmp_file)$size, useBytes = TRUE)
  upload_data = gsub('\"', '', upload_data, fixed = TRUE)
  upload_data = gsub('\n', '', upload_data, fixed = TRUE)
  upload_data = gsub('\t', '', upload_data, fixed = TRUE)
  upload_data = gsub('List\r',  ' ', upload_data, fixed = TRUE)
  upload_data = gsub('\r',  ' ', upload_data, fixed = TRUE)
  
  file.remove(tmp_file)
  
  pr_variables = paste0('{"data" : "',upload_data,'", "dataType" : "',
    data_type_,'", "requiredFields" : [], "surveyType" : "',survey_type,
    '", "transactionUuid" : "", "template" : "" }')
  
  pr_pbody = list(query = preview_query,
    variables = pr_variables,
    operationName = operation_name)
  # Query GQL API
  pr_res = httr::POST(url, headers, body = pr_pbody, encode='json')
  pr_content   = httr::content(pr_res, as = 'text')
  pr_content_json = fromJSON(pr_content, flatten = TRUE)
  
  return(pr_content_json$data$bulkPreview$matched)
}


#' @title Get presigned data
#'
#' @description Returns a uuid and presigned url to upload a csv into the AWS bucket
#'
#' @param file_path String full path to CSV file for preview
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param survey_type (optional) String 'bulk_sae' | 'bulk_mae' | 'bulk_hib' | 'bulk_mat'
#' @param branch (optional) String that defaults to 'prod' but can also be
#' 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
#'

get_presigned_data = function(
  project_id,
  token,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  # Get headers for token
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  bucket = get_project_file_bucket(branch = branch)
  
  content_type = 'text/plain'
  key = paste0(project_id, '/bulk-uploads')
  
  variables = paste0('{"bucket" : "',bucket,'", "key" : "',key,'", "contentType" : "',
    content_type,'", "asUuid" : "True"}')
  # GQL Query
  query = 'query RRs3FileServiceUploadFile
  ($bucket: String!, $key: String!, $contentType: String!, $asUuid: Boolean) {
  s3FileServiceUploadFile(bucket: $bucket, key: $key, contentType: $contentType,
  asUuid: $asUuid) {
  s3PresignedUrl
  transactionUuid
  success
  message
  }
  }'
  pbody = list(query = query, variables = variables,
    operationName = 'RRs3FileServiceUploadFile')
  # Query GQL API
  res = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  pre_content_json = fromJSON(content, flatten = TRUE)
  asUUid = pre_content_json$data$s3FileServiceUploadFile$transactionUuid
  presigned_url = pre_content_json$data$s3FileServiceUploadFile$s3PresignedUrl
  
  return (list(asUUid = asUUid, presigned_url = presigned_url))
}


#' @title Upload CSV to aws
#'
#' @description Upload a csv to a presigned url
#'
#' @param presigned_url String Output from get_presigned_data()
#' @param file_path String File path of csv to upload to NABat
#'
#' @export
#'

upload_csv = function(
  presigned_url,
  file_path
){
  content_type = 'text/plain'
  headers_put = httr::add_headers('Content-Type' = content_type)
  res_put = httr::PUT(presigned_url,
    body = upload_file(file_path, type = content_type),
    headers_put)
  return(res_put)
}



#' @title Process uploaded CSV
#'
#' @description
#' Process a csv and upload to your NABat Project
#'
#' @param user_id Numeric NABat user Id - from get_user_id_by_email()
#' @param project_id Numeric or String a project id
#' @param asUUid String asUUid - from get_presigned_data()
#' @param template Dataframe - from get_upload_file_preview()
#' @param file_path String full path to CSV file for preview
#' @param file_name String Name of file to be uploaded into NABat website
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param survey_type String Type of data to process: 'bulk_sae' | 'bulk_mae'
#' | 'bulk_hib' | 'bulk_mat' | 'bulk_ee'
#' @param branch (optional) String that defaults to 'prod' but can also be
#' 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
#'
process_uploaded_csv = function(
  user_id,
  project_id,
  asUUid,
  template,
  survey_type,
  token,
  file_path,
  file_name = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  # Get headers for token
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  # Set subtype
  sub_type = 'null'
  if (survey_type == 'bulk_hib'){
    sub_type = 9
    survey_type = 'bulk_cc'
  } else if (survey_type == 'bulk_mat'){
    sub_type = 10
    survey_type = 'bulk_cc'
  }
  
  operation_name = paste0('RR',survey_type,'CsvProcess')
  # GQL Query
  process_query = paste0('query ',operation_name,' (
    $userId: Int!
    $projectId: Int!
    $transactionUuid: String!
    $fileName: String!
    $type: String!
    $subType: Int
    $template: JSON!
    $requiredFields: JSON
  ) {
    startBulkCsvProcess(
    userId: $userId
    projectId: $projectId
    transactionUuid: $transactionUuid
    fileName: $fileName
    type: $type
    subType: $subType
    template: $template
    requiredFields: $requiredFields
    ) {
    success
    }
  }')

  template_df = template %>% dplyr::select(-c(options)) %>%
    dplyr::select(key, name, meta, type, required) %>%
    dplyr::mutate(missing = FALSE)
  template_json = jsonlite::toJSON(template_df)
  
  if (!is.null(file_name)){
    short_name = file_name
  } else {
    short_name = basename(file_path)
  }
  
  short_name = file_name
  proc_variables = paste0('{"userId" : ',user_id,',
    "projectId" : ',project_id,',
    "transactionUuid" : "',asUUid,'",
    "fileName" : "',short_name,'",
    "type" : "',survey_type,'",
    "template" : ',template_json,' ,
    "subType" : ',sub_type,',
    "requiredFields" : "{}" }')
  
  pro_pbody = list(query = process_query, variables = proc_variables,
    operationName = operation_name)
  # Query GQL API
  pro_res = httr::POST(url, headers, body = pro_pbody, encode='json')
  pro_content = httr::content(pro_res, as = 'text')
  content_json = fromJSON(pro_content, flatten = TRUE)
  return(pro_content)
}


#' @title Get Token Headers
#'
#' @description
#' Create and return correct headers for querying NABat
#' API. This function also refreshes your NABat GQL token
#'
#' @export
#'
get_token_headers = function(
  token,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){
  
  # Set URL based on branch if url is
  if (is.null(url)) url = get_gql_url(branch)
  
  # If running in docker
  if (docker){
    # If in AWS
    if(!is.null(aws_gql)){
      url = paste0(aws_alb, '/graphql')
      token = get_refresh_token(token, url = url, aws_gql = aws_gql,
        aws_alb = aws_alb, docker = docker)
      headers = httr::add_headers(host = aws_gql,
        Authorization = paste0("Bearer ", token$access_token))
      # If not in AWS
    }else {
      token = get_refresh_token(token, url = url)
      headers = httr::add_headers(Authorization = paste0("Bearer ",
        token$access_token))
    }
    # If not running in docker
  } else{
    token = get_refresh_token(token, url = url)
    headers = httr::add_headers(Authorization = paste0('Bearer ',
      token$access_token))
  }
  return (list(token = token, headers = headers, url = url))
}


#' @title Get GQL url
#'
#' @description Combine the spatial information (states and counties)
#' with the detector info and species detected
#'
#' @export
#'
get_gql_url =  function(
  branch = 'prod'){
  if (branch == 'prod'){
    url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
  } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
    url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
  }
  return(url)
}



#' @title Get Project file bucket for AWS
#'
#' @description Returns the name of the AWS bucket based on beta/prod
#'
#' @param branch String Branch to use either 'beta' or 'prod'
#'
#' @export
#'
get_project_file_bucket = function(
  branch = 'prod'){
  if (branch == 'prod'){
    bucket = 'nabat-prod-project-files'
  } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
    bucket = 'nabat-beta-project-files'
  }
  return(bucket)
}

#######################################testing#############
#' @title Search NABat Projects
#'
#' @description
#' Returns all projects that the user has permissions to view
#'
#' @param token List token created from get_nabat_gql_token()
#' or get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but
#'  can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker
#' container or not
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' project_df = get_projects(token)
#' }
#'
#' @export
#'
get_projects = function(
  token,
  branch ='prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker=FALSE){
  
  # Get headers for token
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  # Sample frame lookup
  sample_frame_df = data.frame(ids = c(12,14,15,19,20,21),
                               sample_frame_short = c('Mexico', 'Continental US', 'Hawaii',
                                                      'Canada', 'Alaska', 'Puerto Rico'),
                               sample_frame_description = c('Mexico 10x10km Grid',
                                                            'Conus (Continental US) 10x10km Grid', 'Hawaii 5x5km Grid',
                                                            'Canada 10x10km Grid', 'Alaska 10x10km Grid', 'Puerto Rico 5x5km Grid'))
  
  # GQL Query
  query = 'query RRallProjects{allProjects{
                       nodes{
                          id
                          projectName
                          projectKey
                          description
                          sampleFrameId
                          organizationByOrganizationId{
                          name
                          address
                          city
                          stateProvince
                          postalCode
                          }
                        }
                        }
          }'
  pbody = list(query = query, operationName = 'RRallProjects')
  # Post to nabat GQL
  res       = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  proj_json = fromJSON(content, flatten = TRUE)
  proj_df   = rename_project_df(as.data.frame(proj_json,
                                              stringsAsFactors = FALSE)) %>%
    left_join(sample_frame_df, by = c('sample_frame_id' = 'ids'))
  

  
  # Return dataframe of projects
  return (proj_df)
}
#############################testing####################

rename_project_df = function(project_df){
  #' Return the project dataframe renamed with appropriate field headers
  #'
  #' @param project_df Dataframe output from get_projects()
  #'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.id'] =
    'project_id'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.projectName'] =
    'project_name'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.projectKey'] =
    'project_key'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.description'] =
    'project_description'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.mrOwnerEmail'] =
    'owner_email'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.sampleFrameId'] =
    'sample_frame_id'
  names(project_df)[names(project_df) ==
                      'data.allProjects.nodes.organizationByOrganizationId.name'] =
    'organization'
  names(project_df)[names(project_df) ==
                      'data.allProjects.nodes.organizationByOrganizationId.address'] =
    'organization_address'
  names(project_df)[names(project_df) ==
                      'data.allProjects.nodes.organizationByOrganizationId.city'] =
    'organization_city'
  names(project_df)[names(project_df) ==
                      'data.allProjects.nodes.organizationByOrganizationId.stateProvince'] =
    'organization_state_province'
  names(project_df)[names(project_df) ==
                      'data.allProjects.nodes.organizationByOrganizationId.postalCode'] =
    'organization_postal_code'
  
  row.names(project_df) = c()
  return (project_df)
}