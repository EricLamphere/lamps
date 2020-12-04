### Title:    Gmail Helper Functions
### Author:   Eric Lamphere (ericjlamphere@gmail.com)
### Time:     2020-03-21 13:41:55

##########################################################-
# FUNCTIONS ####
##########################################################-

# TODO: finish this function
getGmailAttachments <- function(search = NULL, output_dir = getwd(), user_id = "me", # REQUIRED
                                #TODO: file_types = c(".csv", ".xlsx"),
                                num_results = NULL, label_ids = NULL, include_spam_trash = FALSE, page_token = NULL # NOT REQUIRED
){
  # Prepare authentication with gmail api
  # TODO: gmailr::use_secret_file() # CHANGE THIS PART

  # check: gmail_ids is specified
  if(is.null(search)) stop("please specify the gmail search term used in the query. Same format as gmail search box: https://support.google.com/mail/answer/7190?hl=en")
  # check: output_dir exists
  if(!dir.exists(output_dir)) stop("File path '" %% output_dir %% "' does not exists")
  # search for the messages and get the IDs
  base::message("Searching for gmails")
  search <- paste(search, "has:attachment")
  msgs <- gmailr::messages(search = search, user_id = user_id, num_results = num_results, label_ids = label_ids, include_spam_trash = include_spam_trash, page_token = page_token)

  # grab the IDs from the msgs df
  ids <- gmailr::id(msgs)

  # save the attachments
  #TODO: only download certain file types
  filepaths <- lapply(ids, function(x) gmailr::save_attachments(gmailr::message(x), path = output_dir, user_id = user_id))

  return(filepaths)
}
