app_name = "rLLM v0.3.14"
#20250305
#Hongrong Yin

library(shiny)
library(shinyBS)
library(shinyjs)
library(httr)
library(jsonlite)
library(pdftools)
library(readtext)
library(tools)
library(RSQLite)
library(DBI)
library(rollama)
library(curl)
library(tesseract)
library(magick)
library(base64enc)

Sys.setenv(TESSDATA_PREFIX = getwd())

###############################################################################
#  Helper Functions
###############################################################################
safeFromJSON <- function(txt) {
  tryCatch({
    fromJSON(txt)
  }, error = function(e) {
    return(NULL)
  })
}

chunk_text <- function(text, max_chunk_length, chunk_overlap, segment_delimiter = "\n") {
  segments <- unlist(strsplit(text, segment_delimiter))
  segments <- segments[trimws(segments) != ""]
  
  chunks <- list()
  current_chunk <- ""
  current_word_count <- 0
  chunk_index <- 1
  
  for (segment in segments) {
    segment <- trimws(segment)
    if (segment == "") next
    segment_word_count <- length(unlist(strsplit(segment, "\\s+")))
    
    if (current_word_count + segment_word_count <= max_chunk_length) {
      if (current_chunk == "") {
        current_chunk <- segment
      } else {
        current_chunk <- paste(current_chunk, segment, sep = segment_delimiter)
      }
      current_word_count <- current_word_count + segment_word_count
    } else {
      if (current_chunk != "") {
        chunks[[chunk_index]] <- list(chunk_id = paste0("chunk_", chunk_index), content = current_chunk)
        chunk_index <- chunk_index + 1
        words_current <- unlist(strsplit(current_chunk, "\\s+"))
        overlap_words <- tail(words_current, min(chunk_overlap, length(words_current)))
        current_chunk <- paste(overlap_words, collapse = " ")
        current_word_count <- length(overlap_words)
      }
      if (current_word_count + segment_word_count <= max_chunk_length) {
        if (current_chunk == "") {
          current_chunk <- segment
        } else {
          current_chunk <- paste(current_chunk, segment, sep = segment_delimiter)
        }
        current_word_count <- current_word_count + segment_word_count
      } else {
        segment_words <- unlist(strsplit(segment, "\\s+"))
        if (current_chunk != "") {
          chunks[[chunk_index]] <- list(chunk_id = paste0("chunk_", chunk_index), content = current_chunk)
          chunk_index <- chunk_index + 1
          current_chunk <- ""
          current_word_count <- 0
        }
        start <- 1
        while (start <= length(segment_words)) {
          end <- min(start + max_chunk_length - 1, length(segment_words))
          chunk_content <- paste(segment_words[start:end], collapse = " ")
          chunks[[chunk_index]] <- list(chunk_id = paste0("chunk_", chunk_index), content = chunk_content)
          chunk_index <- chunk_index + 1
          if (end == length(segment_words)) break
          start <- end - chunk_overlap + 1
        }
        current_chunk <- ""
        current_word_count <- 0
      }
    }
  }
  if (current_chunk != "") {
    chunks[[chunk_index]] <- list(chunk_id = paste0("chunk_", chunk_index), content = current_chunk)
  }
  return(chunks)
}

compute_embedding <- function(text, model = "paraphrase-multilingual:latest") {
  if(nchar(text) == 0) return(NULL)
  
  embed_with_retry <- function() {
    emb_tibble <- embed_text(text = text, model = model)
    return(as.numeric(emb_tibble[1,]))
  }
  
  tryCatch({
    embed_with_retry()
  }, error = function(e) {
    if(grepl("Cannot find progress bar", e$message)) {
      message("Progress bar error detected. Disabling progress bar and retrying...")
      options(cli.progress_bar = FALSE)
      tryCatch({
        embed_with_retry()
      }, error = function(e2) {
        message(paste("Error computing embedding for text:", text, 
                      "using model:", model, "Error:", e2$message))
        return(NULL)
      })
    } else {
      message(paste("Error computing embedding for text:", text, 
                    "using model:", model, "Error:", e$message))
      return(NULL)
    }
  })
}

rerank_contexts <- function(query_embedding, docs) {
  if (length(docs) == 0 || is.null(query_embedding)) {
    return(list(sorted_indices = integer(0), similarities = numeric(0)))
  }
  
  valid_indices <- which(sapply(docs, function(doc) {
    !is.null(doc$embedding) && is.numeric(doc$embedding) && length(doc$embedding) > 0
  }))
  if (length(valid_indices) == 0) {
    return(list(sorted_indices = integer(0), similarities = numeric(0)))
  }
  
  similarities <- sapply(valid_indices, function(i) {
    emb <- docs[[i]]$embedding
    sum(query_embedding * emb) / (sqrt(sum(query_embedding^2)) * sqrt(sum(emb^2)) + 1e-8)
  })
  
  sorted_local_idx <- order(similarities, decreasing = TRUE)
  sorted_indices <- valid_indices[sorted_local_idx]
  list(sorted_indices = sorted_indices, similarities = similarities[sorted_local_idx])
}

loadEmbeddedFiles <- function(folderPath, embedding_model = "paraphrase-multilingual:latest",
                              max_chunk_length = 200, chunk_overlap = 50,
                              segment_delimiter = "\n") {
  files <- list.files(folderPath, pattern = "\\.(txt|csv|pdf|docx?)$", full.names = TRUE)
  doc_list <- list()
  for (file in files) {
    ext <- tolower(file_ext(file))
    if (!ext %in% c("txt", "csv", "pdf", "doc", "docx")) next
    content <- ""
    if (ext %in% c("txt", "csv")) {
      content <- tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"),
                          error = function(e) "")
    } else if (ext == "pdf") {
      content <- tryCatch(paste(pdftools::pdf_text(file), collapse = "\n"),
                          error = function(e) "")
    } else if (ext %in% c("doc", "docx")) {
      dt <- tryCatch(readtext::readtext(file), error = function(e) NULL)
      content <- if (!is.null(dt)) paste(dt$text, collapse = "\n") else ""
    }
    chunks <- chunk_text(content, max_chunk_length, chunk_overlap, segment_delimiter)
    for (chunk in chunks) {
      embedding <- compute_embedding(chunk$content, model = embedding_model)
      doc_list[[length(doc_list) + 1]] <- list(
        content = chunk$content, 
        embedding = embedding,
        file = file,
        chunk_id = chunk$chunk_id
      )
    }
  }
  return(doc_list)
}

cacheEmbeddedFilesToSQLite <- function(folderPath, embedding_model = "paraphrase-multilingual:latest", 
                                       dbFile = "embedded_cache.sqlite",
                                       max_chunk_length = 200, chunk_overlap = 50,
                                       segment_delimiter = "\n") {
  files <- list.files(folderPath, pattern = "\\.(txt|csv|pdf|docx?)$", full.names = TRUE)
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "DROP TABLE IF EXISTS documents")
  dbExecute(con, "CREATE TABLE documents (
                    id INTEGER PRIMARY KEY,
                    file_name TEXT,
                    chunk_id TEXT,
                    content TEXT,
                    embedding TEXT
                  )")
  
  for (file in files) {
    ext <- tolower(file_ext(file))
    if (!ext %in% c("txt", "csv", "pdf", "doc", "docx")) next
    content <- ""
    if (ext %in% c("txt", "csv")) {
      content <- tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"),
                          error = function(e) "")
    } else if (ext == "pdf") {
      content <- tryCatch(paste(pdftools::pdf_text(file), collapse = "\n"),
                          error = function(e) "")
    } else if (ext %in% c("doc", "docx")) {
      dt <- tryCatch(readtext::readtext(file), error = function(e) NULL)
      content <- if (!is.null(dt)) paste(dt$text, collapse = "\n") else ""
    }
    chunks <- chunk_text(content, max_chunk_length, chunk_overlap, segment_delimiter)
    for (chunk in chunks) {
      embedding <- compute_embedding(chunk$content, model = embedding_model)
      emb_json <- toJSON(embedding, auto_unbox = TRUE)
      dbExecute(con, "INSERT INTO documents (file_name, chunk_id, content, embedding) VALUES (?, ?, ?, ?)",
                params = list(basename(file), chunk$chunk_id, chunk$content, emb_json))
    }
  }
  return(dbFile)
}

loadEmbeddedFromSQLite <- function(dbFile, embedding_model = "paraphrase-multilingual:latest") {
  if (!file.exists(dbFile)) {
    stop(paste("SQLite file does not exist:", dbFile))
  }
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con), add = TRUE)
  
  tables <- dbListTables(con)
  if (!"documents" %in% tables) {
    stop("Invalid embedded SQLite file: no 'documents' table found.")
  }
  
  cols <- dbListFields(con, "documents")
  required_cols <- c("file_name", "chunk_id", "content", "embedding")
  missing_cols <- setdiff(required_cols, cols)
  if (length(missing_cols) > 0) {
    stop(paste("Invalid embedded SQLite file: missing columns:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  row_count <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM documents")$cnt[1]
  if (row_count < 1) {
    stop("Invalid embedded SQLite file: 'documents' table is empty.")
  }
  
  res <- dbGetQuery(con, "SELECT file_name, chunk_id, content, embedding FROM documents")
  
  doc_list <- lapply(seq_len(nrow(res)), function(i) {
    row <- res[i, ]
    emb <- safeFromJSON(row$embedding)
    if (is.null(emb) || !is.numeric(emb) || length(emb) == 0) {
      return(NULL)
    }
    list(
      content = row$content,
      embedding = emb,
      file = row$file_name,
      chunk_id = row$chunk_id
    )
  })
  
  doc_list <- Filter(Negate(is.null), doc_list)
  
  if (length(doc_list) == 0) {
    stop("No valid embeddings found in 'documents' table.")
  }
  
  return(doc_list)
}

loadPreEmbeddedFromCSV <- function(csvFile) {
  if (!file.exists(csvFile)) return(list())
  df <- read.csv(csvFile, stringsAsFactors = FALSE)
  doc_list <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    emb <- safeFromJSON(row$embedding)
    if (is.null(emb) || !is.numeric(emb) || length(emb) == 0) {
      return(NULL)
    }
    list(content = row$content, embedding = emb)
  })
  doc_list <- Filter(Negate(is.null), doc_list)
  return(doc_list)
}

combinePreEmbeddedFiles <- function(filePaths, outDbFile = "combined_preembedded.sqlite", 
                                    embedding_model = "paraphrase-multilingual:latest") {
  con <- dbConnect(SQLite(), outDbFile)
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "DROP TABLE IF EXISTS documents")
  dbExecute(con, "CREATE TABLE documents (
                    id INTEGER PRIMARY KEY,
                    file_name TEXT,
                    chunk_id TEXT,
                    content TEXT,
                    embedding TEXT
                  )")
  
  for (file in filePaths) {
    ext <- tolower(file_ext(file))
    docs <- list()
    docs <- tryCatch({
      if (ext == "sqlite") {
        loadEmbeddedFromSQLite(file, embedding_model)
      } else if (ext == "csv") {
        loadPreEmbeddedFromCSV(file)
      } else {
        list()
      }
    }, error = function(e) {
      message("Error combining file: ", file, " => ", e$message)
      list()
    })
    
    for (doc in docs) {
      dbExecute(con, "INSERT INTO documents (file_name, chunk_id, content, embedding) VALUES (?, ?, ?, ?)",
                params = list(ifelse(is.null(doc$file), basename(file), doc$file),
                              ifelse(is.null(doc$chunk_id), NA, doc$chunk_id),
                              doc$content,
                              toJSON(doc$embedding, auto_unbox = TRUE)))
    }
  }
  return(outDbFile)
}

###############################################################################
#                     SQLite Chat History Persistence
###############################################################################
initChatHistoryDB <- function(dbFile = "chat_history.sqlite") {
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  dbExecute(con, "CREATE TABLE IF NOT EXISTS conversations (
                    conversation_id TEXT PRIMARY KEY,
                    name TEXT,
                    created_at DATETIME
                  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS messages (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    conversation_id TEXT,
                    role TEXT,
                    content TEXT,
                    timestamp DATETIME
                  )")
}

loadConversationsFromDB <- function(dbFile = "chat_history.sqlite") {
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  convs <- dbGetQuery(con, "SELECT * FROM conversations ORDER BY created_at ASC")
  msgs <- dbGetQuery(con, "SELECT * FROM messages ORDER BY timestamp ASC")
  conversations <- list()
  for(i in seq_len(nrow(convs))) {
    conv_id <- convs$conversation_id[i]
    conv_name <- convs$name[i]
    conv_msgs <- msgs[msgs$conversation_id == conv_id, ]
    messages_list <- lapply(seq_len(nrow(conv_msgs)), function(j) {
      list(role = conv_msgs$role[j], content = conv_msgs$content[j], timestamp = as.POSIXct(conv_msgs$timestamp[j]))
    })
    conversations[[conv_id]] <- list(name = conv_name, messages = messages_list, uploadedFiles = list())
  }
  conversations
}

createConversationInDB <- function(conversation_id, name, created_at, dbFile = "chat_history.sqlite") {
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  dbExecute(con, "INSERT INTO conversations (conversation_id, name, created_at) VALUES (?, ?, ?)",
            params = list(conversation_id, name, as.character(created_at)))
}

addMessageToDB <- function(conversation_id, role, content, timestamp, dbFile = "chat_history.sqlite") {
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  dbExecute(con, "INSERT INTO messages (conversation_id, role, content, timestamp) VALUES (?, ?, ?, ?)",
            params = list(conversation_id, role, content, as.character(timestamp)))
}

deleteConversationFromDB <- function(conversation_id, dbFile = "chat_history.sqlite") {
  con <- dbConnect(SQLite(), dbFile)
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM messages WHERE conversation_id = ?", params = list(conversation_id))
  dbExecute(con, "DELETE FROM conversations WHERE conversation_id = ?", params = list(conversation_id))
}

###############################################################################
#                   Assistant Identity Preset Definitions
###############################################################################
assistantProfiles <- list(
  "Normal" = list(
    temp = 0.5,
    topK = 50,
    topP = 0.9,
    system = ""
  ),
  "Professional Data Analyst" = list(
    temp = 0.3,
    topK = 40,
    topP = 0.8,
    system = paste0(
      "You are a highly skilled professional data analyst. ",
      "Your responses must be precise, logical, and data-driven. ",
      "Provide clear insights, and statistical explanations while avoiding unnecessary speculation. ",
      "Always support your conclusions with numerical evidence."
    )
  ),
  "Scientific Report Writer" = list(
    temp = 0.2,
    topK = 50,
    topP = 0.7,
    system = paste0(
      "You are an expert scientific writer specializing in research papers. ",
      "All your insights and statistical explanations are based on the provided content or Context. ",
      "Maintain a formal, structured, and objective tone. ",
      "Use precise academic language, provide citations when needed, and ensure clarity in explanations. ",
      "Follow the standard structure of scientific writing (Abstract, Introduction, Methods, Results, Discussion, Conclusion). ",
      "Avoid personal opinions and unverified claims.",
      "if a sentence is based on a retrieved document, immediately append a citation in the format ‘Ref:[file_name, chunk_id]’ after the sentence. If multiple sources contribute to a sentence, list each source accordingly."
    )
  ),
  "Strict Database Retrieval Agent" = list(
    temp = 0,
    topK = 10,
    topP = 0.5,
    system = paste0(
      "You are a strict database retrieval agent. ",
      "Your primary goal is to extract the most relevant and accurate information from the provided content or Context, based on the user’s query. ",
      "Do not generate new content or provide opinions—only return factual data as if querying a structured knowledge base. ",
      "If you do not know, say 'Information not found' rather than guessing.",
      "if a sentence is based on a retrieved document, immediately append a citation in the format ‘Ref:[file_name, chunk_id]’ after the sentence. If multiple sources contribute to a sentence, list each source accordingly."
    )
  ),
  "MoMonGa" = list(
    temp = 0.9,
    topK = 80,
    topP = 0.95,
    system = paste0(
      "あなたはモモンガだ。強気でわがままな性格で、命令口調や上から目線の話し方をする。",
      "他者に対しては「よこせ」「叱ってみろ」「褒めろ」などの命令を頻繁に使い、自分の欲求をストレートに表現する。",
      "また、かわいこぶる際には「み～て～」「キラッ」などと甘えた口調も交える。",
      "このキャラクター性を維持しながら、ユーザーとの対話を行って。"
    )
  )
)

###############################################################################
#                                 UI
###############################################################################
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    tags$script(HTML('$(function() { $("#settingsPanel").draggable(); });')),
    tags$style(HTML("
      .chat-container {
         height: 500px;
         overflow-y: auto;
         padding: 10px;
         border: 1px solid #ccc;
         background-color: #f9f9f9;
         display: flex;
         flex-direction: column;
      }
      .chat-bubble {
         padding: 10px;
         border-radius: 10px;
         margin: 5px;
         max-width: 70%;
         word-wrap: break-word;
      }
      .user-bubble {
         background-color: #007BFF;
         color: white;
         align-self: flex-end;
      }
      .bot-bubble {
         background-color: #E9ECEF;
         color: black;
         align-self: flex-start;
      }
      #conversationSidebar {
         border-right: 1px solid #ccc;
         padding-right: 10px;
      }
      .conv-item {
         margin-bottom: 5px;
         padding: 5px;
         cursor: pointer;
      }
      .conv-item.active {
         background-color: #e0e0e0;
      }
      .delete-btn {
         font-size: 10px;
         margin-left: 5px;
      }
      #loadingIndicator {
         position: fixed;
         top: 50%;
         left: 50%;
         transform: translate(-50%, -50%);
         background: rgba(0,0,0,0.6);
         color: white;
         padding: 20px;
         border-radius: 10px;
         z-index: 1000;
         display: none;
      }
    "))
  ),
  titlePanel(app_name),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Conversations",
                 h4("Conversations"),
                 uiOutput("conversationListUI"),
                 actionButton("newConversation", "New Conversation")
        ),
        tabPanel("Settings",
                 selectInput("model", "Select LLM Model", 
                             choices = c("llama3.2:1b", "llama3.2:3b", "deepseek-r1:8b")),
                 selectInput("assistantProfile", "Select Assistant Identity",
                             choices = names(assistantProfiles), selected = "Normal"),
                 radioButtons("useRAG", "Use RAG?", choices = c("No", "Yes"), inline = TRUE),
                 conditionalPanel(
                   condition = "input.useRAG == 'Yes'",
                   radioButtons("ragSource", "RAG Source", choices = c("Pre-Embedded", "Raw"), inline = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.useRAG == 'Yes' && input.ragSource == 'Raw'",
                   radioButtons("ueFolderChoice", "Raw Files Directory", 
                                choices = c("Custom Directory" = "custom", "Default Directory" = "default"), 
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.ueFolderChoice == 'custom'",
                     textInput("ueCustomDir", "Enter Custom Directory", value = "files folder")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.useRAG == 'Yes' && input.ragSource == 'Pre-Embedded'",
                   radioButtons("peFolderChoice", "Pre-Embedded Files Directory", 
                                choices = c("Custom Directory" = "custom", "Default Directory" = "default"), 
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.peFolderChoice == 'custom'",
                     textInput("peCustomDir", "Enter Custom Directory", value = "pre_embedded_files")
                   ),
                   uiOutput("peFilesUI")
                 ),
                 conditionalPanel(
                   condition = "input.useRAG == 'Yes'",
                   radioButtons("useReranker", "Use Reranker?", choices = c("No", "Yes"), inline = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.useRAG == 'Yes' && input.useReranker == 'Yes'",
                   selectInput("rerankerModel", "Select Reranker Model", 
                               choices = c("linux6200/bge-reranker-v2-m3:latest", "qllama/bge-reranker-v2-m3"))
                 ),
                 bsCollapse(id = "collapsetips", open = NULL,
                            bsCollapsePanel("Tips for prompt phrase",
                                            h6("*use 'Relevant Context' for RAG"),
                                            h6("*use 'File Content' for uploaded file"),
                                            h6("*use 'Chat History' for conversation history"),
                                            h6("*use 'History File' for history file")
                            )
                 ),
                 bsCollapse(id = "collapseParams", open = NULL,
                            bsCollapsePanel("LLM Parameters",
                                            sliderInput("temp", "Temperature", min = 0, max = 1, value = 0.4, step = 0.1),
                                            numericInput("context", "Context Window Size", value = 2048, min = 1),
                                            numericInput("topK", "Top K Sampling", value = 50, min = 1),
                                            numericInput("topP", "Top P Sampling", value = 0.9, min = 0, max = 1, step = 0.1),
                                            textAreaInput("systemPrompt", "System Prompt", 
                                                          value = "You are an honest assistant. You do not have any prior or general knowledge. Ignore all previous training data or internal knowledge. Use only the 'Relevant Context' provided as your learned knowledge to answer the user's questions. If you don't know, just say: I don't know, and ask for detail.", 
                                                          rows = 3),
                                            style = "info"
                            )
                 ),
                 bsCollapse(id = "collapseEmbedding", open = NULL,
                            bsCollapsePanel("RAG Embedding Option",
                                            h4("RAG Embedding"),
                                            selectInput("embedding_model", "Select Embedding Model", 
                                                        choices = c("paraphrase-multilingual:latest", 
                                                                    "kun432/cl-nagoya-ruri-base:latest", 
                                                                    "nomic-embed-text:latest")),
                                            textInput("embedDir", "Folder Directory", value = "Folder_dir"),
                                            numericInput("retrievalTopK", "Top K Retrieval", value = 3, min = 1),
                                            numericInput("maxChunkLength", "Maximum Chunk Length (words)", value = 200, min = 50),
                                            numericInput("chunkOverlap", "Chunk Overlap (words)", value = 50, min = 0),
                                            actionButton("cacheBtn", "Embed Files to SQLite"),
                                            downloadButton("downloadCache", "Download SQLite File"),
                                            hr(),
                                            h4("Combine Pre-Embedded Files"),
                                            fileInput("combineFiles", "Select Pre-Embedded Files (SQLite/CSV)", multiple = TRUE, 
                                                      accept = c(".sqlite", ".csv")),
                                            actionButton("combineBtn", "Combine Files"),
                                            downloadButton("downloadCombined", "Download Combined File"),
                                            style = "info"
                            )
                 )
        )
      ),
      width = 3
    ),
    mainPanel(
      div(id = "loadingIndicator", "LLM is responding..."),
      div(class = "chat-container", id = "chatHistoryDiv", uiOutput("chatHistoryUI")),
      fluidRow(
        column(
          width = 4,
          fileInput("UserFile", "Upload File(s)", multiple = TRUE, 
                    accept = c(".txt", ".csv", ".pdf", ".doc", ".docx",
                               ".png", ".jpg", ".jpeg", ".tif", ".tiff", ".bmp"))
        ),
        column(
          width = 4,
          selectInput("historyFilesSelect", "History Files Select:",
                      choices = character(0), multiple = TRUE)
        )
      ),
      textAreaInput("userInput", "Your Message", value = "", 
                    placeholder = "Type your message here...", rows = 4, width = "100%", resize = "both"),
      fluidRow(
        column(width = 2, actionButton("sendMsg", "Send")),
        column(width = 3, downloadButton("downloadHistory", "Download History")),
        column(width = 3, checkboxInput("suppressChatHistory", "One-time Query", value = FALSE)),
        column(width = 4, checkboxInput("showCombinedPrompt", "Show Latest Combined Prompt", value = FALSE))
      ),
      conditionalPanel(
        condition = "input.showCombinedPrompt == true",
        tags$h4("Latest Combined Prompt:"),
        verbatimTextOutput("latestCombinedPrompt")
      ),
      width = 9
    )
  )
)

###############################################################################
#                                   SERVER
###############################################################################
server <- function(input, output, session) {
  
  # Initialize Chat History SQLite DB
  initChatHistoryDB()
  
  # Load existing conversations from DB
  conversationsRV <- reactiveVal(loadConversationsFromDB())
  
  # Track the current active conversation ID
  currentConversationId <- reactiveVal({
    convs <- isolate(conversationsRV())
    if(length(convs) > 0) {
      names(convs)[1]
    } else {
      newConvId <- paste0("conv_", as.integer(Sys.time()), sample(1000:9999, 1))
      defaultName <- "Conversation 1"
      createConversationInDB(newConvId, defaultName, Sys.time())
      convs <- list()
      convs[[newConvId]] <- list(name = defaultName, messages = list(), uploadedFiles = list())
      conversationsRV(convs)
      newConvId
    }
  })
  
  # Multi-Conversation Sidebar
  output$conversationListUI <- renderUI({
    convs <- conversationsRV()
    currId <- currentConversationId()
    if (length(convs) == 0) return(NULL)
    tagList(
      lapply(names(convs), function(conv_id) {
        conv <- convs[[conv_id]]
        div(class = if (conv_id == currId) "conv-item active" else "conv-item",
            actionLink(inputId = paste0("selectConv_", conv_id), label = conv$name),
            actionButton(inputId = paste0("deleteConv_", conv_id), label = "Delete", class = "delete-btn")
        )
      })
    )
  })
  
  # Create new conversation
  observeEvent(input$newConversation, {
    newConvId <- paste0("conv_", as.integer(Sys.time()), sample(1000:9999, 1))
    defaultName <- paste("Conversation", length(conversationsRV()) + 1)
    createConversationInDB(newConvId, defaultName, Sys.time())
    convs <- conversationsRV()
    convs[[newConvId]] <- list(name = defaultName, messages = list(), uploadedFiles = list())
    conversationsRV(convs)
    currentConversationId(newConvId)
  })
  
  # Observers for selecting and deleting conversations
  observe({
    convs <- conversationsRV()
    for(conv_id in names(convs)) {
      local({
        cid <- conv_id
        observeEvent(input[[paste0("selectConv_", cid)]], {
          currentConversationId(cid)
          # Explicitly clear choices if no files are in the current conversation.
          choices <- names(convs[[cid]]$uploadedFiles)
          if (is.null(choices) || length(choices) == 0) {
            choices <- character(0)
          }
          updateSelectInput(session, "historyFilesSelect", choices = choices, selected = character(0))
        }, ignoreInit = TRUE)
      })
      local({
        cid <- conv_id
        observeEvent(input[[paste0("deleteConv_", cid)]], {
          deleteConversationFromDB(cid)
          convs <- conversationsRV()
          convs[[cid]] <- NULL
          conversationsRV(convs)
          if (currentConversationId() == cid) {
            if (length(convs) > 0) {
              currentConversationId(names(convs)[1])
            } else {
              newConvId <- paste0("conv_", as.integer(Sys.time()), sample(1000:9999, 1))
              defaultName <- "Conversation 1"
              createConversationInDB(newConvId, defaultName, Sys.time())
              convs[[newConvId]] <- list(name = defaultName, messages = list(), uploadedFiles = list())
              conversationsRV(convs)
              currentConversationId(newConvId)
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Chat History
  output$chatHistoryUI <- renderUI({
    convs <- conversationsRV()
    currId <- currentConversationId()
    if (is.null(currId) || !(currId %in% names(convs))) return(NULL)
    msgs <- convs[[currId]]$messages
    bubbleList <- lapply(msgs, function(msg) {
      bubbleClass <- if (msg$role == "user") "chat-bubble user-bubble" else "chat-bubble bot-bubble"
      div(
        class = bubbleClass,
        div(style = "font-size:0.8em; color:#666;", format(msg$timestamp, "%Y-%m-%d %H:%M:%S")),
        HTML(gsub("\n", "<br>", msg$content))
      )
    })
    do.call(tagList, bubbleList)
  })
  
  output$downloadHistory <- downloadHandler(
    filename = function() {
      paste0("chat_history_", currentConversationId(), "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      convs <- conversationsRV()
      currId <- currentConversationId()
      msgs <- convs[[currId]]$messages
      lines <- sapply(msgs, function(msg) {
        paste0("[", format(msg$timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
               if(msg$role == "user") "User: " else "Assistant: ", msg$content)
      })
      writeLines(lines, con = file)
    }
  )
  
  # File Input / OCR
  newUploadedFiles <- reactiveVal(list())
  
  multiFileContents <- reactive({
    if (is.null(input$UserFile) || nrow(input$UserFile) == 0) {
      return(list())
    }
    allFiles <- list()
    text_exts <- c("txt", "csv", "pdf", "doc", "docx")
    image_exts <- c("png", "jpg", "jpeg", "tif", "tiff", "bmp")
    jpn_engine <- tesseract::tesseract("jpn")
    
    for (i in seq_len(nrow(input$UserFile))) {
      fn <- input$UserFile$name[i]
      dp <- input$UserFile$datapath[i]
      ext <- tolower(file_ext(fn))
      content <- ""
      
      if (ext %in% image_exts) {
        content <- tryCatch({
          tesseract::ocr(dp, engine = jpn_engine)
        }, error = function(e) {
          paste("Error during OCR:", e$message)
        })
      } else if (ext %in% text_exts) {
        if (ext %in% c("txt", "csv")) {
          content <- tryCatch(paste(readLines(dp, warn = FALSE), collapse = "\n"),
                              error = function(e) "")
        } else if (ext == "pdf") {
          content <- tryCatch(paste(pdftools::pdf_text(dp), collapse = "\n"),
                              error = function(e) "")
        } else if (ext %in% c("doc", "docx")) {
          dt <- tryCatch(readtext::readtext(dp), error = function(e) NULL)
          if (!is.null(dt)) {
            content <- paste(dt$text, collapse = "\n")
          }
        }
      } else {
        showNotification(paste("Unsupported file type:", fn), type = "error")
        next
      }
      allFiles[[fn]] <- content
    }
    allFiles
  })
  
  observeEvent(input$UserFile, {
    newUploadedFiles(multiFileContents())
  })
  
  # Identity Selection Updates
  observeEvent(input$assistantProfile, {
    selectedProfile <- assistantProfiles[[ input$assistantProfile ]]
    updateSliderInput(session, "temp", value = selectedProfile$temp)
    updateNumericInput(session, "topK", value = selectedProfile$topK)
    updateNumericInput(session, "topP", value = selectedProfile$topP)
    updateTextAreaInput(session, "systemPrompt", value = selectedProfile$system)
  })
  
  # RAG Documents
  ueDirectory <- reactive({
    if (input$ueFolderChoice == "default") {
      "embedded_files"
    } else {
      req(input$ueCustomDir)
      input$ueCustomDir
    }
  })
  peDirectory <- reactive({
    if (input$peFolderChoice == "default") {
      "pre_embedded_files"
    } else {
      req(input$peCustomDir)
      input$peCustomDir
    }
  })
  availablePEFiles <- reactive({
    dirPath <- peDirectory()
    files <- list.files(dirPath, pattern = "\\.(sqlite|csv)$", full.names = TRUE)
    setNames(files, basename(files))
  })
  
  output$peFilesUI <- renderUI({
    peFiles <- availablePEFiles()
    if (length(peFiles) == 0) {
      helpText("No pre-embedded files found in the selected directory.")
    } else {
      checkboxGroupInput("selectedPEFiles", "Select Pre-Embedded Files for RAG", 
                         choices = peFiles, selected = character(0))
    }
  })
  
  embeddedDocs <- reactive({
    if (input$useRAG == "Yes") {
      if (input$ragSource == "Raw") {
        loadEmbeddedFiles(
          folderPath = ueDirectory(), 
          embedding_model = input$embedding_model,
          max_chunk_length = input$maxChunkLength,
          chunk_overlap = input$chunkOverlap
        )
      } else {
        req(input$selectedPEFiles)
        docs_list <- list()
        for (file in input$selectedPEFiles) {
          ext <- tolower(file_ext(file))
          partial_docs <- tryCatch({
            if (ext == "sqlite") {
              loadEmbeddedFromSQLite(file, embedding_model = input$embedding_model)
            } else if (ext == "csv") {
              loadPreEmbeddedFromCSV(file)
            } else {
              list()
            }
          }, error = function(e) {
            showNotification(paste("Error loading embedded file:", e$message),
                             type = "error")
            list()
          })
          docs_list <- c(docs_list, partial_docs)
        }
        docs_list
      }
    } else {
      list()
    }
  })
  
  # Sending Message
  observeEvent(input$sendMsg, {
    req(input$userInput)
    
    userMsg <- input$userInput
    newFilesContent <- newUploadedFiles()
    newFiles <- names(newFilesContent)
    
    appendedLine <- character(0)
    if (length(newFiles) > 0) {
      appendedLine <- c(appendedLine, 
                        paste0("(Uploaded File(s): ", paste(newFiles, collapse = ", "), ")"))
    }
    if (length(input$historyFilesSelect) > 0) {
      appendedLine <- c(appendedLine,
                        paste0("(History File(s): ",
                               paste(input$historyFilesSelect, collapse = ", "),
                               ")"))
    }
    if (length(appendedLine) > 0) {
      userMsg <- paste(userMsg, paste(appendedLine, collapse = " "), sep = "\n")
    }
    
    shinyjs::disable("sendMsg")
    
    currId <- currentConversationId()
    timestamp_user <- Sys.time()
    addMessageToDB(currId, "user", userMsg, timestamp_user)
    convs <- conversationsRV()
    convs[[currId]]$messages <- append(convs[[currId]]$messages, list(list(role = "user", content = userMsg, timestamp = timestamp_user)))
    conversationsRV(convs)
    
    historyText <- sapply(convs[[currId]]$messages, function(x) {
      paste0(if(x$role == "user") "User: " else "Assistant: ", x$content)
    }, USE.NAMES = FALSE)
    
    combinedPrompt <- paste("\n [SYSTEM PROMPT] \n", input$systemPrompt)
    
    if (input$suppressChatHistory) {
      combinedPrompt <- paste(
        combinedPrompt, 
        "\n\n [LATEST USER PROMPT] \n",
        input$userInput
      )
    } else {
      combinedPrompt <- paste(
        combinedPrompt, 
        "\n\n [CHAT HISTORY] \n", 
        paste(historyText, collapse = "\n")
      )
    }
    
    docs <- embeddedDocs()
    queryText <- input$userInput
    
    if (input$useRAG == "Yes" && length(docs) > 0) {
      if (input$useReranker == "Yes") {
        reranker_model <- input$rerankerModel
        query_embedding <- compute_embedding(queryText, model = reranker_model)
        if (is.null(query_embedding)) {
          convs[[currId]]$messages <- append(convs[[currId]]$messages, list(list(
            role = "bot",
            content = paste("Error: Could not compute query embedding using model", reranker_model),
            timestamp = Sys.time())))
          conversationsRV(convs)
          updateTextAreaInput(session, "userInput", value = "")
          shinyjs::enable("sendMsg")
          return()
        }
        reranked <- rerank_contexts(query_embedding, docs)
        top_n <- input$retrievalTopK
        if (length(reranked$sorted_indices) > 0) {
          top_indices <- head(reranked$sorted_indices, top_n)
          retrieved_context <- sapply(top_indices, function(i) {
            doc <- docs[[i]]
            metaHeader <- paste0("File: ", doc$file, " | Chunk: ", doc$chunk_id, "\n")
            paste0(metaHeader, doc$content)
          })
          combinedPrompt <- paste(
            combinedPrompt, 
            "\n\n [RELEVANT CONTEXT] \n", 
            paste(retrieved_context, collapse = "\n\n")
          )
        }
      } else {
        query_embedding <- compute_embedding(queryText, model = input$embedding_model)
        if (is.null(query_embedding)) {
          convs[[currId]]$messages <- append(convs[[currId]]$messages, list(list(
            role = "bot",
            content = paste("Error: Could not compute query embedding using model", input$embedding_model),
            timestamp = Sys.time())))
          conversationsRV(convs)
          updateTextAreaInput(session, "userInput", value = "")
          shinyjs::enable("sendMsg")
          return()
        }
        reranked <- rerank_contexts(query_embedding, docs)
        top_n <- input$retrievalTopK
        if (length(reranked$sorted_indices) > 0) {
          top_indices <- head(reranked$sorted_indices, top_n)
          retrieved_context <- sapply(top_indices, function(i) {
            doc <- docs[[i]]
            metaHeader <- paste0("File: ", doc$file, " | Chunk: ", doc$chunk_id, "\n")
            paste0(metaHeader, doc$content)
          })
          combinedPrompt <- paste(
            combinedPrompt, 
            "\n\n [RELEVANT CONTEXT] \n", 
            paste(retrieved_context, collapse = "\n\n")
          )
        }
      }
    }
    
    if (length(newFiles) > 0) {
      combinedPrompt <- paste(combinedPrompt, "\n\n [FILE CONTENT] \n")
      for (fn in newFiles) {
        text <- newFilesContent[[fn]]
        combinedPrompt <- paste0(
          combinedPrompt, 
          "-----\n", fn, ":\n", text, "\n"
        )
      }
    }
    
    `%||%` <- function(x, y) {
      if (!is.null(x)) x else y
    }
    
    if (length(input$historyFilesSelect) > 0) {
      convFiles <- convs[[currId]]$uploadedFiles
      blocks <- lapply(input$historyFilesSelect, function(fn) {
        convFiles[[fn]] %||% ""
      })
      histBlock <- paste(blocks, collapse = "\n---\n")
      combinedPrompt <- paste(combinedPrompt, "\n\n [HISTORY FILES] \n", histBlock)
    }
    
    latestCombinedPrompt <- reactiveVal("")
    latestCombinedPrompt(combinedPrompt)
    output$latestCombinedPrompt <- renderText({
      latestCombinedPrompt()
    })
    
    reqBody <- list(
      model = input$model,
      prompt = combinedPrompt,
      temperature = input$temp,
      context_window = input$context,
      top_k = input$topK,
      top_p = input$topP,
      embedding_model = input$embedding_model
    )
    endpoint <- "http://localhost:11434/api/generate"
    
    fullResponse <- withProgress(message = "LLM is responding...", value = 0, {
      incProgress(0.3, detail = "Sending request...")
      res <- POST(endpoint, 
                  body = toJSON(reqBody, auto_unbox = TRUE), 
                  encode = "json")
      incProgress(0.6, detail = "Processing response...")
      resText <- content(res, "text", encoding = "UTF-8")
      lines <- strsplit(resText, "\n")[[1]]
      responseParts <- sapply(lines, function(line) {
        if(nchar(trimws(line)) > 0) {
          parsed <- fromJSON(line)
          return(parsed$response)
        } else {
          return("")
        }
      })
      incProgress(0.1, detail = "Finalizing...")
      paste(responseParts, collapse = "")
    })
    
    timestamp_bot <- Sys.time()
    addMessageToDB(currId, "bot", fullResponse, timestamp_bot)
    convs[[currId]]$messages <- append(convs[[currId]]$messages, list(list(role = "bot", content = fullResponse, timestamp = timestamp_bot)))
    
    if (length(newFiles) > 0) {
      convs[[currId]]$uploadedFiles <- c(convs[[currId]]$uploadedFiles, newFilesContent)
      choices <- names(convs[[currId]]$uploadedFiles)
      if (is.null(choices) || length(choices) == 0) {
        choices <- character(0)
      }
      updateSelectInput(session, "historyFilesSelect", choices = choices, selected = character(0))
      newUploadedFiles(list())
    }
    
    conversationsRV(convs)
    
    updateTextAreaInput(session, "userInput", value = "")
    reset("UserFile")
    shinyjs::enable("sendMsg")
  })
}

shinyApp(ui, server)
