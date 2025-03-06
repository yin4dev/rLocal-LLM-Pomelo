# rLocal-LLM-Pomelo
Local LLM with RAG using R Shiny and ollama

Welcome to **rLocal-LLM-Pomelo**! This open-source Shiny application is designed for conversational interactions with Large Language Models (LLMs), **which is developed to be applicable to R-portable**. I encourage free editing, contributions, and improvements. Feel free to fork this project, submit pull requests, or customize it to suit your needs.

---

## Table of Contents
1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Dependencies](#dependencies)
4. [Installation and Setup](#installation-and-setup)
   - [Required Tools](#required-tools)
   - [Database Schemas](#database-schemas)
5. [Usage](#usage)
   - [Running the Shiny App](#running-the-shiny-app)
   - [Conversation Management](#conversation-management)
   - [File Upload and OCR](#file-upload-and-ocr)
   - [Retrieval-Augmented Generation (RAG)](#retrieval-augmented-generation-rag)
   - [Embedding Caching and Combination](#embedding-caching-and-combination)
6. [Configuration](#configuration)
7. [Special Thanks](#special-thanks)
8. [License](#license)

---

## Overview

**rLocal-LLM-Pomelo** is an R/Shiny application that provides an intuitive interface for interacting with LLMs. It supports multi-conversation management, document embedding, retrieval-augmented generation (RAG), and even OCR for image files. The application integrates with local LLM services (e.g., via Ollama) to retrieve context and generate responses based solely on provided information.

---

## Key Features

- **Multi-Conversation UI**: Create, select, and delete individual conversations, with each session stored persistently.
- **Customizable Assistant Profiles**: Easily switch between predefined personas (e.g., Normal, Professional Data Analyst, Scientific Report Writer, etc.) with dedicated system prompts and parameters.
- **RAG Pipeline**: Retrieve relevant document chunks via embeddings to enrich LLM responses. Supports on-the-fly embedding and pre-embedded sources.
- **File Upload & OCR**: Upload various document types and images (with OCR capabilities) to extract and incorporate text.
- **Database Persistence**: Uses SQLite databases to store both chat histories and document embeddings.

---

## Dependencies

- **R Packages**:  
  `shiny`, `shinyBS`, `shinyjs`, `httr`, `jsonlite`, `pdftools`, `readtext`, `tools`, `RSQLite`, `DBI`, `rollama`, `curl`, `tesseract`, `magick`, `base64enc`

- **System Tools**:
  - [Ollama](https://www.ollama.com/) for pulling and running LLM models locally
  - [Tesseract OCR](https://github.com/tesseract-ocr/tesseract).

---

## Installation and Setup

### Required Tools

1. **Clone or Download the Repository**  
   Clone this repository or download the source files to your local machine.

2. **Install Ollama and Pull LLM Models**  
   - **Ollama**:  
     Install [Ollama](https://www.ollama.com/) to run local LLM models.  
     After installing Ollama, pull the required LLM models using commands such as:
     ```bash
     ollama pull llama3.2:1b
     ollama pull llama3.2:3b
     ollama pull deepseek-r1:8b
     ```
   - **Tesseract OCR**:  
     Install Tesseract OCR and ensure that the environment variable is set:
     ```r
     Sys.setenv(TESSDATA_PREFIX = "<path_to_tessdata>")
     ```

3. **Install R Packages**  
   Install the required R packages:
   ```r
   install.packages(c(
     "shiny", "shinyBS", "shinyjs", "httr", "jsonlite", "pdftools",
     "readtext", "tools", "RSQLite", "DBI", "curl", "tesseract",
     "magick", "base64enc"
   ))
   # Note: 'rollama' may need to be installed from its source repository.
   ```

4. **Review and Configure the Code**  
   - Modify file paths and configuration parameters as needed (e.g., folder directories for document embeddings).
   - The LLM endpoint is set to `http://localhost:11434/api/generate` by default; change this if your LLM service runs on a different address.

### Database Schemas

#### Conversation History SQLite Schema (`chat_history.sqlite`)

The conversation history is stored in an SQLite database with the following tables:

- **conversations** table:
  ```sql
  CREATE TABLE IF NOT EXISTS conversations (
    conversation_id TEXT PRIMARY KEY,
    name TEXT,
    created_at DATETIME
  );
  ```
- **messages** table:
  ```sql
  CREATE TABLE IF NOT EXISTS messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    conversation_id TEXT,
    role TEXT,
    content TEXT,
    timestamp DATETIME
  );
  ```

#### Embedding Cache SQLite Schema (`embedded_cache.sqlite`)

Document embeddings are cached in an SQLite database with the following schema:
```sql
CREATE TABLE documents (
  id INTEGER PRIMARY KEY,
  file_name TEXT,
  chunk_id TEXT,
  content TEXT,
  embedding TEXT
);
```

---

## Usage

### Running the Shiny App

1. **Open an R Session**  
2. **Run the App**  
   ```r
   library(shiny)
   runApp("path_to_your_app_directory")
   ```
3. **Access the App**  
   A browser window should open automatically; if not, navigate to the displayed URL (typically `http://127.0.0.1:xxxx`).

### Conversation Management

- **Multi-Conversation UI**:  
  The sidebar displays a list of active conversations loaded from the `chat_history.sqlite` file.
  - **New Conversation**: Create a fresh conversation with its own message history.
  - **Select Conversation**: Click on a conversation to view its messages.
  - **Delete Conversation**: Remove a conversation from the database.

### File Upload and OCR

- **Supported File Types**:  
  `.txt`, `.csv`, `.pdf`, `.doc/.docx` and common image formats (`.png`, `.jpg`, etc.).
- **OCR Functionality**:  
  Automatically extracts text from image files using Tesseract (Japanese engine is included as an example).

### Retrieval-Augmented Generation (RAG)

- **RAG Options**:  
  - **Raw Files**: Embed documents on-the-fly from a specified directory.
  - **Pre-Embedded Files**: Load embeddings from existing SQLite or CSV files.
- **Reranking**:  
  Optionally re-rank retrieved document chunks with an alternative embedding model for improved context selection.
- **Prompt Construction**:  
  The system constructs a combined prompt from the system prompt, chat history, and retrieved document context before querying the LLM.

### Embedding Caching and Combination

- **Embedding Cache**:  
  Cache document embeddings into an SQLite database (`embedded_cache.sqlite`) for fast retrieval in future queries.
- **Combine Pre-Embedded Files**:  
  Merge multiple pre-embedded SQLite or CSV files into a single database to streamline RAG operations.

---

## Configuration

- **System Prompts & Assistant Profiles**:  
  Choose from several predefined profiles (e.g., Normal, Professional Data Analyst, Scientific Report Writer, etc.) to automatically adjust generation parameters.
- **LLM Parameters**:  
  Customize temperature, top-k, top-p, and context window size via the UI.
- **LLM Endpoint**:  
  The default endpoint is `http://localhost:11434/api/generate`. Update this as needed for your LLM service.

---

## Special Thanks
A huge thank you to my loving Aimi Yago for her invaluable understanding, support, and inspiration in making this project better!  🎉

---
## License

This project is licensed under the [GNU General Public License v3.0 (GPL-3.0)](https://www.gnu.org/licenses/gpl-3.0.html).


Copyright (C) 2025 Hongrong Yin

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.
