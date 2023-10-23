output$dlink_download_documentation_word <- downloadHandler(
  filename = function() {
    "BioModME_Documentation.docx"
  },
  content = function(file) {
    path.to.file <- "www/documentation/BioModME_Supplement_v1.1_0.docx"
    file.copy(path.to.file, file)
  },
  contentType = 
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)