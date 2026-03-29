library(xml2)
library(officer)
library(docxtractr)

hid_doc <- here::here('hidden_meta.docx')
hid_doc <- officer::read_docx(hid_doc)
xml2  <- officer::docx_body_xml(hid_doc)
xml2
write_xml(xml2, "hidden-pk.xml", options = "format")


doca_path <- here::here("results", "table", "eda", "alt-pk-data-sum-dose.docx")
doc <- read_docx(doca_path)

tbl <- docx_extract_tbl(doc, tbl_number = 1, header = TRUE)
tbl

doc1 <- docxtractr::read_docx(doca_path)
tbl  <- docx_extract_tbl(doc1, 1)

# For XML
doc2 <- officer::read_docx(doca_path)
doc2
xml  <- officer::docx_body_xml(doc2)

xml

write_xml(xml, "document_body-pk.xml", options = "format")

remove_footnote_paragraphs <- function(doc) {
  
  xml <- docx_body_xml(doc)
  
  # find paragraphs containing footnote keywords
  nodes <- xml_find_all(xml, "//w:p[w:r/w:t[contains(., 'Categorical summary')]] |
                               //w:p[w:r/w:t[contains(., 'nEDA')]] |
                               //w:p[w:r/w:t[contains(., 'SD')]] |
                               //w:p[w:r/w:t[contains(., 'IQR')]]")
  
  # xml_remove(nodes)
  
  nodes
  
  doc
}

remove_footnote_paragraphs(doc)


extract_table_width_v2 <- function(docx_path) {
  
  temp_dir <- tempfile()
  unzip(docx_path, exdir = temp_dir)
  
  doc <- read_xml(file.path(temp_dir, "word", "document.xml"))
  tables <- xml_find_all(doc, "//w:tbl")
  
  results <- lapply(tables, function(tbl) {
    
    # Try direct table width
    tblW <- xml_find_first(tbl, ".//w:tblPr/w:tblW")
    
    if (!is.na(tblW) && !is.na(xml_attr(tblW, "w:w"))) {
      return(as.numeric(xml_attr(tblW, "w:w")))
    }
    
    # Fallback → sum column widths
    cols <- xml_find_all(tbl, ".//w:tblGrid/w:gridCol")
    col_widths <- as.numeric(xml_attr(cols, "w:w"))
    
    total_width <- sum(col_widths, na.rm = TRUE)
    
    return(total_width)
  })
  
  return(results)
}

extract_table_width_v2(doca_path)





#======================================================
#    Hidden mapping
#======================================================

create_doc_with_real_hidden <- function(output) {
  
  doc <- read_docx()
  xml <- docx_body_xml(doc)
  
  # Create paragraph
  p <- xml_add_child(xml, "w:p")
  
  # Visible run
  r1 <- xml_add_child(p, "w:r")
  xml_add_child(r1, "w:t", "This is a visible paragraph.")
  
  # Hidden run
  r2 <- xml_add_child(p, "w:r")
  
  rpr <- xml_add_child(r2, "w:rPr")
  xml_add_child(rpr, "w:vanish") 
  
  xml_add_child(r2, "w:t", "[[ID::para_001]]")
  
  print(doc, target = output)
}

create_doc_with_real_hidden("real_hidden.docx")


doc <- read_docx("real_hidden.docx")
xml <- docx_body_xml(doc)
xml


doca_path <- here::here("results", "table", "eda", "alt-pk-data-sum-dose.docx")
doc <- read_docx(doca_path)

doc
doc_xml <- docx_body_xml(doc)
ns <- xml_ns(doc_xml)
bookmarks <- xml_find_all(doc_xml, ".//w:bookmarkStart", ns)

bookmark_data <- data.frame(
  id = xml_attr(bookmarks, "w:id"),
  name = xml_attr(bookmarks, "w:name")
)

print(bookmark_data)

write_xml(xml, "document_body-pk.xml", options = "format")
