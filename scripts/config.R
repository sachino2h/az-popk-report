get_config <- function() {
  list(
    paths = list(
      source_table_dirs = c(
        here::here("results", "table", "eda"),
        here::here("results", "table", "102"),
        here::here("results", "table", "106")
      ),
      source_figure_dirs = c(
        here::here("results", "figure", "eda"),
        here::here("results", "figure", "106")
      ),
      template_in = here::here("PopPK_Report_merge_TFL_filled_draft.docx"),
      report_out_dir = here::here("output", "output"),
      report_versions_dir = here::here("output", "reports"),
      magic_doc_out = here::here("output", "intermediate", "PopPK_Report_merge_TFL_filled_draft-magic.docx"),
      mapping_yaml = here::here("output", "mapping.yaml"),
      mapping_html = here::here("output", "mapping.html")
    ),
    mapping = list(
      default_block_status = "new",
      html_template = here::here("scripts", "templates", "mapping-editor-template.html")
    ),
    block_paragraph_styles = list(
      table_title = "Table Title",
      table_footnote = "Table Footnote Info",
      image_title = "Figure Title",
      image_footnote = "Table Footnote Info"
    ),
    docx_table_style = list(
      table_header = "Table Head",
      table_split_row = "Table Left",
      table_first_column = "Table Left",
      table_normal_cell = "Table Center",
      table_footnote = "Table Footnote Info",
      extract_table_footnotes = TRUE,
      header_fill = "#FFFFFF"
    )
  )
}
