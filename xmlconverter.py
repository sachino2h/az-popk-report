import zipfile
import os

def extract_docx_xml(docx_path, output_dir):
    """
    Extract all XML files from a .docx file
    """
    if not zipfile.is_zipfile(docx_path):
        raise ValueError("Not a valid .docx file")

    with zipfile.ZipFile(docx_path, 'r') as zip_ref:
        zip_ref.extractall(output_dir)

    print(f"Extracted to: {output_dir}")

    # Return main document XML path
    main_xml = os.path.join(output_dir, "word", "document.xml")
    return main_xml

docx_file = "PopPK_Report_merge_TFL_filled_draft.docx"
output_folder = "extracted_docx_xml"

xml_path = extract_docx_xml(docx_file, output_folder)
print("Main XML:", xml_path)