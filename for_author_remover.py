from docx import Document
import os

def remove_author_sections(input_file, output_file=None, debug=False):
    """
    Removes sections like 'For the author' from a .docx file.
    
    Args:
        input_file (str): Path to input .docx
        output_file (str): Path to save cleaned file (optional)
        debug (bool): Print removed content for verification
    """

    if not os.path.exists(input_file):
        raise FileNotFoundError(f"File not found: {input_file}")

    if output_file is None:
        output_file = input_file.replace(".docx", "_cleaned.docx")

    doc = Document(input_file)
    body = doc._element.body

    elements_to_remove = []

    for element in body:
        from lxml import etree
        xml_str = etree.tostring(element, encoding="unicode").lower()

        # Step 1: Detect box-like structures
        is_box = ("w:drawing" in xml_str) or ("w:shd" in xml_str) or ("w:tbl" in xml_str)

        # Step 2: Detect "For the author" content
        is_author_section = "for the author" in xml_str

        # Final condition (SAFE)
        if is_box and is_author_section:
            if debug:
                print("\n--- REMOVING BLOCK ---")
                print(xml_str[:500])  # preview
                print("----------------------\n")

            elements_to_remove.append(element)

    # Step 3: Remove identified elements
    for el in elements_to_remove:
        body.remove(el)

    # Step 4: Save cleaned document
    doc.save(output_file)

    print(f"✅ Cleaned file saved at: {output_file}")
    print(f"🗑️ Removed {len(elements_to_remove)} section(s)")


# 🔥 Example Usage
if __name__ == "__main__":
    input_path = "PopPK_Report_merge_TFL_filled_draft.docx"  # change this
    remove_author_sections(input_path, debug=True)