from docx import Document
from lxml import etree

def remove_bookmarked_sections(input_file, output_file, bookmark_prefix="for_author"):

    doc = Document(input_file)
    body = doc._element.body

    # Find all bookmarkStart elements
    bookmarks = body.findall(".//w:bookmarkStart", namespaces=body.nsmap)

    for bm in bookmarks:
        name = bm.get("{http://schemas.openxmlformats.org/wordprocessingml/2006/main}name")

        if name and name.startswith(bookmark_prefix):

            bm_id = bm.get("{http://schemas.openxmlformats.org/wordprocessingml/2006/main}id")
            print(f"🗑️ Removing bookmark: {name}")

            current = bm
            elements_to_remove = []

            # Move forward in document order
            while current is not None:
                elements_to_remove.append(current)

                # Stop at matching bookmarkEnd
                if (
                    current.tag.endswith("bookmarkEnd")
                    and current.get("{http://schemas.openxmlformats.org/wordprocessingml/2006/main}id") == bm_id
                ):
                    break

                current = current.getnext()

            # Remove ONLY these nodes (not parent table)
            for el in elements_to_remove:
                parent = el.getparent()
                if parent is not None:
                    try:
                        parent.remove(el)
                    except:
                        pass

    doc.save(output_file)
    print(f"✅ Cleaned file saved at: {output_file}")

# Usage
remove_bookmarked_sections("PopPK_Report_merge_TFL_filled_draft.docx", "cleaned.docx")