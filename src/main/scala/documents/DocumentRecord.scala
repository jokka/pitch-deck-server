package documents

import auth.Session

case class DocumentRecord(document: Document, owner: Session)
