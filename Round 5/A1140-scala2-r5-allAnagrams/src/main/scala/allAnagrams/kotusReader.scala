// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package allAnagrams

import java.util.Locale
import scala.jdk.CollectionConverters._

object kotusReader {
  val alphabet = Set('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','ä','ö','å')
  /**
   * Read words from the zipped kotus dictionary.
   */
  def apply(zipFileName: String): Seq[String] = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    val p = f.newSAXParser()
    val rootzip = new java.util.zip.ZipFile(zipFileName)
    val wordsXmlFileName = rootzip.entries.asScala.find(_.getName.endsWith(".xml")).get
    val wordsXml = scala.xml.XML.withSAXParser(p).load(rootzip.getInputStream(wordsXmlFileName))
    val wordNodes = wordsXml \ "st" \ "s"
    val words = wordNodes.map(_.text.toLowerCase(Locale.ENGLISH))
    val filtered = words.filter(w => w.forall(c => alphabet contains c))
    filtered.toVector
  }
}
