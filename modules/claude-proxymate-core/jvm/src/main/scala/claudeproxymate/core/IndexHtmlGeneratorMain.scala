package claudeproxymate.core

import java.nio.file.{Files, Path}

/** CLI entry point that writes the generated `index.html` to disk.
  *
  * Usage: {{{IndexHtmlGeneratorMain <output-path> <i18n-dir>}}}
  *
  * Called by the `devUi` sbt task via `runMain`.
  */
object IndexHtmlGeneratorMain {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(outputPath, i18nDir) =>
        val koLocale = I18nPropertiesLoader.load(Path.of(i18nDir, "ko.properties"))
        val path     = Path.of(outputPath)
        Files.createDirectories(path.getParent): Unit
        Files.writeString(path, IndexHtmlGenerator.generate(koLocale)): Unit
      case _ =>
        System.err.println("Usage: IndexHtmlGeneratorMain <output-path> <i18n-dir>")
        sys.exit(1)
    }
  }
}
