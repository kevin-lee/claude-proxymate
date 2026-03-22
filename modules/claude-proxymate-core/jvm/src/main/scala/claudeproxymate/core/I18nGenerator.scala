package claudeproxymate.core

import io.circe.Json
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Converts `.properties` locale files to `.json` for browser consumption.
  *
  * Usage: `I18nGenerator <inputDir> <outputDir>`
  *
  * Reads every `*.properties` file in `inputDir`, converts to a sorted JSON object,
  * and writes `<locale>.json` to `outputDir`.
  */
object I18nGenerator {

  def main(args: Array[String]): Unit = {
    args match {
      case Array(i, o) => generate(Path.of(i), Path.of(o))
      case _ =>
        System.err.println("Usage: I18nGenerator <inputDir> <outputDir>")
        sys.exit(1)
    }
  }

  private def generate(inputDir: Path, outputDir: Path): Unit = {
    Files.createDirectories(outputDir): Unit

    Files
      .list(inputDir)
      .iterator()
      .asScala
      .filter(p => p.toString.endsWith(".properties"))
      .foreach { propFile =>
        val locale     = propFile.getFileName.toString.stripSuffix(".properties")
        val entries    = I18nPropertiesLoader.load(propFile)
        val json       =
          Json.fromFields(entries.toList.sortBy { case (k, _) => k }.map { case (k, v) => k -> Json.fromString(v) })
        val outputFile = outputDir.resolve(s"$locale.json")
        Files.writeString(outputFile, json.spaces2): Unit
      }
  }
}
