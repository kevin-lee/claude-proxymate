package claudeproxymate.core

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Properties
import scala.jdk.CollectionConverters.*

/** Loads a `.properties` file (UTF-8) into a Scala `Map[String, String]`. */
object I18nPropertiesLoader {

  def load(path: Path): Map[String, String] = {
    val props = new Properties()
    val reader = new BufferedReader(new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))
    try props.load(reader)
    finally reader.close()
    props.asScala.toMap
  }
}
