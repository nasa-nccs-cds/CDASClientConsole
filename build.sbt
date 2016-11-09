val kernelPackages = settingKey[ Seq[String] ]("A list of user-defined Kernel packages")

name := "cdasClientConsole"

version := "1.2-SNAPSHOT"

scalaVersion := "2.11.7"

organization := "nasa.nccs"

lazy val root = project in file(".")

//  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Unidata maven repository" at "http://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"
resolvers += "Java.net repository" at "http://download.java.net/maven/2"
resolvers += "Open Source Geospatial Foundation Repository" at "http://download.osgeo.org/webdav/geotools"
resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Boundless Maven Repository" at "http://repo.boundlessgeo.com/main"
resolvers += "spray repo" at "http://repo.spray.io"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Dependencies.scala

libraryDependencies ++= Dependencies.cache

libraryDependencies ++= Dependencies.CDS2

libraryDependencies ++= Dependencies.netcdf

fork in run:= false
fork in test:= false

javaOptions in run ++= Seq( "-Xmx2G", "-Xms512M")

import java.util.Properties

lazy val cdasProperties = settingKey[Properties]("The cdas properties map")
lazy val cdasPropertiesFile = settingKey[File]("The cdas properties file")
lazy val cdasLocalCollectionsFile = settingKey[File]("The cdas local Collections file")
lazy val uvcdat_prefix = settingKey[File]("The UVCDAT env directory.")

cdasPropertiesFile :=  baseDirectory.value / "project" / "cdshell.properties"
uvcdat_prefix := getUvcdatEnv

cdasProperties := {
  val prop = new Properties()
  try{ IO.load( prop, cdasPropertiesFile.value ) } catch { case err: Exception => println("No properties file found") }
  prop
}

dependencyOverrides ++= Set( "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.4" )

def getUvcdatEnv(): File =
  sys.env.get("CONDA_PREFIX") match {
    case Some(uvcdat_dir) => file(uvcdat_dir)
    case None => file( System.getProperty("user.home") )
  }

resolvers += "Local CDAS Repository" at "file:///" + getPublishDir( cdasProperties.value ).toString

def getPublishDir( properties: Properties ): File =
  sys.env.get("SBT_PUBLISH_DIR") match {
    case Some(cache_dir) => file(cache_dir)
    case None =>
      val home = file(System.getProperty("user.home"))
      val cache_dir = properties.getProperty("cdas.publish.dir", "")
      if (cache_dir.isEmpty) { home / ".cdas" / "cache" } else file( cache_dir )
  }

def getCacheDir( properties: Properties ): File =
  sys.env.get("CDAS_CACHE_DIR") match {
    case Some(cache_dir) => file(cache_dir)
    case None =>
      val home = file(System.getProperty("user.home"))
      val cache_dir = properties.getProperty("cdas.cache.dir", "")
      if (cache_dir.isEmpty) { home / ".cdas" / "cache" } else file( cache_dir )
  }

lazy val cdas_cache_dir = settingKey[File]("The CDAS cache directory.")

cdas_cache_dir := {
  val cache_dir = getCacheDir( cdasProperties.value )
  cache_dir.mkdirs()
  cdasProperties.value.put( "cdas.cache.dir", cache_dir.getAbsolutePath )
  try{ IO.write( cdasProperties.value, "", cdasPropertiesFile.value ) } catch { case err: Exception => println("Error writing to properties file: " + err.getMessage ) }
  cache_dir
}

unmanagedClasspath in Compile += cdas_cache_dir.value
unmanagedClasspath in Runtime += cdas_cache_dir.value
unmanagedClasspath in Runtime +=  uvcdat_prefix.value / "lib"
unmanagedClasspath in Test += cdas_cache_dir.value
unmanagedClasspath in Test +=  uvcdat_prefix.value / "lib"


    