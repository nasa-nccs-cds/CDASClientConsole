import sbt._

object Version {
  val mockito   = "1.10.19"
  val scalaTest = "2.2.4"
  val slf4j     = "1.7.6"
}

object Library {
  val mockitoAll     = "org.mockito"       %  "mockito-all"     % Version.mockito
  val scalaTest      = "org.scalatest"     %% "scalatest"       % Version.scalaTest
  val slf4jApi       = "org.slf4j"         %  "slf4j-api"       % Version.slf4j
  val scalaxml       = "org.scala-lang.modules" %% "scala-xml"  % "1.0.3"
  val scalaparser    = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

  val cdm            = "edu.ucar"           % "cdm"             % "4.6.10"
  val clcommon       = "edu.ucar"           % "clcommon"        % "4.6.10"
  val netcdf4        = "edu.ucar"           % "netcdf4"         % "4.6.10"
  val nd4s           = "org.nd4j"           % "nd4s_2.11"       % "0.4-rc3.8"
  val nd4j           =  "org.nd4j"          % "nd4j-x86"        % "0.4-rc3.8"
  val opendap        = "edu.ucar"           % "opendap"         % "2.2.2"
  val httpservices   = "edu.ucar"           %  "httpservices"   % "4.6.0"
  val udunits        = "edu.ucar"           %  "udunits"        % "4.6.0"
  val joda           = "joda-time"          % "joda-time"       % "2.8.1"
  val natty          = "com.joestelmach"    % "natty"           % "0.11"
  val guava          = "com.google.guava"   % "guava"           % "18.0"
  val geotools       = "org.geotools"      %  "gt-shapefile"    % "13.2"
  val breeze         = "org.scalanlp"      %% "breeze"          % "0.12"
  val sprayCache     = "io.spray"       % "spray-caching_2.11" % "1.3.3"
  val sprayUtil      = "io.spray"       % "spray-util_2.11"    % "1.3.3"
  val scalactic      = "org.scalactic" %% "scalactic" % "2.2.6"
  val scalatest      = "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  val concurrentlinkedhashmap = "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2"

  val cds2           = "nasa.nccs"         %% "cdas2"       % "1.2.2-SNAPSHOT"
}

object Dependencies {
  import Library._

  val scala = Seq( slf4jApi, joda, natty, scalactic, scalatest )

  val scala_xml = Seq( scalaxml, scalaparser )

  val cache = Seq( concurrentlinkedhashmap )

  val CDS2 = Seq( cds2 )

  val netcdf = Seq( cdm, clcommon, netcdf4 )
}

//<groupId>com.googlecode.concurrentlinkedhashmap</groupId>
//  <artifactId>concurrentlinkedhashmap-lru</artifactId>
//  <version>1.4.2</version>










