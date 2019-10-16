package com.chriniko.adt.example.query.parsing
import org.apache.log4j.Logger

trait Logging {
  val log: Logger = Logger.getLogger(this.getClass.getName)

}
