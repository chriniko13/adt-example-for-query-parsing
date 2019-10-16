package com.chriniko.adt.example.query.parsing.domain

import java.util.UUID

case class Id(value: String)

object Id {
  def apply(): Id = new Id(UUID.randomUUID().toString)
}
