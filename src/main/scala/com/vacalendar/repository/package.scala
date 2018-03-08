package com.vacalendar

import java.time.{ Instant, LocalDate }
import doobie._

package object repository {
  
  implicit val InstantMeta: Meta[Instant] =
    Meta[java.sql.Timestamp].xmap(
      ts => ts.toInstant,
      inst => java.sql.Timestamp.from(inst)
    )

  implicit val LocalDateMeta: Meta[LocalDate] =
    Meta[java.sql.Date].xmap(
      sqlDate => sqlDate.toLocalDate,
      ld => java.sql.Date.valueOf(ld)
    )
}