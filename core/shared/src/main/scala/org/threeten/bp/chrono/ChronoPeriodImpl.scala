/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten.bp.chrono

import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import java.io.Serializable
import java.util.{ Arrays, Collections, Objects }
import org.threeten.bp.DateTimeException
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

/** An implementation of {@code ChronoPeriod}.
  */
@SerialVersionUID(275618735781L)
final class ChronoPeriodImpl(
  private val chronology: Chronology,
  private val years:      Int,
  private val months:     Int,
  private val days:       Int
) extends ChronoPeriod
    with Serializable {

  def get(unit: TemporalUnit): Long =
    if (unit eq YEARS) years.toLong
    else if (unit eq MONTHS) months.toLong
    else if (unit eq DAYS) days.toLong
    else throw throw UnsupportedTemporalTypeException.unit(unit)

  def getUnits: java.util.List[TemporalUnit] =
    Collections.unmodifiableList(Arrays.asList[TemporalUnit](YEARS, MONTHS, DAYS))

  def getChronology: Chronology = chronology

  def plus(amountToAdd: TemporalAmount): ChronoPeriod =
    amountToAdd match {
      case amount: ChronoPeriodImpl if amount.getChronology == getChronology =>
        new ChronoPeriodImpl(chronology,
                             Math.addExact(years, amount.years),
                             Math.addExact(months, amount.months),
                             Math.addExact(days, amount.days))
      case _ =>
        throw new DateTimeException(s"Unable to add amount: $amountToAdd")
    }

  def minus(amountToSubtract: TemporalAmount): ChronoPeriod =
    amountToSubtract match {
      case amount: ChronoPeriodImpl if amount.getChronology == getChronology =>
        new ChronoPeriodImpl(chronology,
                             Math.subtractExact(years, amount.years),
                             Math.subtractExact(months, amount.months),
                             Math.subtractExact(days, amount.days))
      case _ =>
        throw new DateTimeException(s"Unable to subtract amount: $amountToSubtract")
    }

  def multipliedBy(scalar: Int): ChronoPeriod =
    new ChronoPeriodImpl(chronology,
                         Math.multiplyExact(years, scalar),
                         Math.multiplyExact(months, scalar),
                         Math.multiplyExact(days, scalar))

  def normalized: ChronoPeriod =
    if (chronology.range(ChronoField.MONTH_OF_YEAR).isFixed) {
      val monthLength: Long = chronology.range(ChronoField.MONTH_OF_YEAR).getMaximum - chronology
        .range(ChronoField.MONTH_OF_YEAR)
        .getMinimum + 1
      val total: Long  = years * monthLength + months
      val _years: Int  = Math.toIntExact(total / monthLength)
      val _months: Int = Math.toIntExact(total % monthLength)
      new ChronoPeriodImpl(chronology, _years, _months, days)
    } else
      this

  def addTo(temporal: Temporal): Temporal = {
    Objects.requireNonNull(temporal, "temporal")
    var _temporal                  = temporal
    val temporalChrono: Chronology = _temporal.query(TemporalQueries.chronology)
    if (temporalChrono != null && !(chronology == temporalChrono))
      throw new DateTimeException(
        s"Invalid chronology, required: ${chronology.getId}, but was: ${temporalChrono.getId}"
      )
    if (years != 0)
      _temporal = _temporal.plus(years.toLong, YEARS)
    if (months != 0)
      _temporal = _temporal.plus(months.toLong, MONTHS)
    if (days != 0)
      _temporal = _temporal.plus(days.toLong, DAYS)
    _temporal
  }

  def subtractFrom(temporal: Temporal): Temporal = {
    Objects.requireNonNull(temporal, "temporal")
    var _temporal                  = temporal
    val temporalChrono: Chronology = _temporal.query(TemporalQueries.chronology)
    if (temporalChrono != null && !(chronology == temporalChrono))
      throw new DateTimeException(
        s"Invalid chronology, required: ${chronology.getId}, but was: ${temporalChrono.getId}"
      )
    if (years != 0)
      _temporal = _temporal.minus(years.toLong, YEARS)
    if (months != 0)
      _temporal = _temporal.minus(months.toLong, MONTHS)
    if (days != 0)
      _temporal = _temporal.minus(days.toLong, DAYS)
    _temporal
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: ChronoPeriodImpl =>
        (this eq other) || (years == other.years && months == other.months && days == other.days && (chronology == other.chronology))
      case _ => false
    }

  override def hashCode: Int =
    chronology.hashCode + Integer.rotateLeft(years, 16) + Integer.rotateLeft(months, 8) + days

  override def toString: String =
    if (isZero) chronology + " P0D"
    else {
      val buf: StringBuilder = new StringBuilder
      buf.append(chronology).append(' ').append('P')
      if (years != 0)
        buf.append(years).append('Y')
      if (months != 0)
        buf.append(months).append('M')
      if (days != 0)
        buf.append(days).append('D')
      buf.toString
    }
}
