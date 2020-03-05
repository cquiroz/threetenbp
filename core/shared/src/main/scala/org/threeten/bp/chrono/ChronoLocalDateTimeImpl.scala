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

import java.util.Objects

import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import java.io.IOException
import java.io.ObjectInput
import java.io.ObjectOutput
import java.io.Serializable
import org.threeten.bp.LocalTime
import org.threeten.bp.ZoneId
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(4556003607393004514L)
private[chrono] object ChronoLocalDateTimeImpl {

  /** Hours per minute. */
  private def HOURS_PER_DAY: Int = 24

  /** Minutes per hour. */
  private def MINUTES_PER_HOUR: Int = 60

  /** Minutes per day. */
  private def MINUTES_PER_DAY: Int = MINUTES_PER_HOUR * HOURS_PER_DAY

  /** Seconds per minute. */
  private def SECONDS_PER_MINUTE: Int = 60

  /** Seconds per hour. */
  private def SECONDS_PER_HOUR: Int = SECONDS_PER_MINUTE * MINUTES_PER_HOUR

  /** Seconds per day. */
  private def SECONDS_PER_DAY: Int = SECONDS_PER_HOUR * HOURS_PER_DAY

  /** Milliseconds per day. */
  private def MILLIS_PER_DAY: Long = SECONDS_PER_DAY * 1000L

  /** Microseconds per day. */
  private def MICROS_PER_DAY: Long = SECONDS_PER_DAY * 1000000L

  /** Nanos per second. */
  private def NANOS_PER_SECOND: Long = 1000000000L

  /** Nanos per minute. */
  private def NANOS_PER_MINUTE: Long = NANOS_PER_SECOND * SECONDS_PER_MINUTE

  /** Nanos per hour. */
  private def NANOS_PER_HOUR: Long = NANOS_PER_MINUTE * MINUTES_PER_HOUR

  /** Nanos per day. */
  private def NANOS_PER_DAY: Long = NANOS_PER_HOUR * HOURS_PER_DAY

  /** Obtains an instance of {@code ChronoLocalDateTime} from a date and time.
    *
    * @param date  the local date, not null
    * @param time  the local time, not null
    * @return the local date-time, not null
    */
  private[chrono] def of[R <: ChronoLocalDate](
    date: R,
    time: LocalTime
  ): ChronoLocalDateTimeImpl[R] =
    new ChronoLocalDateTimeImpl[R](date, time)

  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  private[chrono] def readExternal(in: ObjectInput): ChronoLocalDateTime[_] = {
    val date: ChronoLocalDate = in.readObject.asInstanceOf[ChronoLocalDate]
    val time: LocalTime       = in.readObject.asInstanceOf[LocalTime]
    date.atTime(time)
  }
}

/** A date-time without a time-zone for the calendar neutral API.
  *
  * {@code ChronoLocalDateTime} is an immutable date-time object that represents a date-time, often
  * viewed as year-month-day-hour-minute-second. This object can also access other
  * fields such as day-of-year, day-of-week and week-of-year.
  *
  * This class stores all date and time fields, to a precision of nanoseconds.
  * It does not store or represent a time-zone. For example, the value
  * "2nd October 2007 at 13:45.30.123456789" can be stored in an {@code ChronoLocalDateTime}.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @tparam D the date type
  *
  * @constructor
  * @param date  the date part of the date-time, not null
  * @param time  the time part of the date-time, not null
  */
@SerialVersionUID(4556003607393004514L)
final class ChronoLocalDateTimeImpl[D <: ChronoLocalDate] private (
  private val date: D,
  private val time: LocalTime
) extends ChronoLocalDateTime[D]
    with Temporal
    with TemporalAdjuster
    with Serializable {
  Objects.requireNonNull(date, "date")
  Objects.requireNonNull(time, "time")

  /** Returns a copy of this date-time with the new date and time, checking
    * to see if a new object is in fact required.
    *
    * @param newDate  the date of the new date-time, not null
    * @param newTime  the time of the new date-time, not null
    * @return the date-time, not null
    */
  private def `with`(newDate: Temporal, newTime: LocalTime): ChronoLocalDateTimeImpl[D] =
    if ((date eq newDate) && (time eq newTime)) {
      this
    } else {
      val cd: D = date.getChronology.ensureChronoLocalDate[D](newDate)
      new ChronoLocalDateTimeImpl[D](cd, newTime)
    }

  def toLocalDate: D = date

  def toLocalTime: LocalTime = time

  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField]) field.isDateBased || field.isTimeBased
    else field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit]) unit.isDateBased || unit.isTimeBased
    else unit != null && unit.isSupportedBy(this)

  override def range(field: TemporalField): ValueRange =
    if (field.isInstanceOf[ChronoField])
      if (field.isTimeBased) time.range(field)
      else date.range(field)
    else
      field.rangeRefinedBy(this)

  override def get(field: TemporalField): Int =
    if (field.isInstanceOf[ChronoField])
      if (field.isTimeBased) time.get(field)
      else date.get(field)
    else
      range(field).checkValidIntValue(getLong(field), field)

  def getLong(field: TemporalField): Long =
    if (field.isInstanceOf[ChronoField])
      if (field.isTimeBased) time.getLong(field)
      else date.getLong(field)
    else
      field.getFrom(this)

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDateTimeImpl[D] =
    if (adjuster.isInstanceOf[ChronoLocalDate])
      `with`(adjuster.asInstanceOf[ChronoLocalDate], time)
    else if (adjuster.isInstanceOf[LocalTime])
      `with`(date, adjuster.asInstanceOf[LocalTime])
    else if (adjuster.isInstanceOf[ChronoLocalDateTimeImpl[_ <: ChronoLocalDate]])
      date.getChronology.ensureChronoLocalDateTime(
        adjuster.asInstanceOf[ChronoLocalDateTimeImpl[_]]
      )
    else
      date.getChronology.ensureChronoLocalDateTime(
        adjuster.adjustInto(this).asInstanceOf[ChronoLocalDateTimeImpl[_]]
      )

  def `with`(field: TemporalField, newValue: Long): ChronoLocalDateTimeImpl[D] =
    if (field.isInstanceOf[ChronoField])
      if (field.isTimeBased) `with`(date, time.`with`(field, newValue))
      else `with`(date.`with`(field, newValue), time)
    else
      date.getChronology.ensureChronoLocalDateTime(field.adjustInto(this, newValue))

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoLocalDateTimeImpl[D] =
    if (unit.isInstanceOf[ChronoUnit]) {
      val f: ChronoUnit = unit.asInstanceOf[ChronoUnit]
      import ChronoUnit._
      f match {
        case NANOS => plusNanos(amountToAdd)
        case MICROS =>
          plusDays(amountToAdd / ChronoLocalDateTimeImpl.MICROS_PER_DAY)
            .plusNanos((amountToAdd % ChronoLocalDateTimeImpl.MICROS_PER_DAY) * 1000)
        case MILLIS =>
          plusDays(amountToAdd / ChronoLocalDateTimeImpl.MILLIS_PER_DAY)
            .plusNanos((amountToAdd % ChronoLocalDateTimeImpl.MILLIS_PER_DAY) * 1000000)
        case SECONDS   => plusSeconds(amountToAdd)
        case MINUTES   => plusMinutes(amountToAdd)
        case HOURS     => plusHours(amountToAdd)
        case HALF_DAYS => plusDays(amountToAdd / 256).plusHours((amountToAdd % 256) * 12)
        case _         => `with`(date.plus(amountToAdd, unit), time)
      }
    } else {
      date.getChronology.ensureChronoLocalDateTime(unit.addTo(this, amountToAdd))
    }

  private def plusDays(days: Long): ChronoLocalDateTimeImpl[D] =
    `with`(date.plus(days, ChronoUnit.DAYS), time)

  private def plusHours(hours: Long): ChronoLocalDateTimeImpl[D] =
    plusWithOverflow(date, hours, 0, 0, 0)

  private def plusMinutes(minutes: Long): ChronoLocalDateTimeImpl[D] =
    plusWithOverflow(date, 0, minutes, 0, 0)

  private[chrono] def plusSeconds(seconds: Long): ChronoLocalDateTimeImpl[D] =
    plusWithOverflow(date, 0, 0, seconds, 0)

  private def plusNanos(nanos: Long): ChronoLocalDateTimeImpl[D] =
    plusWithOverflow(date, 0, 0, 0, nanos)

  private def plusWithOverflow(
    newDate: D,
    hours:   Long,
    minutes: Long,
    seconds: Long,
    nanos:   Long
  ): ChronoLocalDateTimeImpl[D] = {
    if ((hours | minutes | seconds | nanos) == 0)
      return `with`(newDate, time)
    var totDays: Long =
      nanos / ChronoLocalDateTimeImpl.NANOS_PER_DAY + seconds / ChronoLocalDateTimeImpl.SECONDS_PER_DAY + minutes / ChronoLocalDateTimeImpl.MINUTES_PER_DAY + hours / ChronoLocalDateTimeImpl.HOURS_PER_DAY
    var totNanos: Long =
      nanos % ChronoLocalDateTimeImpl.NANOS_PER_DAY + (seconds % ChronoLocalDateTimeImpl.SECONDS_PER_DAY) * ChronoLocalDateTimeImpl.NANOS_PER_SECOND + (minutes % ChronoLocalDateTimeImpl.MINUTES_PER_DAY) * ChronoLocalDateTimeImpl.NANOS_PER_MINUTE + (hours % ChronoLocalDateTimeImpl.HOURS_PER_DAY) * ChronoLocalDateTimeImpl.NANOS_PER_HOUR
    val curNoD: Long = time.toNanoOfDay
    totNanos = totNanos + curNoD
    totDays += Math.floorDiv(totNanos, ChronoLocalDateTimeImpl.NANOS_PER_DAY)
    val newNoD: Long       = Math.floorMod(totNanos, ChronoLocalDateTimeImpl.NANOS_PER_DAY)
    val newTime: LocalTime = if (newNoD == curNoD) time else LocalTime.ofNanoOfDay(newNoD)
    `with`(newDate.plus(totDays, ChronoUnit.DAYS), newTime)
  }

  def atZone(zoneId: ZoneId): ChronoZonedDateTime[D] =
    ChronoZonedDateTimeImpl.ofBest(this, zoneId, null)

  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: ChronoLocalDateTime[D] =
      toLocalDate.getChronology.localDateTime(endExclusive).asInstanceOf[ChronoLocalDateTime[D]]
    if (unit.isInstanceOf[ChronoUnit]) {
      val f: ChronoUnit = unit.asInstanceOf[ChronoUnit]
      if (f.isTimeBased) {
        var amount: Long = end.getLong(EPOCH_DAY) - date.getLong(EPOCH_DAY)
        import ChronoUnit._
        f match {
          case NANOS  => amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.NANOS_PER_DAY)
          case MICROS => amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.MICROS_PER_DAY)
          case MILLIS => amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.MILLIS_PER_DAY)
          case SECONDS =>
            amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.SECONDS_PER_DAY.toLong)
          case MINUTES =>
            amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.MINUTES_PER_DAY.toLong)
          case HOURS =>
            amount = Math.multiplyExact(amount, ChronoLocalDateTimeImpl.HOURS_PER_DAY.toLong)
          case HALF_DAYS => amount = Math.multiplyExact(amount, 2)
        }
        return Math.addExact(amount, time.until(end.toLocalTime, unit))
      }
      var endDate: ChronoLocalDate = end.toLocalDate
      if (end.toLocalTime.isBefore(time))
        endDate = endDate.minus(1, ChronoUnit.DAYS)
      return date.until(endDate, unit)
    }
    unit.between(this, end)
  }

  private def writeReplace: AnyRef = new Ser(Ser.CHRONO_LOCALDATETIME_TYPE, this)

  @throws[IOException]
  private[chrono] def writeExternal(out: ObjectOutput): Unit = {
    out.writeObject(date)
    out.writeObject(time)
  }
}
