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

import org.threeten.bp.chrono.ThaiBuddhistChronology.YEARS_DIFFERENCE
import org.threeten.bp.temporal.ChronoField._
import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.Serializable
import org.threeten.bp.Clock
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.Period
import org.threeten.bp.ZoneId
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(-8722293800195731463L)
object ThaiBuddhistDate {

  /** Obtains the current {@code ThaiBuddhistDate} from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current date.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current date using the system clock and default time-zone, not null
    */
  def now: ThaiBuddhistDate = now(Clock.systemDefaultZone)

  /** Obtains the current {@code ThaiBuddhistDate} from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current date.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current date using the system clock, not null
    */
  def now(zone: ZoneId): ThaiBuddhistDate = now(Clock.system(zone))

  /** Obtains the current {@code ThaiBuddhistDate} from the specified clock.
    *
    * This will query the specified clock to obtain the current date - today.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@linkplain Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current date, not null
    * @throws DateTimeException if the current date cannot be obtained
    */
  def now(clock: Clock): ThaiBuddhistDate = new ThaiBuddhistDate(LocalDate.now(clock))

  /** Obtains a {@code ThaiBuddhistDate} representing a date in the Thai Buddhist calendar
    * system from the proleptic-year, month-of-year and day-of-month fields.
    *
    * This returns a {@code ThaiBuddhistDate} with the specified fields.
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * @param prolepticYear  the Thai Buddhist proleptic-year
    * @param month  the Thai Buddhist month-of-year, from 1 to 12
    * @param dayOfMonth  the Thai Buddhist day-of-month, from 1 to 31
    * @return the date in Thai Buddhist calendar system, not null
    * @throws DateTimeException if the value of any field is out of range,
    *                           or if the day-of-month is invalid for the month-year
    */
  def of(prolepticYear: Int, month: Int, dayOfMonth: Int): ThaiBuddhistDate =
    ThaiBuddhistChronology.INSTANCE.date(prolepticYear, month, dayOfMonth)

  /** Obtains a {@code ThaiBuddhistDate} from a temporal object.
    *
    * This obtains a date in the Thai Buddhist calendar system based on the specified temporal.
    * A {@code TemporalAccessor} represents an arbitrary set of date and time information,
    * which this factory converts to an instance of {@code ThaiBuddhistDate}.
    *
    * The conversion typically uses the {@link ChronoField#EPOCH_DAY EPOCH_DAY}
    * field, which is standardized across calendar systems.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code ThaiBuddhistDate::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the date in Thai Buddhist calendar system, not null
    * @throws DateTimeException if unable to convert to a { @code ThaiBuddhistDate}
    */
  def from(temporal: TemporalAccessor): ThaiBuddhistDate =
    ThaiBuddhistChronology.INSTANCE.date(temporal)

  @throws[IOException]
  private[chrono] def readExternal(in: DataInput): ChronoLocalDate = {
    val year: Int       = in.readInt
    val month: Int      = in.readByte.toInt
    val dayOfMonth: Int = in.readByte.toInt
    ThaiBuddhistChronology.INSTANCE.date(year, month, dayOfMonth)
  }
}

/** A date in the Thai Buddhist calendar system.
  *
  * This date operates using the {@linkplain ThaiBuddhistChronology Thai Buddhist calendar}.
  * This calendar system is primarily used in Thailand.
  * Dates are aligned such that {@code 2484-01-01 (Buddhist)} is {@code 1941-01-01 (ISO)}.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor Creates an instance from an ISO date.
  *
  * @param isoDate  the standard local date, validated not null
  */
@SerialVersionUID(-8722293800195731463L)
final class ThaiBuddhistDate private[chrono] (private val isoDate: LocalDate)
    extends ChronoDateImpl[ThaiBuddhistDate]
    with Serializable {
  Objects.requireNonNull(isoDate, "date")

  def getChronology: ThaiBuddhistChronology = ThaiBuddhistChronology.INSTANCE

  override def getEra: ThaiBuddhistEra = super.getEra.asInstanceOf[ThaiBuddhistEra]

  def lengthOfMonth: Int = isoDate.lengthOfMonth

  override def range(field: TemporalField): ValueRange =
    field match {
      case f: ChronoField =>
        if (isSupported(field)) {
          f match {
            case DAY_OF_MONTH | DAY_OF_YEAR | ALIGNED_WEEK_OF_MONTH => isoDate.range(field)
            case YEAR_OF_ERA =>
              val range: ValueRange = YEAR.range
              val max: Long =
                if (getProlepticYear <= 0) -(range.getMinimum + YEARS_DIFFERENCE) + 1
                else range.getMaximum + YEARS_DIFFERENCE
              ValueRange.of(1, max)
            case _ => getChronology.range(f)
          }
        } else {
          throw throw UnsupportedTemporalTypeException.field(field)
        }
      case _ =>
        field.rangeRefinedBy(this)
    }

  def getLong(field: TemporalField): Long =
    field match {
      case PROLEPTIC_MONTH => getProlepticMonth
      case YEAR_OF_ERA =>
        val prolepticYear: Int = getProlepticYear
        if (prolepticYear >= 1) prolepticYear.toLong else 1 - prolepticYear.toLong
      case YEAR           => getProlepticYear.toLong
      case ERA            => if (getProlepticYear >= 1) 1 else 0
      case _: ChronoField => isoDate.getLong(field)
      case _              => field.getFrom(this)
    }

  private def getProlepticMonth: Long = getProlepticYear * 12L + isoDate.getMonthValue - 1

  private def getProlepticYear: Int = isoDate.getYear + YEARS_DIFFERENCE

  override def `with`(adjuster: TemporalAdjuster): ThaiBuddhistDate =
    super.`with`(adjuster).asInstanceOf[ThaiBuddhistDate]

  def `with`(field: TemporalField, newValue: Long): ThaiBuddhistDate = {
    field match {
      case f: ChronoField =>
        if (getLong(f) == newValue)
          return this
        f match {
          case PROLEPTIC_MONTH =>
            getChronology.range(f).checkValidValue(newValue, f)
            return plusMonths(newValue - getProlepticMonth)
          case YEAR_OF_ERA | YEAR | ERA =>
            val nvalue: Int = getChronology.range(f).checkValidIntValue(newValue, f)
            f match {
              case YEAR_OF_ERA =>
                return `with`(
                  isoDate.withYear(
                    (if (getProlepticYear >= 1) nvalue else 1 - nvalue) - YEARS_DIFFERENCE
                  )
                )
              case YEAR =>
                return `with`(isoDate.withYear(nvalue - YEARS_DIFFERENCE))
              case ERA =>
                return `with`(isoDate.withYear((1 - getProlepticYear) - YEARS_DIFFERENCE))
            }
          case _ =>
            return `with`(isoDate.`with`(field, newValue))
        }
      case _ =>
    }
    field.adjustInto(this, newValue)
  }

  override def plus(amount: TemporalAmount): ThaiBuddhistDate =
    super.plus(amount).asInstanceOf[ThaiBuddhistDate]

  override def plus(amountToAdd: Long, unit: TemporalUnit): ThaiBuddhistDate =
    super.plus(amountToAdd, unit).asInstanceOf[ThaiBuddhistDate]

  override def minus(amount: TemporalAmount): ThaiBuddhistDate =
    super.minus(amount).asInstanceOf[ThaiBuddhistDate]

  override def minus(amountToAdd: Long, unit: TemporalUnit): ThaiBuddhistDate =
    super.minus(amountToAdd, unit).asInstanceOf[ThaiBuddhistDate]

  private[chrono] def plusYears(years: Long): ThaiBuddhistDate = `with`(isoDate.plusYears(years))

  private[chrono] def plusMonths(months: Long): ThaiBuddhistDate =
    `with`(isoDate.plusMonths(months))

  private[chrono] def plusDays(days: Long): ThaiBuddhistDate = `with`(isoDate.plusDays(days))

  private def `with`(newDate: LocalDate): ThaiBuddhistDate =
    if (newDate == isoDate) this
    else new ThaiBuddhistDate(newDate)

  override def atTime(localTime: LocalTime): ChronoLocalDateTime[ThaiBuddhistDate] =
    super.atTime(localTime).asInstanceOf[ChronoLocalDateTime[ThaiBuddhistDate]]

  override def until(endDate: ChronoLocalDate): ChronoPeriod = {
    val period: Period = isoDate.until(endDate)
    getChronology.period(period.getYears, period.getMonths, period.getDays)
  }

  override def toEpochDay: Long = isoDate.toEpochDay

  override def equals(obj: Any): Boolean =
    obj match {
      case otherDate: ThaiBuddhistDate => (this eq otherDate) || this.isoDate == otherDate.isoDate
      case _                           => false
    }

  override def hashCode: Int = getChronology.getId.hashCode ^ isoDate.hashCode

  private def writeReplace: AnyRef = new Ser(Ser.THAIBUDDHIST_DATE_TYPE, this)

  @throws[IOException]
  private[chrono] def writeExternal(out: DataOutput): Unit = {
    out.writeInt(this.get(YEAR))
    out.writeByte(this.get(MONTH_OF_YEAR))
    out.writeByte(this.get(DAY_OF_MONTH))
  }
}
