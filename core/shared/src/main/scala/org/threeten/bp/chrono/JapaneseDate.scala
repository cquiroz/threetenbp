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

import org.threeten.bp.temporal.ChronoField._
import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.ObjectInputStream
import java.io.Serializable
import java.util.{ Calendar, Objects }
import org.threeten.bp.Clock
import org.threeten.bp.DateTimeException
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

@SerialVersionUID(-305327627230580483L)
object JapaneseDate {

  /** Minimum date. */
  private[chrono] def MIN_DATE: LocalDate = LocalDate.of(1873, 1, 1)

  /** Obtains the current {@code JapaneseDate} from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current date.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current date using the system clock and default time-zone, not null
    */
  def now: JapaneseDate = now(Clock.systemDefaultZone)

  /** Obtains the current {@code JapaneseDate} from the system clock in the specified time-zone.
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
  def now(zone: ZoneId): JapaneseDate = now(Clock.system(zone))

  /** Obtains the current {@code JapaneseDate} from the specified clock.
    *
    * This will query the specified clock to obtain the current date - today.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@linkplain Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current date, not null
    * @throws DateTimeException if the current date cannot be obtained
    */
  def now(clock: Clock): JapaneseDate = new JapaneseDate(LocalDate.now(clock))

  /** Obtains a {@code JapaneseDate} representing a date in the Japanese calendar
    * system from the era, year-of-era, month-of-year and day-of-month fields.
    *
    * This returns a {@code JapaneseDate} with the specified fields.
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * The Japanese month and day-of-month are the same as those in the
    * ISO calendar system. They are not reset when the era changes.
    * For example:
    * <pre>
    * 6th Jan Showa 64 = ISO 1989-01-06
    * 7th Jan Showa 64 = ISO 1989-01-07
    * 8th Jan Heisei 1 = ISO 1989-01-08
    * 9th Jan Heisei 1 = ISO 1989-01-09
    * </pre>
    *
    * @param era  the Japanese era, not null
    * @param yearOfEra  the Japanese year-of-era
    * @param month  the Japanese month-of-year, from 1 to 12
    * @param dayOfMonth  the Japanese day-of-month, from 1 to 31
    * @return the date in Japanese calendar system, not null
    * @throws DateTimeException if the value of any field is out of range,
    *                           or if the day-of-month is invalid for the month-year
    */
  def of(era: JapaneseEra, yearOfEra: Int, month: Int, dayOfMonth: Int): JapaneseDate = {
    Objects.requireNonNull(era, "era")
    if (yearOfEra < 1)
      throw new DateTimeException(s"Invalid YearOfEra: $yearOfEra")
    val eraStartDate: LocalDate = era.startDate
    val eraEndDate: LocalDate   = era.endDate
    val yearOffset: Int         = eraStartDate.getYear - 1
    val date: LocalDate         = LocalDate.of(yearOfEra + yearOffset, month, dayOfMonth)
    if (date.isBefore(eraStartDate) || date.isAfter(eraEndDate))
      throw new DateTimeException(s"Requested date is outside bounds of era $era")
    new JapaneseDate(era, yearOfEra, date)
  }

  /** Obtains a {@code JapaneseDate} representing a date in the Japanese calendar
    * system from the era, year-of-era and day-of-year fields.
    *
    * This returns a {@code JapaneseDate} with the specified fields.
    * The day must be valid for the year, otherwise an exception will be thrown.
    * The Japanese day-of-year is reset when the era changes.
    *
    * @param era  the Japanese era, not null
    * @param yearOfEra  the Japanese year-of-era
    * @param dayOfYear  the Japanese day-of-year, from 1 to 31
    * @return the date in Japanese calendar system, not null
    * @throws DateTimeException if the value of any field is out of range,
    *                           or if the day-of-year is invalid for the year
    */
  private[chrono] def ofYearDay(era: JapaneseEra, yearOfEra: Int, dayOfYear: Int): JapaneseDate = {
    var _dayOfYear = dayOfYear
    Objects.requireNonNull(era, "era")
    if (yearOfEra < 1)
      throw new DateTimeException(s"Invalid YearOfEra: $yearOfEra")
    val eraStartDate: LocalDate = era.startDate
    val eraEndDate: LocalDate   = era.endDate
    if (yearOfEra == 1) {
      _dayOfYear += eraStartDate.getDayOfYear - 1
      if (_dayOfYear > eraStartDate.lengthOfYear)
        throw new DateTimeException(
          s"DayOfYear exceeds maximum allowed in the first year of era $era"
        )
    }
    val yearOffset: Int    = eraStartDate.getYear - 1
    val isoDate: LocalDate = LocalDate.ofYearDay(yearOfEra + yearOffset, _dayOfYear)
    if (isoDate.isBefore(eraStartDate) || isoDate.isAfter(eraEndDate))
      throw new DateTimeException(s"Requested date is outside bounds of era $era")
    new JapaneseDate(era, yearOfEra, isoDate)
  }

  /** Obtains a {@code JapaneseDate} representing a date in the Japanese calendar
    * system from the proleptic-year, month-of-year and day-of-month fields.
    *
    * This returns a {@code JapaneseDate} with the specified fields.
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * The Japanese proleptic year, month and day-of-month are the same as those
    * in the ISO calendar system. They are not reset when the era changes.
    *
    * @param prolepticYear  the Japanese proleptic-year
    * @param month  the Japanese month-of-year, from 1 to 12
    * @param dayOfMonth  the Japanese day-of-month, from 1 to 31
    * @return the date in Japanese calendar system, not null
    * @throws DateTimeException if the value of any field is out of range,
    *                           or if the day-of-month is invalid for the month-year
    */
  def of(prolepticYear: Int, month: Int, dayOfMonth: Int): JapaneseDate =
    new JapaneseDate(LocalDate.of(prolepticYear, month, dayOfMonth))

  /** Obtains a {@code JapaneseDate} from a temporal object.
    *
    * This obtains a date in the Japanese calendar system based on the specified temporal.
    * A {@code TemporalAccessor} represents an arbitrary set of date and time information,
    * which this factory converts to an instance of {@code JapaneseDate}.
    *
    * The conversion typically uses the {@link ChronoField#EPOCH_DAY EPOCH_DAY}
    * field, which is standardized across calendar systems.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code JapaneseDate::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the date in Japanese calendar system, not null
    * @throws DateTimeException if unable to convert to a { @code JapaneseDate}
    */
  def from(temporal: TemporalAccessor): JapaneseDate = JapaneseChronology.INSTANCE.date(temporal)

  @throws[IOException]
  private[chrono] def readExternal(in: DataInput): ChronoLocalDate = {
    val year: Int       = in.readInt
    val month: Int      = in.readByte.toInt
    val dayOfMonth: Int = in.readByte.toInt
    JapaneseChronology.INSTANCE.date(year, month, dayOfMonth)
  }
}

/** A date in the Japanese Imperial calendar system.
  *
  * This date operates using the {@linkplain JapaneseChronology Japanese Imperial calendar}.
  * This calendar system is primarily used in Japan.
  *
  * The Japanese Imperial calendar system is the same as the ISO calendar system
  * apart from the era-based year numbering. The proleptic-year is defined to be
  * equal to the ISO proleptic-year.
  *
  * Japan introduced the Gregorian calendar starting with Meiji 6.
  * Only Meiji and later eras are supported.
  *
  * For example, the Japanese year "Heisei 24" corresponds to ISO year "2012".<br>
  * Calling {@code japaneseDate.get(YEAR_OF_ERA)} will return 24.<br>
  * Calling {@code japaneseDate.get(YEAR)} will return 2012.<br>
  * Calling {@code japaneseDate.get(ERA)} will return 2, corresponding to
  * {@code JapaneseChEra.HEISEI}.<br>
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor Constructs a {@code JapaneseDate}.
  *
  * This constructor does NOT validate the given parameters,
  * and {@code era} and {@code year} must agree with {@code isoDate}.
  * @param era  the era, validated not null
  * @param yearOfEra  the year-of-era, validated
  * @param isoDate  the standard local date, validated not null
  */
@SerialVersionUID(-305327627230580483L)
final class JapaneseDate private[chrono] (
  @transient private var era:       JapaneseEra,
  @transient private var yearOfEra: Int,
  private val isoDate:              LocalDate
) extends ChronoDateImpl[JapaneseDate]
    with Serializable {
  if (isoDate.isBefore(JapaneseDate.MIN_DATE))
    throw new DateTimeException("Minimum supported date is January 1st Meiji 6")

  /** Creates an instance from an ISO date.
    *
    * @param isoDate  the standard local date, validated not null
    */
  private[chrono] def this(isoDate: LocalDate) {
    // !!!! FIXME: JapaneseEra.from(isoDate) is called twice, because this call must be first ...
    this(JapaneseEra.from(isoDate),
         isoDate.getYear - (JapaneseEra.from(isoDate).startDate.getYear - 1),
         isoDate)
  }

  /** Reconstitutes this object from a stream.
    *
    * @param stream object input stream
    */
  @throws[IOException]
  @throws[ClassNotFoundException]
  private def readObject(stream: ObjectInputStream): Unit = {
    stream.defaultReadObject()
    this.era = JapaneseEra.from(isoDate)
    val yearOffset: Int = this.era.startDate.getYear - 1
    this.yearOfEra = isoDate.getYear - yearOffset
  }

  def getChronology: JapaneseChronology = JapaneseChronology.INSTANCE

  override def getEra: JapaneseEra = era

  def lengthOfMonth: Int = isoDate.lengthOfMonth

  override def lengthOfYear: Int = {
    val jcal: Calendar = Calendar.getInstance(JapaneseChronology.LOCALE)
    jcal.set(Calendar.ERA, era.getValue + JapaneseEra.ERA_OFFSET)
    jcal.set(yearOfEra, isoDate.getMonthValue - 1, isoDate.getDayOfMonth)
    jcal.getActualMaximum(Calendar.DAY_OF_YEAR)
  }

  /** Checks if the specified field is supported.
    *
    * This checks if this date can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The supported fields are:
    * <ul>
    * <li>{@code DAY_OF_WEEK}
    * <li>{@code DAY_OF_MONTH}
    * <li>{@code DAY_OF_YEAR}
    * <li>{@code EPOCH_DAY}
    * <li>{@code MONTH_OF_YEAR}
    * <li>{@code PROLEPTIC_MONTH}
    * <li>{@code YEAR_OF_ERA}
    * <li>{@code YEAR}
    * <li>{@code ERA}
    * </ul>
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this date, false if not
    */
  override def isSupported(field: TemporalField): Boolean =
    field match {
      case ALIGNED_DAY_OF_WEEK_IN_MONTH | ALIGNED_DAY_OF_WEEK_IN_YEAR | ALIGNED_WEEK_OF_MONTH |
          ALIGNED_WEEK_OF_YEAR =>
        false
      case _ => super.isSupported(field)
    }

  override def range(field: TemporalField): ValueRange =
    field match {
      case f: ChronoField =>
        if (isSupported(field)) {
          f match {
            case DAY_OF_YEAR => actualRange(Calendar.DAY_OF_YEAR)
            case YEAR_OF_ERA => actualRange(Calendar.YEAR)
            case _           => getChronology.range(f)
          }
        } else {
          throw throw UnsupportedTemporalTypeException.field(field)
        }
      case _ =>
        field.rangeRefinedBy(this)
    }

  private def actualRange(calendarField: Int): ValueRange = {
    val jcal: Calendar = Calendar.getInstance(JapaneseChronology.LOCALE)
    jcal.set(Calendar.ERA, era.getValue + JapaneseEra.ERA_OFFSET)
    jcal.set(yearOfEra, isoDate.getMonthValue - 1, isoDate.getDayOfMonth)
    ValueRange.of(jcal.getActualMinimum(calendarField).toLong,
                  jcal.getActualMaximum(calendarField).toLong)
  }

  def getLong(field: TemporalField): Long =
    field match {
      case f: ChronoField =>
        f match {
          case ALIGNED_DAY_OF_WEEK_IN_MONTH | ALIGNED_DAY_OF_WEEK_IN_YEAR | ALIGNED_WEEK_OF_MONTH |
              ALIGNED_WEEK_OF_YEAR =>
            throw throw UnsupportedTemporalTypeException.field(field)
          case YEAR_OF_ERA => yearOfEra.toLong
          case ERA         => era.getValue.toLong
          case DAY_OF_YEAR => getDayOfYear
          case _           => isoDate.getLong(field)
        }
      case _ =>
        field.getFrom(this)
    }

  private def getDayOfYear: Long =
    if (yearOfEra == 1)
      isoDate.getDayOfYear.toLong - era.startDate.getDayOfYear.toLong + 1L
    else
      isoDate.getDayOfYear.toLong

  override def `with`(adjuster: TemporalAdjuster): JapaneseDate =
    super.`with`(adjuster).asInstanceOf[JapaneseDate]

  def `with`(field: TemporalField, newValue: Long): JapaneseDate =
    field match {
      case f: ChronoField =>
        if (getLong(f) == newValue)
          return this
        f match {
          case DAY_OF_YEAR | YEAR_OF_ERA | ERA =>
            val nvalue: Int = getChronology.range(f).checkValidIntValue(newValue, f)
            f match {
              case DAY_OF_YEAR => `with`(isoDate.plusDays(nvalue - getDayOfYear))
              case YEAR_OF_ERA => this.withYear(nvalue)
              case ERA         => this.withYear(JapaneseEra.of(nvalue), yearOfEra)
            }
          case _ => `with`(isoDate.`with`(field, newValue))
        }
      case _ =>
        field.adjustInto(this, newValue)
    }

  override def plus(amount: TemporalAmount): JapaneseDate =
    super.plus(amount).asInstanceOf[JapaneseDate]

  override def plus(amountToAdd: Long, unit: TemporalUnit): JapaneseDate =
    super.plus(amountToAdd, unit).asInstanceOf[JapaneseDate]

  override def minus(amount: TemporalAmount): JapaneseDate =
    super.minus(amount).asInstanceOf[JapaneseDate]

  override def minus(amountToAdd: Long, unit: TemporalUnit): JapaneseDate =
    super.minus(amountToAdd, unit).asInstanceOf[JapaneseDate]

  /** Returns a copy of this date with the year altered.
    *
    * This method changes the year of the date.
    * If the month-day is invalid for the year, then the previous valid day
    * will be selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param era  the era to set in the result, not null
    * @param yearOfEra  the year-of-era to set in the returned date
    * @return a { @code JapaneseDate} based on this date with the requested year, never null
    * @throws DateTimeException if { @code year} is invalid
    */
  private def withYear(era: JapaneseEra, yearOfEra: Int): JapaneseDate = {
    val year: Int = JapaneseChronology.INSTANCE.prolepticYear(era, yearOfEra)
    `with`(isoDate.withYear(year))
  }

  /** Returns a copy of this date with the year-of-era altered.
    *
    * This method changes the year-of-era of the date.
    * If the month-day is invalid for the year, then the previous valid day
    * will be selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param year  the year to set in the returned date
    * @return a { @code JapaneseDate} based on this date with the requested year-of-era, never null
    * @throws DateTimeException if { @code year} is invalid
    */
  private def withYear(year: Int): JapaneseDate = withYear(getEra, year)

  private[chrono] def plusYears(years: Long): JapaneseDate = `with`(isoDate.plusYears(years))

  private[chrono] def plusMonths(months: Long): JapaneseDate = `with`(isoDate.plusMonths(months))

  private[chrono] def plusDays(days: Long): JapaneseDate = `with`(isoDate.plusDays(days))

  private def `with`(newDate: LocalDate): JapaneseDate =
    if (newDate == isoDate) this else new JapaneseDate(newDate)

  override def atTime(localTime: LocalTime): ChronoLocalDateTime[JapaneseDate] =
    super.atTime(localTime).asInstanceOf[ChronoLocalDateTime[JapaneseDate]]

  override def until(endDate: ChronoLocalDate): ChronoPeriod = {
    val period: Period = isoDate.until(endDate)
    getChronology.period(period.getYears, period.getMonths, period.getDays)
  }

  override def toEpochDay: Long = isoDate.toEpochDay

  override def equals(obj: Any): Boolean =
    obj match {
      case otherDate: JapaneseDate => (this eq otherDate) || (this.isoDate == otherDate.isoDate)
      case _                       => false
    }

  override def hashCode: Int = getChronology.getId.hashCode ^ isoDate.hashCode

  private def writeReplace: AnyRef = new Ser(Ser.JAPANESE_DATE_TYPE, this)

  @throws[IOException]
  private[chrono] def writeExternal(out: DataOutput): Unit = {
    out.writeInt(get(YEAR))
    out.writeByte(get(MONTH_OF_YEAR))
    out.writeByte(get(DAY_OF_MONTH))
  }
}
