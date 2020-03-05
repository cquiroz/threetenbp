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
package org.threeten.bp

import java.util.Objects
import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.InvalidObjectException
import java.io.ObjectStreamException
import java.io.Serializable

import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.Era
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.zone.ZoneOffsetTransition
import org.threeten.bp.zone.ZoneRules

@SerialVersionUID(2942565459149668126L)
object LocalDate {

  /** The minimum supported {@code LocalDate}, '-999999999-01-01'.
    * This could be used by an application as a "far past" date.
    */
  lazy val MIN: LocalDate = LocalDate.of(Year.MIN_VALUE, 1, 1)

  /** The maximum supported {@code LocalDate}, '+999999999-12-31'.
    * This could be used by an application as a "far future" date.
    */
  lazy val MAX: LocalDate = LocalDate.of(Year.MAX_VALUE, 12, 31)

  /** The number of days in a 400 year cycle. */
  private def DAYS_PER_CYCLE: Int = 146097

  /** The number of days from year zero to year 1970.
    * There are five 400 year cycles from year zero to 2000.
    * There are 7 leap years from 1970 to 2000.
    */
  private[bp] def DAYS_0000_TO_1970: Long = (DAYS_PER_CYCLE * 5L) - (30L * 365L + 7L)

  /** Obtains the current date from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current date.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current date using the system clock and default time-zone, not null
    */
  def now: LocalDate = now(Clock.systemDefaultZone)

  /** Obtains the current date from the system clock in the specified time-zone.
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
  def now(zone: ZoneId): LocalDate = now(Clock.system(zone))

  /** Obtains the current date from the specified clock.
    *
    * This will query the specified clock to obtain the current date - today.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current date, not null
    */
  def now(clock: Clock): LocalDate = {
    Objects.requireNonNull(clock, "clock")
    val now: Instant       = clock.instant
    val offset: ZoneOffset = clock.getZone.getRules.getOffset(now)
    val epochSec: Long     = now.getEpochSecond + offset.getTotalSeconds
    val epochDay: Long     = Math.floorDiv(epochSec, LocalTime.SECONDS_PER_DAY.toLong)
    LocalDate.ofEpochDay(epochDay)
  }

  /** Obtains an instance of {@code LocalDate} from a year, month and day.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, not null
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @return the local date, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(year: Int, month: Month, dayOfMonth: Int): LocalDate = {
    YEAR.checkValidValue(year.toLong)
    Objects.requireNonNull(month, "month")
    DAY_OF_MONTH.checkValidValue(dayOfMonth.toLong)
    create(year, month, dayOfMonth)
  }

  /** Obtains an instance of {@code LocalDate} from a year, month and day.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @return the local date, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(year: Int, month: Int, dayOfMonth: Int): LocalDate = {
    YEAR.checkValidValue(year.toLong)
    MONTH_OF_YEAR.checkValidValue(month.toLong)
    DAY_OF_MONTH.checkValidValue(dayOfMonth.toLong)
    create(year, Month.of(month), dayOfMonth)
  }

  /** Obtains an instance of {@code LocalDate} from a year and day-of-year.
    *
    * The day-of-year must be valid for the year, otherwise an exception will be thrown.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param dayOfYear  the day-of-year to represent, from 1 to 366
    * @return the local date, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-year is invalid for the month-year
    */
  def ofYearDay(year: Int, dayOfYear: Int): LocalDate = {
    YEAR.checkValidValue(year.toLong)
    DAY_OF_YEAR.checkValidValue(dayOfYear.toLong)
    val leap: Boolean = IsoChronology.INSTANCE.isLeapYear(year.toLong)
    if (dayOfYear == 366 && !leap)
      throw new DateTimeException(s"Invalid date 'DayOfYear 366' as '$year' is not a leap year")
    var moy: Month    = Month.of((dayOfYear - 1) / 31 + 1)
    val monthEnd: Int = moy.firstDayOfYear(leap) + moy.length(leap) - 1
    if (dayOfYear > monthEnd)
      moy = moy.plus(1)
    val dom: Int = dayOfYear - moy.firstDayOfYear(leap) + 1
    create(year, moy, dom)
  }

  /** Obtains an instance of {@code LocalDate} from the epoch day count.
    *
    * The Epoch Day count is a simple incrementing count of days
    * where day 0 is 1970-01-01. Negative numbers represent earlier days.
    *
    * @param epochDay  the Epoch Day to convert, based on the epoch 1970-01-01
    * @return the local date, not null
    * @throws DateTimeException if the epoch days exceeds the supported date range
    */
  def ofEpochDay(epochDay: Long): LocalDate = {
    EPOCH_DAY.checkValidValue(epochDay)
    var zeroDay: Long = epochDay + DAYS_0000_TO_1970
    zeroDay -= 60
    var adjust: Long = 0
    if (zeroDay < 0) {
      val adjustCycles: Long = (zeroDay + 1) / DAYS_PER_CYCLE - 1
      adjust = adjustCycles * 400
      zeroDay += -adjustCycles * DAYS_PER_CYCLE
    }
    var yearEst: Long = (400 * zeroDay + 591) / DAYS_PER_CYCLE
    var doyEst: Long  = zeroDay - (365 * yearEst + yearEst / 4 - yearEst / 100 + yearEst / 400)
    if (doyEst < 0) {
      yearEst -= 1
      doyEst = zeroDay - (365 * yearEst + yearEst / 4 - yearEst / 100 + yearEst / 400)
    }
    yearEst += adjust
    val marchDoy0: Int   = doyEst.toInt
    val marchMonth0: Int = (marchDoy0 * 5 + 2) / 153
    val month: Int       = (marchMonth0 + 2) % 12 + 1
    val dom: Int         = marchDoy0 - (marchMonth0 * 306 + 5) / 10 + 1
    yearEst += marchMonth0 / 10
    val year: Int = YEAR.checkValidIntValue(yearEst)
    new LocalDate(year, month, dom)
  }

  /** Obtains an instance of {@code LocalDate} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code LocalDate}.
    *
    * The conversion uses the {@link TemporalQueries#localDate()} query, which relies
    * on extracting the {@link ChronoField#EPOCH_DAY EPOCH_DAY} field.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code LocalDate::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the local date, not null
    * @throws DateTimeException if unable to convert to a { @code LocalDate}
    */
  def from(temporal: TemporalAccessor): LocalDate = {
    val date: LocalDate = temporal.query(TemporalQueries.localDate)
    if (date == null)
      throw new DateTimeException(
        s"Unable to obtain LocalDate from TemporalAccessor: $temporal, type ${temporal.getClass.getName}"
      )
    date
  }

  /** Obtains an instance of {@code LocalDate} from a text string such as {@code 2007-12-03}.
    *
    * The string must represent a valid date and is parsed using
    * {@link org.threeten.bp.format.DateTimeFormatter#ISO_LOCAL_DATE}.
    *
    * @param text  the text to parse such as "2007-12-03", not null
    * @return the parsed local date, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): LocalDate = parse(text, DateTimeFormatter.ISO_LOCAL_DATE)

  /** Obtains an instance of {@code LocalDate} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a date.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed local date, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): LocalDate = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[LocalDate] {
      override def queryFrom(temporal: TemporalAccessor): LocalDate = LocalDate.from(temporal)
    })
  }

  /** Creates a local date from the year, month and day fields.
    *
    * @param year  the year to represent, validated from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, validated not null
    * @param dayOfMonth  the day-of-month to represent, validated from 1 to 31
    * @return the local date, not null
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  private def create(year: Int, month: Month, dayOfMonth: Int): LocalDate =
    if (dayOfMonth > 28 && dayOfMonth > month.length(
          IsoChronology.INSTANCE.isLeapYear(year.toLong)
        ))
      if (dayOfMonth == 29)
        throw new DateTimeException(s"Invalid date 'February 29' as '$year' is not a leap year")
      else throw new DateTimeException(s"Invalid date '${month.name} $dayOfMonth'")
    else
      new LocalDate(year, month.getValue, dayOfMonth)

  /** Resolves the date, resolving days past the end of month.
    *
    * @param year  the year to represent, validated from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, validated from 1 to 12
    * @param day  the day-of-month to represent, validated from 1 to 31
    * @return the resolved date, not null
    */
  private def resolvePreviousValid(year: Int, month: Int, day: Int): LocalDate = {
    val _day = month match {
      case 2              => Math.min(day, if (IsoChronology.INSTANCE.isLeapYear(year.toLong)) 29 else 28)
      case 4 | 6 | 9 | 11 => Math.min(day, 30)
      case _              => day
    }
    LocalDate.of(year, month, _day)
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): LocalDate = {
    val year: Int       = in.readInt
    val month: Int      = in.readByte.toInt
    val dayOfMonth: Int = in.readByte.toInt
    LocalDate.of(year, month, dayOfMonth)
  }
}

/** A date without a time-zone in the ISO-8601 calendar system,
  * such as {@code 2007-12-03}.
  *
  * {@code LocalDate} is an immutable date-time object that represents a date,
  * often viewed as year-month-day. Other date fields, such as day-of-year,
  * day-of-week and week-of-year, can also be accessed.
  * For example, the value "2nd October 2007" can be stored in a {@code LocalDate}.
  *
  * This class does not store or represent a time or time-zone.
  * Instead, it is a description of the date, as used for birthdays.
  * It cannot represent an instant on the time-line without additional information
  * such as an offset or time-zone.
  *
  * The ISO-8601 calendar system is the modern civil calendar system used today
  * in most of the world. It is equivalent to the proleptic Gregorian calendar
  * system, in which today's rules for leap years are applied for all time.
  * For most applications written today, the ISO-8601 rules are entirely suitable.
  * However, any application that makes use of historical dates, and requires them
  * to be accurate will find the ISO-8601 approach unsuitable.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor Constructor, previously validated.
  * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
  * @param monthOfYear  the month-of-year to represent, not null
  * @param dayOfMonth  the day-of-month to represent, valid for year-month, from 1 to 31
  */
@SerialVersionUID(2942565459149668126L)
final class LocalDate private (private val year: Int, monthOfYear: Int, dayOfMonth: Int)
    extends ChronoLocalDate
    with Temporal
    with TemporalAdjuster
    with Serializable {

  /** The month-of-year. */
  private val month: Short = monthOfYear.toShort

  /** The day-of-month. */
  private val day: Short = dayOfMonth.toShort

  /** Checks if the specified field is supported.
    *
    * This checks if this date can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time.
    * The supported fields are:
    * <ul>
    * <li>{@code DAY_OF_WEEK}
    * <li>{@code ALIGNED_DAY_OF_WEEK_IN_MONTH}
    * <li>{@code ALIGNED_DAY_OF_WEEK_IN_YEAR}
    * <li>{@code DAY_OF_MONTH}
    * <li>{@code DAY_OF_YEAR}
    * <li>{@code EPOCH_DAY}
    * <li>{@code ALIGNED_WEEK_OF_MONTH}
    * <li>{@code ALIGNED_WEEK_OF_YEAR}
    * <li>{@code MONTH_OF_YEAR}
    * <li>{@code EPOCH_MONTH}
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
  override def isSupported(field: TemporalField): Boolean = super.isSupported(field)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This date is used to enhance the accuracy of the returned range.
    * If it is not possible to return the range, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return
    * appropriate range instances.
    * All other {@code ChronoField} instances will throw a {@code DateTimeException}.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.rangeRefinedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the range can be obtained is determined by the field.
    *
    * @param field  the field to query the range for, not null
    * @return the range of valid values for the field, not null
    * @throws DateTimeException if the range for the field cannot be obtained
    */
  override def range(field: TemporalField): ValueRange =
    if (field.isInstanceOf[ChronoField]) {
      val f: ChronoField = field.asInstanceOf[ChronoField]
      if (f.isDateBased) {
        f match {
          case DAY_OF_MONTH => ValueRange.of(1, lengthOfMonth.toLong)
          case DAY_OF_YEAR  => ValueRange.of(1, lengthOfYear.toLong)
          case ALIGNED_WEEK_OF_MONTH =>
            ValueRange.of(1, if ((getMonth eq Month.FEBRUARY) && !isLeapYear) 4 else 5)
          case YEAR_OF_ERA =>
            if (getYear <= 0) ValueRange.of(1, Year.MAX_VALUE + 1L)
            else ValueRange.of(1, Year.MAX_VALUE.toLong)
          case _ => field.range
        }
      } else {
        throw UnsupportedTemporalTypeException.field(field)
      }
    } else {
      field.rangeRefinedBy(this)
    }

  /** Gets the value of the specified field from this date as an {@code int}.
    *
    * This queries this date for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date, except {@code EPOCH_DAY} and {@code EPOCH_MONTH}
    * which are too large to fit in an {@code int} and throw a {@code DateTimeException}.
    * All other {@code ChronoField} instances will throw a {@code DateTimeException}.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.getFrom(TemporalAccessor)}
    * passing {@code this} as the argument. Whether the value can be obtained,
    * and what the value represents, is determined by the field.
    *
    * @param field  the field to get, not null
    * @return the value for the field
    * @throws DateTimeException if a value for the field cannot be obtained
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def get(field: TemporalField): Int =
    if (field.isInstanceOf[ChronoField]) get0(field).toInt
    else super.get(field)

  /** Gets the value of the specified field from this date as a {@code long}.
    *
    * This queries this date for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date.
    * All other {@code ChronoField} instances will throw a {@code DateTimeException}.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.getFrom(TemporalAccessor)}
    * passing {@code this} as the argument. Whether the value can be obtained,
    * and what the value represents, is determined by the field.
    *
    * @param field  the field to get, not null
    * @return the value for the field
    * @throws DateTimeException if a value for the field cannot be obtained
    * @throws ArithmeticException if numeric overflow occurs
    */
  def getLong(field: TemporalField): Long =
    if (field.isInstanceOf[ChronoField]) {
      if (field eq EPOCH_DAY)
        return toEpochDay
      if (field eq PROLEPTIC_MONTH)
        return getProlepticMonth
      get0(field)
    } else {
      field.getFrom(this)
    }

  private def get0(field: TemporalField): Long =
    field.asInstanceOf[ChronoField] match {
      case DAY_OF_WEEK                  => getDayOfWeek.getValue.toLong
      case ALIGNED_DAY_OF_WEEK_IN_MONTH => ((day - 1L) % 7) + 1
      case ALIGNED_DAY_OF_WEEK_IN_YEAR  => ((getDayOfYear - 1L) % 7) + 1
      case DAY_OF_MONTH                 => day.toLong
      case DAY_OF_YEAR                  => getDayOfYear.toLong
      case EPOCH_DAY                    => throw new DateTimeException(s"Field too large for an int: $field")
      case ALIGNED_WEEK_OF_MONTH        => ((day - 1L) / 7) + 1
      case ALIGNED_WEEK_OF_YEAR         => ((getDayOfYear - 1L) / 7) + 1
      case MONTH_OF_YEAR                => month.toLong
      case PROLEPTIC_MONTH              => throw new DateTimeException(s"Field too large for an int: $field")
      case YEAR_OF_ERA                  => if (year >= 1) year.toLong else 1 - year.toLong
      case YEAR                         => year.toLong
      case ERA                          => if (year >= 1) 1 else 0
      case _                            => throw UnsupportedTemporalTypeException.field(field)
    }

  private def getProlepticMonth: Long = (year * 12L) + (month - 1)

  /** Gets the chronology of this date, which is the ISO calendar system.
    *
    * The {@code Chronology} represents the calendar system in use.
    * The ISO-8601 calendar system is the modern civil calendar system used today
    * in most of the world. It is equivalent to the proleptic Gregorian calendar
    * system, in which todays's rules for leap years are applied for all time.
    *
    * @return the ISO chronology, not null
    */
  def getChronology: IsoChronology = IsoChronology.INSTANCE

  /** Gets the era applicable at this date.
    *
    * The official ISO-8601 standard does not define eras, however {@code IsoChronology} does.
    * It defines two eras, 'CE' from year one onwards and 'BCE' from year zero backwards.
    * Since dates before the Julian-Gregorian cutover are not in line with history,
    * the cutover between 'BCE' and 'CE' is also not aligned with the commonly used
    * eras, often referred to using 'BC' and 'AD'.
    *
    * Users of this class should typically ignore this method as it exists primarily
    * to fulfill the {@link ChronoLocalDate} contract where it is necessary to support
    * the Japanese calendar system.
    *
    * The returned era will be a singleton capable of being compared with the constants
    * in {@link IsoChronology} using the {@code ==} operator.
    *
    * @return the { @code IsoChronology} era constant applicable at this date, not null
    */
  override def getEra: Era = super.getEra

  /** Gets the year field.
    *
    * This method returns the primitive {@code int} value for the year.
    *
    * The year returned by this method is proleptic as per {@code get(YEAR)}.
    * To obtain the year-of-era, use {@code get(YEAR_OF_ERA}.
    *
    * @return the year, from MIN_YEAR to MAX_YEAR
    */
  def getYear: Int = year

  /** Gets the month-of-year field from 1 to 12.
    *
    * This method returns the month as an {@code int} from 1 to 12.
    * Application code is frequently clearer if the enum {@link Month}
    * is used by calling {@link #getMonth()}.
    *
    * @return the month-of-year, from 1 to 12
    * @see #getMonth()
    */
  def getMonthValue: Int = month.toInt

  /** Gets the month-of-year field using the {@code Month} enum.
    *
    * This method returns the enum {@link Month} for the month.
    * This avoids confusion as to what {@code int} values mean.
    * If you need access to the primitive {@code int} value then the enum
    * provides the {@link Month#getValue() int value}.
    *
    * @return the month-of-year, not null
    * @see #getMonthValue()
    */
  def getMonth: Month = Month.of(month.toInt)

  /** Gets the day-of-month field.
    *
    * This method returns the primitive {@code int} value for the day-of-month.
    *
    * @return the day-of-month, from 1 to 31
    */
  def getDayOfMonth: Int = day.toInt

  /** Gets the day-of-year field.
    *
    * This method returns the primitive {@code int} value for the day-of-year.
    *
    * @return the day-of-year, from 1 to 365, or 366 in a leap year
    */
  def getDayOfYear: Int = getMonth.firstDayOfYear(isLeapYear) + day - 1

  /** Gets the day-of-week field, which is an enum {@code DayOfWeek}.
    *
    * This method returns the enum {@link DayOfWeek} for the day-of-week.
    * This avoids confusion as to what {@code int} values mean.
    * If you need access to the primitive {@code int} value then the enum
    * provides the {@link DayOfWeek#getValue() int value}.
    *
    * Additional information can be obtained from the {@code DayOfWeek}.
    * This includes textual names of the values.
    *
    * @return the day-of-week, not null
    */
  def getDayOfWeek: DayOfWeek = {
    val dow0: Int = Math.floorMod(toEpochDay + 3, 7).toInt
    DayOfWeek.of(dow0 + 1)
  }

  /** Checks if the year is a leap year, according to the ISO proleptic
    * calendar system rules.
    *
    * This method applies the current rules for leap years across the whole time-line.
    * In general, a year is a leap year if it is divisible by four without
    * remainder. However, years divisible by 100, are not leap years, with
    * the exception of years divisible by 400 which are.
    *
    * For example, 1904 is a leap year it is divisible by 4.
    * 1900 was not a leap year as it is divisible by 100, however 2000 was a
    * leap year as it is divisible by 400.
    *
    * The calculation is proleptic - applying the same rules into the far future and far past.
    * This is historically inaccurate, but is correct for the ISO-8601 standard.
    *
    * @return true if the year is leap, false otherwise
    */
  override def isLeapYear: Boolean = IsoChronology.INSTANCE.isLeapYear(year.toLong)

  /** Returns the length of the month represented by this date.
    *
    * This returns the length of the month in days.
    * For example, a date in January would return 31.
    *
    * @return the length of the month in days
    */
  def lengthOfMonth: Int =
    month match {
      case 2              => if (isLeapYear) 29 else 28
      case 4 | 6 | 9 | 11 => 30
      case _              => 31
    }

  /** Returns the length of the year represented by this date.
    *
    * This returns the length of the year in days, either 365 or 366.
    *
    * @return 366 if the year is leap, 365 otherwise
    */
  override def lengthOfYear: Int = if (isLeapYear) 366 else 365

  /** Returns an adjusted copy of this date.
    *
    * This returns a new {@code LocalDate}, based on this one, with the date adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * A simple adjuster might simply set the one of the fields, such as the year field.
    * A more complex adjuster might set the date to the last day of the month.
    * A selection of common adjustments is provided in {@link TemporalAdjusters}.
    * These include finding the "last day of the month" and "next Wednesday".
    * Key date-time classes also implement the {@code TemporalAdjuster} interface,
    * such as {@link Month} and {@link MonthDay}.
    * The adjuster is responsible for handling special cases, such as the varying
    * lengths of month and leap years.
    *
    * For example this code returns a date on the last day of July:
    * <pre>
    * import static org.threeten.bp.Month.*;
    * import static org.threeten.bp.temporal.Adjusters.*;
    *
    * result = localDate.with(JULY).with(lastDayOfMonth());
    * </pre>
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return a { @code LocalDate} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): LocalDate =
    if (adjuster.isInstanceOf[LocalDate]) adjuster.asInstanceOf[LocalDate]
    else adjuster.adjustInto(this).asInstanceOf[LocalDate]

  /** Returns a copy of this date with the specified field set to a new value.
    *
    * This returns a new {@code LocalDate}, based on this one, with the value
    * for the specified field changed.
    * This can be used to change any supported field, such as the year, month or day-of-month.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * In some cases, changing the specified field can cause the resulting date to become invalid,
    * such as changing the month from 31st January to February would make the day-of-month invalid.
    * In cases like this, the field is responsible for resolving the date. Typically it will choose
    * the previous valid date, which would be the last valid day of February in this example.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The supported fields behave as follows:
    * <ul>
    * <li>{@code DAY_OF_WEEK} -
    * Returns a {@code LocalDate} with the specified day-of-week.
    * The date is adjusted up to 6 days forward or backward within the boundary
    * of a Monday to Sunday week.
    * <li>{@code ALIGNED_DAY_OF_WEEK_IN_MONTH} -
    * Returns a {@code LocalDate} with the specified aligned-day-of-week.
    * The date is adjusted to the specified month-based aligned-day-of-week.
    * Aligned weeks are counted such that the first week of a given month starts
    * on the first day of that month.
    * This may cause the date to be moved up to 6 days into the following month.
    * <li>{@code ALIGNED_DAY_OF_WEEK_IN_YEAR} -
    * Returns a {@code LocalDate} with the specified aligned-day-of-week.
    * The date is adjusted to the specified year-based aligned-day-of-week.
    * Aligned weeks are counted such that the first week of a given year starts
    * on the first day of that year.
    * This may cause the date to be moved up to 6 days into the following year.
    * <li>{@code DAY_OF_MONTH} -
    * Returns a {@code LocalDate} with the specified day-of-month.
    * The month and year will be unchanged. If the day-of-month is invalid for the
    * year and month, then a {@code DateTimeException} is thrown.
    * <li>{@code DAY_OF_YEAR} -
    * Returns a {@code LocalDate} with the specified day-of-year.
    * The year will be unchanged. If the day-of-year is invalid for the
    * year, then a {@code DateTimeException} is thrown.
    * <li>{@code EPOCH_DAY} -
    * Returns a {@code LocalDate} with the specified epoch-day.
    * This completely replaces the date and is equivalent to {@link #ofEpochDay(long)}.
    * <li>{@code ALIGNED_WEEK_OF_MONTH} -
    * Returns a {@code LocalDate} with the specified aligned-week-of-month.
    * Aligned weeks are counted such that the first week of a given month starts
    * on the first day of that month.
    * This adjustment moves the date in whole week chunks to match the specified week.
    * The result will have the same day-of-week as this date.
    * This may cause the date to be moved into the following month.
    * <li>{@code ALIGNED_WEEK_OF_YEAR} -
    * Returns a {@code LocalDate} with the specified aligned-week-of-year.
    * Aligned weeks are counted such that the first week of a given year starts
    * on the first day of that year.
    * This adjustment moves the date in whole week chunks to match the specified week.
    * The result will have the same day-of-week as this date.
    * This may cause the date to be moved into the following year.
    * <li>{@code MONTH_OF_YEAR} -
    * Returns a {@code LocalDate} with the specified month-of-year.
    * The year will be unchanged. The day-of-month will also be unchanged,
    * unless it would be invalid for the new month and year. In that case, the
    * day-of-month is adjusted to the maximum valid value for the new month and year.
    * <li>{@code PROLEPTIC_MONTH} -
    * Returns a {@code LocalDate} with the specified proleptic-month.
    * The day-of-month will be unchanged, unless it would be invalid for the new month
    * and year. In that case, the day-of-month is adjusted to the maximum valid value
    * for the new month and year.
    * <li>{@code YEAR_OF_ERA} -
    * Returns a {@code LocalDate} with the specified year-of-era.
    * The era and month will be unchanged. The day-of-month will also be unchanged,
    * unless it would be invalid for the new month and year. In that case, the
    * day-of-month is adjusted to the maximum valid value for the new month and year.
    * <li>{@code YEAR} -
    * Returns a {@code LocalDate} with the specified year.
    * The month will be unchanged. The day-of-month will also be unchanged,
    * unless it would be invalid for the new month and year. In that case, the
    * day-of-month is adjusted to the maximum valid value for the new month and year.
    * <li>{@code ERA} -
    * Returns a {@code LocalDate} with the specified era.
    * The year-of-era and month will be unchanged. The day-of-month will also be unchanged,
    * unless it would be invalid for the new month and year. In that case, the
    * day-of-month is adjusted to the maximum valid value for the new month and year.
    * </ul>
    *
    * In all cases, if the new value is outside the valid range of values for the field
    * then a {@code DateTimeException} will be thrown.
    *
    * All other {@code ChronoField} instances will throw a {@code DateTimeException}.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.adjustInto(Temporal, long)}
    * passing {@code this} as the argument. In this case, the field determines
    * whether and how to adjust the instant.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param field  the field to set in the result, not null
    * @param newValue  the new value of the field in the result
    * @return a { @code LocalDate} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): LocalDate =
    if (field.isInstanceOf[ChronoField]) {
      val f: ChronoField = field.asInstanceOf[ChronoField]
      f.checkValidValue(newValue)
      f match {
        case DAY_OF_WEEK => plusDays(newValue - getDayOfWeek.getValue)
        case ALIGNED_DAY_OF_WEEK_IN_MONTH =>
          plusDays(newValue - getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
        case ALIGNED_DAY_OF_WEEK_IN_YEAR =>
          plusDays(newValue - getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
        case DAY_OF_MONTH          => withDayOfMonth(newValue.toInt)
        case DAY_OF_YEAR           => withDayOfYear(newValue.toInt)
        case EPOCH_DAY             => LocalDate.ofEpochDay(newValue)
        case ALIGNED_WEEK_OF_MONTH => plusWeeks(newValue - getLong(ALIGNED_WEEK_OF_MONTH))
        case ALIGNED_WEEK_OF_YEAR  => plusWeeks(newValue - getLong(ALIGNED_WEEK_OF_YEAR))
        case MONTH_OF_YEAR         => withMonth(newValue.toInt)
        case PROLEPTIC_MONTH       => plusMonths(newValue - getLong(PROLEPTIC_MONTH))
        case YEAR_OF_ERA           => withYear((if (year >= 1) newValue else 1 - newValue).toInt)
        case YEAR                  => withYear(newValue.toInt)
        case ERA                   => if (getLong(ERA) == newValue) this else withYear(1 - year)
        case _                     => throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
      }
    } else {
      field.adjustInto(this, newValue)
    }

  /** Returns a copy of this date with the year altered.
    * If the day-of-month is invalid for the year, it will be changed to the last valid day of the month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param year  the year to set in the result, from MIN_YEAR to MAX_YEAR
    * @return a { @code LocalDate} based on this date with the requested year, not null
    * @throws DateTimeException if the year value is invalid
    */
  def withYear(year: Int): LocalDate =
    if (this.year == year)
      this
    else {
      YEAR.checkValidValue(year.toLong)
      LocalDate.resolvePreviousValid(year, month.toInt, day.toInt)
    }

  /** Returns a copy of this date with the month-of-year altered.
    * If the day-of-month is invalid for the year, it will be changed to the last valid day of the month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param month  the month-of-year to set in the result, from 1 (January) to 12 (December)
    * @return a { @code LocalDate} based on this date with the requested month, not null
    * @throws DateTimeException if the month-of-year value is invalid
    */
  def withMonth(month: Int): LocalDate =
    if (this.month == month)
      this
    else {
      MONTH_OF_YEAR.checkValidValue(month.toLong)
      LocalDate.resolvePreviousValid(year, month, day.toInt)
    }

  /** Returns a copy of this date with the day-of-month altered.
    * If the resulting date is invalid, an exception is thrown.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param dayOfMonth  the day-of-month to set in the result, from 1 to 28-31
    * @return a { @code LocalDate} based on this date with the requested day, not null
    * @throws DateTimeException if the day-of-month value is invalid
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def withDayOfMonth(dayOfMonth: Int): LocalDate =
    if (this.day == dayOfMonth) this
    else LocalDate.of(year, month.toInt, dayOfMonth)

  /** Returns a copy of this date with the day-of-year altered.
    * If the resulting date is invalid, an exception is thrown.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param dayOfYear  the day-of-year to set in the result, from 1 to 365-366
    * @return a { @code LocalDate} based on this date with the requested day, not null
    * @throws DateTimeException if the day-of-year value is invalid
    * @throws DateTimeException if the day-of-year is invalid for the year
    */
  def withDayOfYear(dayOfYear: Int): LocalDate =
    if (this.getDayOfYear == dayOfYear) this
    else LocalDate.ofYearDay(year, dayOfYear)

  /** Returns a copy of this date with the specified period added.
    *
    * This method returns a new date based on this date with the specified period added.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return a { @code LocalDate} based on this date with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): LocalDate = amount.addTo(this).asInstanceOf[LocalDate]

  /** Returns a copy of this date with the specified period added.
    *
    * This method returns a new date based on this date with the specified period added.
    * This can be used to add any period that is defined by a unit, for example to add years, months or days.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the amount of the unit to add to the result, may be negative
    * @param unit  the unit of the period to add, not null
    * @return a { @code LocalDate} based on this date with the specified period added, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): LocalDate =
    if (unit.isInstanceOf[ChronoUnit]) {
      val f: ChronoUnit = unit.asInstanceOf[ChronoUnit]
      import ChronoUnit._
      f match {
        case DAYS      => plusDays(amountToAdd)
        case WEEKS     => plusWeeks(amountToAdd)
        case MONTHS    => plusMonths(amountToAdd)
        case YEARS     => plusYears(amountToAdd)
        case DECADES   => plusYears(Math.multiplyExact(amountToAdd, 10))
        case CENTURIES => plusYears(Math.multiplyExact(amountToAdd, 100))
        case MILLENNIA => plusYears(Math.multiplyExact(amountToAdd, 1000))
        case ERAS      => `with`(ERA, Math.addExact(getLong(ERA), amountToAdd))
        case _         => throw throw UnsupportedTemporalTypeException.unit(unit)
      }
    } else {
      unit.addTo(this, amountToAdd)
    }

  /** Returns a copy of this {@code LocalDate} with the specified period in years added.
    *
    * This method adds the specified amount to the years field in three steps:
    * <ol>
    * <li>Add the input years to the year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2008-02-29 (leap year) plus one year would result in the
    * invalid date 2009-02-29 (standard year). Instead of returning an invalid
    * result, the last valid day of the month, 2009-02-28, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToAdd  the years to add, may be negative
    * @return a { @code LocalDate} based on this date with the years added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusYears(yearsToAdd: Long): LocalDate =
    if (yearsToAdd == 0)
      this
    else {
      val newYear: Int = YEAR.checkValidIntValue(year + yearsToAdd)
      LocalDate.resolvePreviousValid(newYear, month.toInt, day.toInt)
    }

  /** Returns a copy of this {@code LocalDate} with the specified period in months added.
    *
    * This method adds the specified amount to the months field in three steps:
    * <ol>
    * <li>Add the input months to the month-of-year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2007-03-31 plus one month would result in the invalid date
    * 2007-04-31. Instead of returning an invalid result, the last valid day
    * of the month, 2007-04-30, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToAdd  the months to add, may be negative
    * @return a { @code LocalDate} based on this date with the months added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusMonths(monthsToAdd: Long): LocalDate =
    if (monthsToAdd == 0)
      this
    else {
      val monthCount: Long = year * 12L + (month - 1)
      val calcMonths: Long = monthCount + monthsToAdd
      val newYear: Int     = YEAR.checkValidIntValue(Math.floorDiv(calcMonths, 12))
      val newMonth: Int    = Math.floorMod(calcMonths, 12).toInt + 1
      LocalDate.resolvePreviousValid(newYear, newMonth, day.toInt)
    }

  /** Returns a copy of this {@code LocalDate} with the specified period in weeks added.
    *
    * This method adds the specified amount in weeks to the days field incrementing
    * the month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2008-12-31 plus one week would result in 2009-01-07.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param weeksToAdd  the weeks to add, may be negative
    * @return a { @code LocalDate} based on this date with the weeks added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusWeeks(weeksToAdd: Long): LocalDate = plusDays(Math.multiplyExact(weeksToAdd, 7))

  /** Returns a copy of this {@code LocalDate} with the specified number of days added.
    *
    * This method adds the specified amount to the days field incrementing the
    * month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2008-12-31 plus one day would result in 2009-01-01.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToAdd  the days to add, may be negative
    * @return a { @code LocalDate} based on this date with the days added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusDays(daysToAdd: Long): LocalDate =
    if (daysToAdd == 0)
      this
    else {
      val mjDay: Long = Math.addExact(toEpochDay, daysToAdd)
      LocalDate.ofEpochDay(mjDay)
    }

  /** Returns a copy of this date with the specified period subtracted.
    *
    * This method returns a new date based on this date with the specified period subtracted.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to subtract, not null
    * @return a { @code LocalDate} based on this date with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): LocalDate =
    amount.subtractFrom(this).asInstanceOf[LocalDate]

  /** Returns a copy of this date with the specified period subtracted.
    *
    * This method returns a new date based on this date with the specified period subtracted.
    * This can be used to subtract any period that is defined by a unit, for example to subtract years, months or days.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the amount of the unit to subtract from the result, may be negative
    * @param unit  the unit of the period to subtract, not null
    * @return a { @code LocalDate} based on this date with the specified period subtracted, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): LocalDate =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this {@code LocalDate} with the specified period in years subtracted.
    *
    * This method subtracts the specified amount from the years field in three steps:
    * <ol>
    * <li>Subtract the input years to the year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2008-02-29 (leap year) minus one year would result in the
    * invalid date 2007-02-29 (standard year). Instead of returning an invalid
    * result, the last valid day of the month, 2007-02-28, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToSubtract  the years to subtract, may be negative
    * @return a { @code LocalDate} based on this date with the years subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusYears(yearsToSubtract: Long): LocalDate =
    if (yearsToSubtract == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-yearsToSubtract)

  /** Returns a copy of this {@code LocalDate} with the specified period in months subtracted.
    *
    * This method subtracts the specified amount from the months field in three steps:
    * <ol>
    * <li>Subtract the input months to the month-of-year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2007-03-31 minus one month would result in the invalid date
    * 2007-02-31. Instead of returning an invalid result, the last valid day
    * of the month, 2007-02-28, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToSubtract  the months to subtract, may be negative
    * @return a { @code LocalDate} based on this date with the months subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusMonths(monthsToSubtract: Long): LocalDate =
    if (monthsToSubtract == Long.MinValue) plusMonths(Long.MaxValue).plusMonths(1)
    else plusMonths(-monthsToSubtract)

  /** Returns a copy of this {@code LocalDate} with the specified period in weeks subtracted.
    *
    * This method subtracts the specified amount in weeks from the days field decrementing
    * the month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2009-01-07 minus one week would result in 2008-12-31.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param weeksToSubtract  the weeks to subtract, may be negative
    * @return a { @code LocalDate} based on this date with the weeks subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusWeeks(weeksToSubtract: Long): LocalDate =
    if (weeksToSubtract == Long.MinValue) plusWeeks(Long.MaxValue).plusWeeks(1)
    else plusWeeks(-weeksToSubtract)

  /** Returns a copy of this {@code LocalDate} with the specified number of days subtracted.
    *
    * This method subtracts the specified amount from the days field decrementing the
    * month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2009-01-01 minus one day would result in 2008-12-31.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToSubtract  the days to subtract, may be negative
    * @return a { @code LocalDate} based on this date with the days subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusDays(daysToSubtract: Long): LocalDate =
    if (daysToSubtract == Long.MinValue) plusDays(Long.MaxValue).plusDays(1)
    else plusDays(-daysToSubtract)

  /** Queries this date using the specified query.
    *
    * This queries this date using the specified query strategy object.
    * The {@code TemporalQuery} object defines the logic to be used to
    * obtain the result. Read the documentation of the query to understand
    * what the result of this method will be.
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalQuery#queryFrom(TemporalAccessor)} method on the
    * specified query passing {@code this} as the argument.
    *
    * @tparam R the type of the result
    * @param query  the query to invoke, not null
    * @return the query result, null may be returned (defined by the query)
    * @throws DateTimeException if unable to query (defined by the query)
    * @throws ArithmeticException if numeric overflow occurs (defined by the query)
    */
  override def query[R](query: TemporalQuery[R]): R =
    if (query eq TemporalQueries.localDate) this.asInstanceOf[R]
    else super.query(query)

  /** Adjusts the specified temporal object to have the same date as this object.
    *
    * This returns a temporal object of the same observable type as the input
    * with the date changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#EPOCH_DAY} as the field.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisLocalDate.adjustInto(temporal);
    * temporal = temporal.with(thisLocalDate);
    * </pre>
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the target object to be adjusted, not null
    * @return the adjusted object, not null
    * @throws DateTimeException if unable to make the adjustment
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def adjustInto(temporal: Temporal): Temporal = super.adjustInto(temporal)

  /** Calculates the period between this date and another date in
    * terms of the specified unit.
    *
    * This calculates the period between two dates in terms of a single unit.
    * The start and end points are {@code this} and the specified date.
    * The result will be negative if the end is before the start.
    * The {@code Temporal} passed to this method must be a {@code LocalDate}.
    * For example, the period in days between two dates can be calculated
    * using {@code startDate.until(endDate, DAYS)}.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two dates.
    * For example, the period in months between 2012-06-15 and 2012-08-14
    * will only be one month as it is one day short of two months.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * <pre>
    * long period = start.until(end, MONTHS);   // this method
    * dateTime.plus(MONTHS.between(start, end));      // use in plus/minus
    * </pre>
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code DAYS}, {@code WEEKS}, {@code MONTHS}, {@code YEARS},
    * {@code DECADES}, {@code CENTURIES}, {@code MILLENNIA} and {@code ERAS}
    * are supported. Other {@code ChronoUnit} values will throw an exception.
    *
    * If the unit is not a {@code ChronoUnit}, then the result of this method
    * is obtained by invoking {@code TemporalUnit.between(Temporal, Temporal)}
    * passing {@code this} as the first argument and the input temporal as
    * the second argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endExclusive  the end date, which is converted to a { @code LocalDate}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this date and the end date
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: LocalDate = LocalDate.from(endExclusive)
    if (unit.isInstanceOf[ChronoUnit]) {
      import ChronoUnit._
      unit.asInstanceOf[ChronoUnit] match {
        case DAYS      => daysUntil(end)
        case WEEKS     => daysUntil(end) / 7
        case MONTHS    => monthsUntil(end)
        case YEARS     => monthsUntil(end) / 12
        case DECADES   => monthsUntil(end) / 120
        case CENTURIES => monthsUntil(end) / 1200
        case MILLENNIA => monthsUntil(end) / 12000
        case ERAS      => end.getLong(ERA) - getLong(ERA)
        case _         => throw throw UnsupportedTemporalTypeException.unit(unit)
      }

    } else {
      unit.between(this, end)
    }
  }

  private[bp] def daysUntil(end: LocalDate): Long = end.toEpochDay - toEpochDay

  private def monthsUntil(end: LocalDate): Long = {
    val packed1: Long = getProlepticMonth * 32L + getDayOfMonth
    val packed2: Long = end.getProlepticMonth * 32L + end.getDayOfMonth
    (packed2 - packed1) / 32
  }

  /** Calculates the period between this date and another date as a {@code Period}.
    *
    * This calculates the period between two dates in terms of years, months and days.
    * The start and end points are {@code this} and the specified date.
    * The result will be negative if the end is before the start.
    *
    * The calculation is performed using the ISO calendar system.
    * If necessary, the input date will be converted to ISO.
    *
    * The start date is included, but the end date is not.
    * The period is calculated by removing complete months, then calculating
    * the remaining number of days, adjusting to ensure that both have the same sign.
    * The number of months is then normalized into years and months based on a 12 month year.
    * A month is considered to be complete if the end day-of-month is greater
    * than or equal to the start day-of-month.
    * For example, from {@code 2010-01-15} to {@code 2011-03-18} is "1 year, 2 months and 3 days".
    *
    * The result of this method can be a negative period if the end is before the start.
    * The negative sign will be the same in each of year, month and day.
    *
    * There are two equivalent ways of using this method.
    * The first is to invoke this method.
    * The second is to use {@link Period#between(LocalDate, LocalDate)}:
    * <pre>
    * // these two lines are equivalent
    * period = start.until(end);
    * period = Period.between(start, end);
    * </pre>
    * The choice should be made based on which makes the code more readable.
    *
    * @param endDate  the end date, exclusive, which may be in any chronology, not null
    * @return the period between this date and the end date, not null
    */
  def until(endDate: ChronoLocalDate): Period = {
    val end: LocalDate    = LocalDate.from(endDate)
    var totalMonths: Long = end.getProlepticMonth - this.getProlepticMonth
    var days: Int         = end.day - this.day
    if (totalMonths > 0 && days < 0) {
      totalMonths -= 1
      val calcDate: LocalDate = this.plusMonths(totalMonths)
      days = (end.toEpochDay - calcDate.toEpochDay).toInt
    } else if (totalMonths < 0 && days > 0) {
      totalMonths += 1
      days -= end.lengthOfMonth
    }
    val years: Long = totalMonths / 12
    val months: Int = (totalMonths % 12).toInt
    Period.of(Math.toIntExact(years), months, days)
  }

  /** Combines this date with a time to create a {@code LocalDateTime}.
    *
    * This returns a {@code LocalDateTime} formed from this date at the specified time.
    * All possible combinations of date and time are valid.
    *
    * @param time  the time to combine with, not null
    * @return the local date-time formed from this date and the specified time, not null
    */
  override def atTime(time: LocalTime): LocalDateTime = LocalDateTime.of(this, time)

  /** Combines this date with a time to create a {@code LocalDateTime}.
    *
    * This returns a {@code LocalDateTime} formed from this date at the
    * specified hour and minute.
    * The seconds and nanosecond fields will be set to zero.
    * The individual time fields must be within their valid range.
    * All possible combinations of date and time are valid.
    *
    * @param hour  the hour-of-day to use, from 0 to 23
    * @param minute  the minute-of-hour to use, from 0 to 59
    * @return the local date-time formed from this date and the specified time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def atTime(hour: Int, minute: Int): LocalDateTime = atTime(LocalTime.of(hour, minute))

  /** Combines this date with a time to create a {@code LocalDateTime}.
    *
    * This returns a {@code LocalDateTime} formed from this date at the
    * specified hour, minute and second.
    * The nanosecond field will be set to zero.
    * The individual time fields must be within their valid range.
    * All possible combinations of date and time are valid.
    *
    * @param hour  the hour-of-day to use, from 0 to 23
    * @param minute  the minute-of-hour to use, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @return the local date-time formed from this date and the specified time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def atTime(hour: Int, minute: Int, second: Int): LocalDateTime =
    atTime(LocalTime.of(hour, minute, second))

  /** Combines this date with a time to create a {@code LocalDateTime}.
    *
    * This returns a {@code LocalDateTime} formed from this date at the
    * specified hour, minute, second and nanosecond.
    * The individual time fields must be within their valid range.
    * All possible combinations of date and time are valid.
    *
    * @param hour  the hour-of-day to use, from 0 to 23
    * @param minute  the minute-of-hour to use, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @return the local date-time formed from this date and the specified time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def atTime(hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalDateTime =
    atTime(LocalTime.of(hour, minute, second, nanoOfSecond))

  /** Combines this date with an offset time to create an {@code OffsetDateTime}.
    *
    * This returns an {@code OffsetDateTime} formed from this date at the specified time.
    * All possible combinations of date and time are valid.
    *
    * @param time  the time to combine with, not null
    * @return the offset date-time formed from this date and the specified time, not null
    */
  def atTime(time: OffsetTime): OffsetDateTime =
    OffsetDateTime.of(LocalDateTime.of(this, time.toLocalTime), time.getOffset)

  /** Combines this date with the time of midnight to create a {@code LocalDateTime}
    * at the start of this date.
    *
    * This returns a {@code LocalDateTime} formed from this date at the time of
    * midnight, 00:00, at the start of this date.
    *
    * @return the local date-time of midnight at the start of this date, not null
    */
  def atStartOfDay: LocalDateTime = LocalDateTime.of(this, LocalTime.MIDNIGHT)

  /** Combines this date with a time-zone to create a {@code ZonedDateTime}
    * at the start of the day
    *
    * This returns a {@code ZonedDateTime} formed from this date at the
    * specified zone, with the time set to be the earliest valid time according
    * to the rules in the time-zone.
    *
    * Time-zone rules, such as daylight savings, mean that not every local date-time
    * is valid for the specified zone, thus the local date-time may not be midnight.
    *
    * In most cases, there is only one valid offset for a local date-time.
    * In the case of an overlap, there are two valid offsets, and the earlier one is used,
    * corresponding to the first occurrence of midnight on the date.
    * In the case of a gap, the zoned date-time will represent the instant just after the gap.
    *
    * If the zone ID is a {@link ZoneOffset}, then the result always has a time of midnight.
    *
    * To convert to a specific time in a given time-zone call {@link #atTime(LocalTime)}
    * followed by {@link LocalDateTime#atZone(ZoneId)}.
    *
    * @param zone  the zone ID to use, not null
    * @return the zoned date-time formed from this date and the earliest valid time for the zone, not null
    */
  def atStartOfDay(zone: ZoneId): ZonedDateTime = {
    Objects.requireNonNull(zone, "zone")
    var ldt: LocalDateTime = atTime(LocalTime.MIDNIGHT)
    if (!zone.isInstanceOf[ZoneOffset]) {
      val rules: ZoneRules            = zone.getRules
      val trans: ZoneOffsetTransition = rules.getTransition(ldt)
      if (trans != null && trans.isGap) {
        ldt = trans.getDateTimeAfter
      }
    }
    ZonedDateTime.of(ldt, zone)
  }

  override def toEpochDay: Long = {
    val y: Long     = year.toLong
    val m: Long     = month.toLong
    var total: Long = 0
    total += 365 * y
    if (y >= 0)
      total += (y + 3) / 4 - (y + 99) / 100 + (y + 399) / 400
    else
      total -= y / -4 - y / -100 + y / -400
    total += ((367 * m - 362) / 12)
    total += day - 1
    if (m > 2) {
      total -= 1
      if (!isLeapYear)
        total -= 1
    }
    total - LocalDate.DAYS_0000_TO_1970
  }

  /** Compares this date to another date.
    *
    * The comparison is primarily based on the date, from earliest to latest.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * If all the dates being compared are instances of {@code LocalDate},
    * then the comparison will be entirely based on the date.
    * If some dates being compared are in different chronologies, then the
    * chronology is also considered, see {@link ChronoLocalDate#compareTo}.
    *
    * @param other  the other date to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  override def compareTo(other: ChronoLocalDate): Int =
    if (other.isInstanceOf[LocalDate]) compareTo0(other.asInstanceOf[LocalDate])
    else super.compareTo(other)

  private[bp] def compareTo0(otherDate: LocalDate): Int = {
    var cmp: Int = year - otherDate.year
    if (cmp == 0) {
      cmp = month - otherDate.month
      if (cmp == 0)
        cmp = day - otherDate.day
    }
    cmp
  }

  /** Checks if this date is after the specified date.
    *
    * This checks to see if this date represents a point on the
    * local time-line after the other date.
    * <pre>
    * LocalDate a = LocalDate.of(2012, 6, 30);
    * LocalDate b = LocalDate.of(2012, 7, 1);
    * a.isAfter(b) == false
    * a.isAfter(a) == false
    * b.isAfter(a) == true
    * </pre>
    *
    * This method only considers the position of the two dates on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDate)},
    * but is the same approach as {@link #DATE_COMPARATOR}.
    *
    * @param other  the other date to compare to, not null
    * @return true if this date is after the specified date
    */
  override def isAfter(other: ChronoLocalDate): Boolean =
    if (other.isInstanceOf[LocalDate]) compareTo0(other.asInstanceOf[LocalDate]) > 0
    else super.isAfter(other)

  /** Checks if this date is before the specified date.
    *
    * This checks to see if this date represents a point on the
    * local time-line before the other date.
    * <pre>
    * LocalDate a = LocalDate.of(2012, 6, 30);
    * LocalDate b = LocalDate.of(2012, 7, 1);
    * a.isBefore(b) == true
    * a.isBefore(a) == false
    * b.isBefore(a) == false
    * </pre>
    *
    * This method only considers the position of the two dates on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDate)},
    * but is the same approach as {@link #DATE_COMPARATOR}.
    *
    * @param other  the other date to compare to, not null
    * @return true if this date is before the specified date
    */
  override def isBefore(other: ChronoLocalDate): Boolean =
    if (other.isInstanceOf[LocalDate]) compareTo0(other.asInstanceOf[LocalDate]) < 0
    else super.isBefore(other)

  /** Checks if this date is equal to the specified date.
    *
    * This checks to see if this date represents the same point on the
    * local time-line as the other date.
    * <pre>
    * LocalDate a = LocalDate.of(2012, 6, 30);
    * LocalDate b = LocalDate.of(2012, 7, 1);
    * a.isEqual(b) == false
    * a.isEqual(a) == true
    * b.isEqual(a) == false
    * </pre>
    *
    * This method only considers the position of the two dates on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDate)}
    * but is the same approach as {@link #DATE_COMPARATOR}.
    *
    * @param other  the other date to compare to, not null
    * @return true if this date is equal to the specified date
    */
  override def isEqual(other: ChronoLocalDate): Boolean =
    if (other.isInstanceOf[LocalDate]) compareTo0(other.asInstanceOf[LocalDate]) == 0
    else super.isEqual(other)

  /** Checks if this date is equal to another date.
    *
    * Compares this {@code LocalDate} with another ensuring that the date is the same.
    *
    * Only objects of type {@code LocalDate} are compared, other types return false.
    * To compare the dates of two {@code TemporalAccessor} instances, including dates
    * in two different chronologies, use {@link ChronoField#EPOCH_DAY} as a comparator.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other date
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case otherDate: LocalDate => (this eq otherDate) || (compareTo0(otherDate) == 0)
      case _                    => false
    }

  /** A hash code for this date.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = {
    val yearValue: Int  = year
    val monthValue: Int = month.toInt
    val dayValue: Int   = day.toInt
    (yearValue & 0xFFFFF800) ^ ((yearValue << 11) + (monthValue << 6) + dayValue)
  }

  /** Outputs this date as a {@code String}, such as {@code 2007-12-03}.
    *
    * The output will be in the ISO-8601 format {@code yyyy-MM-dd}.
    *
    * @return a string representation of this date, not null
    */
  override def toString: String = {
    val yearValue: Int     = year
    val monthValue: Int    = month.toInt
    val dayValue: Int      = day.toInt
    val absYear: Int       = Math.abs(yearValue)
    val buf: StringBuilder = new StringBuilder(10)
    if (absYear < 1000) {
      if (yearValue < 0)
        buf.append(yearValue - 10000).deleteCharAt(1)
      else
        buf.append(yearValue + 10000).deleteCharAt(0)
    } else {
      if (yearValue > 9999)
        buf.append('+')
      buf.append(yearValue)
    }
    buf
      .append(if (monthValue < 10) "-0" else "-")
      .append(monthValue)
      .append(if (dayValue < 10) "-0" else "-")
      .append(dayValue)
      .toString
  }

  /** Outputs this date as a {@code String} using the formatter.
    *
    * This date will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted date string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  override def format(formatter: DateTimeFormatter): String = super.format(formatter)

  private def writeReplace: AnyRef = new Ser(Ser.LOCAL_DATE_TYPE, this)

  /** Defend against malicious streams.
    *
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit = {
    out.writeInt(year)
    out.writeByte(month.toInt)
    out.writeByte(day.toInt)
  }
}
