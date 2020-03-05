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

import org.threeten.bp.LocalTime.HOURS_PER_DAY
import org.threeten.bp.LocalTime.MICROS_PER_DAY
import org.threeten.bp.LocalTime.MILLIS_PER_DAY
import org.threeten.bp.LocalTime.MINUTES_PER_DAY
import org.threeten.bp.LocalTime.NANOS_PER_DAY
import org.threeten.bp.LocalTime.NANOS_PER_HOUR
import org.threeten.bp.LocalTime.NANOS_PER_MINUTE
import org.threeten.bp.LocalTime.NANOS_PER_SECOND
import org.threeten.bp.LocalTime.SECONDS_PER_DAY
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.ChronoLocalDateTime
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.ChronoField
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
import org.threeten.bp.zone.ZoneRules

@SerialVersionUID(6207766400415563566L)
object LocalDateTime {

  /** The minimum supported {@code LocalDateTime}, '-999999999-01-01T00:00:00'.
    * This is the local date-time of midnight at the start of the minimum date.
    * This combines {@link LocalDate#MIN} and {@link LocalTime#MIN}.
    * This could be used by an application as a "far past" date-time.
    */
  lazy val MIN: LocalDateTime = LocalDateTime.of(LocalDate.MIN, LocalTime.MIN)

  /** The maximum supported {@code LocalDateTime}, '+999999999-12-31T23:59:59.999999999'.
    * This is the local date-time just before midnight at the end of the maximum date.
    * This combines {@link LocalDate#MAX} and {@link LocalTime#MAX}.
    * This could be used by an application as a "far future" date-time.
    */
  lazy val MAX: LocalDateTime = LocalDateTime.of(LocalDate.MAX, LocalTime.MAX)

  /** Obtains the current date-time from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current date-time.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current date-time using the system clock and default time-zone, not null
    */
  def now: LocalDateTime = now(Clock.systemDefaultZone)

  /** Obtains the current date-time from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current date-time.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current date-time using the system clock, not null
    */
  def now(zone: ZoneId): LocalDateTime = now(Clock.system(zone))

  /** Obtains the current date-time from the specified clock.
    *
    * This will query the specified clock to obtain the current date-time.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current date-time, not null
    */
  def now(clock: Clock): LocalDateTime = {
    Objects.requireNonNull(clock, "clock")
    val now: Instant       = clock.instant
    val offset: ZoneOffset = clock.getZone.getRules.getOffset(now)
    ofEpochSecond(now.getEpochSecond, now.getNano, offset)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour and minute, setting the second and nanosecond to zero.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    * The second and nanosecond fields will be set to zero.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, not null
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(year: Int, month: Month, dayOfMonth: Int, hour: Int, minute: Int): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour, minute and second, setting the nanosecond to zero.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    * The nanosecond field will be set to zero.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, not null
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(
    year:       Int,
    month:      Month,
    dayOfMonth: Int,
    hour:       Int,
    minute:     Int,
    second:     Int
  ): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour, minute, second and nanosecond.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, not null
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(
    year:         Int,
    month:        Month,
    dayOfMonth:   Int,
    hour:         Int,
    minute:       Int,
    second:       Int,
    nanoOfSecond: Int
  ): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour and minute, setting the second and nanosecond to zero.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    * The second and nanosecond fields will be set to zero.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour, minute and second, setting the nanosecond to zero.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    * The nanosecond field will be set to zero.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(
    year:       Int,
    month:      Int,
    dayOfMonth: Int,
    hour:       Int,
    minute:     Int,
    second:     Int
  ): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from year, month,
    * day, hour, minute, second and nanosecond.
    *
    * The day must be valid for the year and month, otherwise an exception will be thrown.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @return the local date-time, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def of(
    year:         Int,
    month:        Int,
    dayOfMonth:   Int,
    hour:         Int,
    minute:       Int,
    second:       Int,
    nanoOfSecond: Int
  ): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from a date and time.
    *
    * @param date  the local date, not null
    * @param time  the local time, not null
    * @return the local date-time, not null
    */
  def of(date: LocalDate, time: LocalTime): LocalDateTime = {
    Objects.requireNonNull(date, "date")
    Objects.requireNonNull(time, "time")
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from an {@code Instant} and zone ID.
    *
    * This creates a local date-time based on the specified instant.
    * First, the offset from UTC/Greenwich is obtained using the zone ID and instant,
    * which is simple as there is only one valid offset for each instant.
    * Then, the instant and offset are used to calculate the local date-time.
    *
    * @param instant  the instant to create the date-time from, not null
    * @param zone  the time-zone, which may be an offset, not null
    * @return the local date-time, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def ofInstant(instant: Instant, zone: ZoneId): LocalDateTime = {
    Objects.requireNonNull(instant, "instant")
    Objects.requireNonNull(zone, "zone")
    val rules: ZoneRules   = zone.getRules
    val offset: ZoneOffset = rules.getOffset(instant)
    ofEpochSecond(instant.getEpochSecond, instant.getNano, offset)
  }

  /** Obtains an instance of {@code LocalDateTime} using seconds from the
    * epoch of 1970-01-01T00:00:00Z.
    *
    * This allows the {@link ChronoField#INSTANT_SECONDS epoch-second} field
    * to be converted to a local date-time. This is primarily intended for
    * low-level conversions rather than general application usage.
    *
    * @param epochSecond  the number of seconds from the epoch of 1970-01-01T00:00:00Z
    * @param nanoOfSecond  the nanosecond within the second, from 0 to 999,999,999
    * @param offset  the zone offset, not null
    * @return the local date-time, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def ofEpochSecond(epochSecond: Long, nanoOfSecond: Int, offset: ZoneOffset): LocalDateTime = {
    Objects.requireNonNull(offset, "offset")
    val localSecond: Long   = epochSecond + offset.getTotalSeconds
    val localEpochDay: Long = Math.floorDiv(localSecond, SECONDS_PER_DAY.toLong)
    val secsOfDay: Int      = Math.floorMod(localSecond, SECONDS_PER_DAY.toLong).toInt
    val date: LocalDate     = LocalDate.ofEpochDay(localEpochDay)
    val time: LocalTime     = LocalTime.ofSecondOfDay(secsOfDay.toLong, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  /** Obtains an instance of {@code LocalDateTime} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code LocalDateTime}.
    *
    * The conversion extracts and combines {@code LocalDate} and {@code LocalTime}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code LocalDateTime::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the local date-time, not null
    * @throws DateTimeException if unable to convert to a { @code LocalDateTime}
    */
  def from(temporal: TemporalAccessor): LocalDateTime =
    temporal match {
      case l: LocalDateTime => l
      case z: ZonedDateTime => z.toLocalDateTime
      case _ =>
        try {
          val date: LocalDate = LocalDate.from(temporal)
          val time: LocalTime = LocalTime.from(temporal)
          new LocalDateTime(date, time)
        } catch {
          case _: DateTimeException =>
            throw new DateTimeException(
              s"Unable to obtain LocalDateTime from TemporalAccessor: $temporal, type ${temporal.getClass.getName}"
            )
        }
    }

  /** Obtains an instance of {@code LocalDateTime} from a text string such as {@code 2007-12-03T10:15:30}.
    *
    * The string must represent a valid date-time and is parsed using
    * {@link org.threeten.bp.format.DateTimeFormatter#ISO_LOCAL_DATE_TIME}.
    *
    * @param text  the text to parse such as "2007-12-03T10:15:30", not null
    * @return the parsed local date-time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): LocalDateTime = parse(text, DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  /** Obtains an instance of {@code LocalDateTime} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a date-time.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed local date-time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): LocalDateTime = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[LocalDateTime] {
      override def queryFrom(temporal: TemporalAccessor): LocalDateTime =
        LocalDateTime.from(temporal)
    })
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): LocalDateTime = {
    val date: LocalDate = LocalDate.readExternal(in)
    val time: LocalTime = LocalTime.readExternal(in)
    LocalDateTime.of(date, time)
  }
}

/** A date-time without a time-zone in the ISO-8601 calendar system,
  * such as {@code 2007-12-03T10:15:30}.
  *
  * {@code LocalDateTime} is an immutable date-time object that represents a date-time,
  * often viewed as year-month-day-hour-minute-second. Other date and time fields,
  * such as day-of-year, day-of-week and week-of-year, can also be accessed.
  * Time is represented to nanosecond precision.
  * For example, the value "2nd October 2007 at 13:45.30.123456789" can be
  * stored in a {@code LocalDateTime}.
  *
  * This class does not store or represent a time-zone.
  * Instead, it is a description of the date, as used for birthdays, combined with
  * the local time as seen on a wall clock.
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
  * @constructor
  *
  * @param date  the date part of the date-time, validated not null
  * @param time  the time part of the date-time, validated not null
  */
@SerialVersionUID(6207766400415563566L)
final class LocalDateTime private (private val date: LocalDate, private val time: LocalTime)
    extends ChronoLocalDateTime[LocalDate]
    with Temporal
    with TemporalAdjuster
    with Serializable {

  /** Returns a copy of this date-time with the new date and time, checking
    * to see if a new object is in fact required.
    *
    * @param newDate  the date of the new date-time, not null
    * @param newTime  the time of the new date-time, not null
    * @return the date-time, not null
    */
  private def `with`(newDate: LocalDate, newTime: LocalTime): LocalDateTime =
    if ((date eq newDate) && (time eq newTime)) this
    else new LocalDateTime(newDate, newTime)

  /** Checks if the specified field is supported.
    *
    * This checks if this date-time can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The supported fields are:
    * <ul>
    * <li>{@code NANO_OF_SECOND}
    * <li>{@code NANO_OF_DAY}
    * <li>{@code MICRO_OF_SECOND}
    * <li>{@code MICRO_OF_DAY}
    * <li>{@code MILLI_OF_SECOND}
    * <li>{@code MILLI_OF_DAY}
    * <li>{@code SECOND_OF_MINUTE}
    * <li>{@code SECOND_OF_DAY}
    * <li>{@code MINUTE_OF_HOUR}
    * <li>{@code MINUTE_OF_DAY}
    * <li>{@code HOUR_OF_AMPM}
    * <li>{@code CLOCK_HOUR_OF_AMPM}
    * <li>{@code HOUR_OF_DAY}
    * <li>{@code CLOCK_HOUR_OF_DAY}
    * <li>{@code AMPM_OF_DAY}
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
    * @return true if the field is supported on this date-time, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    field match {
      case _: ChronoField =>
        field.isDateBased || field.isTimeBased
      case _ => field != null && field.isSupportedBy(this)
    }

  def isSupported(unit: TemporalUnit): Boolean =
    unit match {
      case _: ChronoUnit => unit.isDateBased || unit.isTimeBased
      case _             => unit != null && unit.isSupportedBy(this)
    }

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This date-time is used to enhance the accuracy of the returned range.
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
    field match {
      case _: ChronoField =>
        if (field.isTimeBased) time.range(field)
        else date.range(field)
      case _ =>
        field.rangeRefinedBy(this)
    }

  /** Gets the value of the specified field from this date-time as an {@code int}.
    *
    * This queries this date-time for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time, except {@code NANO_OF_DAY}, {@code MICRO_OF_DAY},
    * {@code EPOCH_DAY} and {@code EPOCH_MONTH} which are too large to fit in
    * an {@code int} and throw a {@code DateTimeException}.
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
    field match {
      case _: ChronoField =>
        if (field.isTimeBased) time.get(field)
        else date.get(field)
      case _ =>
        super.get(field)
    }

  /** Gets the value of the specified field from this date-time as a {@code long}.
    *
    * This queries this date-time for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time.
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
    field match {
      case _: ChronoField =>
        if (field.isTimeBased) time.getLong(field)
        else date.getLong(field)
      case _ =>
        field.getFrom(this)
    }

  /** Gets the year field.
    *
    * This method returns the primitive {@code int} value for the year.
    *
    * The year returned by this method is proleptic as per {@code get(YEAR)}.
    * To obtain the year-of-era, use {@code get(YEAR_OF_ERA}.
    *
    * @return the year, from MIN_YEAR to MAX_YEAR
    */
  def getYear: Int = date.getYear

  /** Gets the month-of-year field from 1 to 12.
    *
    * This method returns the month as an {@code int} from 1 to 12.
    * Application code is frequently clearer if the enum {@link Month}
    * is used by calling {@link #getMonth()}.
    *
    * @return the month-of-year, from 1 to 12
    * @see #getMonth()
    */
  def getMonthValue: Int = date.getMonthValue

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
  def getMonth: Month = date.getMonth

  /** Gets the day-of-month field.
    *
    * This method returns the primitive {@code int} value for the day-of-month.
    *
    * @return the day-of-month, from 1 to 31
    */
  def getDayOfMonth: Int = date.getDayOfMonth

  /** Gets the day-of-year field.
    *
    * This method returns the primitive {@code int} value for the day-of-year.
    *
    * @return the day-of-year, from 1 to 365, or 366 in a leap year
    */
  def getDayOfYear: Int = date.getDayOfYear

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
  def getDayOfWeek: DayOfWeek = date.getDayOfWeek

  /** Gets the hour-of-day field.
    *
    * @return the hour-of-day, from 0 to 23
    */
  def getHour: Int = time.getHour

  /** Gets the minute-of-hour field.
    *
    * @return the minute-of-hour, from 0 to 59
    */
  def getMinute: Int = time.getMinute

  /** Gets the second-of-minute field.
    *
    * @return the second-of-minute, from 0 to 59
    */
  def getSecond: Int = time.getSecond

  /** Gets the nano-of-second field.
    *
    * @return the nano-of-second, from 0 to 999,999,999
    */
  def getNano: Int = time.getNano

  /** Returns an adjusted copy of this date-time.
    *
    * This returns a new {@code LocalDateTime}, based on this one, with the date-time adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * A simple adjuster might simply set the one of the fields, such as the year field.
    * A more complex adjuster might set the date to the last day of the month.
    * A selection of common adjustments is provided in {@link TemporalAdjusters}.
    * These include finding the "last day of the month" and "next Wednesday".
    * Key date-time classes also implement the {@code TemporalAdjuster} interface,
    * such as {@link Month} and {@link MonthDay MonthDay}.
    * The adjuster is responsible for handling special cases, such as the varying
    * lengths of month and leap years.
    *
    * For example this code returns a date on the last day of July:
    * <pre>
    *  import static org.threeten.bp.Month.*;
    *  import static org.threeten.bp.temporal.Adjusters.*;
    *
    *  result = localDateTime.with(JULY).with(lastDayOfMonth());
    * </pre>
    *
    * The classes {@link LocalDate} and {@link LocalTime} implement {@code TemporalAdjuster},
    * thus this method can be used to change the date, time or offset:
    * <pre>
    *  result = localDateTime.with(date);
    *  result = localDateTime.with(time);
    * </pre>
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return a { @code LocalDateTime} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): LocalDateTime =
    adjuster match {
      case l: LocalDate     => `with`(l, time)
      case l: LocalTime     => `with`(date, l)
      case l: LocalDateTime => l
      case _ =>
        adjuster.adjustInto(this).asInstanceOf[LocalDateTime]
    }

  /** Returns a copy of this date-time with the specified field set to a new value.
    *
    * This returns a new {@code LocalDateTime}, based on this one, with the value
    * for the specified field changed.
    * This can be used to change any supported field, such as the year, month or day-of-month.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * In some cases, changing the specified field can cause the resulting date-time to become invalid,
    * such as changing the month from 31st January to February would make the day-of-month invalid.
    * In cases like this, the field is responsible for resolving the date. Typically it will choose
    * the previous valid date, which would be the last valid day of February in this example.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will behave as per
    * the matching method on {@link LocalDate#with(TemporalField, long) LocalDate}
    * or {@link LocalTime#with(TemporalField, long) LocalTime}.
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
    * @return a { @code LocalDateTime} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): LocalDateTime =
    // Size checked
    if (field.isInstanceOf[ChronoField])
      if (field.isTimeBased) `with`(date, time.`with`(field, newValue))
      else `with`(date.`with`(field, newValue), time)
    else
      field.adjustInto(this, newValue)

  /** Returns a copy of this {@code LocalDateTime} with the year altered.
    * The time does not affect the calculation and will be the same in the result.
    * If the day-of-month is invalid for the year, it will be changed to the last valid day of the month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param year  the year to set in the result, from MIN_YEAR to MAX_YEAR
    * @return a { @code LocalDateTime} based on this date-time with the requested year, not null
    * @throws DateTimeException if the year value is invalid
    */
  def withYear(year: Int): LocalDateTime = `with`(date.withYear(year), time)

  /** Returns a copy of this {@code LocalDateTime} with the month-of-year altered.
    * The time does not affect the calculation and will be the same in the result.
    * If the day-of-month is invalid for the year, it will be changed to the last valid day of the month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param month  the month-of-year to set in the result, from 1 (January) to 12 (December)
    * @return a { @code LocalDateTime} based on this date-time with the requested month, not null
    * @throws DateTimeException if the month-of-year value is invalid
    */
  def withMonth(month: Int): LocalDateTime = `with`(date.withMonth(month), time)

  /** Returns a copy of this {@code LocalDateTime} with the day-of-month altered.
    * If the resulting {@code LocalDateTime} is invalid, an exception is thrown.
    * The time does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param dayOfMonth  the day-of-month to set in the result, from 1 to 28-31
    * @return a { @code LocalDateTime} based on this date-time with the requested day, not null
    * @throws DateTimeException if the day-of-month value is invalid
    * @throws DateTimeException if the day-of-month is invalid for the month-year
    */
  def withDayOfMonth(dayOfMonth: Int): LocalDateTime = `with`(date.withDayOfMonth(dayOfMonth), time)

  /** Returns a copy of this {@code LocalDateTime} with the day-of-year altered.
    * If the resulting {@code LocalDateTime} is invalid, an exception is thrown.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param dayOfYear  the day-of-year to set in the result, from 1 to 365-366
    * @return a { @code LocalDateTime} based on this date with the requested day, not null
    * @throws DateTimeException if the day-of-year value is invalid
    * @throws DateTimeException if the day-of-year is invalid for the year
    */
  def withDayOfYear(dayOfYear: Int): LocalDateTime = `with`(date.withDayOfYear(dayOfYear), time)

  /** Returns a copy of this {@code LocalDateTime} with the hour-of-day value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hour  the hour-of-day to set in the result, from 0 to 23
    * @return a { @code LocalDateTime} based on this date-time with the requested hour, not null
    * @throws DateTimeException if the hour value is invalid
    */
  def withHour(hour: Int): LocalDateTime =
    `with`(date, time.withHour(hour))

  /** Returns a copy of this {@code LocalDateTime} with the minute-of-hour value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minute  the minute-of-hour to set in the result, from 0 to 59
    * @return a { @code LocalDateTime} based on this date-time with the requested minute, not null
    * @throws DateTimeException if the minute value is invalid
    */
  def withMinute(minute: Int): LocalDateTime = {
    val newTime: LocalTime = time.withMinute(minute)
    `with`(date, newTime)
  }

  /** Returns a copy of this {@code LocalDateTime} with the second-of-minute value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param second  the second-of-minute to set in the result, from 0 to 59
    * @return a { @code LocalDateTime} based on this date-time with the requested second, not null
    * @throws DateTimeException if the second value is invalid
    */
  def withSecond(second: Int): LocalDateTime = {
    val newTime: LocalTime = time.withSecond(second)
    `with`(date, newTime)
  }

  /** Returns a copy of this {@code LocalDateTime} with the nano-of-second value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanoOfSecond  the nano-of-second to set in the result, from 0 to 999,999,999
    * @return a { @code LocalDateTime} based on this date-time with the requested nanosecond, not null
    * @throws DateTimeException if the nano value is invalid
    */
  def withNano(nanoOfSecond: Int): LocalDateTime = {
    val newTime: LocalTime = time.withNano(nanoOfSecond)
    `with`(date, newTime)
  }

  /** Returns a copy of this {@code LocalDateTime} with the time truncated.
    *
    * Truncation returns a copy of the original date-time with fields
    * smaller than the specified unit set to zero.
    * For example, truncating with the {@link ChronoUnit#MINUTES minutes} unit
    * will set the second-of-minute and nano-of-second field to zero.
    *
    * The unit must have a {@linkplain TemporalUnit#getDuration() duration}
    * that divides into the length of a standard day without remainder.
    * This includes all supplied time units on {@link ChronoUnit} and
    * {@link ChronoUnit#DAYS DAYS}. Other units throw an exception.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param unit  the unit to truncate to, not null
    * @return a { @code LocalDateTime} based on this date-time with the time truncated, not null
    * @throws DateTimeException if unable to truncate
    */
  def truncatedTo(unit: TemporalUnit): LocalDateTime = `with`(date, time.truncatedTo(unit))

  /** Returns a copy of this date-time with the specified period added.
    *
    * This method returns a new date-time based on this time with the specified period added.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return a { @code LocalDateTime} based on this date-time with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): LocalDateTime =
    amount.addTo(this).asInstanceOf[LocalDateTime]

  /** Returns a copy of this date-time with the specified period added.
    *
    * This method returns a new date-time based on this date-time with the specified period added.
    * This can be used to add any period that is defined by a unit, for example to add years, months or days.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the amount of the unit to add to the result, may be negative
    * @param unit  the unit of the period to add, not null
    * @return a { @code LocalDateTime} based on this date-time with the specified period added, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): LocalDateTime =
    unit match {
      case f: ChronoUnit =>
        import ChronoUnit._
        f match {
          case NANOS => plusNanos(amountToAdd)
          case MICROS =>
            plusDays(amountToAdd / MICROS_PER_DAY).plusNanos((amountToAdd % MICROS_PER_DAY) * 1000)
          case MILLIS =>
            plusDays(amountToAdd / MILLIS_PER_DAY)
              .plusNanos((amountToAdd % MILLIS_PER_DAY) * 1000000)
          case SECONDS   => plusSeconds(amountToAdd)
          case MINUTES   => plusMinutes(amountToAdd)
          case HOURS     => plusHours(amountToAdd)
          case HALF_DAYS => plusDays(amountToAdd / 256).plusHours((amountToAdd % 256) * 12)
          case _         => `with`(date.plus(amountToAdd, unit), time)
        }
      case _ =>
        unit.addTo(this, amountToAdd)
    }

  /** Returns a copy of this {@code LocalDateTime} with the specified period in years added.
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
    * @param years  the years to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the years added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusYears(years: Long): LocalDateTime = {
    val newDate: LocalDate = date.plusYears(years)
    `with`(newDate, time)
  }

  /** Returns a copy of this {@code LocalDateTime} with the specified period in months added.
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
    * @param months  the months to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the months added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusMonths(months: Long): LocalDateTime = {
    val newDate: LocalDate = date.plusMonths(months)
    `with`(newDate, time)
  }

  /** Returns a copy of this {@code LocalDateTime} with the specified period in weeks added.
    *
    * This method adds the specified amount in weeks to the days field incrementing
    * the month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2008-12-31 plus one week would result in 2009-01-07.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param weeks  the weeks to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the weeks added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusWeeks(weeks: Long): LocalDateTime = {
    val newDate: LocalDate = date.plusWeeks(weeks)
    `with`(newDate, time)
  }

  /** Returns a copy of this {@code LocalDateTime} with the specified period in days added.
    *
    * This method adds the specified amount to the days field incrementing the
    * month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2008-12-31 plus one day would result in 2009-01-01.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param days  the days to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the days added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusDays(days: Long): LocalDateTime = {
    val newDate: LocalDate = date.plusDays(days)
    `with`(newDate, time)
  }

  /** Returns a copy of this {@code LocalDateTime} with the specified period in hours added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hours  the hours to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the hours added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusHours(hours: Long): LocalDateTime = plusWithOverflow(date, hours, 0, 0, 0, 1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in minutes added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutes  the minutes to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the minutes added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusMinutes(minutes: Long): LocalDateTime = plusWithOverflow(date, 0, minutes, 0, 0, 1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in seconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param seconds  the seconds to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the seconds added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusSeconds(seconds: Long): LocalDateTime = plusWithOverflow(date, 0, 0, seconds, 0, 1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in nanoseconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanos  the nanos to add, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the nanoseconds added, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def plusNanos(nanos: Long): LocalDateTime = plusWithOverflow(date, 0, 0, 0, nanos, 1)

  /** Returns a copy of this date-time with the specified period subtracted.
    *
    * This method returns a new date-time based on this time with the specified period subtracted.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to subtract, not null
    * @return a { @code LocalDateTime} based on this date-time with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): LocalDateTime =
    amount.subtractFrom(this).asInstanceOf[LocalDateTime]

  /** Returns a copy of this date-time with the specified period subtracted.
    *
    * This method returns a new date-time based on this date-time with the specified period subtracted.
    * This can be used to subtract any period that is defined by a unit, for example to subtract years, months or days.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the amount of the unit to subtract from the result, may be negative
    * @param unit  the unit of the period to subtract, not null
    * @return a { @code LocalDateTime} based on this date-time with the specified period subtracted, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): LocalDateTime =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in years subtracted.
    *
    * This method subtracts the specified amount from the years field in three steps:
    * <ol>
    * <li>Subtract the input years from the year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2008-02-29 (leap year) minus one year would result in the
    * invalid date 2009-02-29 (standard year). Instead of returning an invalid
    * result, the last valid day of the month, 2009-02-28, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param years  the years to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the years subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusYears(years: Long): LocalDateTime =
    if (years == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-years)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in months subtracted.
    *
    * This method subtracts the specified amount from the months field in three steps:
    * <ol>
    * <li>Subtract the input months from the month-of-year field</li>
    * <li>Check if the resulting date would be invalid</li>
    * <li>Adjust the day-of-month to the last valid day if necessary</li>
    * </ol>
    *
    * For example, 2007-03-31 minus one month would result in the invalid date
    * 2007-04-31. Instead of returning an invalid result, the last valid day
    * of the month, 2007-04-30, is selected instead.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param months  the months to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the months subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusMonths(months: Long): LocalDateTime =
    if (months == Long.MinValue) plusMonths(Long.MaxValue).plusMonths(1)
    else plusMonths(-months)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in weeks subtracted.
    *
    * This method subtracts the specified amount in weeks from the days field decrementing
    * the month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2009-01-07 minus one week would result in 2008-12-31.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param weeks  the weeks to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the weeks subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusWeeks(weeks: Long): LocalDateTime =
    if (weeks == Long.MinValue) plusWeeks(Long.MaxValue).plusWeeks(1)
    else plusWeeks(-weeks)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in days subtracted.
    *
    * This method subtracts the specified amount from the days field incrementing the
    * month and year fields as necessary to ensure the result remains valid.
    * The result is only invalid if the maximum/minimum year is exceeded.
    *
    * For example, 2009-01-01 minus one day would result in 2008-12-31.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param days  the days to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the days subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusDays(days: Long): LocalDateTime =
    if (days == Long.MinValue) plusDays(Long.MaxValue).plusDays(1)
    else plusDays(-days)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in hours subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hours  the hours to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the hours subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusHours(hours: Long): LocalDateTime = plusWithOverflow(date, hours, 0, 0, 0, -1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in minutes subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutes  the minutes to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the minutes subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusMinutes(minutes: Long): LocalDateTime = plusWithOverflow(date, 0, minutes, 0, 0, -1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in seconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param seconds  the seconds to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the seconds subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusSeconds(seconds: Long): LocalDateTime = plusWithOverflow(date, 0, 0, seconds, 0, -1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period in nanoseconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanos  the nanos to subtract, may be negative
    * @return a { @code LocalDateTime} based on this date-time with the nanoseconds subtracted, not null
    * @throws DateTimeException if the result exceeds the supported date range
    */
  def minusNanos(nanos: Long): LocalDateTime = plusWithOverflow(date, 0, 0, 0, nanos, -1)

  /** Returns a copy of this {@code LocalDateTime} with the specified period added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param newDate  the new date to base the calculation on, not null
    * @param hours  the hours to add, may be negative
    * @param minutes the minutes to add, may be negative
    * @param seconds the seconds to add, may be negative
    * @param nanos the nanos to add, may be negative
    * @param sign  the sign to determine add or subtract
    * @return the combined result, not null
    */
  private def plusWithOverflow(
    newDate: LocalDate,
    hours:   Long,
    minutes: Long,
    seconds: Long,
    nanos:   Long,
    sign:    Int
  ): LocalDateTime = {
    if ((hours | minutes | seconds | nanos) == 0)
      return `with`(newDate, time)
    var totDays: Long =
      nanos / NANOS_PER_DAY + seconds / SECONDS_PER_DAY + minutes / MINUTES_PER_DAY + hours / HOURS_PER_DAY
    totDays *= sign
    var totNanos: Long =
      nanos % NANOS_PER_DAY + (seconds % SECONDS_PER_DAY) * NANOS_PER_SECOND + (minutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE + (hours % HOURS_PER_DAY) * NANOS_PER_HOUR
    val curNoD: Long = time.toNanoOfDay
    totNanos = totNanos * sign + curNoD
    totDays += Math.floorDiv(totNanos, NANOS_PER_DAY)
    val newNoD: Long       = Math.floorMod(totNanos, NANOS_PER_DAY)
    val newTime: LocalTime = if (newNoD == curNoD) time else LocalTime.ofNanoOfDay(newNoD)
    `with`(newDate.plusDays(totDays), newTime)
  }

  /** Queries this date-time using the specified query.
    *
    * This queries this date-time using the specified query strategy object.
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
    if (query eq TemporalQueries.localDate) toLocalDate.asInstanceOf[R]
    else super.query(query)

  /** Adjusts the specified temporal object to have the same date and time as this object.
    *
    * This returns a temporal object of the same observable type as the input
    * with the date and time changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * twice, passing {@link ChronoField#EPOCH_DAY} and
    * {@link ChronoField#NANO_OF_DAY} as the fields.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    *   // these two lines are equivalent, but the second approach is recommended
    *   temporal = thisLocalDateTime.adjustInto(temporal);
    *   temporal = temporal.with(thisLocalDateTime);
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

  /** Calculates the period between this date-time and another date-time in
    * terms of the specified unit.
    *
    * This calculates the period between two date-times in terms of a single unit.
    * The start and end points are {@code this} and the specified date-time.
    * The result will be negative if the end is before the start.
    * The {@code Temporal} passed to this method must be a {@code LocalDateTime}.
    * For example, the period in days between two date-times can be calculated
    * using {@code startDateTime.until(endDateTime, DAYS)}.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two date-times.
    * For example, the period in months between 2012-06-15T00:00 and 2012-08-14T23:59
    * will only be one month as it is one minute short of two months.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * <pre>
    *   long period = start.until(end, MONTHS);   // this method
    *   dateTime.plus(MONTHS.between(start, end));      // use in plus/minus
    * </pre>
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code NANOS}, {@code MICROS}, {@code MILLIS}, {@code SECONDS},
    * {@code MINUTES}, {@code HOURS} and {@code HALF_DAYS}, {@code DAYS},
    * {@code WEEKS}, {@code MONTHS}, {@code YEARS}, {@code DECADES},
    * {@code CENTURIES}, {@code MILLENNIA} and {@code ERAS} are supported.
    * Other {@code ChronoUnit} values will throw an exception.
    *
    * If the unit is not a {@code ChronoUnit}, then the result of this method
    * is obtained by invoking {@code TemporalUnit.between(Temporal, Temporal)}
    * passing {@code this} as the first argument and the input temporal as
    * the second argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endExclusive  the end date-time, which is converted to a { @code LocalDateTime}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this date-time and the end date-time
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    // Size reduced
    val end: LocalDateTime = LocalDateTime.from(endExclusive)
    unit match {
      case f: ChronoUnit =>
        if (f.isTimeBased) {
          var daysUntil: Long = date.daysUntil(end.date)
          var timeUntil: Long = end.time.toNanoOfDay - time.toNanoOfDay
          if (daysUntil > 0 && timeUntil < 0) {
            daysUntil -= 1
            timeUntil += NANOS_PER_DAY
          } else if (daysUntil < 0 && timeUntil > 0) {
            daysUntil += 1
            timeUntil -= NANOS_PER_DAY
          }
          import ChronoUnit._
          f match {
            case NANOS =>
              return Math.addExact(Math.multiplyExact(daysUntil, NANOS_PER_DAY), timeUntil)
            case MICROS =>
              return Math.addExact(Math.multiplyExact(daysUntil, MICROS_PER_DAY), timeUntil / 1000)
            case MILLIS =>
              return Math.addExact(Math.multiplyExact(daysUntil, MILLIS_PER_DAY),
                                   timeUntil / 1000000)
            case SECONDS =>
              return Math.addExact(Math.multiplyExact(daysUntil, SECONDS_PER_DAY.toLong),
                                   timeUntil / NANOS_PER_SECOND)
            case MINUTES =>
              return Math.addExact(Math.multiplyExact(daysUntil, MINUTES_PER_DAY.toLong),
                                   timeUntil / NANOS_PER_MINUTE)
            case HOURS =>
              return Math.addExact(Math.multiplyExact(daysUntil, HOURS_PER_DAY.toLong),
                                   timeUntil / NANOS_PER_HOUR)
            case HALF_DAYS =>
              return Math.addExact(Math.multiplyExact(daysUntil, 2),
                                   timeUntil / (NANOS_PER_HOUR * 12))
            case _ => throw UnsupportedTemporalTypeException.unit(unit)

          }
        }
        val endDate =
          if (end.date.isAfter(date) && end.time.isBefore(time)) {
            end.date.minusDays(1)
          } else if (end.date.isBefore(date) && end.time.isAfter(time)) {
            end.date.plusDays(1)
          } else {
            end.date
          }
        date.until(endDate, unit)
      case _ =>
        unit.between(this, end)
    }
  }

  /** Combines this date-time with an offset to create an {@code OffsetDateTime}.
    *
    * This returns an {@code OffsetDateTime} formed from this date-time at the specified offset.
    * All possible combinations of date-time and offset are valid.
    *
    * @param offset  the offset to combine with, not null
    * @return the offset date-time formed from this date-time and the specified offset, not null
    */
  def atOffset(offset: ZoneOffset): OffsetDateTime = OffsetDateTime.of(this, offset)

  /** Combines this date-time with a time-zone to create a {@code ZonedDateTime}.
    *
    * This returns a {@code ZonedDateTime} formed from this date-time at the
    * specified time-zone. The result will match this date-time as closely as possible.
    * Time-zone rules, such as daylight savings, mean that not every local date-time
    * is valid for the specified zone, thus the local date-time may be adjusted.
    *
    * The local date-time is resolved to a single instant on the time-line.
    * This is achieved by finding a valid offset from UTC/Greenwich for the local
    * date-time as defined by the {@link ZoneRules rules} of the zone ID.
    *<p>
    * In most cases, there is only one valid offset for a local date-time.
    * In the case of an overlap, where clocks are set back, there are two valid offsets.
    * This method uses the earlier offset typically corresponding to "summer".
    *
    * In the case of a gap, where clocks jump forward, there is no valid offset.
    * Instead, the local date-time is adjusted to be later by the length of the gap.
    * For a typical one hour daylight savings change, the local date-time will be
    * moved one hour later into the offset typically corresponding to "summer".
    *
    * To obtain the later offset during an overlap, call
    * {@link ZonedDateTime#withLaterOffsetAtOverlap()} on the result of this method.
    * To throw an exception when there is a gap or overlap, use
    * {@link ZonedDateTime#ofStrict(LocalDateTime, ZoneOffset, ZoneId)}.
    *
    * @param zone  the time-zone to use, not null
    * @return the zoned date-time formed from this date-time, not null
    */
  def atZone(zone: ZoneId): ZonedDateTime = ZonedDateTime.of(this, zone)

  /** Gets the {@code LocalDate} part of this date-time.
    *
    * This returns a {@code LocalDate} with the same year, month and day
    * as this date-time.
    *
    * @return the date part of this date-time, not null
    */
  def toLocalDate: LocalDate = date

  /** Gets the {@code LocalTime} part of this date-time.
    *
    * This returns a {@code LocalTime} with the same hour, minute, second and
    * nanosecond as this date-time.
    *
    * @return the time part of this date-time, not null
    */
  def toLocalTime: LocalTime = time

  /** Compares this date-time to another date-time.
    *
    * The comparison is primarily based on the date-time, from earliest to latest.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * If all the date-times being compared are instances of {@code LocalDateTime},
    * then the comparison will be entirely based on the date-time.
    * If some dates being compared are in different chronologies, then the
    * chronology is also considered, see {@link ChronoLocalDateTime#compareTo}.
    *
    * @param other  the other date-time to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  override def compareTo(other: ChronoLocalDateTime[_]): Int =
    other match {
      case l: LocalDateTime => compareTo0(l)
      case _                => super.compareTo(other)
    }

  private def compareTo0(other: LocalDateTime): Int = {
    var cmp: Int = date.compareTo0(other.toLocalDate)
    if (cmp == 0)
      cmp = time.compareTo(other.toLocalTime)
    cmp
  }

  /** Checks if this date-time is after the specified date-time.
    *
    * This checks to see if this date-time represents a point on the
    * local time-line after the other date-time.
    * <pre>
    *   LocalDate a = LocalDateTime.of(2012, 6, 30, 12, 00);
    *   LocalDate b = LocalDateTime.of(2012, 7, 1, 12, 00);
    *   a.isAfter(b) == false
    *   a.isAfter(a) == false
    *   b.isAfter(a) == true
    * </pre>
    *
    * This method only considers the position of the two date-times on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDateTime)},
    * but is the same approach as {@link #DATE_TIME_COMPARATOR}.
    *
    * @param other  the other date-time to compare to, not null
    * @return true if this date-time is after the specified date-time
    */
  override def isAfter(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean =
    other match {
      case l: LocalDateTime => compareTo0(l) > 0
      case _                => super.isAfter(other)
    }

  /** Checks if this date-time is before the specified date-time.
    *
    * This checks to see if this date-time represents a point on the
    * local time-line before the other date-time.
    * <pre>
    *   LocalDate a = LocalDateTime.of(2012, 6, 30, 12, 00);
    *   LocalDate b = LocalDateTime.of(2012, 7, 1, 12, 00);
    *   a.isBefore(b) == true
    *   a.isBefore(a) == false
    *   b.isBefore(a) == false
    * </pre>
    *
    * This method only considers the position of the two date-times on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDateTime)},
    * but is the same approach as {@link #DATE_TIME_COMPARATOR}.
    *
    * @param other  the other date-time to compare to, not null
    * @return true if this date-time is before the specified date-time
    */
  override def isBefore(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean =
    other match {
      case l: LocalDateTime => compareTo0(l) < 0
      case _                => super.isBefore(other)
    }

  /** Checks if this date-time is equal to the specified date-time.
    *
    * This checks to see if this date-time represents the same point on the
    * local time-line as the other date-time.
    * <pre>
    *   LocalDate a = LocalDateTime.of(2012, 6, 30, 12, 00);
    *   LocalDate b = LocalDateTime.of(2012, 7, 1, 12, 00);
    *   a.isEqual(b) == false
    *   a.isEqual(a) == true
    *   b.isEqual(a) == false
    * </pre>
    *
    * This method only considers the position of the two date-times on the local time-line.
    * It does not take into account the chronology, or calendar system.
    * This is different from the comparison in {@link #compareTo(ChronoLocalDateTime)},
    * but is the same approach as {@link #DATE_TIME_COMPARATOR}.
    *
    * @param other  the other date-time to compare to, not null
    * @return true if this date-time is equal to the specified date-time
    */
  override def isEqual(other: ChronoLocalDateTime[_ <: ChronoLocalDate]): Boolean =
    other match {
      case l: LocalDateTime => compareTo0(l) == 0
      case _                => super.isEqual(other)
    }

  /** Checks if this date-time is equal to another date-time.
    *
    * Compares this {@code LocalDateTime} with another ensuring that the date-time is the same.
    * Only objects of type {@code LocalDateTime} are compared, other types return false.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other date-time
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: LocalDateTime => (this eq other) || ((date == other.date) && (time == other.time))
      case _                    => false
    }

  /** A hash code for this date-time.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = date.hashCode ^ time.hashCode

  /** Outputs this date-time as a {@code String}, such as {@code 2007-12-03T10:15:30}.
    *
    * The output will be one of the following ISO-8601 formats:
    *<ul>
    * <li>{@code yyyy-MM-dd'T'HH:mm}</li>
    * <li>{@code yyyy-MM-dd'T'HH:mm:ss}</li>
    * <li>{@code yyyy-MM-dd'T'HH:mm:ss.SSS}</li>
    * <li>{@code yyyy-MM-dd'T'HH:mm:ss.SSSSSS}</li>
    * <li>{@code yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSS}</li>
    * </ul><p>
    * The format used will be the shortest that outputs the full value of
    * the time where the omitted parts are implied to be zero.
    *
    * @return a string representation of this date-time, not null
    */
  override def toString: String = date.toString + 'T' + time.toString

  /** Outputs this date-time as a {@code String} using the formatter.
    *
    * This date-time will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted date-time string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  override def format(formatter: DateTimeFormatter): String = super.format(formatter)

  private def writeReplace: AnyRef = new Ser(Ser.LOCAL_DATE_TIME_TYPE, this)

  /** Defend against malicious streams.
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit = {
    date.writeExternal(out)
    time.writeExternal(out)
  }
}
