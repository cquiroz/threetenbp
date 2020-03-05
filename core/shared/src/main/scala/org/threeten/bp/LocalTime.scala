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

import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.MICRO_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.SECOND_OF_DAY
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
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

@SerialVersionUID(6414437269572265201L)
object LocalTime {

  /** Constants for the local time of each hour. */
  private lazy val HOURS: Array[LocalTime] = {
    val hours  = new Array[LocalTime](24)
    var i: Int = 0
    while (i < hours.length) {
      hours(i) = new LocalTime(i, 0, 0, 0)
      i += 1
    }
    hours
  }

  /** The minimum supported {@code LocalTime}, '00:00'.
    * This is the time of midnight at the start of the day.
    */
  lazy val MIN: LocalTime = HOURS(0)

  /** The maximum supported {@code LocalTime}, '23:59:59.999999999'.
    * This is the time just before midnight at the end of the day.
    */
  lazy val MAX: LocalTime = new LocalTime(23, 59, 59, 999999999)

  /** The time of midnight at the start of the day, '00:00'. */
  lazy val MIDNIGHT: LocalTime = HOURS(0)

  /** The time of noon in the middle of the day, '12:00'. */
  lazy val NOON: LocalTime = HOURS(12)

  /** Hours per day. */
  private[bp] def HOURS_PER_DAY: Int = 24

  /** Minutes per hour. */
  private[bp] def MINUTES_PER_HOUR: Int = 60

  /** Minutes per day. */
  private[bp] def MINUTES_PER_DAY: Int = MINUTES_PER_HOUR * HOURS_PER_DAY

  /** Seconds per minute. */
  private[bp] def SECONDS_PER_MINUTE: Int = 60

  /** Seconds per hour. */
  private[bp] def SECONDS_PER_HOUR: Int = SECONDS_PER_MINUTE * MINUTES_PER_HOUR

  /** Seconds per day. */
  private[bp] def SECONDS_PER_DAY: Int = SECONDS_PER_HOUR * HOURS_PER_DAY

  /** Milliseconds per day. */
  private[bp] def MILLIS_PER_DAY: Long = SECONDS_PER_DAY * 1000L

  /** Microseconds per day. */
  private[bp] def MICROS_PER_DAY: Long = SECONDS_PER_DAY * 1000000L

  /** Nanos per second. */
  private[bp] def NANOS_PER_SECOND: Long = 1000000000L

  /** Nanos per minute. */
  private[bp] def NANOS_PER_MINUTE: Long = NANOS_PER_SECOND * SECONDS_PER_MINUTE

  /** Nanos per hour. */
  private[bp] def NANOS_PER_HOUR: Long = NANOS_PER_MINUTE * MINUTES_PER_HOUR

  /** Nanos per day. */
  private[bp] def NANOS_PER_DAY: Long = NANOS_PER_HOUR * HOURS_PER_DAY

  /** Obtains the current time from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current time.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current time using the system clock and default time-zone, not null
    */
  def now: LocalTime = now(Clock.systemDefaultZone)

  /** Obtains the current time from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current time.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current time using the system clock, not null
    */
  def now(zone: ZoneId): LocalTime = now(Clock.system(zone))

  /** Obtains the current time from the specified clock.
    *
    * This will query the specified clock to obtain the current time.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current time, not null
    */
  def now(clock: Clock): LocalTime = {
    Objects.requireNonNull(clock, "clock")
    val now: Instant       = clock.instant
    val offset: ZoneOffset = clock.getZone.getRules.getOffset(now)
    var secsOfDay: Long    = now.getEpochSecond % SECONDS_PER_DAY
    secsOfDay = (secsOfDay + offset.getTotalSeconds) % SECONDS_PER_DAY
    if (secsOfDay < 0)
      secsOfDay += SECONDS_PER_DAY
    LocalTime.ofSecondOfDay(secsOfDay, now.getNano)
  }

  /** Obtains an instance of {@code LocalTime} from an hour and minute.
    *
    * The second and nanosecond fields will be set to zero by this factory method.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @return the local time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def of(hour: Int, minute: Int): LocalTime = {
    HOUR_OF_DAY.checkValidValue(hour.toLong)
    if (minute == 0)
      HOURS(hour)
    else {
      MINUTE_OF_HOUR.checkValidValue(minute.toLong)
      new LocalTime(hour, minute, 0, 0)
    }
  }

  /** Obtains an instance of {@code LocalTime} from an hour, minute and second.
    *
    * The nanosecond field will be set to zero by this factory method.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @return the local time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def of(hour: Int, minute: Int, second: Int): LocalTime = {
    HOUR_OF_DAY.checkValidValue(hour.toLong)
    if ((minute | second) == 0)
      return HOURS(hour)
    MINUTE_OF_HOUR.checkValidValue(minute.toLong)
    SECOND_OF_MINUTE.checkValidValue(second.toLong)
    new LocalTime(hour, minute, second, 0)
  }

  /** Obtains an instance of {@code LocalTime} from an hour, minute, second and nanosecond.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @return the local time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def of(hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalTime = {
    HOUR_OF_DAY.checkValidValue(hour.toLong)
    MINUTE_OF_HOUR.checkValidValue(minute.toLong)
    SECOND_OF_MINUTE.checkValidValue(second.toLong)
    NANO_OF_SECOND.checkValidValue(nanoOfSecond.toLong)
    create(hour, minute, second, nanoOfSecond)
  }

  /** Obtains an instance of {@code LocalTime} from a second-of-day value.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param secondOfDay  the second-of-day, from { @code 0} to { @code 24 * 60 * 60 - 1}
    * @return the local time, not null
    * @throws DateTimeException if the second-of-day value is invalid
    */
  def ofSecondOfDay(secondOfDay: Long): LocalTime = {
    var _secondOfDay = secondOfDay
    SECOND_OF_DAY.checkValidValue(_secondOfDay)
    val hours: Int = (_secondOfDay / SECONDS_PER_HOUR).toInt
    _secondOfDay -= hours * SECONDS_PER_HOUR
    val minutes: Int = (_secondOfDay / SECONDS_PER_MINUTE).toInt
    _secondOfDay -= minutes * SECONDS_PER_MINUTE
    create(hours, minutes, _secondOfDay.toInt, 0)
  }

  /** Obtains an instance of {@code LocalTime} from a second-of-day value, with
    * associated nanos of second.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param secondOfDay  the second-of-day, from { @code 0} to { @code 24 * 60 * 60 - 1}
    * @param nanoOfSecond  the nano-of-second, from 0 to 999,999,999
    * @return the local time, not null
    * @throws DateTimeException if the either input value is invalid
    */
  private[bp] def ofSecondOfDay(secondOfDay: Long, nanoOfSecond: Int): LocalTime = {
    var _secondOfDay = secondOfDay
    SECOND_OF_DAY.checkValidValue(_secondOfDay)
    NANO_OF_SECOND.checkValidValue(nanoOfSecond.toLong)
    val hours: Int = (_secondOfDay / SECONDS_PER_HOUR).toInt
    _secondOfDay -= hours * SECONDS_PER_HOUR
    val minutes: Int = (_secondOfDay / SECONDS_PER_MINUTE).toInt
    _secondOfDay -= minutes * SECONDS_PER_MINUTE
    create(hours, minutes, _secondOfDay.toInt, nanoOfSecond)
  }

  /** Obtains an instance of {@code LocalTime} from a nanos-of-day value.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param nanoOfDay  the nano of day, from { @code 0} to { @code 24 * 60 * 60 * 1,000,000,000 - 1}
    * @return the local time, not null
    * @throws DateTimeException if the nanos of day value is invalid
    */
  def ofNanoOfDay(nanoOfDay: Long): LocalTime = {
    var _nanoOfDay = nanoOfDay
    NANO_OF_DAY.checkValidValue(_nanoOfDay)
    val hours: Int = (_nanoOfDay / NANOS_PER_HOUR).toInt
    _nanoOfDay -= hours * NANOS_PER_HOUR
    val minutes: Int = (_nanoOfDay / NANOS_PER_MINUTE).toInt
    _nanoOfDay -= minutes * NANOS_PER_MINUTE
    val seconds: Int = (_nanoOfDay / NANOS_PER_SECOND).toInt
    _nanoOfDay -= seconds * NANOS_PER_SECOND
    create(hours, minutes, seconds, _nanoOfDay.toInt)
  }

  /** Obtains an instance of {@code LocalTime} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code LocalTime}.
    *
    * The conversion uses the {@link TemporalQueries#localTime()} query, which relies
    * on extracting the {@link ChronoField#NANO_OF_DAY NANO_OF_DAY} field.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code LocalTime::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the local time, not null
    * @throws DateTimeException if unable to convert to a { @code LocalTime}
    */
  def from(temporal: TemporalAccessor): LocalTime = {
    val time: LocalTime = temporal.query(TemporalQueries.localTime)
    if (time == null)
      throw new DateTimeException(
        s"Unable to obtain LocalTime from TemporalAccessor: $temporal, type ${temporal.getClass.getName}"
      )
    else
      time
  }

  /** Obtains an instance of {@code LocalTime} from a text string such as {@code 10:15}.
    *
    * The string must represent a valid time and is parsed using
    * {@link org.threeten.bp.format.DateTimeFormatter#ISO_LOCAL_TIME}.
    *
    * @param text the text to parse such as "10:15:30", not null
    * @return the parsed local time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): LocalTime = parse(text, DateTimeFormatter.ISO_LOCAL_TIME)

  /** Obtains an instance of {@code LocalTime} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a time.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed local time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): LocalTime = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[LocalTime] {
      override def queryFrom(temporal: TemporalAccessor): LocalTime = LocalTime.from(temporal)
    })
  }

  /** Creates a local time from the hour, minute, second and nanosecond fields.
    *
    * This factory may return a cached value, but applications must not rely on this.
    *
    * @param hour  the hour-of-day to represent, validated from 0 to 23
    * @param minute  the minute-of-hour to represent, validated from 0 to 59
    * @param second  the second-of-minute to represent, validated from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, validated from 0 to 999,999,999
    * @return the local time, not null
    */
  private def create(hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalTime =
    if ((minute | second | nanoOfSecond) == 0) HOURS(hour)
    else new LocalTime(hour, minute, second, nanoOfSecond)

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): LocalTime = {
    var hour: Int   = in.readByte.toInt
    var minute: Int = 0
    var second: Int = 0
    var nano: Int   = 0
    if (hour < 0)
      hour = ~hour
    else {
      minute = in.readByte.toInt
      if (minute < 0)
        minute = ~minute
      else {
        second = in.readByte.toInt
        if (second < 0)
          second = ~second
        else
          nano = in.readInt
      }
    }
    LocalTime.of(hour, minute, second, nano)
  }
}

/** A time without time-zone in the ISO-8601 calendar system,
  * such as {@code 10:15:30}.
  *
  * {@code LocalTime} is an immutable date-time object that represents a time,
  * often viewed as hour-minute-second.
  * Time is represented to nanosecond precision.
  * For example, the value "13:45.30.123456789" can be stored in a {@code LocalTime}.
  *
  * It does not store or represent a date or time-zone.
  * Instead, it is a description of the local time as seen on a wall clock.
  * It cannot represent an instant on the time-line without additional information
  * such as an offset or time-zone.
  *
  * The ISO-8601 calendar system is the modern civil calendar system used today
  * in most of the world. This API assumes that all calendar systems use the same
  * representation, this class, for time-of-day.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor Constructor, previously validated.
  * @param _hour  the hour-of-day to represent, validated from 0 to 23
  * @param _minute  the minute-of-hour to represent, validated from 0 to 59
  * @param _second  the second-of-minute to represent, validated from 0 to 59
  * @param nano  the nano-of-second to represent, validated from 0 to 999,999,999
  */
@SerialVersionUID(6414437269572265201L)
final class LocalTime(_hour: Int, _minute: Int, _second: Int, private val nano: Int)
    extends TemporalAccessor
    with Temporal
    with TemporalAdjuster
    with Ordered[LocalTime]
    with Serializable {

  /** The hour. */
  private val hour: Byte = _hour.toByte

  /** The minute. */
  private val minute: Byte = _minute.toByte

  /** The second. */
  private val second: Byte = _second.toByte

  /** Checks if the specified field is supported.
    *
    * This checks if this time can be queried for the specified field.
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
    * </ul>
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this time, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField]) field.isTimeBased
    else field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit]) unit.isTimeBased
    else unit != null && unit.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This time is used to enhance the accuracy of the returned range.
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
  override def range(field: TemporalField): ValueRange = super.range(field)

  /** Gets the value of the specified field from this time as an {@code int}.
    *
    * This queries this time for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this time, except {@code NANO_OF_DAY} and {@code MICRO_OF_DAY}
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

  /** Gets the value of the specified field from this time as a {@code long}.
    *
    * This queries this time for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this time.
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
      if (field eq NANO_OF_DAY)
        return toNanoOfDay
      if (field eq MICRO_OF_DAY)
        return toNanoOfDay / 1000
      get0(field)
    } else {
      field.getFrom(this)
    }

  private def get0(field: TemporalField): Long = {
    import ChronoField._
    field.asInstanceOf[ChronoField] match {
      case NANO_OF_SECOND     => nano.toLong
      case NANO_OF_DAY        => throw new DateTimeException(s"Field too large for an int: $field")
      case MICRO_OF_SECOND    => nano.toLong / 1000L
      case MICRO_OF_DAY       => throw new DateTimeException(s"Field too large for an int: $field")
      case MILLI_OF_SECOND    => nano.toLong / 1000000L
      case MILLI_OF_DAY       => (toNanoOfDay / 1000000L)
      case SECOND_OF_MINUTE   => second.toLong
      case SECOND_OF_DAY      => toSecondOfDay.toLong
      case MINUTE_OF_HOUR     => minute.toLong
      case MINUTE_OF_DAY      => hour * 60L + minute
      case HOUR_OF_AMPM       => hour % 12L
      case CLOCK_HOUR_OF_AMPM => val ham: Long = hour % 12L; if (ham % 12 == 0) 12 else ham
      case HOUR_OF_DAY        => hour.toLong
      case CLOCK_HOUR_OF_DAY  => if (hour == 0) 24 else hour.toLong
      case AMPM_OF_DAY        => hour.toLong / 12L
      case _                  => throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
    }
  }

  /** Gets the hour-of-day field.
    *
    * @return the hour-of-day, from 0 to 23
    */
  def getHour: Int = hour.toInt

  /** Gets the minute-of-hour field.
    *
    * @return the minute-of-hour, from 0 to 59
    */
  def getMinute: Int = minute.toInt

  /** Gets the second-of-minute field.
    *
    * @return the second-of-minute, from 0 to 59
    */
  def getSecond: Int = second.toInt

  /** Gets the nano-of-second field.
    *
    * @return the nano-of-second, from 0 to 999,999,999
    */
  def getNano: Int = nano

  /** Returns an adjusted copy of this time.
    *
    * This returns a new {@code LocalTime}, based on this one, with the time adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * A simple adjuster might simply set the one of the fields, such as the hour field.
    * A more complex adjuster might set the time to the last hour of the day.
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return a { @code LocalTime} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): LocalTime =
    adjuster match {
      case time: LocalTime => time
      case _               => adjuster.adjustInto(this).asInstanceOf[LocalTime]
    }

  /** Returns a copy of this time with the specified field set to a new value.
    *
    * This returns a new {@code LocalTime}, based on this one, with the value
    * for the specified field changed.
    * This can be used to change any supported field, such as the hour, minute or second.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The supported fields behave as follows:
    * <ul>
    * <li>{@code NANO_OF_SECOND} -
    * Returns a {@code LocalTime} with the specified nano-of-second.
    * The hour, minute and second will be unchanged.
    * <li>{@code NANO_OF_DAY} -
    * Returns a {@code LocalTime} with the specified nano-of-day.
    * This completely replaces the time and is equivalent to {@link #ofNanoOfDay(long)}.
    * <li>{@code MICRO_OF_SECOND} -
    * Returns a {@code LocalTime} with the nano-of-second replaced by the specified
    * micro-of-second multiplied by 1,000.
    * The hour, minute and second will be unchanged.
    * <li>{@code MICRO_OF_DAY} -
    * Returns a {@code LocalTime} with the specified micro-of-day.
    * This completely replaces the time and is equivalent to using {@link #ofNanoOfDay(long)}
    * with the micro-of-day multiplied by 1,000.
    * <li>{@code MILLI_OF_SECOND} -
    * Returns a {@code LocalTime} with the nano-of-second replaced by the specified
    * milli-of-second multiplied by 1,000,000.
    * The hour, minute and second will be unchanged.
    * <li>{@code MILLI_OF_DAY} -
    * Returns a {@code LocalTime} with the specified milli-of-day.
    * This completely replaces the time and is equivalent to using {@link #ofNanoOfDay(long)}
    * with the milli-of-day multiplied by 1,000,000.
    * <li>{@code SECOND_OF_MINUTE} -
    * Returns a {@code LocalTime} with the specified second-of-minute.
    * The hour, minute and nano-of-second will be unchanged.
    * <li>{@code SECOND_OF_DAY} -
    * Returns a {@code LocalTime} with the specified second-of-day.
    * The nano-of-second will be unchanged.
    * <li>{@code MINUTE_OF_HOUR} -
    * Returns a {@code LocalTime} with the specified minute-of-hour.
    * The hour, second-of-minute and nano-of-second will be unchanged.
    * <li>{@code MINUTE_OF_DAY} -
    * Returns a {@code LocalTime} with the specified minute-of-day.
    * The second-of-minute and nano-of-second will be unchanged.
    * <li>{@code HOUR_OF_AMPM} -
    * Returns a {@code LocalTime} with the specified hour-of-am-pm.
    * The AM/PM, minute-of-hour, second-of-minute and nano-of-second will be unchanged.
    * <li>{@code CLOCK_HOUR_OF_AMPM} -
    * Returns a {@code LocalTime} with the specified clock-hour-of-am-pm.
    * The AM/PM, minute-of-hour, second-of-minute and nano-of-second will be unchanged.
    * <li>{@code HOUR_OF_DAY} -
    * Returns a {@code LocalTime} with the specified hour-of-day.
    * The minute-of-hour, second-of-minute and nano-of-second will be unchanged.
    * <li>{@code CLOCK_HOUR_OF_DAY} -
    * Returns a {@code LocalTime} with the specified clock-hour-of-day.
    * The minute-of-hour, second-of-minute and nano-of-second will be unchanged.
    * <li>{@code AMPM_OF_DAY} -
    * Returns a {@code LocalTime} with the specified AM/PM.
    * The hour-of-am-pm, minute-of-hour, second-of-minute and nano-of-second will be unchanged.
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
    * @return a { @code LocalTime} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): LocalTime =
    field match {
      case f: ChronoField =>
        f.checkValidValue(newValue)
        import ChronoField._
        f match {
          case NANO_OF_SECOND     => withNano(newValue.toInt)
          case NANO_OF_DAY        => LocalTime.ofNanoOfDay(newValue)
          case MICRO_OF_SECOND    => withNano(newValue.toInt * 1000)
          case MICRO_OF_DAY       => LocalTime.ofNanoOfDay(newValue * 1000)
          case MILLI_OF_SECOND    => withNano(newValue.toInt * 1000000)
          case MILLI_OF_DAY       => LocalTime.ofNanoOfDay(newValue * 1000000)
          case SECOND_OF_MINUTE   => withSecond(newValue.toInt)
          case SECOND_OF_DAY      => plusSeconds(newValue - toSecondOfDay)
          case MINUTE_OF_HOUR     => withMinute(newValue.toInt)
          case MINUTE_OF_DAY      => plusMinutes(newValue - (hour * 60 + minute))
          case HOUR_OF_AMPM       => plusHours(newValue - (hour % 12))
          case CLOCK_HOUR_OF_AMPM => plusHours((if (newValue == 12) 0 else newValue) - (hour % 12))
          case HOUR_OF_DAY        => withHour(newValue.toInt)
          case CLOCK_HOUR_OF_DAY  => withHour((if (newValue == 24) 0 else newValue).toInt)
          case AMPM_OF_DAY        => plusHours((newValue - (hour / 12)) * 12)
          case _                  => throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
        }
      case _ =>
        field.adjustInto(this, newValue)
    }

  /** Returns a copy of this {@code LocalTime} with the hour-of-day value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hour  the hour-of-day to set in the result, from 0 to 23
    * @return a { @code LocalTime} based on this time with the requested hour, not null
    * @throws DateTimeException if the hour value is invalid
    */
  def withHour(hour: Int): LocalTime =
    if (this.hour == hour)
      this
    else {
      HOUR_OF_DAY.checkValidValue(hour.toLong)
      LocalTime.create(hour, minute.toInt, second.toInt, nano)
    }

  /** Returns a copy of this {@code LocalTime} with the minute-of-hour value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minute  the minute-of-hour to set in the result, from 0 to 59
    * @return a { @code LocalTime} based on this time with the requested minute, not null
    * @throws DateTimeException if the minute value is invalid
    */
  def withMinute(minute: Int): LocalTime =
    if (this.minute == minute)
      this
    else {
      MINUTE_OF_HOUR.checkValidValue(minute.toLong)
      LocalTime.create(hour.toInt, minute, second.toInt, nano)
    }

  /** Returns a copy of this {@code LocalTime} with the second-of-minute value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param second  the second-of-minute to set in the result, from 0 to 59
    * @return a { @code LocalTime} based on this time with the requested second, not null
    * @throws DateTimeException if the second value is invalid
    */
  def withSecond(second: Int): LocalTime =
    if (this.second == second)
      this
    else {
      SECOND_OF_MINUTE.checkValidValue(second.toLong)
      LocalTime.create(hour.toInt, minute.toInt, second, nano)
    }

  /** Returns a copy of this {@code LocalTime} with the nano-of-second value altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanoOfSecond  the nano-of-second to set in the result, from 0 to 999,999,999
    * @return a { @code LocalTime} based on this time with the requested nanosecond, not null
    * @throws DateTimeException if the nanos value is invalid
    */
  def withNano(nanoOfSecond: Int): LocalTime =
    if (this.nano == nanoOfSecond)
      this
    else {
      NANO_OF_SECOND.checkValidValue(nanoOfSecond.toLong)
      LocalTime.create(hour.toInt, minute.toInt, second.toInt, nanoOfSecond)
    }

  /** Returns a copy of this {@code LocalTime} with the time truncated.
    *
    * Truncating the time returns a copy of the original time with fields
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
    * @return a { @code LocalTime} based on this time with the time truncated, not null
    * @throws DateTimeException if unable to truncate
    */
  def truncatedTo(unit: TemporalUnit): LocalTime = {
    if (unit eq ChronoUnit.NANOS)
      return this
    val unitDur: Duration = unit.getDuration
    if (unitDur.getSeconds > LocalTime.SECONDS_PER_DAY)
      throw new DateTimeException("Unit is too large to be used for truncation")
    val dur: Long = unitDur.toNanos
    if ((LocalTime.NANOS_PER_DAY % dur) != 0)
      throw new DateTimeException("Unit must divide into a standard day without remainder")
    val nod: Long = toNanoOfDay
    LocalTime.ofNanoOfDay((nod / dur) * dur)
  }

  /** Returns a copy of this date with the specified period added.
    *
    * This method returns a new time based on this time with the specified period added.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return a { @code LocalTime} based on this time with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): LocalTime = amount.addTo(this).asInstanceOf[LocalTime]

  /** Returns a copy of this time with the specified period added.
    *
    * This method returns a new time based on this time with the specified period added.
    * This can be used to add any period that is defined by a unit, for example to add hours, minutes or seconds.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the amount of the unit to add to the result, may be negative
    * @param unit  the unit of the period to add, not null
    * @return a { @code LocalTime} based on this time with the specified period added, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): LocalTime =
    unit match {
      case f: ChronoUnit =>
        import ChronoUnit._
        f match {
          case NANOS     => plusNanos(amountToAdd)
          case MICROS    => plusNanos((amountToAdd % LocalTime.MICROS_PER_DAY) * 1000)
          case MILLIS    => plusNanos((amountToAdd % LocalTime.MILLIS_PER_DAY) * 1000000)
          case SECONDS   => plusSeconds(amountToAdd)
          case MINUTES   => plusMinutes(amountToAdd)
          case HOURS     => plusHours(amountToAdd)
          case HALF_DAYS => plusHours((amountToAdd % 2) * 12)
          case _         => throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")
        }
      case _ =>
        unit.addTo(this, amountToAdd)
    }

  /** Returns a copy of this {@code LocalTime} with the specified period in hours added.
    *
    * This adds the specified number of hours to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hoursToAdd  the hours to add, may be negative
    * @return a { @code LocalTime} based on this time with the hours added, not null
    */
  def plusHours(hoursToAdd: Long): LocalTime = {
    if (hoursToAdd == 0)
      return this
    val newHour: Int =
      ((hoursToAdd % LocalTime.HOURS_PER_DAY).toInt + hour + LocalTime.HOURS_PER_DAY) % LocalTime.HOURS_PER_DAY
    LocalTime.create(newHour, minute.toInt, second.toInt, nano)
  }

  /** Returns a copy of this {@code LocalTime} with the specified period in minutes added.
    *
    * This adds the specified number of minutes to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutesToAdd  the minutes to add, may be negative
    * @return a { @code LocalTime} based on this time with the minutes added, not null
    */
  def plusMinutes(minutesToAdd: Long): LocalTime = {
    if (minutesToAdd == 0)
      return this
    val mofd: Int = hour * LocalTime.MINUTES_PER_HOUR + minute
    val newMofd: Int =
      ((minutesToAdd % LocalTime.MINUTES_PER_DAY).toInt + mofd + LocalTime.MINUTES_PER_DAY) % LocalTime.MINUTES_PER_DAY
    if (mofd == newMofd)
      return this
    val newHour: Int   = newMofd / LocalTime.MINUTES_PER_HOUR
    val newMinute: Int = newMofd % LocalTime.MINUTES_PER_HOUR
    LocalTime.create(newHour, newMinute, second.toInt, nano)
  }

  /** Returns a copy of this {@code LocalTime} with the specified period in seconds added.
    *
    * This adds the specified number of seconds to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondstoAdd  the seconds to add, may be negative
    * @return a { @code LocalTime} based on this time with the seconds added, not null
    */
  def plusSeconds(secondstoAdd: Long): LocalTime = {
    if (secondstoAdd == 0)
      return this
    val sofd: Int =
      hour * LocalTime.SECONDS_PER_HOUR + minute * LocalTime.SECONDS_PER_MINUTE + second
    val newSofd: Int =
      ((secondstoAdd % LocalTime.SECONDS_PER_DAY).toInt + sofd + LocalTime.SECONDS_PER_DAY) % LocalTime.SECONDS_PER_DAY
    if (sofd == newSofd)
      return this
    val newHour: Int   = newSofd / LocalTime.SECONDS_PER_HOUR
    val newMinute: Int = (newSofd / LocalTime.SECONDS_PER_MINUTE) % LocalTime.MINUTES_PER_HOUR
    val newSecond: Int = newSofd % LocalTime.SECONDS_PER_MINUTE
    LocalTime.create(newHour, newMinute, newSecond, nano)
  }

  /** Returns a copy of this {@code LocalTime} with the specified period in nanoseconds added.
    *
    * This adds the specified number of nanoseconds to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToAdd  the nanos to add, may be negative
    * @return a { @code LocalTime} based on this time with the nanoseconds added, not null
    */
  def plusNanos(nanosToAdd: Long): LocalTime = {
    if (nanosToAdd == 0)
      return this
    val nofd: Long = toNanoOfDay
    val newNofd: Long =
      ((nanosToAdd % LocalTime.NANOS_PER_DAY) + nofd + LocalTime.NANOS_PER_DAY) % LocalTime.NANOS_PER_DAY
    if (nofd == newNofd)
      return this
    val newHour: Int   = (newNofd / LocalTime.NANOS_PER_HOUR).toInt
    val newMinute: Int = ((newNofd / LocalTime.NANOS_PER_MINUTE) % LocalTime.MINUTES_PER_HOUR).toInt
    val newSecond: Int =
      ((newNofd / LocalTime.NANOS_PER_SECOND) % LocalTime.SECONDS_PER_MINUTE).toInt
    val newNano: Int = (newNofd % LocalTime.NANOS_PER_SECOND).toInt
    LocalTime.create(newHour, newMinute, newSecond, newNano)
  }

  /** Returns a copy of this time with the specified period subtracted.
    *
    * This method returns a new time based on this time with the specified period subtracted.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to subtract, not null
    * @return a { @code LocalTime} based on this time with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): LocalTime =
    amount.subtractFrom(this).asInstanceOf[LocalTime]

  /** Returns a copy of this time with the specified period subtracted.
    *
    * This method returns a new time based on this time with the specified period subtracted.
    * This can be used to subtract any period that is defined by a unit, for example to subtract hours, minutes or seconds.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the amount of the unit to subtract from the result, may be negative
    * @param unit  the unit of the period to subtract, not null
    * @return a { @code LocalTime} based on this time with the specified period subtracted, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): LocalTime =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this {@code LocalTime} with the specified period in hours subtracted.
    *
    * This subtracts the specified number of hours from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hoursToSubtract  the hours to subtract, may be negative
    * @return a { @code LocalTime} based on this time with the hours subtracted, not null
    */
  def minusHours(hoursToSubtract: Long): LocalTime =
    plusHours(-(hoursToSubtract % LocalTime.HOURS_PER_DAY))

  /** Returns a copy of this {@code LocalTime} with the specified period in minutes subtracted.
    *
    * This subtracts the specified number of minutes from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutesToSubtract  the minutes to subtract, may be negative
    * @return a { @code LocalTime} based on this time with the minutes subtracted, not null
    */
  def minusMinutes(minutesToSubtract: Long): LocalTime =
    plusMinutes(-(minutesToSubtract % LocalTime.MINUTES_PER_DAY))

  /** Returns a copy of this {@code LocalTime} with the specified period in seconds subtracted.
    *
    * This subtracts the specified number of seconds from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToSubtract  the seconds to subtract, may be negative
    * @return a { @code LocalTime} based on this time with the seconds subtracted, not null
    */
  def minusSeconds(secondsToSubtract: Long): LocalTime =
    plusSeconds(-(secondsToSubtract % LocalTime.SECONDS_PER_DAY))

  /** Returns a copy of this {@code LocalTime} with the specified period in nanoseconds subtracted.
    *
    * This subtracts the specified number of nanoseconds from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToSubtract  the nanos to subtract, may be negative
    * @return a { @code LocalTime} based on this time with the nanoseconds subtracted, not null
    */
  def minusNanos(nanosToSubtract: Long): LocalTime =
    plusNanos(-(nanosToSubtract % LocalTime.NANOS_PER_DAY))

  /** Queries this time using the specified query.
    *
    * This queries this time using the specified query strategy object.
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
    query match {
      case TemporalQueries.precision => ChronoUnit.NANOS.asInstanceOf[R]
      case TemporalQueries.localTime => this.asInstanceOf[R]
      case TemporalQueries.chronology | TemporalQueries.zoneId | TemporalQueries.zone |
          TemporalQueries.offset | TemporalQueries.localDate =>
        null.asInstanceOf[R]
      case _ => query.queryFrom(this)
    }

  /** Adjusts the specified temporal object to have the same time as this object.
    *
    * This returns a temporal object of the same observable type as the input
    * with the time changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#NANO_OF_DAY} as the field.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisLocalTime.adjustInto(temporal);
    * temporal = temporal.with(thisLocalTime);
    * </pre>
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the target object to be adjusted, not null
    * @return the adjusted object, not null
    * @throws DateTimeException if unable to make the adjustment
    * @throws ArithmeticException if numeric overflow occurs
    */
  def adjustInto(temporal: Temporal): Temporal = temporal.`with`(NANO_OF_DAY, toNanoOfDay)

  /** Calculates the period between this time and another time in
    * terms of the specified unit.
    *
    * This calculates the period between two times in terms of a single unit.
    * The start and end points are {@code this} and the specified time.
    * The result will be negative if the end is before the start.
    * The {@code Temporal} passed to this method must be a {@code LocalTime}.
    * For example, the period in hours between two times can be calculated
    * using {@code startTime.until(endTime, HOURS)}.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two times.
    * For example, the period in hours between 11:30 and 13:29 will only
    * be one hour as it is one minute short of two hours.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * <pre>
    * long period = start.until(end, HOURS);   // this method
    * dateTime.plus(HOURS.between(start, end));      // use in plus/minus
    * </pre>
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code NANOS}, {@code MICROS}, {@code MILLIS}, {@code SECONDS},
    * {@code MINUTES}, {@code HOURS} and {@code HALF_DAYS} are supported.
    * Other {@code ChronoUnit} values will throw an exception.
    *
    * If the unit is not a {@code ChronoUnit}, then the result of this method
    * is obtained by invoking {@code TemporalUnit.between(Temporal, Temporal)}
    * passing {@code this} as the first argument and the input temporal as
    * the second argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endExclusive  the end time, which is converted to a { @code LocalTime}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this time and the end time
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: LocalTime = LocalTime.from(endExclusive)
    unit match {
      case u: ChronoUnit =>
        val nanosUntil: Long = end.toNanoOfDay - toNanoOfDay
        import ChronoUnit._
        u match {
          case NANOS     => nanosUntil
          case MICROS    => nanosUntil / 1000
          case MILLIS    => nanosUntil / 1000000
          case SECONDS   => nanosUntil / LocalTime.NANOS_PER_SECOND
          case MINUTES   => nanosUntil / LocalTime.NANOS_PER_MINUTE
          case HOURS     => nanosUntil / LocalTime.NANOS_PER_HOUR
          case HALF_DAYS => nanosUntil / (12 * LocalTime.NANOS_PER_HOUR)
          case _         => throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")
        }
      case _ =>
        unit.between(this, end)
    }
  }

  /** Combines this time with a date to create a {@code LocalDateTime}.
    *
    * This returns a {@code LocalDateTime} formed from this time at the specified date.
    * All possible combinations of date and time are valid.
    *
    * @param date  the date to combine with, not null
    * @return the local date-time formed from this time and the specified date, not null
    */
  def atDate(date: LocalDate): LocalDateTime = LocalDateTime.of(date, this)

  /** Combines this time with an offset to create an {@code OffsetTime}.
    *
    * This returns an {@code OffsetTime} formed from this time at the specified offset.
    * All possible combinations of time and offset are valid.
    *
    * @param offset  the offset to combine with, not null
    * @return the offset time formed from this time and the specified offset, not null
    */
  def atOffset(offset: ZoneOffset): OffsetTime = OffsetTime.of(this, offset)

  /** Extracts the time as seconds of day,
    * from {@code 0} to {@code 24 * 60 * 60 - 1}.
    *
    * @return the second-of-day equivalent to this time
    */
  def toSecondOfDay: Int = {
    var total: Int = hour * LocalTime.SECONDS_PER_HOUR
    total += minute * LocalTime.SECONDS_PER_MINUTE
    total += second
    total
  }

  /** Extracts the time as nanos of day,
    * from {@code 0} to {@code 24 * 60 * 60 * 1,000,000,000 - 1}.
    *
    * @return the nano of day equivalent to this time
    */
  def toNanoOfDay: Long = {
    var total: Long = hour * LocalTime.NANOS_PER_HOUR
    total += minute * LocalTime.NANOS_PER_MINUTE
    total += second * LocalTime.NANOS_PER_SECOND
    total += nano
    total
  }

  /** Compares this {@code LocalTime} to another time.
    *
    * The comparison is based on the time-line position of the local times within a day.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param other  the other time to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    * @throws NullPointerException if { @code other} is null
    */
  def compare(other: LocalTime): Int = {
    var cmp: Int = Integer.compare(hour.toInt, other.hour.toInt)
    if (cmp == 0) {
      cmp = Integer.compare(minute.toInt, other.minute.toInt)
      if (cmp == 0) {
        cmp = Integer.compare(second.toInt, other.second.toInt)
        if (cmp == 0)
          cmp = Integer.compare(nano, other.nano)
      }
    }
    cmp
  }

  override def compareTo(other: LocalTime): Int = compare(other)

  /** Checks if this {@code LocalTime} is after the specified time.
    *
    * The comparison is based on the time-line position of the time within a day.
    *
    * @param other  the other time to compare to, not null
    * @return true if this is after the specified time
    * @throws NullPointerException if { @code other} is null
    */
  def isAfter(other: LocalTime): Boolean = compareTo(other) > 0

  /** Checks if this {@code LocalTime} is before the specified time.
    *
    * The comparison is based on the time-line position of the time within a day.
    *
    * @param other  the other time to compare to, not null
    * @return true if this point is before the specified time
    * @throws NullPointerException if { @code other} is null
    */
  def isBefore(other: LocalTime): Boolean = compareTo(other) < 0

  /** Checks if this time is equal to another time.
    *
    * The comparison is based on the time-line position of the time within a day.
    *
    * Only objects of type {@code LocalTime} are compared, other types return false.
    * To compare the date of two {@code TemporalAccessor} instances, use
    * {@link ChronoField#NANO_OF_DAY} as a comparator.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other time
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: LocalTime =>
        (this eq other) || (hour == other.hour && minute == other.minute && second == other.second && nano == other.nano)
      case _ => false
    }

  /** A hash code for this time.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = {
    val nod: Long = toNanoOfDay
    (nod ^ (nod >>> 32)).toInt
  }

  /** Outputs this time as a {@code String}, such as {@code 10:15}.
    *
    * The output will be one of the following ISO-8601 formats:
    *<ul>
    * <li>{@code HH:mm}</li>
    * <li>{@code HH:mm:ss}</li>
    * <li>{@code HH:mm:ss.SSS}</li>
    * <li>{@code HH:mm:ss.SSSSSS}</li>
    * <li>{@code HH:mm:ss.SSSSSSSSS}</li>
    * </ul><p>
    * The format used will be the shortest that outputs the full value of
    * the time where the omitted parts are implied to be zero.
    *
    * @return a string representation of this time, not null
    */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder(18)
    val hourValue: Int     = hour.toInt
    val minuteValue: Int   = minute.toInt
    val secondValue: Int   = second.toInt
    val nanoValue: Int     = nano
    buf
      .append(if (hourValue < 10) "0" else "")
      .append(hourValue)
      .append(if (minuteValue < 10) ":0" else ":")
      .append(minuteValue)
    if (secondValue > 0 || nanoValue > 0) {
      buf.append(if (secondValue < 10) ":0" else ":").append(secondValue)
      if (nanoValue > 0) {
        buf.append('.')
        if (nanoValue % 1000000 == 0)
          buf.append(Integer.toString((nanoValue / 1000000) + 1000).substring(1))
        else if (nanoValue % 1000 == 0)
          buf.append(Integer.toString((nanoValue / 1000) + 1000000).substring(1))
        else
          buf.append(Integer.toString(nanoValue + 1000000000).substring(1))
      }
    }
    buf.toString
  }

  /** Outputs this time as a {@code String} using the formatter.
    *
    * This time will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted time string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  private def writeReplace: AnyRef = new Ser(Ser.LOCAL_TIME_TYPE, this)

  /** Defend against malicious streams.
    *
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit =
    if (nano == 0) {
      if (second == 0) {
        if (minute == 0)
          out.writeByte(~hour)
        else {
          out.writeByte(hour.toInt)
          out.writeByte(~minute)
        }
      } else {
        out.writeByte(hour.toInt)
        out.writeByte(minute.toInt)
        out.writeByte(~second)
      }
    } else {
      out.writeByte(hour.toInt)
      out.writeByte(minute.toInt)
      out.writeByte(second.toInt)
      out.writeInt(nano)
    }
}
