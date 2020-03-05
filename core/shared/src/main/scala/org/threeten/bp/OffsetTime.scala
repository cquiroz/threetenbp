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

import org.threeten.bp.LocalTime.NANOS_PER_HOUR
import org.threeten.bp.LocalTime.NANOS_PER_MINUTE
import org.threeten.bp.LocalTime.NANOS_PER_SECOND
import org.threeten.bp.LocalTime.SECONDS_PER_DAY
import org.threeten.bp.temporal.ChronoField.NANO_OF_DAY
import org.threeten.bp.temporal.ChronoField.OFFSET_SECONDS
import org.threeten.bp.temporal.ChronoUnit.NANOS
import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.InvalidObjectException
import java.io.ObjectStreamException
import java.io.Serializable
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

@SerialVersionUID(7264499704384272492L)
object OffsetTime {

  /** The minimum supported {@code OffsetTime}, '00:00:00+18:00'.
    * This is the time of midnight at the start of the day in the maximum offset
    * (larger offsets are earlier on the time-line).
    * This combines {@link LocalTime#MIN} and {@link ZoneOffset#MAX}.
    * This could be used by an application as a "far past" date.
    */
  lazy val MIN: OffsetTime = LocalTime.MIN.atOffset(ZoneOffset.MAX)

  /** The maximum supported {@code OffsetTime}, '23:59:59.999999999-18:00'.
    * This is the time just before midnight at the end of the day in the minimum offset
    * (larger negative offsets are later on the time-line).
    * This combines {@link LocalTime#MAX} and {@link ZoneOffset#MIN}.
    * This could be used by an application as a "far future" date.
    */
  lazy val MAX: OffsetTime = LocalTime.MAX.atOffset(ZoneOffset.MIN)

  /** Obtains the current time from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current time.
    * The offset will be calculated from the time-zone in the clock.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current time using the system clock, not null
    */
  def now: OffsetTime = now(Clock.systemDefaultZone)

  /** Obtains the current time from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current time.
    * Specifying the time-zone avoids dependence on the default time-zone.
    * The offset will be calculated from the specified time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current time using the system clock, not null
    */
  def now(zone: ZoneId): OffsetTime = now(Clock.system(zone))

  /** Obtains the current time from the specified clock.
    *
    * This will query the specified clock to obtain the current time.
    * The offset will be calculated from the time-zone in the clock.
    *
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current time, not null
    */
  def now(clock: Clock): OffsetTime = {
    Objects.requireNonNull(clock, "clock")
    val now: Instant = clock.instant
    ofInstant(now, clock.getZone.getRules.getOffset(now))
  }

  /** Obtains an instance of {@code OffsetTime} from a local time and an offset.
    *
    * @param time  the local time, not null
    * @param offset  the zone offset, not null
    * @return the offset time, not null
    */
  def of(time: LocalTime, offset: ZoneOffset): OffsetTime = new OffsetTime(time, offset)

  /** Obtains an instance of {@code OffsetTime} from an hour, minute, second and nanosecond.
    *
    * This creates an offset time with the four specified fields.
    *
    * This method exists primarily for writing test cases.
    * Non test-code will typically use other methods to create an offset time.
    * {@code LocalTime} has two additional convenience variants of the
    * equivalent factory method taking fewer arguments.
    * They are not provided here to reduce the footprint of the API.
    *
    * @param hour  the hour-of-day to represent, from 0 to 23
    * @param minute  the minute-of-hour to represent, from 0 to 59
    * @param second  the second-of-minute to represent, from 0 to 59
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @param offset  the zone offset, not null
    * @return the offset time, not null
    * @throws DateTimeException if the value of any field is out of range
    */
  def of(hour: Int, minute: Int, second: Int, nanoOfSecond: Int, offset: ZoneOffset): OffsetTime =
    new OffsetTime(LocalTime.of(hour, minute, second, nanoOfSecond), offset)

  /** Obtains an instance of {@code OffsetTime} from an {@code Instant} and zone ID.
    *
    * This creates an offset time with the same instant as that specified.
    * Finding the offset from UTC/Greenwich is simple as there is only one valid
    * offset for each instant.
    *
    * The date component of the instant is dropped during the conversion.
    * This means that the conversion can never fail due to the instant being
    * out of the valid range of dates.
    *
    * @param instant  the instant to create the time from, not null
    * @param zone  the time-zone, which may be an offset, not null
    * @return the offset time, not null
    */
  def ofInstant(instant: Instant, zone: ZoneId): OffsetTime = {
    Objects.requireNonNull(instant, "instant")
    Objects.requireNonNull(zone, "zone")
    val rules: ZoneRules   = zone.getRules
    val offset: ZoneOffset = rules.getOffset(instant)
    var secsOfDay: Long    = instant.getEpochSecond % SECONDS_PER_DAY
    secsOfDay = (secsOfDay + offset.getTotalSeconds) % SECONDS_PER_DAY
    if (secsOfDay < 0)
      secsOfDay += SECONDS_PER_DAY
    val time: LocalTime = LocalTime.ofSecondOfDay(secsOfDay, instant.getNano)
    new OffsetTime(time, offset)
  }

  /** Obtains an instance of {@code OffsetTime} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code OffsetTime}.
    *
    * The conversion extracts and combines {@code LocalTime} and {@code ZoneOffset}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code OffsetTime::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the offset time, not null
    * @throws DateTimeException if unable to convert to an { @code OffsetTime}
    */
  def from(temporal: TemporalAccessor): OffsetTime =
    temporal match {
      case time: OffsetTime => time
      case _ =>
        try {
          val time: LocalTime    = LocalTime.from(temporal)
          val offset: ZoneOffset = ZoneOffset.from(temporal)
          new OffsetTime(time, offset)
        } catch {
          case _: DateTimeException =>
            throw new DateTimeException(
              s"Unable to obtain OffsetTime from TemporalAccessor: $temporal, type ${temporal.getClass.getName}"
            )
        }
    }

  /** Obtains an instance of {@code OffsetTime} from a text string such as {@code 10:15:30+01:00}.
    *
    * The string must represent a valid time and is parsed using
    * {@link org.threeten.bp.format.DateTimeFormatter#ISO_OFFSET_TIME}.
    *
    * @param text  the text to parse such as "10:15:30+01:00", not null
    * @return the parsed local time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): OffsetTime = parse(text, DateTimeFormatter.ISO_OFFSET_TIME)

  /** Obtains an instance of {@code OffsetTime} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a time.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed offset time, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): OffsetTime = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[OffsetTime] {
      override def queryFrom(temporal: TemporalAccessor): OffsetTime = OffsetTime.from(temporal)
    })
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): OffsetTime = {
    val time: LocalTime    = LocalTime.readExternal(in)
    val offset: ZoneOffset = ZoneOffset.readExternal(in)
    OffsetTime.of(time, offset)
  }
}

/** A time with an offset from UTC/Greenwich in the ISO-8601 calendar system,
  * such as {@code 10:15:30+01:00}.
  *
  * {@code OffsetTime} is an immutable date-time object that represents a time, often
  * viewed as hour-minute-second-offset.
  * This class stores all time fields, to a precision of nanoseconds,
  * as well as a zone offset.
  * For example, the value "13:45.30.123456789+02:00" can be stored
  * in an {@code OffsetTime}.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor
  * @param time  the local time, not null
  * @param offset  the zone offset, not null
  */
@SerialVersionUID(7264499704384272492L)
final class OffsetTime(private val time: LocalTime, private val offset: ZoneOffset)
    extends TemporalAccessor
    with Temporal
    with TemporalAdjuster
    with Ordered[OffsetTime]
    with Serializable {
  Objects.requireNonNull(time, "time")
  Objects.requireNonNull(offset, "offset")

  /** Returns a new time based on this one, returning {@code this} where possible.
    *
    * @param time  the time to create with, not null
    * @param offset  the zone offset to create with, not null
    */
  private def `with`(time: LocalTime, offset: ZoneOffset): OffsetTime =
    if ((this.time eq time) && (this.offset == offset)) this
    else new OffsetTime(time, offset)

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
    * <li>{@code OFFSET_SECONDS}
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
    if (field.isInstanceOf[ChronoField]) field.isTimeBased || (field eq OFFSET_SECONDS)
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
  override def range(field: TemporalField): ValueRange =
    if (field.isInstanceOf[ChronoField])
      if (field eq OFFSET_SECONDS) field.range
      else time.range(field)
    else
      field.rangeRefinedBy(this)

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
  override def get(field: TemporalField): Int = super.get(field)

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
    if (field.isInstanceOf[ChronoField])
      if (field eq OFFSET_SECONDS) getOffset.getTotalSeconds.toLong
      else time.getLong(field)
    else
      field.getFrom(this)

  /** Gets the zone offset, such as '+01:00'.
    *
    * This is the offset of the local time from UTC/Greenwich.
    *
    * @return the zone offset, not null
    */
  def getOffset: ZoneOffset = offset

  /** Returns a copy of this {@code OffsetTime} with the specified offset ensuring
    * that the result has the same local time.
    *
    * This method returns an object with the same {@code LocalTime} and the specified {@code ZoneOffset}.
    * No calculation is needed or performed.
    * For example, if this time represents {@code 10:30+02:00} and the offset specified is
    * {@code +03:00}, then this method will return {@code 10:30+03:00}.
    *
    * To take into account the difference between the offsets, and adjust the time fields,
    * use {@link #withOffsetSameInstant}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param offset  the zone offset to change to, not null
    * @return an { @code OffsetTime} based on this time with the requested offset, not null
    */
  def withOffsetSameLocal(offset: ZoneOffset): OffsetTime =
    if (offset != null && (offset == this.offset)) this
    else new OffsetTime(time, offset)

  /** Returns a copy of this {@code OffsetTime} with the specified offset ensuring
    * that the result is at the same instant on an implied day.
    *
    * This method returns an object with the specified {@code ZoneOffset} and a {@code LocalTime}
    * adjusted by the difference between the two offsets.
    * This will result in the old and new objects representing the same instant an an implied day.
    * This is useful for finding the local time in a different offset.
    * For example, if this time represents {@code 10:30+02:00} and the offset specified is
    * {@code +03:00}, then this method will return {@code 11:30+03:00}.
    *
    * To change the offset without adjusting the local time use {@link #withOffsetSameLocal}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param offset  the zone offset to change to, not null
    * @return an { @code OffsetTime} based on this time with the requested offset, not null
    */
  def withOffsetSameInstant(offset: ZoneOffset): OffsetTime =
    if (offset == this.offset)
      this
    else {
      val difference: Int     = offset.getTotalSeconds - this.offset.getTotalSeconds
      val adjusted: LocalTime = time.plusSeconds(difference.toLong)
      new OffsetTime(adjusted, offset)
    }

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

  /** Returns an adjusted copy of this time.
    *
    * This returns a new {@code OffsetTime}, based on this one, with the time adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * A simple adjuster might simply set the one of the fields, such as the hour field.
    * A more complex adjuster might set the time to the last hour of the day.
    *
    * The classes {@link LocalTime} and {@link ZoneOffset} implement {@code TemporalAdjuster},
    * thus this method can be used to change the time or offset:
    * <pre>
    * result = offsetTime.with(time);
    * result = offsetTime.with(offset);
    * </pre>
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return an { @code OffsetTime} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): OffsetTime =
    adjuster match {
      case localTime: LocalTime   => `with`(localTime, offset)
      case zoneOffset: ZoneOffset => `with`(time, zoneOffset)
      case offsetTime: OffsetTime => offsetTime
      case _                      => adjuster.adjustInto(this).asInstanceOf[OffsetTime]
    }

  /** Returns a copy of this time with the specified field set to a new value.
    *
    * This returns a new {@code OffsetTime}, based on this one, with the value
    * for the specified field changed.
    * This can be used to change any supported field, such as the hour, minute or second.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    *
    * The {@code OFFSET_SECONDS} field will return a time with the specified offset.
    * The local time is unaltered. If the new offset value is outside the valid range
    * then a {@code DateTimeException} will be thrown.
    *
    * The other {@link #isSupported(TemporalField) supported fields} will behave as per
    * the matching method on {@link LocalTime#with(TemporalField, long)} LocalTime}.
    * In this case, the offset is not part of the calculation and will be unchanged.
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
    * @return an { @code OffsetTime} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): OffsetTime =
    field match {
      case f: ChronoField =>
        if (field eq OFFSET_SECONDS)
          `with`(time, ZoneOffset.ofTotalSeconds(f.checkValidIntValue(newValue)))
        else `with`(time.`with`(field, newValue), offset)
      case _ => field.adjustInto(this, newValue)
    }

  /** Returns a copy of this {@code OffsetTime} with the hour-of-day value altered.
    *
    * The offset does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hour  the hour-of-day to set in the result, from 0 to 23
    * @return an { @code OffsetTime} based on this time with the requested hour, not null
    * @throws DateTimeException if the hour value is invalid
    */
  def withHour(hour: Int): OffsetTime = `with`(time.withHour(hour), offset)

  /** Returns a copy of this {@code OffsetTime} with the minute-of-hour value altered.
    *
    * The offset does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minute  the minute-of-hour to set in the result, from 0 to 59
    * @return an { @code OffsetTime} based on this time with the requested minute, not null
    * @throws DateTimeException if the minute value is invalid
    */
  def withMinute(minute: Int): OffsetTime = `with`(time.withMinute(minute), offset)

  /** Returns a copy of this {@code OffsetTime} with the second-of-minute value altered.
    *
    * The offset does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param second  the second-of-minute to set in the result, from 0 to 59
    * @return an { @code OffsetTime} based on this time with the requested second, not null
    * @throws DateTimeException if the second value is invalid
    */
  def withSecond(second: Int): OffsetTime = `with`(time.withSecond(second), offset)

  /** Returns a copy of this {@code OffsetTime} with the nano-of-second value altered.
    *
    * The offset does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanoOfSecond  the nano-of-second to set in the result, from 0 to 999,999,999
    * @return an { @code OffsetTime} based on this time with the requested nanosecond, not null
    * @throws DateTimeException if the nanos value is invalid
    */
  def withNano(nanoOfSecond: Int): OffsetTime = `with`(time.withNano(nanoOfSecond), offset)

  /** Returns a copy of this {@code OffsetTime} with the time truncated.
    *
    * Truncation returns a copy of the original time with fields
    * smaller than the specified unit set to zero.
    * For example, truncating with the {@link ChronoUnit#MINUTES minutes} unit
    * will set the second-of-minute and nano-of-second field to zero.
    *
    * The unit must have a {@linkplain TemporalUnit#getDuration() duration}
    * that divides into the length of a standard day without remainder.
    * This includes all supplied time units on {@link ChronoUnit} and
    * {@link ChronoUnit#DAYS DAYS}. Other units throw an exception.
    *
    * The offset does not affect the calculation and will be the same in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param unit  the unit to truncate to, not null
    * @return an { @code OffsetTime} based on this time with the time truncated, not null
    * @throws DateTimeException if unable to truncate
    */
  def truncatedTo(unit: TemporalUnit): OffsetTime = `with`(time.truncatedTo(unit), offset)

  /** Returns a copy of this date with the specified period added.
    *
    * This method returns a new time based on this time with the specified period added.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    * The offset is not part of the calculation and will be unchanged in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return an { @code OffsetTime} based on this time with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): OffsetTime =
    amount.addTo(this).asInstanceOf[OffsetTime]

  /** Returns a copy of this time with the specified period added.
    *
    * This method returns a new time based on this time with the specified period added.
    * This can be used to add any period that is defined by a unit, for example to add hours, minutes or seconds.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    * The offset is not part of the calculation and will be unchanged in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the amount of the unit to add to the result, may be negative
    * @param unit  the unit of the period to add, not null
    * @return an { @code OffsetTime} based on this time with the specified period added, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): OffsetTime =
    if (unit.isInstanceOf[ChronoUnit]) `with`(time.plus(amountToAdd, unit), offset)
    else unit.addTo(this, amountToAdd)

  /** Returns a copy of this {@code OffsetTime} with the specified period in hours added.
    *
    * This adds the specified number of hours to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hours  the hours to add, may be negative
    * @return an { @code OffsetTime} based on this time with the hours added, not null
    */
  def plusHours(hours: Long): OffsetTime = `with`(time.plusHours(hours), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in minutes added.
    *
    * This adds the specified number of minutes to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutes  the minutes to add, may be negative
    * @return an { @code OffsetTime} based on this time with the minutes added, not null
    */
  def plusMinutes(minutes: Long): OffsetTime = `with`(time.plusMinutes(minutes), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in seconds added.
    *
    * This adds the specified number of seconds to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param seconds  the seconds to add, may be negative
    * @return an { @code OffsetTime} based on this time with the seconds added, not null
    */
  def plusSeconds(seconds: Long): OffsetTime = `with`(time.plusSeconds(seconds), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in nanoseconds added.
    *
    * This adds the specified number of nanoseconds to this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanos  the nanos to add, may be negative
    * @return an { @code OffsetTime} based on this time with the nanoseconds added, not null
    */
  def plusNanos(nanos: Long): OffsetTime = `with`(time.plusNanos(nanos), offset)

  /** Returns a copy of this time with the specified period subtracted.
    *
    * This method returns a new time based on this time with the specified period subtracted.
    * The amount is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    * The offset is not part of the calculation and will be unchanged in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to subtract, not null
    * @return an { @code OffsetTime} based on this time with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): OffsetTime =
    amount.subtractFrom(this).asInstanceOf[OffsetTime]

  /** Returns a copy of this time with the specified period subtracted.
    *
    * This method returns a new time based on this time with the specified period subtracted.
    * This can be used to subtract any period that is defined by a unit, for example to subtract hours, minutes or seconds.
    * The unit is responsible for the details of the calculation, including the resolution
    * of any edge cases in the calculation.
    * The offset is not part of the calculation and will be unchanged in the result.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the amount of the unit to subtract from the result, may be negative
    * @param unit  the unit of the period to subtract, not null
    * @return an { @code OffsetTime} based on this time with the specified period subtracted, not null
    * @throws DateTimeException if the unit cannot be added to this type
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): OffsetTime =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this {@code OffsetTime} with the specified period in hours subtracted.
    *
    * This subtracts the specified number of hours from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hours  the hours to subtract, may be negative
    * @return an { @code OffsetTime} based on this time with the hours subtracted, not null
    */
  def minusHours(hours: Long): OffsetTime = `with`(time.minusHours(hours), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in minutes subtracted.
    *
    * This subtracts the specified number of minutes from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutes  the minutes to subtract, may be negative
    * @return an { @code OffsetTime} based on this time with the minutes subtracted, not null
    */
  def minusMinutes(minutes: Long): OffsetTime = `with`(time.minusMinutes(minutes), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in seconds subtracted.
    *
    * This subtracts the specified number of seconds from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param seconds  the seconds to subtract, may be negative
    * @return an { @code OffsetTime} based on this time with the seconds subtracted, not null
    */
  def minusSeconds(seconds: Long): OffsetTime = `with`(time.minusSeconds(seconds), offset)

  /** Returns a copy of this {@code OffsetTime} with the specified period in nanoseconds subtracted.
    *
    * This subtracts the specified number of nanoseconds from this time, returning a new time.
    * The calculation wraps around midnight.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanos  the nanos to subtract, may be negative
    * @return an { @code OffsetTime} based on this time with the nanoseconds subtracted, not null
    */
  def minusNanos(nanos: Long): OffsetTime = `with`(time.minusNanos(nanos), offset)

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
      case TemporalQueries.precision                     => NANOS.asInstanceOf[R]
      case TemporalQueries.offset | TemporalQueries.zone => getOffset.asInstanceOf[R]
      case TemporalQueries.localTime                     => time.asInstanceOf[R]
      case TemporalQueries.chronology | TemporalQueries.localDate | TemporalQueries.zoneId =>
        null.asInstanceOf[R]
      case _ => super.query(query)
    }

  /** Adjusts the specified temporal object to have the same offset and time
    * as this object.
    *
    * This returns a temporal object of the same observable type as the input
    * with the offset and time changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * twice, passing {@link ChronoField#NANO_OF_DAY} and
    * {@link ChronoField#OFFSET_SECONDS} as the fields.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisOffsetTime.adjustInto(temporal);
    * temporal = temporal.with(thisOffsetTime);
    * </pre>
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the target object to be adjusted, not null
    * @return the adjusted object, not null
    * @throws DateTimeException if unable to make the adjustment
    * @throws ArithmeticException if numeric overflow occurs
    */
  def adjustInto(temporal: Temporal): Temporal =
    temporal
      .`with`(NANO_OF_DAY, time.toNanoOfDay)
      .`with`(OFFSET_SECONDS, getOffset.getTotalSeconds.toLong)

  /** Calculates the period between this time and another time in
    * terms of the specified unit.
    *
    * This calculates the period between two times in terms of a single unit.
    * The start and end points are {@code this} and the specified time.
    * The result will be negative if the end is before the start.
    * For example, the period in hours between two times can be calculated
    * using {@code startTime.until(endTime, HOURS)}.
    *
    * The {@code Temporal} passed to this method must be an {@code OffsetTime}.
    * If the offset differs between the two times, then the specified
    * end time is normalized to have the same offset as this time.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two times.
    * For example, the period in hours between 11:30Z and 13:29Z will only
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
    * @param endExclusive  the end time, which is converted to an { @code OffsetTime}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this time and the end time
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: OffsetTime = OffsetTime.from(endExclusive)
    unit match {
      case u: ChronoUnit =>
        val nanosUntil: Long = end.toEpochNano - toEpochNano
        import ChronoUnit._
        u match {
          case NANOS     => nanosUntil
          case MICROS    => nanosUntil / 1000
          case MILLIS    => nanosUntil / 1000000
          case SECONDS   => nanosUntil / NANOS_PER_SECOND
          case MINUTES   => nanosUntil / NANOS_PER_MINUTE
          case HOURS     => nanosUntil / NANOS_PER_HOUR
          case HALF_DAYS => nanosUntil / (12 * NANOS_PER_HOUR)
          case _         => throw UnsupportedTemporalTypeException.unit(unit)
        }
      case _ =>
        unit.between(this, end)
    }
  }

  /** Combines this time with a date to create an {@code OffsetDateTime}.
    *
    * This returns an {@code OffsetDateTime} formed from this time and the specified date.
    * All possible combinations of date and time are valid.
    *
    * @param date  the date to combine with, not null
    * @return the offset date-time formed from this time and the specified date, not null
    */
  def atDate(date: LocalDate): OffsetDateTime = OffsetDateTime.of(date, time, offset)

  /** Gets the {@code LocalTime} part of this date-time.
    *
    * This returns a {@code LocalTime} with the same hour, minute, second and
    * nanosecond as this date-time.
    *
    * @return the time part of this date-time, not null
    */
  def toLocalTime: LocalTime = time

  /** Converts this time to epoch nanos based on 1970-01-01Z.
    *
    * @return the epoch nanos value
    */
  private def toEpochNano: Long = {
    val nod: Long         = time.toNanoOfDay
    val offsetNanos: Long = offset.getTotalSeconds * NANOS_PER_SECOND
    nod - offsetNanos
  }

  /** Compares this {@code OffsetTime} to another time.
    *
    * The comparison is based first on the UTC equivalent instant, then on the local time.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * For example, the following is the comparator order:
    * <ol>
    * <li>{@code 10:30+01:00}</li>
    * <li>{@code 11:00+01:00}</li>
    * <li>{@code 12:00+02:00}</li>
    * <li>{@code 11:30+01:00}</li>
    * <li>{@code 12:00+01:00}</li>
    * <li>{@code 12:30+01:00}</li>
    * </ol>
    * Values #2 and #3 represent the same instant on the time-line.
    * When two values represent the same instant, the local time is compared
    * to distinguish them. This step is needed to make the ordering
    * consistent with {@code equals()}.
    *
    * To compare the underlying local time of two {@code TemporalAccessor} instances,
    * use {@link ChronoField#NANO_OF_DAY} as a comparator.
    *
    * @param other  the other time to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    * @throws NullPointerException if { @code other} is null
    */
  def compare(other: OffsetTime): Int =
    if (offset == other.offset)
      time.compareTo(other.time)
    else {
      var compare: Int = java.lang.Long.compare(toEpochNano, other.toEpochNano)
      if (compare == 0)
        compare = time.compareTo(other.time)
      compare
    }

  override def compareTo(other: OffsetTime): Int = compare(other)

  /** Checks if the instant of this {@code OffsetTime} is after that of the
    * specified time applying both times to a common date.
    *
    * This method differs from the comparison in {@link #compareTo} in that it
    * only compares the instant of the time. This is equivalent to converting both
    * times to an instant using the same date and comparing the instants.
    *
    * @param other  the other time to compare to, not null
    * @return true if this is after the instant of the specified time
    */
  def isAfter(other: OffsetTime): Boolean = toEpochNano > other.toEpochNano

  /** Checks if the instant of this {@code OffsetTime} is before that of the
    * specified time applying both times to a common date.
    *
    * This method differs from the comparison in {@link #compareTo} in that it
    * only compares the instant of the time. This is equivalent to converting both
    * times to an instant using the same date and comparing the instants.
    *
    * @param other  the other time to compare to, not null
    * @return true if this is before the instant of the specified time
    */
  def isBefore(other: OffsetTime): Boolean = toEpochNano < other.toEpochNano

  /** Checks if the instant of this {@code OffsetTime} is equal to that of the
    * specified time applying both times to a common date.
    *
    * This method differs from the comparison in {@link #compareTo} and {@link #equals}
    * in that it only compares the instant of the time. This is equivalent to converting both
    * times to an instant using the same date and comparing the instants.
    *
    * @param other  the other time to compare to, not null
    * @return true if this is equal to the instant of the specified time
    */
  def isEqual(other: OffsetTime): Boolean = toEpochNano == other.toEpochNano

  /** Checks if this time is equal to another time.
    *
    * The comparison is based on the local-time and the offset.
    * To compare for the same instant on the time-line, use {@link #isEqual(OffsetTime)}.
    *
    * Only objects of type {@code OffsetTime} are compared, other types return false.
    * To compare the underlying local time of two {@code TemporalAccessor} instances,
    * use {@link ChronoField#NANO_OF_DAY} as a comparator.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other time
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: OffsetTime => (this eq other) || (time == other.time) && (offset == other.offset)
      case _                 => false
    }

  /** A hash code for this time.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = time.hashCode ^ offset.hashCode

  /** Outputs this time as a {@code String}, such as {@code 10:15:30+01:00}.
    *
    * The output will be one of the following ISO-8601 formats:
    *<ul>
    * <li>{@code HH:mmXXXXX}</li>
    * <li>{@code HH:mm:ssXXXXX}</li>
    * <li>{@code HH:mm:ss.SSSXXXXX}</li>
    * <li>{@code HH:mm:ss.SSSSSSXXXXX}</li>
    * <li>{@code HH:mm:ss.SSSSSSSSSXXXXX}</li>
    * </ul><p>
    * The format used will be the shortest that outputs the full value of
    * the time where the omitted parts are implied to be zero.
    *
    * @return a string representation of this time, not null
    */
  override def toString: String = time.toString + offset.toString

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

  private def writeReplace: AnyRef = new Ser(Ser.OFFSET_TIME_TYPE, this)

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
    time.writeExternal(out)
    offset.writeExternal(out)
  }
}
