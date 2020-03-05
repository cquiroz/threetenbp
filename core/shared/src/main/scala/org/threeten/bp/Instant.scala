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

import org.threeten.bp.LocalTime.SECONDS_PER_DAY
import org.threeten.bp.LocalTime.SECONDS_PER_HOUR
import org.threeten.bp.LocalTime.SECONDS_PER_MINUTE
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.INSTANT_SECONDS
import org.threeten.bp.temporal.ChronoField.MICRO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.MILLI_OF_SECOND
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.NANOS
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

@SerialVersionUID(-665713676816604388L)
object Instant {

  /** Constant for the 1970-01-01T00:00:00Z epoch instant. */
  lazy val EPOCH: Instant = new Instant(0, 0)

  /** The minimum supported epoch second. */
  private def MIN_SECOND: Long = -31557014167219200L

  /** The maximum supported epoch second. */
  private def MAX_SECOND: Long = 31556889864403199L

  /** Constant for nanos per second. */
  private def NANOS_PER_SECOND: Int = 1000000000

  /** Constant for nanos per milli. */
  private def NANOS_PER_MILLI: Int = 1000000

  /** Constant for millis per sec. */
  private def MILLIS_PER_SEC = 1000

  /** The minimum supported {@code Instant}, '-1000000000-01-01T00:00Z'.
    * This could be used by an application as a "far past" instant.
    *
    * This is one year earlier than the minimum {@code LocalDateTime}.
    * This provides sufficient values to handle the range of {@code ZoneOffset}
    * which affect the instant in addition to the local date-time.
    * The value is also chosen such that the value of the year fits in
    * an {@code int}.
    */
  lazy val MIN: Instant = Instant.ofEpochSecond(MIN_SECOND, 0)

  /** The maximum supported {@code Instant}, '1000000000-12-31T23:59:59.999999999Z'.
    * This could be used by an application as a "far future" instant.
    *
    * This is one year later than the maximum {@code LocalDateTime}.
    * This provides sufficient values to handle the range of {@code ZoneOffset}
    * which affect the instant in addition to the local date-time.
    * The value is also chosen such that the value of the year fits in
    * an {@code int}.
    */
  lazy val MAX: Instant = Instant.ofEpochSecond(MAX_SECOND, 999999999)

  /** Obtains the current instant from the system clock.
    *
    * This will query the {@link Clock#systemUTC() system UTC clock} to
    * obtain the current instant.
    *
    * Using this method will prevent the ability to use an alternate time-source for
    * testing because the clock is effectively hard-coded.
    *
    * @return the current instant using the system clock, not null
    */
  def now: Instant = Clock.systemUTC.instant

  /** Obtains the current instant from the specified clock.
    *
    * This will query the specified clock to obtain the current time.
    *
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current instant, not null
    */
  def now(clock: Clock): Instant = {
    Objects.requireNonNull(clock, "clock")
    clock.instant
  }

  /** Obtains an instance of {@code Instant} using seconds from the
    * epoch of 1970-01-01T00:00:00Z.
    *
    * The nanosecond field is set to zero.
    *
    * @param epochSecond  the number of seconds from 1970-01-01T00:00:00Z
    * @return an instant, not null
    * @throws DateTimeException if the instant exceeds the maximum or minimum instant
    */
  def ofEpochSecond(epochSecond: Long): Instant = create(epochSecond, 0)

  /** Obtains an instance of {@code Instant} using seconds from the
    * epoch of 1970-01-01T00:00:00Z and nanosecond fraction of second.
    *
    * This method allows an arbitrary number of nanoseconds to be passed in.
    * The factory will alter the values of the second and nanosecond in order
    * to ensure that the stored nanosecond is in the range 0 to 999,999,999.
    * For example, the following will result in the exactly the same instant:
    * <pre>
    * Instant.ofSeconds(3, 1);
    * Instant.ofSeconds(4, -999_999_999);
    * Instant.ofSeconds(2, 1000_000_001);
    * </pre>
    *
    * @param epochSecond  the number of seconds from 1970-01-01T00:00:00Z
    * @param nanoAdjustment  the nanosecond adjustment to the number of seconds, positive or negative
    * @return an instant, not null
    * @throws DateTimeException if the instant exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def ofEpochSecond(epochSecond: Long, nanoAdjustment: Long): Instant = {
    val secs: Long =
      Math.addExact(epochSecond, Math.floorDiv(nanoAdjustment, NANOS_PER_SECOND.toLong))
    val nos: Int = Math.floorMod(nanoAdjustment, NANOS_PER_SECOND.toLong).toInt
    create(secs, nos)
  }

  /** Obtains an instance of {@code Instant} using milliseconds from the
    * epoch of 1970-01-01T00:00:00Z.
    *
    * The seconds and nanoseconds are extracted from the specified milliseconds.
    *
    * @param epochMilli  the number of milliseconds from 1970-01-01T00:00:00Z
    * @return an instant, not null
    * @throws DateTimeException if the instant exceeds the maximum or minimum instant
    */
  def ofEpochMilli(epochMilli: Long): Instant = {
    val secs: Long = Math.floorDiv(epochMilli, 1000)
    val mos: Int   = Math.floorMod(epochMilli, 1000).toInt
    create(secs, mos * NANOS_PER_MILLI)
  }

  /** Obtains an instance of {@code Instant} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code Instant}.
    *
    * The conversion extracts the {@link ChronoField#INSTANT_SECONDS INSTANT_SECONDS}
    * and {@link ChronoField#NANO_OF_SECOND NANO_OF_SECOND} fields.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code Instant::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the instant, not null
    * @throws DateTimeException if unable to convert to an { @code Instant}
    */
  def from(temporal: TemporalAccessor): Instant =
    try {
      val instantSecs: Long = temporal.getLong(INSTANT_SECONDS)
      val nanoOfSecond: Int = temporal.get(NANO_OF_SECOND)
      Instant.ofEpochSecond(instantSecs, nanoOfSecond.toLong)
    } catch {
      case ex: DateTimeException =>
        throw new DateTimeException(
          s"Unable to obtain Instant from TemporalAccessor: $temporal, type ${temporal.getClass.getName}",
          ex
        )
    }

  /** Obtains an instance of {@code Instant} from a text string such as
    * {@code 2007-12-03T10:15:30.000Z}.
    *
    * The string must represent a valid instant in UTC and is parsed using
    * {@link DateTimeFormatter#ISO_INSTANT}.
    *
    * @param text  the text to parse, not null
    * @return the parsed instant, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): Instant =
    DateTimeFormatter.ISO_INSTANT.parse(text, new TemporalQuery[Instant] {
      override def queryFrom(temporal: TemporalAccessor): Instant = Instant.from(temporal)
    })

  /** Obtains an instance of {@code Instant} using seconds and nanoseconds.
    *
    * @param seconds  the length of the duration in seconds
    * @param nanoOfSecond  the nano-of-second, from 0 to 999,999,999
    * @throws DateTimeException if the instant exceeds the maximum or minimum instant
    */
  private def create(seconds: Long, nanoOfSecond: Int): Instant =
    if ((seconds | nanoOfSecond) == 0)
      EPOCH
    else if (seconds < MIN_SECOND || seconds > MAX_SECOND)
      throw new DateTimeException("Instant exceeds minimum or maximum instant")
    else
      new Instant(seconds, nanoOfSecond)

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): Instant = {
    val seconds: Long = in.readLong
    val nanos: Int    = in.readInt
    Instant.ofEpochSecond(seconds, nanos.toLong)
  }
}

/** An instantaneous point on the time-line.
  *
  * This class models a single instantaneous point on the time-line.
  * This might be used to record event time-stamps in the application.
  *
  * For practicality, the instant is stored with some constraints.
  * The measurable time-line is restricted to the number of seconds that can be held
  * in a {@code long}. This is greater than the current estimated age of the universe.
  * The instant is stored to nanosecond resolution.
  *
  * The range of an instant requires the storage of a number larger than a {@code long}.
  * To achieve this, the class stores a {@code long} representing epoch-seconds and an
  * {@code int} representing nanosecond-of-second, which will always be between 0 and 999,999,999.
  * The epoch-seconds are measured from the standard Java epoch of {@code 1970-01-01T00:00:00Z}
  * where instants after the epoch have positive values, and earlier instants have negative values.
  * For both the epoch-second and nanosecond parts, a larger value is always later on the time-line
  * than a smaller value.
  *
  * <h3>Time-scale</h3>
  *
  * The length of the solar day is the standard way that humans measure time.
  * This has traditionally been subdivided into 24 hours of 60 minutes of 60 seconds,
  * forming a 86400 second day.
  *
  * Modern timekeeping is based on atomic clocks which precisely define an SI second
  * relative to the transitions of a Caesium atom. The length of an SI second was defined
  * to be very close to the 86400th fraction of a day.
  *
  * Unfortunately, as the Earth rotates the length of the day varies.
  * In addition, over time the average length of the day is getting longer as the Earth slows.
  * As a result, the length of a solar day in 2012 is slightly longer than 86400 SI seconds.
  * The actual length of any given day and the amount by which the Earth is slowing
  * are not predictable and can only be determined by measurement.
  * The UT1 time-scale captures the accurate length of day, but is only available some
  * time after the day has completed.
  *
  * The UTC time-scale is a standard approach to bundle up all the additional fractions
  * of a second from UT1 into whole seconds, known as <i>leap-seconds</i>.
  * A leap-second may be added or removed depending on the Earth's rotational changes.
  * As such, UTC permits a day to have 86399 SI seconds or 86401 SI seconds where
  * necessary in order to keep the day aligned with the Sun.
  *
  * The modern UTC time-scale was introduced in 1972, introducing the concept of whole leap-seconds.
  * Between 1958 and 1972, the definition of UTC was complex, with minor sub-second leaps and
  * alterations to the length of the notional second. As of 2012, discussions are underway
  * to change the definition of UTC again, with the potential to remove leap seconds or
  * introduce other changes.
  *
  * Given the complexity of accurate timekeeping described above, this Java API defines
  * its own time-scale with a simplification. The Java time-scale is defined as follows:
  *<ul>
  * <li>midday will always be exactly as defined by the agreed international civil time</li>
  * <li>other times during the day will be broadly in line with the agreed international civil time</li>
  * <li>the day will be divided into exactly 86400 subdivisions, referred to as "seconds"</li>
  * <li>the Java "second" may differ from an SI second</li>
  * </ul><p>
  * Agreed international civil time is the base time-scale agreed by international convention,
  * which in 2012 is UTC (with leap-seconds).
  *
  * In 2012, the definition of the Java time-scale is the same as UTC for all days except
  * those where a leap-second occurs. On days where a leap-second does occur, the time-scale
  * effectively eliminates the leap-second, maintaining the fiction of 86400 seconds in the day.
  *
  * The main benefit of always dividing the day into 86400 subdivisions is that it matches the
  * expectations of most users of the API. The alternative is to force every user to understand
  * what a leap second is and to force them to have special logic to handle them.
  * Most applications do not have access to a clock that is accurate enough to record leap-seconds.
  * Most applications also do not have a problem with a second being a very small amount longer or
  * shorter than a real SI second during a leap-second.
  *
  * If an application does have access to an accurate clock that reports leap-seconds, then the
  * recommended technique to implement the Java time-scale is to use the UTC-SLS convention.
  * <a href="http://www.cl.cam.ac.uk/~mgk25/time/utc-sls/">UTC-SLS</a> effectively smoothes the
  * leap-second over the last 1000 seconds of the day, making each of the last 1000 "seconds"
  * 1/1000th longer or shorter than a real SI second.
  *
  * One final problem is the definition of the agreed international civil time before the
  * introduction of modern UTC in 1972. This includes the Java epoch of {@code 1970-01-01}.
  * It is intended that instants before 1972 be interpreted based on the solar day divided
  * into 86400 subdivisions.
  *
  * The Java time-scale is used by all date-time classes.
  * This includes {@code Instant}, {@code LocalDate}, {@code LocalTime}, {@code OffsetDateTime},
  * {@code ZonedDateTime} and {@code Duration}.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * Constructs an instance of {@code Instant} using seconds from the epoch of
  * 1970-01-01T00:00:00Z and nanosecond fraction of second.
  *
  * @param seconds  the number of seconds from 1970-01-01T00:00:00Z
  * @param nanos  the nanoseconds within the second, must be positive and never exceed 999,999,999
  */
@SerialVersionUID(-665713676816604388L)
final class Instant private (private val seconds: Long, private val nanos: Int)
    extends TemporalAccessor
    with Temporal
    with TemporalAdjuster
    with Ordered[Instant]
    with Serializable {

  /** Checks if the specified field is supported.
    *
    * This checks if this instant can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The supported fields are:
    * <ul>
    * <li>{@code NANO_OF_SECOND}
    * <li>{@code MICRO_OF_SECOND}
    * <li>{@code MILLI_OF_SECOND}
    * <li>{@code INSTANT_SECONDS}
    * </ul>
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this instant, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      (field eq INSTANT_SECONDS) || (field eq NANO_OF_SECOND) || (field eq MICRO_OF_SECOND) || (field eq MILLI_OF_SECOND)
    else field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit]) unit.isTimeBased || (unit eq DAYS)
    else unit != null && unit.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This instant is used to enhance the accuracy of the returned range.
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

  /** Gets the value of the specified field from this instant as an {@code int}.
    *
    * This queries this instant for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time, except {@code INSTANT_SECONDS} which is too
    * large to fit in an {@code int} and throws a {@code DateTimeException}.
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
      case f: ChronoField =>
        f match {
          case NANO_OF_SECOND  => nanos
          case MICRO_OF_SECOND => nanos / 1000
          case MILLI_OF_SECOND => nanos / Instant.NANOS_PER_MILLI
          case _               => throw UnsupportedTemporalTypeException.field(field)
        }
      case _ => range(field).checkValidIntValue(field.getFrom(this), field)
    }

  /** Gets the value of the specified field from this instant as a {@code long}.
    *
    * This queries this instant for the value for the specified field.
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
      case f: ChronoField =>
        f match {
          case NANO_OF_SECOND  => nanos.toLong
          case MICRO_OF_SECOND => nanos / 1000L
          case MILLI_OF_SECOND => nanos.toLong / Instant.NANOS_PER_MILLI
          case INSTANT_SECONDS => seconds
          case _               => throw UnsupportedTemporalTypeException.field(field)
        }
      case _ => field.getFrom(this)
    }

  /** Gets the number of seconds from the Java epoch of 1970-01-01T00:00:00Z.
    *
    * The epoch second count is a simple incrementing count of seconds where
    * second 0 is 1970-01-01T00:00:00Z.
    * The nanosecond part of the day is returned by {@code getNanosOfSecond}.
    *
    * @return the seconds from the epoch of 1970-01-01T00:00:00Z
    */
  def getEpochSecond: Long = seconds

  /** Gets the number of nanoseconds, later along the time-line, from the start
    * of the second.
    *
    * The nanosecond-of-second value measures the total number of nanoseconds from
    * the second returned by {@code getEpochSecond}.
    *
    * @return the nanoseconds within the second, always positive, never exceeds 999,999,999
    */
  def getNano: Int = nanos

  /** Returns an adjusted copy of this instant.
    *
    * This returns a new {@code Instant}, based on this one, with the date adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return an { @code Instant} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): Instant =
    adjuster.adjustInto(this).asInstanceOf[Instant]

  /** Returns a copy of this instant with the specified field set to a new value.
    *
    * This returns a new {@code Instant}, based on this one, with the value
    * for the specified field changed.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The supported fields behave as follows:
    * <ul>
    * <li>{@code NANO_OF_SECOND} -
    * Returns an {@code Instant} with the specified nano-of-second.
    * The epoch-second will be unchanged.
    * <li>{@code MICRO_OF_SECOND} -
    * Returns an {@code Instant} with the nano-of-second replaced by the specified
    * micro-of-second multiplied by 1,000. The epoch-second will be unchanged.
    * <li>{@code MILLI_OF_SECOND} -
    * Returns an {@code Instant} with the nano-of-second replaced by the specified
    * milli-of-second multiplied by 1,000,000. The epoch-second will be unchanged.
    * <li>{@code INSTANT_SECONDS} -
    * Returns an {@code Instant} with the specified epoch-second.
    * The nano-of-second will be unchanged.
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
    * @return an { @code Instant} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): Instant = {
    field match {
      case f: ChronoField =>
        f.checkValidValue(newValue)
        f match {
          case MILLI_OF_SECOND =>
            val nval: Int = newValue.toInt * Instant.NANOS_PER_MILLI.toInt
            return if (nval != nanos) Instant.create(seconds, nval) else this
          case MICRO_OF_SECOND =>
            val nval: Int = newValue.toInt * 1000
            return if (nval != nanos) Instant.create(seconds, nval) else this
          case NANO_OF_SECOND =>
            return if (newValue != nanos) Instant.create(seconds, newValue.toInt) else this
          case INSTANT_SECONDS =>
            return if (newValue != seconds) Instant.create(newValue, nanos) else this
          case _ =>
            throw UnsupportedTemporalTypeException.field(field)
        }
      case _ =>
    }
    field.adjustInto(this, newValue)
  }

  /** Returns a copy of this {@code Instant} truncated to the specified unit.
    *
    * Truncating the instant returns a copy of the original with fields
    * smaller than the specified unit set to zero.
    * The fields are calculated on the basis of using a UTC offset as seen
    * in {@code toString}.
    * For example, truncating with the {@link ChronoUnit#MINUTES MINUTES} unit will
    * round down to the nearest minute, setting the seconds and nanoseconds to zero.
    *
    * The unit must have a {@linkplain TemporalUnit#getDuration() duration}
    * that divides into the length of a standard day without remainder.
    * This includes all supplied time units on {@link ChronoUnit} and
    * {@link ChronoUnit#DAYS DAYS}. Other units throw an exception.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param unit  the unit to truncate to, not null
    * @return an { @code Instant} based on this instant with the time truncated, not null
    * @throws DateTimeException if the unit is invalid for truncation
    */
  def truncatedTo(unit: TemporalUnit): Instant = {
    if (unit eq ChronoUnit.NANOS) {
      return this
    }
    val unitDur: Duration = unit.getDuration
    if (unitDur.getSeconds > LocalTime.SECONDS_PER_DAY) {
      throw new DateTimeException("Unit is too large to be used for truncation")
    }
    val dur: Long = unitDur.toNanos
    if ((LocalTime.NANOS_PER_DAY % dur) != 0) {
      throw new DateTimeException("Unit must divide into a standard day without remainder")
    }
    val nod: Long    = (seconds % LocalTime.SECONDS_PER_DAY) * LocalTime.NANOS_PER_SECOND + nanos
    val result: Long = Math.floorDiv(nod, dur) * dur

    plusNanos(result - nod)
  }

  /** {@inheritDoc}
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  override def plus(amount: TemporalAmount): Instant = amount.addTo(this).asInstanceOf[Instant]

  /** {@inheritDoc}
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): Instant = {
    unit match {
      case u: ChronoUnit =>
        import ChronoUnit._
        u match {
          case NANOS =>
            return plusNanos(amountToAdd)
          case MICROS =>
            return plus(amountToAdd / 1000000, (amountToAdd % 1000000) * 1000)
          case MILLIS =>
            return plusMillis(amountToAdd)
          case SECONDS =>
            return plusSeconds(amountToAdd)
          case MINUTES =>
            return plusSeconds(Math.multiplyExact(amountToAdd, SECONDS_PER_MINUTE.toLong))
          case HOURS =>
            return plusSeconds(Math.multiplyExact(amountToAdd, SECONDS_PER_HOUR.toLong))
          case HALF_DAYS =>
            return plusSeconds(Math.multiplyExact(amountToAdd, SECONDS_PER_DAY / 2L))
          case DAYS =>
            return plusSeconds(Math.multiplyExact(amountToAdd, SECONDS_PER_DAY.toLong))
          case _ =>
            throw UnsupportedTemporalTypeException.unit(unit)
        }
      case _ =>
    }
    unit.addTo(this, amountToAdd)
  }

  /** Returns a copy of this instant with the specified duration in seconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToAdd  the seconds to add, positive or negative
    * @return an { @code Instant} based on this instant with the specified seconds added, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusSeconds(secondsToAdd: Long): Instant = plus(secondsToAdd, 0)

  /** Returns a copy of this instant with the specified duration in milliseconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param millisToAdd  the milliseconds to add, positive or negative
    * @return an { @code Instant} based on this instant with the specified milliseconds added, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusMillis(millisToAdd: Long): Instant =
    plus(millisToAdd / 1000, (millisToAdd % 1000) * Instant.NANOS_PER_MILLI)

  /** Returns a copy of this instant with the specified duration in nanoseconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToAdd  the nanoseconds to add, positive or negative
    * @return an { @code Instant} based on this instant with the specified nanoseconds added, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusNanos(nanosToAdd: Long): Instant = plus(0, nanosToAdd)

  /** Returns a copy of this instant with the specified duration added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToAdd  the seconds to add, positive or negative
    * @param nanosToAdd  the nanos to add, positive or negative
    * @return an { @code Instant} based on this instant with the specified seconds added, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  private def plus(secondsToAdd: Long, nanosToAdd: Long): Instant = {
    var _nanosToAdd = nanosToAdd
    if ((secondsToAdd | _nanosToAdd) == 0) {
      return this
    }
    var epochSec: Long = Math.addExact(seconds, secondsToAdd)
    epochSec    = Math.addExact(epochSec, _nanosToAdd / Instant.NANOS_PER_SECOND)
    _nanosToAdd = _nanosToAdd % Instant.NANOS_PER_SECOND
    val nanoAdjustment: Long = nanos + _nanosToAdd
    Instant.ofEpochSecond(epochSec, nanoAdjustment)
  }

  /** {@inheritDoc}
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  override def minus(amount: TemporalAmount): Instant =
    amount.subtractFrom(this).asInstanceOf[Instant]

  /** {@inheritDoc}
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): Instant =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this instant with the specified duration in seconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToSubtract  the seconds to subtract, positive or negative
    * @return an { @code Instant} based on this instant with the specified seconds subtracted, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusSeconds(secondsToSubtract: Long): Instant =
    if (secondsToSubtract == Long.MinValue)
      plusSeconds(Long.MaxValue).plusSeconds(1)
    else
      plusSeconds(-secondsToSubtract)

  /** Returns a copy of this instant with the specified duration in milliseconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param millisToSubtract  the milliseconds to subtract, positive or negative
    * @return an { @code Instant} based on this instant with the specified milliseconds subtracted, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusMillis(millisToSubtract: Long): Instant = {
    if (millisToSubtract == Long.MinValue) {
      return plusMillis(Long.MaxValue).plusMillis(1)
    }
    plusMillis(-millisToSubtract)
  }

  /** Returns a copy of this instant with the specified duration in nanoseconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToSubtract  the nanoseconds to subtract, positive or negative
    * @return an { @code Instant} based on this instant with the specified nanoseconds subtracted, not null
    * @throws DateTimeException if the result exceeds the maximum or minimum instant
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusNanos(nanosToSubtract: Long): Instant =
    if (nanosToSubtract == Long.MinValue)
      plusNanos(Long.MaxValue).plusNanos(1)
    else
      plusNanos(-nanosToSubtract)

  /** Queries this instant using the specified query.
    *
    * This queries this instant using the specified query strategy object.
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
    if (query eq TemporalQueries.precision)
      NANOS.asInstanceOf[R]
    else if ((query eq TemporalQueries.localDate) || (query eq TemporalQueries.localTime) || (query eq TemporalQueries.chronology) || (query eq TemporalQueries.zoneId) || (query eq TemporalQueries.zone) || (query eq TemporalQueries.offset))
      null.asInstanceOf[R]
    else
      query.queryFrom(this)

  /** Adjusts the specified temporal object to have this instant.
    *
    * This returns a temporal object of the same observable type as the input
    * with the instant changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * twice, passing {@link ChronoField#INSTANT_SECONDS} and
    * {@link ChronoField#NANO_OF_SECOND} as the fields.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisInstant.adjustInto(temporal);
    * temporal = temporal.with(thisInstant);
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
    temporal.`with`(INSTANT_SECONDS, seconds).`with`(NANO_OF_SECOND, nanos.toLong)

  /** Calculates the period between this instant and another instant in
    * terms of the specified unit.
    *
    * This calculates the period between two instants in terms of a single unit.
    * The start and end points are {@code this} and the specified instant.
    * The result will be negative if the end is before the start.
    * The calculation returns a whole number, representing the number of
    * complete units between the two instants.
    * The {@code Temporal} passed to this method is converted to a
    * {@code Instant} using {@link #from(TemporalAccessor)}.
    * For example, the period in days between two dates can be calculated
    * using {@code startInstant.until(endInstant, SECONDS)}.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * <pre>
    * long period = start.until(end, SECONDS);   // this method
    * dateTime.plus(SECONDS.between(start, end));      // use in plus/minus
    * </pre>
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code NANOS}, {@code MICROS}, {@code MILLIS}, {@code SECONDS},
    * {@code MINUTES}, {@code HOURS}, {@code HALF_DAYS} and {@code DAYS}
    * are supported. Other {@code ChronoUnit} values will throw an exception.
    *
    * If the unit is not a {@code ChronoUnit}, then the result of this method
    * is obtained by invoking {@code TemporalUnit.between(Temporal, Temporal)}
    * passing {@code this} as the first argument and the input temporal as
    * the second argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endExclusive  the end date, which is converted to an { @code Instant}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this date and the end date
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: Instant = Instant.from(endExclusive)
    unit match {
      case f: ChronoUnit =>
        import ChronoUnit._
        f match {
          case NANOS =>
            return nanosUntil(end)
          case MICROS =>
            return nanosUntil(end) / 1000
          case MILLIS =>
            return Math.subtractExact(end.toEpochMilli, toEpochMilli)
          case SECONDS =>
            return secondsUntil(end)
          case MINUTES =>
            return secondsUntil(end) / SECONDS_PER_MINUTE
          case HOURS =>
            return secondsUntil(end) / SECONDS_PER_HOUR
          case HALF_DAYS =>
            return secondsUntil(end) / (12 * SECONDS_PER_HOUR)
          case DAYS =>
            return secondsUntil(end) / SECONDS_PER_DAY
          case _ =>
            throw throw UnsupportedTemporalTypeException.unit(unit)
        }
      case _ =>
    }
    unit.between(this, end)
  }

  private def nanosUntil(end: Instant): Long = {
    val secsDiff: Long   = Math.subtractExact(end.seconds, seconds)
    val totalNanos: Long = Math.multiplyExact(secsDiff, Instant.NANOS_PER_SECOND.toLong)
    Math.addExact(totalNanos, end.nanos.toLong - nanos.toLong)
  }

  private def secondsUntil(end: Instant): Long = {
    var secsDiff: Long  = Math.subtractExact(end.seconds, seconds)
    val nanosDiff: Long = end.nanos.toLong - nanos.toLong
    if (secsDiff > 0 && nanosDiff < 0) {
      secsDiff -= 1
    } else if (secsDiff < 0 && nanosDiff > 0) {
      secsDiff += 1
    }
    secsDiff
  }

  /** Combines this instant with an offset to create an {@code OffsetDateTime}.
    *
    * This returns an {@code OffsetDateTime} formed from this instant at the
    * specified offset from UTC/Greenwich. An exception will be thrown if the
    * instant is too large to fit into an offset date-time.
    *
    * This method is equivalent to
    * {@link OffsetDateTime#ofInstant(Instant, ZoneId) OffsetDateTime.ofInstant(this, offset)}.
    *
    * @param offset  the offset to combine with, not null
    * @return the offset date-time formed from this instant and the specified offset, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def atOffset(offset: ZoneOffset): OffsetDateTime = OffsetDateTime.ofInstant(this, offset)

  /** Combines this instant with a time-zone to create a {@code ZonedDateTime}.
    *
    * This returns an {@code ZonedDateTime} formed from this instant at the
    * specified time-zone. An exception will be thrown if the instant is too
    * large to fit into a zoned date-time.
    *
    * This method is equivalent to
    * {@link ZonedDateTime#ofInstant(Instant, ZoneId) ZonedDateTime.ofInstant(this, zone)}.
    *
    * @param zone  the zone to combine with, not null
    * @return the zoned date-time formed from this instant and the specified zone, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def atZone(zone: ZoneId): ZonedDateTime = ZonedDateTime.ofInstant(this, zone)

  /** Converts this instant to the number of milliseconds from the epoch
    * of 1970-01-01T00:00:00Z.
    *
    * If this instant represents a point on the time-line too far in the future
    * or past to fit in a {@code long} milliseconds, then an exception is thrown.
    *
    * If this instant has greater than millisecond precision, then the conversion
    * will drop any excess precision information as though the amount in nanoseconds
    * was subject to integer division by one million.
    *
    * @return the number of milliseconds since the epoch of 1970-01-01T00:00:00Z
    * @throws ArithmeticException if numeric overflow occurs
    */
  def toEpochMilli: Long =
    if (seconds >= 0) {
      val millis: Long = Math.multiplyExact(seconds, Instant.MILLIS_PER_SEC.toLong)
      Math.addExact(millis, nanos / Instant.NANOS_PER_MILLI.toLong)
    } else {
      // prevent an overflow in seconds * 1000
      // instead of going form the second farther away from 0
      // going toward 0
      // we go from the second closer to 0 away from 0
      // that way we always stay in the valid long range
      // seconds + 1 can not overflow because it is negative
      val millis = Math.multiplyExact(seconds + 1, Instant.MILLIS_PER_SEC.toLong)
      Math.subtractExact(
        millis,
        Instant.MILLIS_PER_SEC.toLong - nanos.toLong / Instant.NANOS_PER_MILLI.toLong
      )
    }

  /** Compares this instant to the specified instant.
    *
    * The comparison is based on the time-line position of the instants.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param otherInstant  the other instant to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    * @throws NullPointerException if otherInstant is null
    */
  def compare(otherInstant: Instant): Int = {
    val cmp: Int = java.lang.Long.compare(seconds, otherInstant.seconds)
    if (cmp != 0) cmp
    else nanos - otherInstant.nanos
  }

  override def compareTo(other: Instant): Int = compare(other)

  /** Checks if this instant is after the specified instant.
    *
    * The comparison is based on the time-line position of the instants.
    *
    * @param otherInstant  the other instant to compare to, not null
    * @return true if this instant is after the specified instant
    * @throws NullPointerException if otherInstant is null
    */
  def isAfter(otherInstant: Instant): Boolean = compareTo(otherInstant) > 0

  /** Checks if this instant is before the specified instant.
    *
    * The comparison is based on the time-line position of the instants.
    *
    * @param otherInstant  the other instant to compare to, not null
    * @return true if this instant is before the specified instant
    * @throws NullPointerException if otherInstant is null
    */
  def isBefore(otherInstant: Instant): Boolean = compareTo(otherInstant) < 0

  /** Checks if this instant is equal to the specified instant.
    *
    * The comparison is based on the time-line position of the instants.
    *
    * @param other  the other instant, null returns false
    * @return true if the other instant is equal to this one
    */
  override def equals(other: Any): Boolean =
    other match {
      case otherInstant: Instant =>
        (this eq otherInstant) || (this.seconds == otherInstant.seconds && this.nanos == otherInstant.nanos)
      case _ => false
    }

  /** Returns a hash code for this instant.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = (seconds ^ (seconds >>> 32)).toInt + 51 * nanos

  /** A string representation of this instant using ISO-8601 representation.
    *
    * The format used is the same as {@link DateTimeFormatter#ISO_INSTANT}.
    *
    * @return an ISO-8601 representation of this instant, not null
    */
  override def toString: String = DateTimeFormatter.ISO_INSTANT.format(this)

  private def writeReplace: AnyRef = new Ser(Ser.INSTANT_TYPE, this)

  /** Defend against malicious streams.
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit = {
    out.writeLong(seconds)
    out.writeInt(nanos)
  }
}
