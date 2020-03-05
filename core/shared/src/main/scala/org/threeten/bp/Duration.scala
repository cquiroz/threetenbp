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

import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.InvalidObjectException
import java.io.ObjectStreamException
import java.io.Serializable
import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode
import java.util.{ Arrays, Collections, Objects }
import java.util.regex.Matcher
import java.util.regex.Pattern

import org.threeten.bp.LocalTime.SECONDS_PER_DAY
import org.threeten.bp.LocalTime.SECONDS_PER_HOUR
import org.threeten.bp.LocalTime.SECONDS_PER_MINUTE
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoUnit.NANOS
import org.threeten.bp.temporal.ChronoUnit.MICROS
import org.threeten.bp.temporal.ChronoUnit.MILLIS
import org.threeten.bp.temporal.ChronoUnit.SECONDS
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

@SerialVersionUID(3078945930695997490L)
object Duration {

  /** Constant for a duration of zero. */
  lazy val ZERO: Duration = new Duration(0, 0)

  /** Constant for nanos per second. */
  private def NANOS_PER_SECOND: Int = 1000000000

  /** Constant for nanos per milli. */
  private def NANOS_PER_MILLI: Int = 1000000

  /** Constant for nanos per second. */
  private def BI_NANOS_PER_SECOND: BigInteger = BigInteger.valueOf(NANOS_PER_SECOND.toLong)

  /** The pattern for parsing. */
  private def PATTERN: Pattern = Pattern.compile(
    "([-+]?)P(?:([-+]?[0-9]+)D)?" + "(T(?:([-+]?[0-9]+)H)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)(?:[.,]([0-9]{0,9}))?S)?)?",
    Pattern.CASE_INSENSITIVE
  )

  /** Obtains an instance of {@code Duration} from a number of standard 24 hour days.
    *
    * The seconds are calculated based on the standard definition of a day,
    * where each day is 86400 seconds which implies a 24 hour day.
    * The nanosecond in second field is set to zero.
    *
    * @param days  the number of days, positive or negative
    * @return a { @code Duration}, not null
    * @throws ArithmeticException if the input days exceeds the capacity of { @code Duration}
    */
  def ofDays(days: Long): Duration = create(Math.multiplyExact(days, 86400), 0)

  /** Obtains an instance of {@code Duration} from a number of standard length hours.
    *
    * The seconds are calculated based on the standard definition of an hour,
    * where each hour is 3600 seconds.
    * The nanosecond in second field is set to zero.
    *
    * @param hours  the number of hours, positive or negative
    * @return a { @code Duration}, not null
    * @throws ArithmeticException if the input hours exceeds the capacity of { @code Duration}
    */
  def ofHours(hours: Long): Duration = create(Math.multiplyExact(hours, 3600), 0)

  /** Obtains an instance of {@code Duration} from a number of standard length minutes.
    *
    * The seconds are calculated based on the standard definition of a minute,
    * where each minute is 60 seconds.
    * The nanosecond in second field is set to zero.
    *
    * @param minutes  the number of minutes, positive or negative
    * @return a { @code Duration}, not null
    * @throws ArithmeticException if the input minutes exceeds the capacity of { @code Duration}
    */
  def ofMinutes(minutes: Long): Duration = create(Math.multiplyExact(minutes, 60), 0)

  /** Obtains an instance of {@code Duration} from a number of seconds.
    *
    * The nanosecond in second field is set to zero.
    *
    * @param seconds  the number of seconds, positive or negative
    * @return a { @code Duration}, not null
    */
  def ofSeconds(seconds: Long): Duration = create(seconds, 0)

  /** Obtains an instance of {@code Duration} from a number of seconds
    * and an adjustment in nanoseconds.
    *
    * This method allows an arbitrary number of nanoseconds to be passed in.
    * The factory will alter the values of the second and nanosecond in order
    * to ensure that the stored nanosecond is in the range 0 to 999,999,999.
    * For example, the following will result in the exactly the same duration:
    * <pre>
    * Duration.ofSeconds(3, 1);
    * Duration.ofSeconds(4, -999_999_999);
    * Duration.ofSeconds(2, 1000_000_001);
    * </pre>
    *
    * @param seconds  the number of seconds, positive or negative
    * @param nanoAdjustment  the nanosecond adjustment to the number of seconds, positive or negative
    * @return a { @code Duration}, not null
    * @throws ArithmeticException if the adjustment causes the seconds to exceed the capacity of { @code Duration}
    */
  def ofSeconds(seconds: Long, nanoAdjustment: Long): Duration = {
    val secs: Long = Math.addExact(seconds, Math.floorDiv(nanoAdjustment, NANOS_PER_SECOND.toLong))
    val nos: Int   = Math.floorMod(nanoAdjustment, NANOS_PER_SECOND.toLong).toInt
    create(secs, nos)
  }

  /** Obtains an instance of {@code Duration} from a number of milliseconds.
    *
    * The seconds and nanoseconds are extracted from the specified milliseconds.
    *
    * @param millis  the number of milliseconds, positive or negative
    * @return a { @code Duration}, not null
    */
  def ofMillis(millis: Long): Duration = {
    var secs: Long = millis / 1000
    var mos: Int   = (millis % 1000).toInt
    if (mos < 0) {
      mos += 1000
      secs -= 1
    }
    create(secs, mos * NANOS_PER_MILLI)
  }

  /** Obtains an instance of {@code Duration} from a number of nanoseconds.
    *
    * The seconds and nanoseconds are extracted from the specified nanoseconds.
    *
    * @param nanos  the number of nanoseconds, positive or negative
    * @return a { @code Duration}, not null
    */
  def ofNanos(nanos: Long): Duration = {
    var secs: Long = nanos / NANOS_PER_SECOND
    var nos: Int   = (nanos % NANOS_PER_SECOND).toInt
    if (nos < 0) {
      nos += NANOS_PER_SECOND
      secs -= 1
    }
    create(secs, nos)
  }

  /** Obtains an instance of {@code Duration} from a duration in the specified unit.
    *
    * The parameters represent the two parts of a phrase like '6 Hours'. For example:
    * <pre>
    * Duration.of(3, SECONDS);
    * Duration.of(465, HOURS);
    * </pre>
    * Only a subset of units are accepted by this method.
    * The unit must either have an {@link TemporalUnit#isDurationEstimated() exact duration} or
    * be {@link ChronoUnit#DAYS} which is treated as 24 hours. Other units throw an exception.
    *
    * @param amount  the amount of the duration, measured in terms of the unit, positive or negative
    * @param unit  the unit that the duration is measured in, must have an exact duration, not null
    * @return a { @code Duration}, not null
    * @throws DateTimeException if the period unit has an estimated duration
    * @throws ArithmeticException if a numeric overflow occurs
    */
  def of(amount: Long, unit: TemporalUnit): Duration = ZERO.plus(amount, unit)

  /** Obtains an instance of {@code Duration} from an amount.
    *
    * This obtains a duration based on the specified amount.
    * A TemporalAmount represents an amount of time, which may be date-based
    * or time-based, which this factory extracts to a duration.
    *
    * The conversion loops around the set of units from the amount and uses
    * the duration of the unit to calculate the total Duration.
    * Only a subset of units are accepted by this method.
    * The unit must either have an exact duration or be ChronoUnit.DAYS which
    * is treated as 24 hours. If any other units are found then an exception is thrown.
    *
    * @param amount  the amount to convert, not null
    * @return a { @code Duration}, not null
    * @throws DateTimeException if the amount cannot be converted
    * @throws ArithmeticException if a numeric overflow occurs
    */
  def from(amount: TemporalAmount): Duration = {
    Objects.requireNonNull(amount, "amount")
    var duration: Duration = ZERO
    val units              = amount.getUnits.iterator
    while (units.hasNext) {
      val unit = units.next()
      duration = duration.plus(amount.get(unit), unit)
    }
    duration
  }

  /** Obtains an instance of {@code Duration} representing the duration between two instants.
    *
    * Obtains a {@code Duration} representing the duration between two instants.
    * This calculates the duration between two temporal objects of the same type.
    * The difference in seconds is calculated using {@link Temporal#until(Temporal, TemporalUnit)}.
    * The difference in nanoseconds is calculated using by querying the
    * {@link ChronoField#NANO_OF_SECOND NANO_OF_SECOND} field.
    *
    * The result of this method can be a negative period if the end is before the start.
    * To guarantee to obtain a positive duration call abs() on the result.
    *
    * @param startInclusive  the start instant, inclusive, not null
    * @param endExclusive  the end instant, exclusive, not null
    * @return a { @code Duration}, not null
    * @throws DateTimeException if the seconds between the temporals cannot be obtained
    * @throws ArithmeticException if the calculation exceeds the capacity of { @code Duration}
    */
  def between(startInclusive: Temporal, endExclusive: Temporal): Duration = {
    var secs: Long  = startInclusive.until(endExclusive, SECONDS)
    var nanos: Long = 0
    if (startInclusive.isSupported(NANO_OF_SECOND) && endExclusive.isSupported(NANO_OF_SECOND)) {
      try {
        val startNos: Long = startInclusive.getLong(NANO_OF_SECOND)
        nanos = endExclusive.getLong(NANO_OF_SECOND) - startNos
        if (secs > 0 && nanos < 0) {
          nanos += NANOS_PER_SECOND
        } else if (secs < 0 && nanos > 0) {
          nanos -= NANOS_PER_SECOND
        } else if (secs == 0 && nanos != 0) {
          val adjustedEnd: Temporal = endExclusive.`with`(NANO_OF_SECOND, startNos)
          secs = startInclusive.until(adjustedEnd, SECONDS)
        }
      } catch {
        case _: DateTimeException   =>
        case _: ArithmeticException =>
      }
    }
    ofSeconds(secs, nanos)
  }

  /** Obtains a {@code Duration} from a text string such as {@code PnDTnHnMn.nS}.
    *
    * This will parse a textual representation of a duration, including the
    * string produced by {@code toString()}. The formats accepted are based
    * on the ISO-8601 duration format {@code PnDTnHnMn.nS} with days
    * considered to be exactly 24 hours.
    *
    * The string starts with an optional sign, denoted by the ASCII negative
    * or positive symbol. If negative, the whole period is negated.
    * The ASCII letter "P" is next in upper or lower case.
    * There are then four sections, each consisting of a number and a suffix.
    * The sections have suffixes in ASCII of "D", "H", "M" and "S" for
    * days, hours, minutes and seconds, accepted in upper or lower case.
    * The suffixes must occur in order. The ASCII letter "T" must occur before
    * the first occurrence, if any, of an hour, minute or second section.
    * At least one of the four sections must be present, and if "T" is present
    * there must be at least one section after the "T".
    * The number part of each section must consist of one or more ASCII digits.
    * The number may be prefixed by the ASCII negative or positive symbol.
    * The number of days, hours and minutes must parse to a {@code long}.
    * The number of seconds must parse to a {@code long} with optional fraction.
    * The decimal point may be either a dot or a comma.
    * The fractional part may have from zero to 9 digits.
    *
    * The leading plus/minus sign, and negative values for other units are
    * not part of the ISO-8601 standard.
    *
    * Examples:
    * <pre>
    * "PT20.345S" -> parses as "20.345 seconds"
    * "PT15M"     -> parses as "15 minutes" (where a minute is 60 seconds)
    * "PT10H"     -> parses as "10 hours" (where an hour is 3600 seconds)
    * "P2D"       -> parses as "2 days" (where a day is 24 hours or 86400 seconds)
    * "P2DT3H4M"  -> parses as "2 days, 3 hours and 4 minutes"
    * "P-6H3M"    -> parses as "-6 hours and +3 minutes"
    * "-P6H3M"    -> parses as "-6 hours and -3 minutes"
    * "-P-6H+3M"  -> parses as "+6 hours and -3 minutes"
    * </pre>
    *
    * @param text  the text to parse, not null
    * @return the parsed duration, not null
    * @throws DateTimeParseException if the text cannot be parsed to a duration
    */
  def parse(text: CharSequence): Duration = {
    Objects.requireNonNull(text, "text")
    val matcher: Matcher = PATTERN.matcher(text)
    if (matcher.matches) {
      if (!("T" == matcher.group(3))) {
        val negate: Boolean       = "-" == matcher.group(1)
        val dayMatch: String      = matcher.group(2)
        val hourMatch: String     = matcher.group(4)
        val minuteMatch: String   = matcher.group(5)
        val secondMatch: String   = matcher.group(6)
        val fractionMatch: String = matcher.group(7)
        if (dayMatch != null || hourMatch != null || minuteMatch != null || secondMatch != null) {
          val daysAsSecs: Long      = parseNumber(text, dayMatch, SECONDS_PER_DAY, "days")
          val hoursAsSecs: Long     = parseNumber(text, hourMatch, SECONDS_PER_HOUR, "hours")
          val minsAsSecs: Long      = parseNumber(text, minuteMatch, SECONDS_PER_MINUTE, "minutes")
          val seconds: Long         = parseNumber(text, secondMatch, 1, "seconds")
          val negativeSecs: Boolean = secondMatch != null && secondMatch.charAt(0) == '-'
          val nanos: Int            = parseFraction(text, fractionMatch, if (negativeSecs) -1 else 1)
          try return create(negate, daysAsSecs, hoursAsSecs, minsAsSecs, seconds, nanos)
          catch {
            case ex: ArithmeticException =>
              throw new DateTimeParseException("Text cannot be parsed to a Duration: overflow",
                                               text,
                                               0,
                                               ex)
          }
        }
      }
    }
    throw new DateTimeParseException("Text cannot be parsed to a Duration", text, 0)
  }

  private def parseNumber(
    text:       CharSequence,
    parsed:     String,
    multiplier: Int,
    errorText:  String
  ): Long = {
    var _parsed = parsed
    if (_parsed == null)
      return 0
    try {
      if (_parsed.startsWith("+"))
        _parsed = _parsed.substring(1)
      val `val`: Long = java.lang.Long.parseLong(_parsed)
      Math.multiplyExact(`val`, multiplier.toLong)
    } catch {
      case ex: NumberFormatException =>
        throw new DateTimeParseException(s"Text cannot be parsed to a Duration: $errorText",
                                         text,
                                         0,
                                         ex)
      case ex: ArithmeticException =>
        throw new DateTimeParseException(s"Text cannot be parsed to a Duration: $errorText",
                                         text,
                                         0,
                                         ex)
    }
  }

  private def parseFraction(text: CharSequence, parsed: String, negate: Int): Int = {
    var _parsed = parsed
    if (_parsed == null || _parsed.length == 0)
      return 0
    try {
      _parsed = (_parsed + "000000000").substring(0, 9)
      _parsed.toInt * negate
    } catch {
      case ex: NumberFormatException =>
        throw new DateTimeParseException("Text cannot be parsed to a Duration: fraction",
                                         text,
                                         0,
                                         ex)
      case ex: ArithmeticException =>
        throw new DateTimeParseException("Text cannot be parsed to a Duration: fraction",
                                         text,
                                         0,
                                         ex)
    }
  }

  private def create(
    negate:      Boolean,
    daysAsSecs:  Long,
    hoursAsSecs: Long,
    minsAsSecs:  Long,
    secs:        Long,
    nanos:       Int
  ): Duration = {
    val seconds: Long =
      Math.addExact(daysAsSecs, Math.addExact(hoursAsSecs, Math.addExact(minsAsSecs, secs)))
    if (negate)
      ofSeconds(seconds, nanos.toLong).negated
    else
      ofSeconds(seconds, nanos.toLong)
  }

  /** Obtains an instance of {@code Duration} using seconds and nanoseconds.
    *
    * @param seconds  the length of the duration in seconds, positive or negative
    * @param nanoAdjustment  the nanosecond adjustment within the second, from 0 to 999,999,999
    */
  private def create(seconds: Long, nanoAdjustment: Int): Duration =
    if ((seconds | nanoAdjustment) == 0) ZERO
    else new Duration(seconds, nanoAdjustment)

  /** Creates an instance of {@code Duration} from a number of seconds.
    *
    * @param seconds  the number of seconds, up to scale 9, positive or negative
    * @return a { @code Duration}, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  private def create(seconds: BigDecimal): Duration = {
    val nanos: BigInteger         = seconds.movePointRight(9).toBigIntegerExact
    val divRem: Array[BigInteger] = nanos.divideAndRemainder(BI_NANOS_PER_SECOND)
    if (divRem(0).bitLength > 63)
      throw new ArithmeticException(s"Exceeds capacity of Duration: $nanos")
    ofSeconds(divRem(0).longValue, divRem(1).longValue)
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): Duration = {
    val seconds: Long = in.readLong
    val nanos: Int    = in.readInt
    Duration.ofSeconds(seconds, nanos.toLong)
  }
}

/** A time-based amount of time, such as '34.5 seconds'.
  *
  * This class models a quantity or amount of time in terms of seconds and nanoseconds.
  * It can be accessed using other duration-based units, such as minutes and hours.
  * In addition, the {@link ChronoUnit#DAYS DAYS} unit can be used and is treated as
  * exactly equal to 24 hours, thus ignoring daylight savings effects.
  * See {@link Period} for the date-based equivalent to this class.
  *
  * A physical duration could be of infinite length.
  * For practicality, the duration is stored with constraints similar to {@link Instant}.
  * The duration uses nanosecond resolution with a maximum value of the seconds that can
  * be held in a {@code long}. This is greater than the current estimated age of the universe.
  *
  * The range of a duration requires the storage of a number larger than a {@code long}.
  * To achieve this, the class stores a {@code long} representing seconds and an {@code int}
  * representing nanosecond-of-second, which will always be between 0 and 999,999,999.
  *
  * The duration is measured in "seconds", but these are not necessarily identical to
  * the scientific "SI second" definition based on atomic clocks.
  * This difference only impacts durations measured near a leap-second and should not affect
  * most applications.
  * See {@link Instant} for a discussion as to the meaning of the second and time-scales.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * Constructs an instance of {@code Duration} using seconds and nanoseconds.
  *
  * @param seconds  the length of the duration in seconds, positive or negative
  * @param nanos  the nanoseconds within the second, from 0 to 999,999,999
  */
@SerialVersionUID(3078945930695997490L)
final class Duration private (private val seconds: Long, private val nanos: Int)
    extends TemporalAmount
    with Ordered[Duration]
    with Serializable {

  def getUnits: java.util.List[TemporalUnit] =
    Collections.unmodifiableList[TemporalUnit](Arrays.asList(SECONDS, NANOS))

  def get(unit: TemporalUnit): Long =
    if (unit eq SECONDS) seconds
    else if (unit eq NANOS) nanos.toLong
    else throw UnsupportedTemporalTypeException.unit(unit)

  /** Checks if this duration is zero length.
    *
    * A {@code Duration} represents a directed distance between two points on
    * the time-line and can therefore be positive, zero or negative.
    * This method checks whether the length is zero.
    *
    * @return true if this duration has a total length equal to zero
    */
  def isZero: Boolean = (seconds | nanos) == 0

  /** Checks if this duration is negative, excluding zero.
    *
    * A {@code Duration} represents a directed distance between two points on
    * the time-line and can therefore be positive, zero or negative.
    * This method checks whether the length is less than zero.
    *
    * @return true if this duration has a total length less than zero
    */
  def isNegative: Boolean = seconds < 0

  /** Gets the number of seconds in this duration.
    *
    * The length of the duration is stored using two fields - seconds and nanoseconds.
    * The nanoseconds part is a value from 0 to 999,999,999 that is an adjustment to
    * the length in seconds.
    * The total duration is defined by calling this method and {@link #getNano()}.
    *
    * A {@code Duration} represents a directed distance between two points on the time-line.
    * A negative duration is expressed by the negative sign of the seconds part.
    * A duration of -1 nanosecond is stored as -1 seconds plus 999,999,999 nanoseconds.
    *
    * @return the whole seconds part of the length of the duration, positive or negative
    */
  def getSeconds: Long = seconds

  /** Gets the number of nanoseconds within the second in this duration.
    *
    * The length of the duration is stored using two fields - seconds and nanoseconds.
    * The nanoseconds part is a value from 0 to 999,999,999 that is an adjustment to
    * the length in seconds.
    * The total duration is defined by calling this method and {@link #getSeconds()}.
    *
    * A {@code Duration} represents a directed distance between two points on the time-line.
    * A negative duration is expressed by the negative sign of the seconds part.
    * A duration of -1 nanosecond is stored as -1 seconds plus 999,999,999 nanoseconds.
    *
    * @return the nanoseconds within the second part of the length of the duration, from 0 to 999,999,999
    */
  def getNano: Int = nanos

  /** Returns a copy of this duration with the specified amount of seconds.
    *
    * This returns a duration with the specified seconds, retaining the
    * nano-of-second part of this duration.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param seconds  the seconds to represent, may be negative
    * @return a { @code Duration} based on this period with the requested seconds, not null
    */
  def withSeconds(seconds: Long): Duration = Duration.create(seconds, nanos)

  /** Returns a copy of this duration with the specified nano-of-second.
    *
    * This returns a duration with the specified nano-of-second, retaining the
    * seconds part of this duration.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
    * @return a { @code Duration} based on this period with the requested nano-of-second, not null
    * @throws DateTimeException if the nano-of-second is invalid
    */
  def withNanos(nanoOfSecond: Int): Duration = {
    NANO_OF_SECOND.checkValidIntValue(nanoOfSecond.toLong)
    Duration.create(seconds, nanoOfSecond)
  }

  /** Returns a copy of this duration with the specified duration added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param duration  the duration to add, positive or negative, not null
    * @return a { @code Duration} based on this duration with the specified duration added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plus(duration: Duration): Duration = plus(duration.getSeconds, duration.getNano.toLong)

  /** Returns a copy of this duration with the specified duration added.
    *
    * The duration amount is measured in terms of the specified unit.
    * Only a subset of units are accepted by this method.
    * The unit must either have an {@link TemporalUnit#isDurationEstimated() exact duration} or
    * be {@link ChronoUnit#DAYS} which is treated as 24 hours. Other units throw an exception.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the amount of the period, measured in terms of the unit, positive or negative
    * @param unit  the unit that the period is measured in, must have an exact duration, not null
    * @return a { @code Duration} based on this duration with the specified duration added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): Duration = {
    Objects.requireNonNull(unit, "unit")
    if (unit eq DAYS)
      return plus(Math.multiplyExact(amountToAdd, SECONDS_PER_DAY.toLong), 0)
    if (unit.isDurationEstimated)
      throw new DateTimeException("Unit must not have an estimated duration")
    if (amountToAdd == 0)
      return this
    unit match {
      case u: ChronoUnit =>
        u match {
          case NANOS => plusNanos(amountToAdd)
          case MICROS =>
            plusSeconds((amountToAdd / (1000000L * 1000)) * 1000)
              .plusNanos((amountToAdd % (1000000L * 1000)) * 1000)
          case MILLIS  => plusMillis(amountToAdd)
          case SECONDS => plusSeconds(amountToAdd)
          case _       => plusSeconds(Math.multiplyExact(unit.getDuration.seconds, amountToAdd))
        }
      case _ =>
        val duration: Duration = unit.getDuration.multipliedBy(amountToAdd)
        plusSeconds(duration.getSeconds).plusNanos(duration.getNano.toLong)
    }
  }

  /** Returns a copy of this duration with the specified duration in 24 hour days added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToAdd  the days to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified days added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusDays(daysToAdd: Long): Duration =
    plus(Math.multiplyExact(daysToAdd, SECONDS_PER_DAY.toLong), 0)

  /** Returns a copy of this duration with the specified duration in hours added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hoursToAdd  the hours to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified hours added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusHours(hoursToAdd: Long): Duration =
    plus(Math.multiplyExact(hoursToAdd, SECONDS_PER_HOUR.toLong), 0)

  /** Returns a copy of this duration with the specified duration in minutes added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutesToAdd  the minutes to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified minutes added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusMinutes(minutesToAdd: Long): Duration =
    plus(Math.multiplyExact(minutesToAdd, SECONDS_PER_MINUTE.toLong), 0)

  /** Returns a copy of this duration with the specified duration in seconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToAdd  the seconds to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified seconds added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusSeconds(secondsToAdd: Long): Duration = plus(secondsToAdd, 0)

  /** Returns a copy of this duration with the specified duration in milliseconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param millisToAdd  the milliseconds to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified milliseconds added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusMillis(millisToAdd: Long): Duration =
    plus(millisToAdd / 1000, (millisToAdd % 1000) * Duration.NANOS_PER_MILLI)

  /** Returns a copy of this duration with the specified duration in nanoseconds added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToAdd  the nanoseconds to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified nanoseconds added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusNanos(nanosToAdd: Long): Duration = plus(0, nanosToAdd)

  /** Returns a copy of this duration with the specified duration added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToAdd  the seconds to add, positive or negative
    * @param nanosToAdd  the nanos to add, positive or negative
    * @return a { @code Duration} based on this duration with the specified seconds added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  private def plus(secondsToAdd: Long, nanosToAdd: Long): Duration = {
    var _nanosToAdd = nanosToAdd
    if ((secondsToAdd | _nanosToAdd) == 0)
      return this
    var epochSec: Long = Math.addExact(seconds, secondsToAdd)
    epochSec    = Math.addExact(epochSec, _nanosToAdd / Duration.NANOS_PER_SECOND)
    _nanosToAdd = _nanosToAdd % Duration.NANOS_PER_SECOND
    val nanoAdjustment: Long = nanos + _nanosToAdd
    Duration.ofSeconds(epochSec, nanoAdjustment)
  }

  /** Returns a copy of this duration with the specified duration subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param duration  the duration to subtract, positive or negative, not null
    * @return a { @code Duration} based on this duration with the specified duration subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minus(duration: Duration): Duration = {
    val secsToSubtract: Long = duration.getSeconds
    val nanosToSubtract: Int = duration.getNano
    if (secsToSubtract == Long.MinValue) plus(Long.MaxValue, -nanosToSubtract.toLong).plus(1, 0)
    else plus(-secsToSubtract, -nanosToSubtract.toLong)
  }

  /** Returns a copy of this duration with the specified duration subtracted.
    *
    * The duration amount is measured in terms of the specified unit.
    * Only a subset of units are accepted by this method.
    * The unit must either have an {@link TemporalUnit#isDurationEstimated() exact duration} or
    * be {@link ChronoUnit#DAYS} which is treated as 24 hours. Other units throw an exception.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the amount of the period, measured in terms of the unit, positive or negative
    * @param unit  the unit that the period is measured in, must have an exact duration, not null
    * @return a { @code Duration} based on this duration with the specified duration subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minus(amountToSubtract: Long, unit: TemporalUnit): Duration =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this duration with the specified duration in 24 hour days subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToSubtract  the days to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified days subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusDays(daysToSubtract: Long): Duration =
    if (daysToSubtract == Long.MinValue) plusDays(Long.MaxValue).plusDays(1)
    else plusDays(-daysToSubtract)

  /** Returns a copy of this duration with the specified duration in hours subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param hoursToSubtract  the hours to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified hours subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusHours(hoursToSubtract: Long): Duration =
    if (hoursToSubtract == Long.MinValue) plusHours(Long.MaxValue).plusHours(1)
    else plusHours(-hoursToSubtract)

  /** Returns a copy of this duration with the specified duration in minutes subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param minutesToSubtract  the minutes to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified minutes subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusMinutes(minutesToSubtract: Long): Duration =
    if (minutesToSubtract == Long.MinValue) plusMinutes(Long.MaxValue).plusMinutes(1)
    else plusMinutes(-minutesToSubtract)

  /** Returns a copy of this duration with the specified duration in seconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param secondsToSubtract  the seconds to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified seconds subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusSeconds(secondsToSubtract: Long): Duration =
    if (secondsToSubtract == Long.MinValue) plusSeconds(Long.MaxValue).plusSeconds(1)
    else plusSeconds(-secondsToSubtract)

  /** Returns a copy of this duration with the specified duration in milliseconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param millisToSubtract  the milliseconds to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified milliseconds subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusMillis(millisToSubtract: Long): Duration =
    if (millisToSubtract == Long.MinValue) plusMillis(Long.MaxValue).plusMillis(1)
    else plusMillis(-millisToSubtract)

  /** Returns a copy of this duration with the specified duration in nanoseconds subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param nanosToSubtract  the nanoseconds to subtract, positive or negative
    * @return a { @code Duration} based on this duration with the specified nanoseconds subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusNanos(nanosToSubtract: Long): Duration =
    if (nanosToSubtract == Long.MinValue) plusNanos(Long.MaxValue).plusNanos(1)
    else plusNanos(-nanosToSubtract)

  /** Returns a copy of this duration multiplied by the scalar.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param multiplicand  the value to multiply the duration by, positive or negative
    * @return a { @code Duration} based on this duration multiplied by the specified scalar, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def multipliedBy(multiplicand: Long): Duration =
    if (multiplicand == 0) Duration.ZERO
    else if (multiplicand == 1) this
    else Duration.create(toSeconds.multiply(BigDecimal.valueOf(multiplicand)))

  /** Returns a copy of this duration divided by the specified value.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param divisor  the value to divide the duration by, positive or negative, not zero
    * @return a { @code Duration} based on this duration divided by the specified divisor, not null
    * @throws ArithmeticException if the divisor is zero
    * @throws ArithmeticException if numeric overflow occurs
    */
  def dividedBy(divisor: Long): Duration =
    if (divisor == 0) throw new ArithmeticException("Cannot divide by zero")
    else if (divisor == 1) this
    else Duration.create(toSeconds.divide(BigDecimal.valueOf(divisor), RoundingMode.DOWN))

  /** Converts this duration to the total length in seconds and
    * fractional nanoseconds expressed as a {@code BigDecimal}.
    *
    * @return the total length of the duration in seconds, with a scale of 9, not null
    */
  private def toSeconds: BigDecimal =
    BigDecimal.valueOf(seconds).add(BigDecimal.valueOf(nanos.toLong, 9))

  /** Returns a copy of this duration with the length negated.
    *
    * This method swaps the sign of the total length of this duration.
    * For example, {@code PT1.3S} will be returned as {@code PT-1.3S}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return a { @code Duration} based on this duration with the amount negated, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def negated: Duration = multipliedBy(-1)

  /** Returns a copy of this duration with a positive length.
    *
    * This method returns a positive duration by effectively removing the sign from any negative total length.
    * For example, {@code PT-1.3S} will be returned as {@code PT1.3S}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return a { @code Duration} based on this duration with an absolute length, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def abs: Duration = if (isNegative) negated else this

  /** Adds this duration to the specified temporal object.
    *
    * This returns a temporal object of the same observable type as the input
    * with this duration added.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#plus(TemporalAmount)}.
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * dateTime = thisDuration.addTo(dateTime);
    * dateTime = dateTime.plus(thisDuration);
    * </pre>
    *
    * The calculation will add the seconds, then nanos.
    * Only non-zero amounts will be added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the temporal object to adjust, not null
    * @return an object of the same type with the adjustment made, not null
    * @throws DateTimeException if unable to add
    * @throws ArithmeticException if numeric overflow occurs
    */
  def addTo(temporal: Temporal): Temporal = {
    var _temporal = temporal
    if (seconds != 0)
      _temporal = _temporal.plus(seconds, SECONDS)
    if (nanos != 0)
      _temporal = _temporal.plus(nanos.toLong, NANOS)
    _temporal
  }

  /** Subtracts this duration from the specified temporal object.
    *
    * This returns a temporal object of the same observable type as the input
    * with this duration subtracted.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#minus(TemporalAmount)}.
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * dateTime = thisDuration.subtractFrom(dateTime);
    * dateTime = dateTime.minus(thisDuration);
    * </pre>
    *
    * The calculation will subtract the seconds, then nanos.
    * Only non-zero amounts will be added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the temporal object to adjust, not null
    * @return an object of the same type with the adjustment made, not null
    * @throws DateTimeException if unable to subtract
    * @throws ArithmeticException if numeric overflow occurs
    */
  def subtractFrom(temporal: Temporal): Temporal = {
    var _temporal = temporal
    if (seconds != 0)
      _temporal = _temporal.minus(seconds, SECONDS)
    if (nanos != 0)
      _temporal = _temporal.minus(nanos.toLong, NANOS)
    _temporal
  }

  /** Gets the number of days in this duration.
    *
    * This returns the total number of days in the duration by dividing the
    * number of seconds by 86400.
    * This is based on the standard definition of a day as 24 hours.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return the number of days in the duration, may be negative
    */
  def toDays: Long = seconds / SECONDS_PER_DAY

  /** Gets the number of hours in this duration.
    *
    * This returns the total number of hours in the duration by dividing the
    * number of seconds by 3600.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return the number of hours in the duration, may be negative
    */
  def toHours: Long = seconds / SECONDS_PER_HOUR

  /** Gets the number of minutes in this duration.
    *
    * This returns the total number of minutes in the duration by dividing the
    * number of seconds by 60.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return the number of minutes in the duration, may be negative
    */
  def toMinutes: Long = seconds / SECONDS_PER_MINUTE

  /** Converts this duration to the total length in milliseconds.
    *
    * If this duration is too large to fit in a {@code long} milliseconds, then an
    * exception is thrown.
    *
    * If this duration has greater than millisecond precision, then the conversion
    * will drop any excess precision information as though the amount in nanoseconds
    * was subject to integer division by one million.
    *
    * @return the total length of the duration in milliseconds
    * @throws ArithmeticException if numeric overflow occurs
    */
  def toMillis: Long = {
    val result: Long = Math.multiplyExact(seconds, 1000)
    Math.addExact(result, nanos.toLong / Duration.NANOS_PER_MILLI)
  }

  /** Converts this duration to the total length in nanoseconds expressed as a {@code long}.
    *
    * If this duration is too large to fit in a {@code long} nanoseconds, then an
    * exception is thrown.
    *
    * @return the total length of the duration in nanoseconds
    * @throws ArithmeticException if numeric overflow occurs
    */
  def toNanos: Long = {
    val result: Long = Math.multiplyExact(seconds, Duration.NANOS_PER_SECOND.toLong)
    Math.addExact(result, nanos.toLong)
  }

  /** Compares this duration to the specified {@code Duration}.
    *
    * The comparison is based on the total length of the durations.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param otherDuration  the other duration to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(otherDuration: Duration): Int = {
    val cmp: Int = java.lang.Long.compare(seconds, otherDuration.seconds)
    if (cmp != 0) cmp
    else nanos - otherDuration.nanos
  }

  override def compareTo(other: Duration): Int = compare(other)

  /** Checks if this duration is equal to the specified {@code Duration}.
    *
    * The comparison is based on the total length of the durations.
    *
    * @param other  the other duration, null returns false
    * @return true if the other duration is equal to this one
    */
  override def equals(other: Any): Boolean =
    other match {
      case otherDuration: Duration =>
        (this eq otherDuration) || (this.seconds == otherDuration.seconds && this.nanos == otherDuration.nanos)
      case _ => false
    }

  /** A hash code for this duration.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = (seconds ^ (seconds >>> 32)).toInt + (51 * nanos)

  /** A string representation of this duration using ISO-8601 seconds
    * based representation, such as {@code PT8H6M12.345S}.
    *
    * The format of the returned string will be {@code PTnHnMnS}, where n is
    * the relevant hours, minutes or seconds part of the duration.
    * Any fractional seconds are placed after a decimal point i the seconds section.
    * If a section has a zero value, it is omitted.
    * The hours, minutes and seconds will all have the same sign.
    *
    * Examples:
    * <pre>
    * "20.345 seconds"                 -> "PT20.345S
    * "15 minutes" (15 * 60 seconds)   -> "PT15M"
    * "10 hours" (10 * 3600 seconds)   -> "PT10H"
    * "2 days" (2 * 86400 seconds)     -> "PT48H"
    * </pre>
    * Note that multiples of 24 hours are not output as days to avoid confusion
    * with {@code Period}.
    *
    * @return an ISO-8601 representation of this duration, not null
    */
  override def toString: String = {
    if (this eq Duration.ZERO)
      return "PT0S"
    val hours: Long        = seconds / SECONDS_PER_HOUR
    val minutes: Int       = ((seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE).asInstanceOf[Int]
    val secs: Int          = (seconds % SECONDS_PER_MINUTE).asInstanceOf[Int]
    val buf: StringBuilder = new StringBuilder(24)
    buf.append("PT")
    if (hours != 0)
      buf.append(hours).append('H')
    if (minutes != 0)
      buf.append(minutes).append('M')
    if (secs == 0 && nanos == 0 && buf.length > 2)
      return buf.toString
    if (secs < 0 && nanos > 0) {
      if (secs == -1) buf.append("-0")
      else buf.append(secs + 1)
    } else {
      buf.append(secs)
    }
    if (nanos > 0) {
      val pos: Int = buf.length
      if (secs < 0) buf.append(2 * Duration.NANOS_PER_SECOND - nanos)
      else buf.append(nanos + Duration.NANOS_PER_SECOND)
      while (buf.charAt(buf.length - 1) == '0')
        buf.setLength(buf.length - 1)
      buf.setCharAt(pos, '.')
    }
    buf.append('S')
    buf.toString
  }

  private def writeReplace: AnyRef = new Ser(Ser.DURATION_TYPE, this)

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
    out.writeLong(seconds)
    out.writeInt(nanos)
  }
}
