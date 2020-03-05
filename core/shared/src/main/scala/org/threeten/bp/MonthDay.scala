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

import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(-939150713474957432L)
object MonthDay {

  /** Parser. */
  private def PARSER: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendLiteral("--")
      .appendValue(MONTH_OF_YEAR, 2)
      .appendLiteral('-')
      .appendValue(DAY_OF_MONTH, 2)
      .toFormatter

  /** Obtains the current month-day from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current month-day.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current month-day using the system clock and default time-zone, not null
    */
  def now: MonthDay = now(Clock.systemDefaultZone)

  /** Obtains the current month-day from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current month-day.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current month-day using the system clock, not null
    */
  def now(zone: ZoneId): MonthDay = now(Clock.system(zone))

  /** Obtains the current month-day from the specified clock.
    *
    * This will query the specified clock to obtain the current month-day.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current month-day, not null
    */
  def now(clock: Clock): MonthDay = {
    val now: LocalDate = LocalDate.now(clock)
    MonthDay.of(now.getMonth, now.getDayOfMonth)
  }

  /** Obtains an instance of {@code MonthDay}.
    *
    * The day-of-month must be valid for the month within a leap year.
    * Hence, for February, day 29 is valid.
    *
    * For example, passing in April and day 31 will throw an exception, as
    * there can never be April 31st in any year. By contrast, passing in
    * February 29th is permitted, as that month-day can sometimes be valid.
    *
    * @param month  the month-of-year to represent, not null
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @return the month-day, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month
    */
  def of(month: Month, dayOfMonth: Int): MonthDay = {
    Objects.requireNonNull(month, "month")
    DAY_OF_MONTH.checkValidValue(dayOfMonth.toLong)
    if (dayOfMonth > month.maxLength) {
      throw new DateTimeException(
        s"Illegal value for DayOfMonth field, value $dayOfMonth is not valid for month ${month.name}"
      )
    }
    new MonthDay(month.getValue, dayOfMonth)
  }

  /** Obtains an instance of {@code MonthDay}.
    *
    * The day-of-month must be valid for the month within a leap year.
    * Hence, for month 2 (February), day 29 is valid.
    *
    * For example, passing in month 4 (April) and day 31 will throw an exception, as
    * there can never be April 31st in any year. By contrast, passing in
    * February 29th is permitted, as that month-day can sometimes be valid.
    *
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @param dayOfMonth  the day-of-month to represent, from 1 to 31
    * @return the month-day, not null
    * @throws DateTimeException if the value of any field is out of range
    * @throws DateTimeException if the day-of-month is invalid for the month
    */
  def of(month: Int, dayOfMonth: Int): MonthDay = of(Month.of(month), dayOfMonth)

  /** Obtains an instance of {@code MonthDay} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code MonthDay}.
    *
    * The conversion extracts the {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} and
    * {@link ChronoField#DAY_OF_MONTH DAY_OF_MONTH} fields.
    * The extraction is only permitted if the date-time has an ISO chronology.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code MonthDay::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the month-day, not null
    * @throws DateTimeException if unable to convert to a { @code MonthDay}
    */
  def from(temporal: TemporalAccessor): MonthDay = {
    var _temporal = temporal
    _temporal match {
      case day: MonthDay =>
        day
      case _ =>
        try {
          if (IsoChronology.INSTANCE != Chronology.from(_temporal)) {
            _temporal = LocalDate.from(_temporal)
          }
          of(_temporal.get(MONTH_OF_YEAR), _temporal.get(DAY_OF_MONTH))
        } catch {
          case _: DateTimeException =>
            throw new DateTimeException(
              s"Unable to obtain MonthDay from TemporalAccessor: ${_temporal}, type ${_temporal.getClass.getName}"
            )
        }
    }
  }

  /** Obtains an instance of {@code MonthDay} from a text string such as {@code --12-03}.
    *
    * The string must represent a valid month-day.
    * The format is {@code --MM-dd}.
    *
    * @param text  the text to parse such as "--12-03", not null
    * @return the parsed month-day, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): MonthDay = parse(text, PARSER)

  /** Obtains an instance of {@code MonthDay} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a month-day.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed month-day, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): MonthDay = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[MonthDay] {
      override def queryFrom(temporal: TemporalAccessor): MonthDay = MonthDay.from(temporal)
    })
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): MonthDay = {
    val month: Byte = in.readByte
    val day: Byte   = in.readByte
    MonthDay.of(month.toInt, day.toInt)
  }
}

/** A month-day in the ISO-8601 calendar system, such as {@code --12-03}.
  *
  * {@code MonthDay} is an immutable date-time object that represents the combination
  * of a year and month. Any field that can be derived from a month and day, such as
  * quarter-of-year, can be obtained.
  *
  * This class does not store or represent a year, time or time-zone.
  * For example, the value "December 3rd" can be stored in a {@code MonthDay}.
  *
  * Since a {@code MonthDay} does not possess a year, the leap day of
  * February 29th is considered valid.
  *
  * This class implements {@link TemporalAccessor} rather than {@link Temporal}.
  * This is because it is not possible to define whether February 29th is valid or not
  * without external information, preventing the implementation of plus/minus.
  * Related to this, {@code MonthDay} only provides access to query and set the fields
  * {@code MONTH_OF_YEAR} and {@code DAY_OF_MONTH}.
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
  *
  * @param month  the month-of-year to represent, validated from 1 to 12
  * @param day  the day-of-month to represent, validated from 1 to 29-31
  */
@SerialVersionUID(-939150713474957432L)
final class MonthDay private (private val month: Int, private val day: Int)
    extends TemporalAccessor
    with TemporalAdjuster
    with Ordered[MonthDay]
    with Serializable {

  /** Checks if the specified field is supported.
    *
    * This checks if this month-day can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time.
    * The supported fields are:
    * <ul>
    * <li>{@code MONTH_OF_YEAR}
    * <li>{@code YEAR}
    * </ul>
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this month-day, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      (field eq MONTH_OF_YEAR) || (field eq DAY_OF_MONTH)
    else
      field != null && field.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This month-day is used to enhance the accuracy of the returned range.
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
    if (field eq MONTH_OF_YEAR)
      field.range
    else if (field eq DAY_OF_MONTH)
      ValueRange.of(1, getMonth.minLength.toLong, getMonth.maxLength.toLong)
    else if (field.isInstanceOf[ChronoField])
      if (isSupported(field)) field.range
      else throw UnsupportedTemporalTypeException.field(field)
    else
      field.rangeRefinedBy(this)

  /** Gets the value of the specified field from this month-day as an {@code int}.
    *
    * This queries this month-day for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this month-day.
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
    range(field).checkValidIntValue(getLong(field), field)

  /** Gets the value of the specified field from this month-day as a {@code long}.
    *
    * This queries this month-day for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this month-day.
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
      case field1: ChronoField =>
        field1 match {
          case DAY_OF_MONTH  => day.toLong
          case MONTH_OF_YEAR => month.toLong
          case _             => throw UnsupportedTemporalTypeException.field(field)
        }
      case _ => field.getFrom(this)
    }

  /** Gets the month-of-year field from 1 to 12.
    *
    * This method returns the month as an {@code int} from 1 to 12.
    * Application code is frequently clearer if the enum {@link Month}
    * is used by calling {@link #getMonth()}.
    *
    * @return the month-of-year, from 1 to 12
    * @see #getMonth()
    */
  def getMonthValue: Int = month

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
  def getMonth: Month = Month.of(month)

  /** Gets the day-of-month field.
    *
    * This method returns the primitive {@code int} value for the day-of-month.
    *
    * @return the day-of-month, from 1 to 31
    */
  def getDayOfMonth: Int = day

  /** Checks if the year is valid for this month-day.
    *
    * This method checks whether this month and day and the input year form
    * a valid date. This can only return false for February 29th.
    *
    * @param year  the year to validate, an out of range value returns false
    * @return true if the year is valid for this month-day
    * @see Year#isValidMonthDay(MonthDay)
    */
  def isValidYear(year: Int): Boolean = !(day == 29 && month == 2 && !Year.isLeap(year.toLong))

  /** Returns a copy of this {@code MonthDay} with the month-of-year altered.
    *
    * This returns a month-day with the specified month.
    * If the day-of-month is invalid for the specified month, the day will
    * be adjusted to the last valid day-of-month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param month  the month-of-year to set in the returned month-day, from 1 (January) to 12 (December)
    * @return a { @code MonthDay} based on this month-day with the requested month, not null
    * @throws DateTimeException if the month-of-year value is invalid
    */
  def withMonth(month: Int): MonthDay = `with`(Month.of(month))

  /** Returns a copy of this {@code MonthDay} with the month-of-year altered.
    *
    * This returns a month-day with the specified month.
    * If the day-of-month is invalid for the specified month, the day will
    * be adjusted to the last valid day-of-month.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param month  the month-of-year to set in the returned month-day, not null
    * @return a { @code MonthDay} based on this month-day with the requested month, not null
    */
  def `with`(month: Month): MonthDay = {
    Objects.requireNonNull(month, "month")
    if (month.getValue == this.month) {
      return this
    }
    val day: Int = Math.min(this.day, month.maxLength)
    new MonthDay(month.getValue, day)
  }

  /** Returns a copy of this {@code MonthDay} with the day-of-month altered.
    *
    * This returns a month-day with the specified day-of-month.
    * If the day-of-month is invalid for the month, an exception is thrown.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param dayOfMonth  the day-of-month to set in the return month-day, from 1 to 31
    * @return a { @code MonthDay} based on this month-day with the requested day, not null
    * @throws DateTimeException if the day-of-month value is invalid
    * @throws DateTimeException if the day-of-month is invalid for the month
    */
  def withDayOfMonth(dayOfMonth: Int): MonthDay =
    if (dayOfMonth == this.day) this
    else MonthDay.of(month, dayOfMonth)

  /** Queries this month-day using the specified query.
    *
    * This queries this month-day using the specified query strategy object.
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
    if (query eq TemporalQueries.chronology)
      IsoChronology.INSTANCE.asInstanceOf[R]
    else if ((query eq TemporalQueries.zoneId) || (query eq TemporalQueries.precision))
      null.asInstanceOf[R]
    else
      query.queryFrom(this)

  /** Adjusts the specified temporal object to have this month-day.
    *
    * This returns a temporal object of the same observable type as the input
    * with the month and day-of-month changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * twice, passing {@link ChronoField#MONTH_OF_YEAR} and
    * {@link ChronoField#DAY_OF_MONTH} as the fields.
    * If the specified temporal object does not use the ISO calendar system then
    * a {@code DateTimeException} is thrown.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisMonthDay.adjustInto(temporal);
    * temporal = temporal.with(thisMonthDay);
    * </pre>
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the target object to be adjusted, not null
    * @return the adjusted object, not null
    * @throws DateTimeException if unable to make the adjustment
    * @throws ArithmeticException if numeric overflow occurs
    */
  def adjustInto(temporal: Temporal): Temporal = {
    var _temporal = temporal
    if (!(Chronology.from(_temporal) == IsoChronology.INSTANCE)) {
      throw new DateTimeException("Adjustment only supported on ISO date-time")
    }
    _temporal = _temporal.`with`(MONTH_OF_YEAR, month.toLong)
    _temporal.`with`(DAY_OF_MONTH, Math.min(_temporal.range(DAY_OF_MONTH).getMaximum, day.toLong))
  }

  /** Combines this month-day with a year to create a {@code LocalDate}.
    *
    * This returns a {@code LocalDate} formed from this month-day and the specified year.
    *
    * A month-day of February 29th will be adjusted to February 28th in the resulting
    * date if the year is not a leap year.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param year  the year to use, from MIN_YEAR to MAX_YEAR
    * @return the local date formed from this month-day and the specified year, not null
    * @throws DateTimeException if the year is outside the valid range of years
    */
  def atYear(year: Int): LocalDate = LocalDate.of(year, month, if (isValidYear(year)) day else 28)

  /** Compares this month-day to another month-day.
    *
    * The comparison is based first on value of the month, then on the value of the day.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param other  the other month-day to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(other: MonthDay): Int = {
    var cmp: Int = month - other.month
    if (cmp == 0)
      cmp = day - other.day
    cmp
  }

  override def compareTo(other: MonthDay): Int = compare(other)

  /** Is this month-day after the specified month-day.
    *
    * @param other  the other month-day to compare to, not null
    * @return true if this is after the specified month-day
    */
  def isAfter(other: MonthDay): Boolean = compareTo(other) > 0

  /** Is this month-day before the specified month-day.
    *
    * @param other  the other month-day to compare to, not null
    * @return true if this point is before the specified month-day
    */
  def isBefore(other: MonthDay): Boolean = compareTo(other) < 0

  /** Checks if this month-day is equal to another month-day.
    *
    * The comparison is based on the time-line position of the month-day within a year.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other month-day
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: MonthDay => (this eq other) || (month == other.month && day == other.day)
      case _               => false
    }

  /** A hash code for this month-day.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = (month << 6) + day

  /** Outputs this month-day as a {@code String}, such as {@code --12-03}.
    *
    * The output will be in the format {@code --MM-dd}:
    *
    * @return a string representation of this month-day, not null
    */
  override def toString: String =
    new StringBuilder(10)
      .append("--")
      .append(if (month < 10) "0" else "")
      .append(month)
      .append(if (day < 10) "-0" else "-")
      .append(day)
      .toString

  /** Outputs this month-day as a {@code String} using the formatter.
    *
    * This month-day will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted month-day string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  private def writeReplace: AnyRef = new Ser(Ser.MONTH_DAY_TYPE, this)

  /** Defend against malicious streams.
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit = {
    out.writeByte(month)
    out.writeByte(day)
  }
}
