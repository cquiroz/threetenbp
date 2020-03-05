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
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit.CENTURIES
import org.threeten.bp.temporal.ChronoUnit.DECADES
import org.threeten.bp.temporal.ChronoUnit.ERAS
import org.threeten.bp.temporal.ChronoUnit.MILLENNIA
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.YEARS
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
import org.threeten.bp.format.SignStyle
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

/** A year-month in the ISO-8601 calendar system, such as {@code 2007-12}.
  *
  * {@code YearMonth} is an immutable date-time object that represents the combination
  * of a year and month. Any field that can be derived from a year and month, such as
  * quarter-of-year, can be obtained.
  *
  * This class does not store or represent a day, time or time-zone.
  * For example, the value "October 2007" can be stored in a {@code YearMonth}.
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
  */
@SerialVersionUID(4183400860270640070L)
object YearMonth {

  /** Parser. */
  private lazy val PARSER: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
    .appendLiteral('-')
    .appendValue(MONTH_OF_YEAR, 2)
    .toFormatter

  /** Obtains the current year-month from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current year-month.
    * The zone and offset will be set based on the time-zone in the clock.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current year-month using the system clock and default time-zone, not null
    */
  def now: YearMonth = now(Clock.systemDefaultZone)

  /** Obtains the current year-month from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current year-month.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current year-month using the system clock, not null
    */
  def now(zone: ZoneId): YearMonth = now(Clock.system(zone))

  /** Obtains the current year-month from the specified clock.
    *
    * This will query the specified clock to obtain the current year-month.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current year-month, not null
    */
  def now(clock: Clock): YearMonth = {
    val now: LocalDate = LocalDate.now(clock)
    YearMonth.of(now.getYear, now.getMonth)
  }

  /** Obtains an instance of {@code YearMonth} from a year and month.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, not null
    * @return the year-month, not null
    * @throws DateTimeException if the year value is invalid
    */
  def of(year: Int, month: Month): YearMonth = {
    Objects.requireNonNull(month, "month")
    of(year, month.getValue)
  }

  /** Obtains an instance of {@code YearMonth} from a year and month.
    *
    * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @return the year-month, not null
    * @throws DateTimeException if either field value is invalid
    */
  def of(year: Int, month: Int): YearMonth = {
    YEAR.checkValidValue(year.toLong)
    MONTH_OF_YEAR.checkValidValue(month.toLong)
    new YearMonth(year, month)
  }

  /** Obtains an instance of {@code YearMonth} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code YearMonth}.
    *
    * The conversion extracts the {@link ChronoField#YEAR YEAR} and
    * {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} fields.
    * The extraction is only permitted if the temporal object has an ISO
    * chronology, or can be converted to a {@code LocalDate}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code YearMonth::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the year-month, not null
    * @throws DateTimeException if unable to convert to a { @code YearMonth}
    */
  def from(temporal: TemporalAccessor): YearMonth = {
    var _temporal = temporal
    if (_temporal.isInstanceOf[YearMonth])
      return _temporal.asInstanceOf[YearMonth]
    try {
      if (IsoChronology.INSTANCE != Chronology.from(_temporal))
        _temporal = LocalDate.from(_temporal)
      of(_temporal.get(YEAR), _temporal.get(MONTH_OF_YEAR))
    } catch {
      case _: DateTimeException =>
        throw new DateTimeException(
          s"Unable to obtain YearMonth from TemporalAccessor: ${_temporal}, type ${_temporal.getClass.getName}"
        )
    }
  }

  /** Obtains an instance of {@code YearMonth} from a text string such as {@code 2007-12}.
    *
    * The string must represent a valid year-month.
    * The format must be {@code yyyy-MM}.
    * Years outside the range 0000 to 9999 must be prefixed by the plus or minus symbol.
    *
    * @param text  the text to parse such as "2007-12", not null
    * @return the parsed year-month, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): YearMonth = parse(text, PARSER)

  /** Obtains an instance of {@code YearMonth} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a year-month.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed year-month, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): YearMonth = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[YearMonth] {
      override def queryFrom(temporal: TemporalAccessor): YearMonth = YearMonth.from(temporal)
    })
  }

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): YearMonth = {
    val year: Int   = in.readInt
    val month: Byte = in.readByte
    YearMonth.of(year, month.toInt)
  }
}

/** @constructor
  * @param year  the year to represent, validated from MIN_YEAR to MAX_YEAR
  * @param month  the month-of-year to represent, validated from 1 (January) to 12 (December)
  */
@SerialVersionUID(4183400860270640070L)
final class YearMonth private (private val year: Int, private val month: Int)
    extends TemporalAccessor
    with Temporal
    with TemporalAdjuster
    with Ordered[YearMonth]
    with Serializable {

  /** Returns a copy of this year-month with the new year and month, checking
    * to see if a new object is in fact required.
    *
    * @param newYear  the year to represent, validated from MIN_YEAR to MAX_YEAR
    * @param newMonth  the month-of-year to represent, validated not null
    * @return the year-month, not null
    */
  private def `with`(newYear: Int, newMonth: Int): YearMonth =
    if (year == newYear && month == newMonth)
      this
    else
      new YearMonth(newYear, newMonth)

  /** Checks if the specified field is supported.
    *
    * This checks if this year-month can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time.
    * The supported fields are:
    * <ul>
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
    * @return true if the field is supported on this year-month, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      (field eq YEAR) || (field eq MONTH_OF_YEAR) || (field eq PROLEPTIC_MONTH) || (field eq YEAR_OF_ERA) || (field eq ERA)
    else
      field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit])
      (unit eq MONTHS) || (unit eq YEARS) || (unit eq DECADES) || (unit eq CENTURIES) || (unit eq MILLENNIA) || (unit eq ERAS)
    else
      unit != null && unit.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This year-month is used to enhance the accuracy of the returned range.
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
    if (field eq YEAR_OF_ERA)
      if (getYear <= 0) ValueRange.of(1, Year.MAX_VALUE.toLong + 1L)
      else ValueRange.of(1, Year.MAX_VALUE.toLong)
    else if (field.isInstanceOf[ChronoField])
      if (isSupported(field)) field.range
      else throw UnsupportedTemporalTypeException.field(field)
    else
      field.rangeRefinedBy(this)

  /** Gets the value of the specified field from this year-month as an {@code int}.
    *
    * This queries this year-month for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this year-month, except {@code EPOCH_MONTH} which is too
    * large to fit in an {@code int} and throw a {@code DateTimeException}.
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

  /** Gets the value of the specified field from this year-month as a {@code long}.
    *
    * This queries this year-month for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this year-month.
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
      case MONTH_OF_YEAR   => month.toLong
      case PROLEPTIC_MONTH => getProlepticMonth
      case YEAR_OF_ERA     => if (year < 1) 1L - year.toLong else year.toLong
      case YEAR            => year.toLong
      case ERA             => if (year < 1) 0 else 1
      case _: ChronoField =>
        throw UnsupportedTemporalTypeException.field(field)
      case _ => field.getFrom(this)
    }

  private def getProlepticMonth: Long = (year * 12L) + (month - 1)

  /** Gets the year field.
    *
    * This method returns the primitive {@code int} value for the year.
    *
    * The year returned by this method is proleptic as per {@code get(YEAR)}.
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
  def getMonthValue: Int = month

  /** Gets the month-of-year field using the {@code Month} enum.
    *
    * This method returns the enum {@link Month} for the month.
    * This avoids confusion as to what {@code int} values mean.
    * If you need access to the primitive {@code int} value then the enum
    * provides the {@link Month#getValue() int value}.
    *
    * @return the month-of-year, not null
    */
  def getMonth: Month = Month.of(month)

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
  def isLeapYear: Boolean = IsoChronology.INSTANCE.isLeapYear(year.toLong)

  /** Checks if the day-of-month is valid for this year-month.
    *
    * This method checks whether this year and month and the input day form
    * a valid date.
    *
    * @param dayOfMonth  the day-of-month to validate, from 1 to 31, invalid value returns false
    * @return true if the day is valid for this year-month
    */
  def isValidDay(dayOfMonth: Int): Boolean = dayOfMonth >= 1 && dayOfMonth <= lengthOfMonth

  /** Returns the length of the month, taking account of the year.
    *
    * This returns the length of the month in days.
    * For example, a date in January would return 31.
    *
    * @return the length of the month in days, from 28 to 31
    */
  def lengthOfMonth: Int = getMonth.length(isLeapYear)

  /** Returns the length of the year.
    *
    * This returns the length of the year in days, either 365 or 366.
    *
    * @return 366 if the year is leap, 365 otherwise
    */
  def lengthOfYear: Int = if (isLeapYear) 366 else 365

  /** Returns an adjusted copy of this year-month.
    *
    * This returns a new {@code YearMonth}, based on this one, with the year-month adjusted.
    * The adjustment takes place using the specified adjuster strategy object.
    * Read the documentation of the adjuster to understand what adjustment will be made.
    *
    * A simple adjuster might simply set the one of the fields, such as the year field.
    * A more complex adjuster might set the year-month to the next month that
    * Halley's comet will pass the Earth.
    *
    * The result of this method is obtained by invoking the
    * {@link TemporalAdjuster#adjustInto(Temporal)} method on the
    * specified adjuster passing {@code this} as the argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param adjuster the adjuster to use, not null
    * @return a { @code YearMonth} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): YearMonth =
    adjuster.adjustInto(this).asInstanceOf[YearMonth]

  /** Returns a copy of this year-month with the specified field set to a new value.
    *
    * This returns a new {@code YearMonth}, based on this one, with the value
    * for the specified field changed.
    * This can be used to change any supported field, such as the year or month.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The supported fields behave as follows:
    * <ul>
    * <li>{@code MONTH_OF_YEAR} -
    * Returns a {@code YearMonth} with the specified month-of-year.
    * The year will be unchanged.
    * <li>{@code PROLEPTIC_MONTH} -
    * Returns a {@code YearMonth} with the specified proleptic-month.
    * This completely replaces the year and month of this object.
    * <li>{@code YEAR_OF_ERA} -
    * Returns a {@code YearMonth} with the specified year-of-era
    * The month and era will be unchanged.
    * <li>{@code YEAR} -
    * Returns a {@code YearMonth} with the specified year.
    * The month will be unchanged.
    * <li>{@code ERA} -
    * Returns a {@code YearMonth} with the specified era.
    * The month and year-of-era will be unchanged.
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
    * @return a { @code YearMonth} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): YearMonth = {
    if (field.isInstanceOf[ChronoField]) {
      val f: ChronoField = field.asInstanceOf[ChronoField]
      f.checkValidValue(newValue)
      f match {
        case MONTH_OF_YEAR =>
          return withMonth(newValue.toInt)
        case PROLEPTIC_MONTH =>
          return plusMonths(newValue - getLong(PROLEPTIC_MONTH))
        case YEAR_OF_ERA =>
          return withYear((if (year < 1) 1 - newValue else newValue).toInt)
        case YEAR =>
          return withYear(newValue.toInt)
        case ERA =>
          return if (getLong(ERA) == newValue) this else withYear(1 - year)
        case _ =>
          throw UnsupportedTemporalTypeException.field(field)
      }
    }
    field.adjustInto(this, newValue)
  }

  /** Returns a copy of this {@code YearMonth} with the year altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param year  the year to set in the returned year-month, from MIN_YEAR to MAX_YEAR
    * @return a { @code YearMonth} based on this year-month with the requested year, not null
    * @throws DateTimeException if the year value is invalid
    */
  def withYear(year: Int): YearMonth = {
    YEAR.checkValidValue(year.toLong)
    `with`(year, month)
  }

  /** Returns a copy of this {@code YearMonth} with the month-of-year altered.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param month  the month-of-year to set in the returned year-month, from 1 (January) to 12 (December)
    * @return a { @code YearMonth} based on this year-month with the requested month, not null
    * @throws DateTimeException if the month-of-year value is invalid
    */
  def withMonth(month: Int): YearMonth = {
    MONTH_OF_YEAR.checkValidValue(month.toLong)
    `with`(year, month)
  }

  /** Returns a copy of this year-month with the specified period added.
    *
    * This method returns a new year-month based on this year-month with the specified period added.
    * The adder is typically {@link org.threeten.bp.Period Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return a { @code YearMonth} based on this year-month with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): YearMonth = amount.addTo(this).asInstanceOf[YearMonth]

  /** {@inheritDoc}
    *
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): YearMonth =
    if (unit.isInstanceOf[ChronoUnit]) {
      unit.asInstanceOf[ChronoUnit] match {
        case MONTHS    => plusMonths(amountToAdd)
        case YEARS     => plusYears(amountToAdd)
        case DECADES   => plusYears(Math.multiplyExact(amountToAdd, 10))
        case CENTURIES => plusYears(Math.multiplyExact(amountToAdd, 100))
        case MILLENNIA => plusYears(Math.multiplyExact(amountToAdd, 1000))
        case ERAS      => `with`(ERA, Math.addExact(getLong(ERA), amountToAdd))
        case _         => throw UnsupportedTemporalTypeException.unit(unit)
      }
    } else {
      unit.addTo(this, amountToAdd)
    }

  /** Returns a copy of this year-month with the specified period in years added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToAdd  the years to add, may be negative
    * @return a { @code YearMonth} based on this year-month with the years added, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def plusYears(yearsToAdd: Long): YearMonth =
    if (yearsToAdd == 0)
      this
    else {
      val newYear: Int = YEAR.checkValidIntValue(year + yearsToAdd)
      `with`(newYear, month)
    }

  /** Returns a copy of this year-month with the specified period in months added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToAdd  the months to add, may be negative
    * @return a { @code YearMonth} based on this year-month with the months added, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def plusMonths(monthsToAdd: Long): YearMonth = {
    if (monthsToAdd == 0)
      return this
    val monthCount: Long = year * 12L + (month - 1)
    val calcMonths: Long = monthCount + monthsToAdd
    val newYear: Int     = YEAR.checkValidIntValue(Math.floorDiv(calcMonths, 12))
    val newMonth: Int    = Math.floorMod(calcMonths, 12).toInt + 1
    `with`(newYear, newMonth)
  }

  /** Returns a copy of this year-month with the specified period subtracted.
    *
    * This method returns a new year-month based on this year-month with the specified period subtracted.
    * The subtractor is typically {@link org.threeten.bp.Period Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to aubtract, not null
    * @return a { @code YearMonth} based on this year-month with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): YearMonth =
    amount.subtractFrom(this).asInstanceOf[YearMonth]

  /** {@inheritDoc}
    *
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): YearMonth =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this year-month with the specified period in years subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToSubtract  the years to subtract, may be negative
    * @return a { @code YearMonth} based on this year-month with the years subtracted, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def minusYears(yearsToSubtract: Long): YearMonth =
    if (yearsToSubtract == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-yearsToSubtract)

  /** Returns a copy of this year-month with the specified period in months subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToSubtract  the months to subtract, may be negative
    * @return a { @code YearMonth} based on this year-month with the months subtracted, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def minusMonths(monthsToSubtract: Long): YearMonth =
    if (monthsToSubtract == Long.MinValue) plusMonths(Long.MaxValue).plusMonths(1)
    else plusMonths(-monthsToSubtract)

  /** Queries this year-month using the specified query.
    *
    * This queries this year-month using the specified query strategy object.
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
    else if (query eq TemporalQueries.precision)
      MONTHS.asInstanceOf[R]
    else if ((query eq TemporalQueries.localDate) || (query eq TemporalQueries.localTime) || (query eq TemporalQueries.zone) || (query eq TemporalQueries.zoneId) || (query eq TemporalQueries.offset))
      null.asInstanceOf[R]
    else
      query.queryFrom(this)

  /** Adjusts the specified temporal object to have this year-month.
    *
    * This returns a temporal object of the same observable type as the input
    * with the year and month changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#PROLEPTIC_MONTH} as the field.
    * If the specified temporal object does not use the ISO calendar system then
    * a {@code DateTimeException} is thrown.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisYearMonth.adjustInto(temporal);
    * temporal = temporal.with(thisYearMonth);
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
    if (!(Chronology.from(temporal) == IsoChronology.INSTANCE))
      throw new DateTimeException("Adjustment only supported on ISO date-time")
    else
      temporal.`with`(PROLEPTIC_MONTH, getProlepticMonth)

  /** Calculates the period between this year-month and another year-month in
    * terms of the specified unit.
    *
    * This calculates the period between two year-months in terms of a single unit.
    * The start and end points are {@code this} and the specified year-month.
    * The result will be negative if the end is before the start.
    * The {@code Temporal} passed to this method must be a {@code YearMonth}.
    * For example, the period in years between two year-months can be calculated
    * using {@code startYearMonth.until(endYearMonth, YEARS)}.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two year-months.
    * For example, the period in decades between 2012-06 and 2032-05
    * will only be one decade as it is one month short of two decades.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * {{{
    * val period: Long = start.until(end, YEARS);   // this method
    * dateTime.plus(YEARS.between(start, end));     // use in plus/minus
    * }}}
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code MONTHS}, {@code YEARS}, {@code DECADES},
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
    * @param endExclusive  the end year-month, which is converted to a { @code YearMonth}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this year-month and the end year-month
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: YearMonth = YearMonth.from(endExclusive)
    if (unit.isInstanceOf[ChronoUnit]) {
      val monthsUntil: Long = end.getProlepticMonth - getProlepticMonth
      unit.asInstanceOf[ChronoUnit] match {
        case MONTHS    => monthsUntil
        case YEARS     => monthsUntil / 12
        case DECADES   => monthsUntil / 120
        case CENTURIES => monthsUntil / 1200
        case MILLENNIA => monthsUntil / 12000
        case ERAS      => end.getLong(ERA) - getLong(ERA)
        case _         => throw UnsupportedTemporalTypeException.unit(unit)

      }
    } else {
      unit.between(this, end)
    }
  }

  /** Combines this year-month with a day-of-month to create a {@code LocalDate}.
    *
    * This returns a {@code LocalDate} formed from this year-month and the specified day-of-month.
    *
    * The day-of-month value must be valid for the year-month.
    *
    * This method can be used as part of a chain to produce a date:
    * <pre>
    * LocalDate date = year.atMonth(month).atDay(day);
    * </pre>
    *
    * @param dayOfMonth  the day-of-month to use, from 1 to 31
    * @return the date formed from this year-month and the specified day, not null
    * @throws DateTimeException if the day is invalid for the year-month
    * @see #isValidDay(int)
    */
  def atDay(dayOfMonth: Int): LocalDate = LocalDate.of(year, month, dayOfMonth)

  /** Returns a {@code LocalDate} at the end of the month.
    *
    * This returns a {@code LocalDate} based on this year-month.
    * The day-of-month is set to the last valid day of the month, taking
    * into account leap years.
    *
    * This method can be used as part of a chain to produce a date:
    * <pre>
    * LocalDate date = year.atMonth(month).atEndOfMonth();
    * </pre>
    *
    * @return the last valid date of this year-month, not null
    */
  def atEndOfMonth: LocalDate = LocalDate.of(year, month, lengthOfMonth)

  /** Compares this year-month to another year-month.
    *
    * The comparison is based first on the value of the year, then on the value of the month.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param other  the other year-month to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(other: YearMonth): Int = {
    var cmp: Int = year - other.year
    if (cmp == 0)
      cmp = month - other.month
    cmp
  }

  override def compareTo(other: YearMonth): Int = compare(other)

  /** Is this year-month after the specified year-month.
    *
    * @param other  the other year-month to compare to, not null
    * @return true if this is after the specified year-month
    */
  def isAfter(other: YearMonth): Boolean = compareTo(other) > 0

  /** Is this year-month before the specified year-month.
    *
    * @param other  the other year-month to compare to, not null
    * @return true if this point is before the specified year-month
    */
  def isBefore(other: YearMonth): Boolean = compareTo(other) < 0

  /** Checks if this year-month is equal to another year-month.
    *
    * The comparison is based on the time-line position of the year-months.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other year-month
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: YearMonth => (this eq other) || (year == other.year && month == other.month)
      case _                => false
    }

  /** A hash code for this year-month.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = year ^ (month << 27)

  /** Outputs this year-month as a {@code String}, such as {@code 2007-12}.
    *
    * The output will be in the format {@code yyyy-MM}:
    *
    * @return a string representation of this year-month, not null
    */
  override def toString: String = {
    val absYear: Int       = Math.abs(year)
    val buf: StringBuilder = new StringBuilder(9)
    if (absYear < 1000)
      if (year < 0)
        buf.append(year - 10000).deleteCharAt(1)
      else
        buf.append(year + 10000).deleteCharAt(0)
    else
      buf.append(year)
    buf.append(if (month < 10) "-0" else "-").append(month).toString
  }

  /** Outputs this year-month as a {@code String} using the formatter.
    *
    * This year-month will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted year-month string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  private def writeReplace: AnyRef = new Ser(Ser.YEAR_MONTH_TYPE, this)

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
    out.writeByte(month)
  }
}
