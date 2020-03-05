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
import org.threeten.bp.format.SignStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoUnit.CENTURIES
import org.threeten.bp.temporal.ChronoUnit.DECADES
import org.threeten.bp.temporal.ChronoUnit.ERAS
import org.threeten.bp.temporal.ChronoUnit.MILLENNIA
import org.threeten.bp.temporal.ChronoUnit.YEARS
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

@SerialVersionUID(-23038383694477807L)
object Year {

  /** The minimum supported year, '-999,999,999'. */
  lazy val MIN_VALUE: Int = -999999999

  /** The maximum supported year, '+999,999,999'. */
  lazy val MAX_VALUE: Int = 999999999

  /** Parser. */
  private lazy val PARSER: DateTimeFormatter =
    new DateTimeFormatterBuilder().appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD).toFormatter

  /** Obtains the current year from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current year.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @return the current year using the system clock and default time-zone, not null
    */
  def now: Year = now(Clock.systemDefaultZone)

  /** Obtains the current year from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current year.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current year using the system clock, not null
    */
  def now(zone: ZoneId): Year = now(Clock.system(zone))

  /** Obtains the current year from the specified clock.
    *
    * This will query the specified clock to obtain the current year.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current year, not null
    */
  def now(clock: Clock): Year = {
    val now: LocalDate = LocalDate.now(clock)
    Year.of(now.getYear)
  }

  /** Obtains an instance of {@code Year}.
    *
    * This method accepts a year value from the proleptic ISO calendar system.
    *
    * The year 2AD/CE is represented by 2.<br>
    * The year 1AD/CE is represented by 1.<br>
    * The year 1BC/BCE is represented by 0.<br>
    * The year 2BC/BCE is represented by -1.<br>
    *
    * @param isoYear  the ISO proleptic year to represent, from { @code MIN_VALUE} to { @code MAX_VALUE}
    * @return the year, not null
    * @throws DateTimeException if the field is invalid
    */
  def of(isoYear: Int): Year = {
    YEAR.checkValidValue(isoYear.toLong)
    new Year(isoYear)
  }

  /** Obtains an instance of {@code Year} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code Year}.
    *
    * The conversion extracts the {@link ChronoField#YEAR year} field.
    * The extraction is only permitted if the temporal object has an ISO
    * chronology, or can be converted to a {@code LocalDate}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code Year::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the year, not null
    * @throws DateTimeException if unable to convert to a { @code Year}
    */
  def from(temporal: TemporalAccessor): Year = {
    var _temporal = temporal
    _temporal match {
      case year: Year =>
        year
      case _ =>
        try {
          if (IsoChronology.INSTANCE != Chronology.from(_temporal)) {
            _temporal = LocalDate.from(_temporal)
          }
          of(_temporal.get(YEAR))
        } catch {
          case _: DateTimeException =>
            throw new DateTimeException(
              s"Unable to obtain Year from TemporalAccessor: ${_temporal}, type ${_temporal.getClass.getName}"
            )
        }
    }
  }

  /** Obtains an instance of {@code Year} from a text string such as {@code 2007}.
    *
    * The string must represent a valid year.
    * Years outside the range 0000 to 9999 must be prefixed by the plus or minus symbol.
    *
    * @param text  the text to parse such as "2007", not null
    * @return the parsed year, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence): Year = parse(text, PARSER)

  /** Obtains an instance of {@code Year} from a text string using a specific formatter.
    *
    * The text is parsed using the formatter, returning a year.
    *
    * @param text  the text to parse, not null
    * @param formatter  the formatter to use, not null
    * @return the parsed year, not null
    * @throws DateTimeParseException if the text cannot be parsed
    */
  def parse(text: CharSequence, formatter: DateTimeFormatter): Year = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.parse(text, new TemporalQuery[Year] {
      override def queryFrom(temporal: TemporalAccessor): Year = Year.from(temporal)
    })
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
    * @param year  the year to check
    * @return true if the year is leap, false otherwise
    */
  def isLeap(year: Long): Boolean = ((year & 3) == 0) && ((year % 100) != 0 || (year % 400) == 0)

  @throws[IOException]
  private[bp] def readExternal(in: DataInput): Year = Year.of(in.readInt)
}

/** A year in the ISO-8601 calendar system, such as {@code 2007}.
  *
  * {@code Year} is an immutable date-time object that represents a year.
  * Any field that can be derived from a year can be obtained.
  *
  * <b>Note that years in the ISO chronology only align with years in the
  * Gregorian-Julian system for modern years. Parts of Russia did not switch to the
  * modern Gregorian/ISO rules until 1920.
  * As such, historical years must be treated with caution.</b>
  *
  * This class does not store or represent a month, day, time or time-zone.
  * For example, the value "2007" can be stored in a {@code Year}.
  *
  * Years represented by this class follow the ISO-8601 standard and use
  * the proleptic numbering system. Year 1 is preceded by year 0, then by year -1.
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
  * @param year  the year to represent
  */
@SerialVersionUID(-23038383694477807L)
final class Year private (private val year: Int)
    extends TemporalAccessor
    with Temporal
    with TemporalAdjuster
    with Ordered[Year]
    with Serializable {

  /** Gets the year value.
    *
    * The year returned by this method is proleptic as per {@code get(YEAR)}.
    *
    * @return the year, { @code MIN_VALUE} to { @code MAX_VALUE}
    */
  def getValue: Int = year

  /** Checks if the specified field is supported.
    *
    * This checks if this year can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this date-time.
    * The supported fields are:
    * <ul>
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
    * @return true if the field is supported on this year, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      (field eq YEAR) || (field eq YEAR_OF_ERA) || (field eq ERA)
    else
      field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit])
      (unit eq YEARS) || (unit eq DECADES) || (unit eq CENTURIES) || (unit eq MILLENNIA) || (unit eq ERAS)
    else
      unit != null && unit.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This year is used to enhance the accuracy of the returned range.
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
      if (year <= 0) ValueRange.of(1, Year.MAX_VALUE.toLong + 1L)
      else ValueRange.of(1, Year.MAX_VALUE.toLong)
    else
      super.range(field)

  /** Gets the value of the specified field from this year as an {@code int}.
    *
    * This queries this year for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this year.
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

  /** Gets the value of the specified field from this year as a {@code long}.
    *
    * This queries this year for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the query is implemented here.
    * The {@link #isSupported(TemporalField) supported fields} will return valid
    * values based on this year.
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
          case YEAR_OF_ERA => if (year < 1) 1L - year.toLong else year.toLong
          case YEAR        => year.toLong
          case ERA         => if (year < 1) 0 else 1
          case _           => throw throw UnsupportedTemporalTypeException.field(field)
        }
      case _ => field.getFrom(this)
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
  def isLeap: Boolean = Year.isLeap(year.toLong)

  /** Checks if the month-day is valid for this year.
    *
    * This method checks whether this year and the input month and day form
    * a valid date.
    *
    * @param monthDay  the month-day to validate, null returns false
    * @return true if the month and day are valid for this year
    */
  def isValidMonthDay(monthDay: MonthDay): Boolean = monthDay != null && monthDay.isValidYear(year)

  /** Gets the length of this year in days.
    *
    * @return the length of this year in days, 365 or 366
    */
  def length: Int = if (isLeap) 366 else 365

  /** Returns an adjusted copy of this year.
    *
    * This returns a new {@code Year}, based on this one, with the year adjusted.
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
    * @return a { @code Year} based on { @code this} with the adjustment made, not null
    * @throws DateTimeException if the adjustment cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def `with`(adjuster: TemporalAdjuster): Year =
    adjuster.adjustInto(this).asInstanceOf[Year]

  /** Returns a copy of this year with the specified field set to a new value.
    *
    * This returns a new {@code Year}, based on this one, with the value
    * for the specified field changed.
    * If it is not possible to set the value, because the field is not supported or for
    * some other reason, an exception is thrown.
    *
    * If the field is a {@link ChronoField} then the adjustment is implemented here.
    * The supported fields behave as follows:
    * <ul>
    * <li>{@code YEAR_OF_ERA} -
    * Returns a {@code Year} with the specified year-of-era
    * The era will be unchanged.
    * <li>{@code YEAR} -
    * Returns a {@code Year} with the specified year.
    * This completely replaces the date and is equivalent to {@link #of(int)}.
    * <li>{@code ERA} -
    * Returns a {@code Year} with the specified era.
    * The year-of-era will be unchanged.
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
    * @return a { @code Year} based on { @code this} with the specified field set, not null
    * @throws DateTimeException if the field cannot be set
    * @throws ArithmeticException if numeric overflow occurs
    */
  def `with`(field: TemporalField, newValue: Long): Year =
    field match {
      case f: ChronoField =>
        f.checkValidValue(newValue)
        f match {
          case YEAR_OF_ERA => Year.of((if (year < 1) 1 - newValue else newValue).toInt)
          case YEAR        => Year.of(newValue.toInt)
          case ERA         => if (getLong(ERA) == newValue) this else Year.of(1 - year)
          case _           => throw throw UnsupportedTemporalTypeException.field(field)
        }
      case _ =>
        field.adjustInto(this, newValue)
    }

  /** Returns a copy of this year with the specified period added.
    *
    * This method returns a new year based on this year with the specified period added.
    * The adder is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #plus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to add, not null
    * @return a { @code Year} based on this year with the addition made, not null
    * @throws DateTimeException if the addition cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def plus(amount: TemporalAmount): Year = amount.addTo(this).asInstanceOf[Year]

  /** {@inheritDoc}
    *
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  def plus(amountToAdd: Long, unit: TemporalUnit): Year =
    unit match {
      case u: ChronoUnit =>
        u match {
          case YEARS     => plusYears(amountToAdd)
          case DECADES   => plusYears(Math.multiplyExact(amountToAdd, 10))
          case CENTURIES => plusYears(Math.multiplyExact(amountToAdd, 100))
          case MILLENNIA => plusYears(Math.multiplyExact(amountToAdd, 1000))
          case ERAS      => `with`(ERA, Math.addExact(getLong(ERA), amountToAdd))
          case _         => throw throw UnsupportedTemporalTypeException.unit(unit)
        }
      case _ =>
        unit.addTo(this, amountToAdd)
    }

  /** Returns a copy of this year with the specified number of years added.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToAdd  the years to add, may be negative
    * @return a { @code Year} based on this year with the period added, not null
    * @throws DateTimeException if the result exceeds the supported year range
    */
  def plusYears(yearsToAdd: Long): Year =
    if (yearsToAdd == 0)
      this
    else
      Year.of(YEAR.checkValidIntValue(year + yearsToAdd))

  /** Returns a copy of this year with the specified period subtracted.
    *
    * This method returns a new year based on this year with the specified period subtracted.
    * The subtractor is typically {@link Period} but may be any other type implementing
    * the {@link TemporalAmount} interface.
    * The calculation is delegated to the specified adjuster, which typically calls
    * back to {@link #minus(long, TemporalUnit)}.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amount  the amount to subtract, not null
    * @return a { @code Year} based on this year with the subtraction made, not null
    * @throws DateTimeException if the subtraction cannot be made
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def minus(amount: TemporalAmount): Year = amount.subtractFrom(this).asInstanceOf[Year]

  /** {@inheritDoc}
    *
    * @throws DateTimeException { @inheritDoc}
    * @throws ArithmeticException { @inheritDoc}
    */
  override def minus(amountToSubtract: Long, unit: TemporalUnit): Year =
    if (amountToSubtract == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amountToSubtract, unit)

  /** Returns a copy of this year with the specified number of years subtracted.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToSubtract  the years to subtract, may be negative
    * @return a { @code Year} based on this year with the period subtracted, not null
    * @throws DateTimeException if the result exceeds the supported year range
    */
  def minusYears(yearsToSubtract: Long): Year =
    if (yearsToSubtract == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-yearsToSubtract)

  /** Queries this year using the specified query.
    *
    * This queries this year using the specified query strategy object.
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
      YEARS.asInstanceOf[R]
    else if ((query eq TemporalQueries.localDate) || (query eq TemporalQueries.localTime) || (query eq TemporalQueries.zone) || (query eq TemporalQueries.zoneId) || (query eq TemporalQueries.offset))
      null.asInstanceOf[R]
    else
      super.query(query)

  /** Adjusts the specified temporal object to have this year.
    *
    * This returns a temporal object of the same observable type as the input
    * with the year changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#YEAR} as the field.
    * If the specified temporal object does not use the ISO calendar system then
    * a {@code DateTimeException} is thrown.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisYear.adjustInto(temporal);
    * temporal = temporal.with(thisYear);
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
      temporal.`with`(YEAR, year.toLong)

  /** Calculates the period between this year and another year in
    * terms of the specified unit.
    *
    * This calculates the period between two years in terms of a single unit.
    * The start and end points are {@code this} and the specified year.
    * The result will be negative if the end is before the start.
    * The {@code Temporal} passed to this method must be a {@code Year}.
    * For example, the period in decades between two year can be calculated
    * using {@code startYear.until(endYear, DECADES)}.
    *
    * The calculation returns a whole number, representing the number of
    * complete units between the two years.
    * For example, the period in decades between 2012 and 2031
    * will only be one decade as it is one year short of two decades.
    *
    * This method operates in association with {@link TemporalUnit#between}.
    * The result of this method is a {@code long} representing the amount of
    * the specified unit. By contrast, the result of {@code between} is an
    * object that can be used directly in addition/subtraction:
    * <pre>
    * long period = start.until(end, YEARS);   // this method
    * dateTime.plus(YEARS.between(start, end));      // use in plus/minus
    * </pre>
    *
    * The calculation is implemented in this method for {@link ChronoUnit}.
    * The units {@code YEARS}, {@code DECADES}, {@code CENTURIES},
    * {@code MILLENNIA} and {@code ERAS} are supported.
    * Other {@code ChronoUnit} values will throw an exception.
    *
    * If the unit is not a {@code ChronoUnit}, then the result of this method
    * is obtained by invoking {@code TemporalUnit.between(Temporal, Temporal)}
    * passing {@code this} as the first argument and the input temporal as
    * the second argument.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endExclusive  the end year, which is converted to a { @code Year}, not null
    * @param unit  the unit to measure the period in, not null
    * @return the amount of the period between this year and the end year
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    val end: Year = Year.from(endExclusive)
    unit match {
      case u: ChronoUnit =>
        val yearsUntil: Long = end.year.toLong - year
        u match {
          case YEARS     => yearsUntil
          case DECADES   => yearsUntil / 10
          case CENTURIES => yearsUntil / 100
          case MILLENNIA => yearsUntil / 1000
          case ERAS      => end.getLong(ERA) - getLong(ERA)
          case _         => throw UnsupportedTemporalTypeException.unit(unit)
        }
      case _ =>
        unit.between(this, end)
    }
  }

  /** Combines this year with a day-of-year to create a {@code LocalDate}.
    *
    * This returns a {@code LocalDate} formed from this year and the specified day-of-year.
    *
    * The day-of-year value 366 is only valid in a leap year.
    *
    * @param dayOfYear  the day-of-year to use, not null
    * @return the local date formed from this year and the specified date of year, not null
    * @throws DateTimeException if the day of year is zero or less, 366 or greater or equal
    *                           to 366 and this is not a leap year
    */
  def atDay(dayOfYear: Int): LocalDate = LocalDate.ofYearDay(year, dayOfYear)

  /** Combines this year with a month to create a {@code YearMonth}.
    *
    * This returns a {@code YearMonth} formed from this year and the specified month.
    * All possible combinations of year and month are valid.
    *
    * This method can be used as part of a chain to produce a date:
    * <pre>
    * LocalDate date = year.atMonth(month).atDay(day);
    * </pre>
    *
    * @param month  the month-of-year to use, not null
    * @return the year-month formed from this year and the specified month, not null
    */
  def atMonth(month: Month): YearMonth = YearMonth.of(year, month)

  /** Combines this year with a month to create a {@code YearMonth}.
    *
    * This returns a {@code YearMonth} formed from this year and the specified month.
    * All possible combinations of year and month are valid.
    *
    * This method can be used as part of a chain to produce a date:
    * <pre>
    * LocalDate date = year.atMonth(month).atDay(day);
    * </pre>
    *
    * @param month  the month-of-year to use, from 1 (January) to 12 (December)
    * @return the year-month formed from this year and the specified month, not null
    * @throws DateTimeException if the month is invalid
    */
  def atMonth(month: Int): YearMonth = YearMonth.of(year, month)

  /** Combines this year with a month-day to create a {@code LocalDate}.
    *
    * This returns a {@code LocalDate} formed from this year and the specified month-day.
    *
    * A month-day of February 29th will be adjusted to February 28th in the resulting
    * date if the year is not a leap year.
    *
    * @param monthDay  the month-day to use, not null
    * @return the local date formed from this year and the specified month-day, not null
    */
  def atMonthDay(monthDay: MonthDay): LocalDate = monthDay.atYear(year)

  /** Compares this year to another year.
    *
    * The comparison is based on the value of the year.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * @param other  the other year to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(other: Year): Int = year - other.year

  override def compareTo(other: Year): Int = compare(other)

  /** Is this year after the specified year.
    *
    * @param other  the other year to compare to, not null
    * @return true if this is after the specified year
    */
  def isAfter(other: Year): Boolean = year > other.year

  /** Is this year before the specified year.
    *
    * @param other  the other year to compare to, not null
    * @return true if this point is before the specified year
    */
  def isBefore(other: Year): Boolean = year < other.year

  /** Checks if this year is equal to another year.
    *
    * The comparison is based on the time-line position of the years.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other year
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case thatYear: Year => (this eq thatYear) || (year == thatYear.year)
      case _              => false
    }

  /** A hash code for this year.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = year

  /** Outputs this year as a {@code String}.
    *
    * @return a string representation of this year, not null
    */
  override def toString: String = Integer.toString(year)

  /** Outputs this year as a {@code String} using the formatter.
    *
    * This year will be passed to the formatter
    * {@link DateTimeFormatter#format(TemporalAccessor) print method}.
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted year string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  private def writeReplace: AnyRef = new Ser(Ser.YEAR_TYPE, this)

  /** Defend against malicious streams.
    *
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[bp] def writeExternal(out: DataOutput): Unit = out.writeInt(year)
}
