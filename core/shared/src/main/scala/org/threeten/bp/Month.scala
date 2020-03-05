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

import java.util.Locale

import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.TextStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

/** A month-of-year, such as 'July'.
  *
  * {@code Month} is an enum representing the 12 months of the year -
  * January, February, March, April, May, June, July, August, September, October,
  * November and December.
  *
  * In addition to the textual enum name, each month-of-year has an {@code int} value.
  * The {@code int} value follows normal usage and the ISO-8601 standard,
  * from 1 (January) to 12 (December). It is recommended that applications use the enum
  * rather than the {@code int} value to ensure code clarity.
  *
  * <b>Do not use {@code ordinal()} to obtain the numeric representation of {@code Month}.
  * Use {@code getValue()} instead.</b>
  *
  * This enum represents a common concept that is found in many calendar systems.
  * As such, this enum may be used by any calendar system that has the month-of-year
  * concept defined exactly equivalent to the ISO-8601 calendar system.
  *
  * <h3>Specification for implementors</h3>
  * This is an immutable and thread-safe enum.
  */
object Month {

  /** The singleton instance for the month of January with 31 days.
    * This has the numeric value of {@code 1}.
    */
  lazy val JANUARY = new Month("JANUARY", 0)

  /** The singleton instance for the month of February with 28 days, or 29 in a leap year.
    * This has the numeric value of {@code 2}.
    */
  lazy val FEBRUARY = new Month("FEBRUARY", 1)

  /** The singleton instance for the month of March with 31 days.
    * This has the numeric value of {@code 3}.
    */
  lazy val MARCH = new Month("MARCH", 2)

  /** The singleton instance for the month of April with 30 days.
    * This has the numeric value of {@code 4}.
    */
  lazy val APRIL = new Month("APRIL", 3)

  /** The singleton instance for the month of May with 31 days.
    * This has the numeric value of {@code 5}.
    */
  lazy val MAY = new Month("MAY", 4)

  /** The singleton instance for the month of June with 30 days.
    * This has the numeric value of {@code 6}.
    */
  lazy val JUNE = new Month("JUNE", 5)

  /** The singleton instance for the month of July with 31 days.
    * This has the numeric value of {@code 7}.
    */
  lazy val JULY = new Month("JULY", 6)

  /** The singleton instance for the month of August with 31 days.
    * This has the numeric value of {@code 8}.
    */
  lazy val AUGUST = new Month("AUGUST", 7)

  /** The singleton instance for the month of September with 30 days.
    * This has the numeric value of {@code 9}.
    */
  lazy val SEPTEMBER = new Month("SEPTEMBER", 8)

  /** The singleton instance for the month of October with 31 days.
    * This has the numeric value of {@code 10}.
    */
  lazy val OCTOBER = new Month("OCTOBER", 9)

  /** The singleton instance for the month of November with 30 days.
    * This has the numeric value of {@code 11}.
    */
  val NOVEMBER = new Month("NOVEMBER", 10)

  /** The singleton instance for the month of December with 31 days.
    * This has the numeric value of {@code 12}.
    */
  lazy val DECEMBER = new Month("DECEMBER", 11)

  lazy val values: Array[Month] = Array(JANUARY,
                                        FEBRUARY,
                                        MARCH,
                                        APRIL,
                                        MAY,
                                        JUNE,
                                        JULY,
                                        AUGUST,
                                        SEPTEMBER,
                                        OCTOBER,
                                        NOVEMBER,
                                        DECEMBER)
  def valueOf(enum: String): Month = values.find(_.name() == enum) match {
    case Some(month) => month
    case _           => throw new IllegalArgumentException(s"Unrecognized month name: $enum")
  }

  /** Private cache of all the constants.
    */
  private def ENUMS: Array[Month] = Month.values

  /** Obtains an instance of {@code Month} from an {@code int} value.
    *
    * {@code Month} is an enum representing the 12 months of the year.
    * This factory allows the enum to be obtained from the {@code int} value.
    * The {@code int} value follows the ISO-8601 standard, from 1 (January) to 12 (December).
    *
    * @param month  the month-of-year to represent, from 1 (January) to 12 (December)
    * @return the month-of-year, not null
    * @throws DateTimeException if the month-of-year is invalid
    */
  def of(month: Int): Month =
    if (month < 1 || month > 12)
      throw new DateTimeException(s"Invalid value for MonthOfYear: $month")
    else
      ENUMS(month - 1)

  /** Obtains an instance of {@code Month} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code Month}.
    *
    * The conversion extracts the {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} field.
    * The extraction is only permitted if the temporal object has an ISO
    * chronology, or can be converted to a {@code LocalDate}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code Month::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the month-of-year, not null
    * @throws DateTimeException if unable to convert to a { @code Month}
    */
  def from(temporal: TemporalAccessor): Month = {
    var _temporal = temporal
    _temporal match {
      case month: Month => month
      case _ =>
        try {
          if (IsoChronology.INSTANCE != Chronology.from(_temporal))
            _temporal = LocalDate.from(_temporal)
          of(_temporal.get(MONTH_OF_YEAR))
        } catch {
          case ex: DateTimeException =>
            throw new DateTimeException(
              s"Unable to obtain Month from TemporalAccessor: ${_temporal}, type ${_temporal.getClass.getName}",
              ex
            )
        }
    }
  }
}

final class Month private (name: String, ordinal: Int)
    extends Enum[Month](name, ordinal)
    with TemporalAccessor
    with TemporalAdjuster {
  import Month._

  /** Gets the month-of-year {@code int} value.
    *
    * The values are numbered following the ISO-8601 standard,
    * from 1 (January) to 12 (December).
    *
    * @return the month-of-year, from 1 (January) to 12 (December)
    */
  def getValue: Int = ordinal + 1

  /** Gets the textual representation, such as 'Jan' or 'December'.
    *
    * This returns the textual name used to identify the month-of-year.
    * The parameters control the length of the returned text and the locale.
    *
    * If no textual mapping is found then the {@link #getValue() numeric value} is returned.
    *
    * @param style  the length of the text required, not null
    * @param locale  the locale to use, not null
    * @return the text value of the month-of-year, not null
    */
  def getDisplayName(style: TextStyle, locale: Locale): String =
    new DateTimeFormatterBuilder().appendText(MONTH_OF_YEAR, style).toFormatter(locale).format(this)

  /** Checks if the specified field is supported.
    *
    * This checks if this month-of-year can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} then
    * this method returns true.
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this month-of-year, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      field eq MONTH_OF_YEAR
    else
      field != null && field.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This month is used to enhance the accuracy of the returned range.
    * If it is not possible to return the range, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} then the
    * range of the month-of-year, from 1 to 12, will be returned.
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
    else if (field.isInstanceOf[ChronoField])
      throw UnsupportedTemporalTypeException.field(field)
    else
      field.rangeRefinedBy(this)

  /** Gets the value of the specified field from this month-of-year as an {@code int}.
    *
    * This queries this month for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} then the
    * value of the month-of-year, from 1 to 12, will be returned.
    * All other {@code ChronoField} instances will throw a {@code DateTimeException}.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.getFrom(TemporalAccessor)}
    * passing {@code this} as the argument. Whether the value can be obtained,
    * and what the value represents, is determined by the field.
    *
    * @param field  the field to get, not null
    * @return the value for the field, within the valid range of values
    * @throws DateTimeException if a value for the field cannot be obtained
    * @throws DateTimeException if the range of valid values for the field exceeds an { @code int}
    * @throws DateTimeException if the value is outside the range of valid values for the field
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def get(field: TemporalField): Int =
    if (field eq MONTH_OF_YEAR)
      getValue
    else
      range(field).checkValidIntValue(getLong(field), field)

  /** Gets the value of the specified field from this month-of-year as a {@code long}.
    *
    * This queries this month for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} then the
    * value of the month-of-year, from 1 to 12, will be returned.
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
    if (field eq MONTH_OF_YEAR)
      getValue.toLong
    else if (field.isInstanceOf[ChronoField])
      throw throw UnsupportedTemporalTypeException.field(field)
    else
      field.getFrom(this)

  /** Returns the month-of-year that is the specified number of quarters after this one.
    *
    * The calculation rolls around the end of the year from December to January.
    * The specified period may be negative.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param months  the months to add, positive or negative
    * @return the resulting month, not null
    */
  def plus(months: Long): Month = {
    val amount: Int = (months % 12).toInt
    Month.ENUMS((ordinal + (amount + 12)) % 12)
  }

  /** Returns the month-of-year that is the specified number of months before this one.
    *
    * The calculation rolls around the start of the year from January to December.
    * The specified period may be negative.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param months  the months to subtract, positive or negative
    * @return the resulting month, not null
    */
  def minus(months: Long): Month = plus(-(months % 12))

  /** Gets the length of this month in days.
    *
    * This takes a flag to determine whether to return the length for a leap year or not.
    *
    * February has 28 days in a standard year and 29 days in a leap year.
    * April, June, September and November have 30 days.
    * All other months have 31 days.
    *
    * @param leapYear  true if the length is required for a leap year
    * @return the length of this month in days, from 28 to 31
    */
  def length(leapYear: Boolean): Int =
    this match {
      case FEBRUARY                            => if (leapYear) 29 else 28
      case APRIL | JUNE | SEPTEMBER | NOVEMBER => 30
      case _                                   => 31
    }

  /** Gets the minimum length of this month in days.
    *
    * February has a minimum length of 28 days.
    * April, June, September and November have 30 days.
    * All other months have 31 days.
    *
    * @return the minimum length of this month in days, from 28 to 31
    */
  def minLength: Int =
    this match {
      case FEBRUARY                            => 28
      case APRIL | JUNE | SEPTEMBER | NOVEMBER => 30
      case _                                   => 31
    }

  /** Gets the maximum length of this month in days.
    *
    * February has a maximum length of 29 days.
    * April, June, September and November have 30 days.
    * All other months have 31 days.
    *
    * @return the maximum length of this month in days, from 29 to 31
    */
  def maxLength: Int =
    this match {
      case FEBRUARY                            => 29
      case APRIL | JUNE | SEPTEMBER | NOVEMBER => 30
      case _                                   => 31
    }

  /** Gets the day-of-year corresponding to the first day of this month.
    *
    * This returns the day-of-year that this month begins on, using the leap
    * year flag to determine the length of February.
    *
    * @param leapYear  true if the length is required for a leap year
    * @return the day of year corresponding to the first day of this month, from 1 to 336
    */
  def firstDayOfYear(leapYear: Boolean): Int = {
    val leap: Int = if (leapYear) 1 else 0
    this match {
      case JANUARY   => 1
      case FEBRUARY  => 32
      case MARCH     => 60 + leap
      case APRIL     => 91 + leap
      case MAY       => 121 + leap
      case JUNE      => 152 + leap
      case JULY      => 182 + leap
      case AUGUST    => 213 + leap
      case SEPTEMBER => 244 + leap
      case OCTOBER   => 274 + leap
      case NOVEMBER  => 305 + leap
      case DECEMBER  => 335 + leap
    }
  }

  /** Gets the month corresponding to the first month of this quarter.
    *
    * The year can be divided into four quarters.
    * This method returns the first month of the quarter for the base month.
    * January, February and March return January.
    * April, May and June return April.
    * July, August and September return July.
    * October, November and December return October.
    *
    * @return the first month of the quarter corresponding to this month, not null
    */
  def firstMonthOfQuarter: Month = Month.ENUMS((ordinal / 3) * 3)

  /** Queries this month-of-year using the specified query.
    *
    * This queries this month-of-year using the specified query strategy object.
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

  /** Adjusts the specified temporal object to have this month-of-year.
    *
    * This returns a temporal object of the same observable type as the input
    * with the month-of-year changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#MONTH_OF_YEAR} as the field.
    * If the specified temporal object does not use the ISO calendar system then
    * a {@code DateTimeException} is thrown.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisMonth.adjustInto(temporal);
    * temporal = temporal.with(thisMonth);
    * </pre>
    *
    * For example, given a date in May, the following are output:
    * <pre>
    * dateInMay.with(JANUARY);    // four months earlier
    * dateInMay.with(APRIL);      // one months earlier
    * dateInMay.with(MAY);        // same date
    * dateInMay.with(JUNE);       // one month later
    * dateInMay.with(DECEMBER);   // seven months later
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
    if (Chronology.from(temporal) != IsoChronology.INSTANCE)
      throw new DateTimeException("Adjustment only supported on ISO date-time")
    else
      temporal.`with`(MONTH_OF_YEAR, getValue.toLong)
}
