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

import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.TextStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

/** A day-of-week, such as 'Tuesday'.
  *
  * {@code DayOfWeek} is an enum representing the 7 days of the week -
  * Monday, Tuesday, Wednesday, Thursday, Friday, Saturday and Sunday.
  *
  * In addition to the textual enum name, each day-of-week has an {@code int} value.
  * The {@code int} value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
  * It is recommended that applications use the enum rather than the {@code int} value
  * to ensure code clarity.
  *
  * This enum provides access to the localized textual form of the day-of-week.
  * Some locales also assign different numeric values to the days, declaring
  * Sunday to have the value 1, however this class provides no support for this.
  * See {@link WeekFields} for localized week-numbering.
  *
  * <b>Do not use {@code ordinal()} to obtain the numeric representation of {@code DayOfWeek}.
  * Use {@code getValue()} instead.</b>
  *
  * This enum represents a common concept that is found in many calendar systems.
  * As such, this enum may be used by any calendar system that has the day-of-week
  * concept defined exactly equivalent to the ISO calendar system.
  *
  * <h3>Specification for implementors</h3>
  * This is an immutable and thread-safe enum.
  */
object DayOfWeek {

  /** The singleton instance for the day-of-week of Monday.
    * This has the numeric value of {@code 1}.
    */
  lazy val MONDAY = new DayOfWeek("MONDAY", 0)

  /** The singleton instance for the day-of-week of Tuesday.
    * This has the numeric value of {@code 2}.
    */
  lazy val TUESDAY = new DayOfWeek("TUESDAY", 1)

  /** The singleton instance for the day-of-week of Wednesday.
    * This has the numeric value of {@code 3}.
    */
  lazy val WEDNESDAY = new DayOfWeek("WEDNESDAY", 2)

  /** The singleton instance for the day-of-week of Thursday.
    * This has the numeric value of {@code 4}.
    */
  lazy val THURSDAY = new DayOfWeek("THURSDAY", 3)

  /** The singleton instance for the day-of-week of Friday.
    * This has the numeric value of {@code 5}.
    */
  lazy val FRIDAY = new DayOfWeek("FRIDAY", 4)

  /** The singleton instance for the day-of-week of Saturday.
    * This has the numeric value of {@code 6}.
    */
  lazy val SATURDAY = new DayOfWeek("SATURDAY", 5)

  /** The singleton instance for the day-of-week of Sunday.
    * This has the numeric value of {@code 7}.
    */
  lazy val SUNDAY = new DayOfWeek("SUNDAY", 6)

  lazy val values: Array[DayOfWeek] =
    Array(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)
  def valueOf(enum: String): DayOfWeek = values.find(_.name() == enum) match {
    case Some(dayOfWeek) => dayOfWeek
    case _ =>
      throw new IllegalArgumentException(s"Unrecognized day of week name: $enum")
  }

  /** Private cache of all the constants.
    */
  private def ENUMS: Array[DayOfWeek] = DayOfWeek.values

  /** Obtains an instance of {@code DayOfWeek} from an {@code int} value.
    *
    * {@code DayOfWeek} is an enum representing the 7 days of the week.
    * This factory allows the enum to be obtained from the {@code int} value.
    * The {@code int} value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
    *
    * @param dayOfWeek  the day-of-week to represent, from 1 (Monday) to 7 (Sunday)
    * @return the day-of-week singleton, not null
    * @throws DateTimeException if the day-of-week is invalid
    */
  def of(dayOfWeek: Int): DayOfWeek =
    if (dayOfWeek < 1 || dayOfWeek > 7)
      throw new DateTimeException(s"Invalid value for DayOfWeek: $dayOfWeek")
    else
      ENUMS(dayOfWeek - 1)

  /** Obtains an instance of {@code DayOfWeek} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code DayOfWeek}.
    *
    * The conversion extracts the {@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} field.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code DayOfWeek::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the day-of-week, not null
    * @throws DateTimeException if unable to convert to a { @code DayOfWeek}
    */
  def from(temporal: TemporalAccessor): DayOfWeek = {
    temporal match {
      case week: DayOfWeek => return week
      case _               =>
    }
    try of(temporal.get(DAY_OF_WEEK))
    catch {
      case ex: DateTimeException =>
        throw new DateTimeException(
          s"Unable to obtain DayOfWeek from TemporalAccessor: $temporal, type ${temporal.getClass.getName}",
          ex
        )
    }
  }
}

final class DayOfWeek(name: String, ordinal: Int)
    extends Enum[DayOfWeek](name, ordinal)
    with TemporalAccessor
    with TemporalAdjuster {

  /** Gets the day-of-week {@code int} value.
    *
    * The values are numbered following the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
    * See {@link WeekFields#dayOfWeek} for localized week-numbering.
    *
    * @return the day-of-week, from 1 (Monday) to 7 (Sunday)
    */
  def getValue: Int = ordinal + 1

  /** Gets the textual representation, such as 'Mon' or 'Friday'.
    *
    * This returns the textual name used to identify the day-of-week.
    * The parameters control the length of the returned text and the locale.
    *
    * If no textual mapping is found then the {@link #getValue() numeric value} is returned.
    *
    * @param style  the length of the text required, not null
    * @param locale  the locale to use, not null
    * @return the text value of the day-of-week, not null
    */
  def getDisplayName(style: TextStyle, locale: Locale): String =
    new DateTimeFormatterBuilder().appendText(DAY_OF_WEEK, style).toFormatter(locale).format(this)

  /** Checks if the specified field is supported.
    *
    * This checks if this day-of-week can be queried for the specified field.
    * If false, then calling the {@link #range(TemporalField) range} and
    * {@link #get(TemporalField) get} methods will throw an exception.
    *
    * If the field is {@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} then
    * this method returns true.
    * All other {@code ChronoField} instances will return false.
    *
    * If the field is not a {@code ChronoField}, then the result of this method
    * is obtained by invoking {@code TemporalField.isSupportedBy(TemporalAccessor)}
    * passing {@code this} as the argument.
    * Whether the field is supported is determined by the field.
    *
    * @param field  the field to check, null returns false
    * @return true if the field is supported on this day-of-week, false if not
    */
  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField])
      field eq DAY_OF_WEEK
    else
      field != null && field.isSupportedBy(this)

  /** Gets the range of valid values for the specified field.
    *
    * The range object expresses the minimum and maximum valid values for a field.
    * This day-of-week is used to enhance the accuracy of the returned range.
    * If it is not possible to return the range, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} then the
    * range of the day-of-week, from 1 to 7, will be returned.
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
    if (field eq DAY_OF_WEEK)
      field.range
    else if (field.isInstanceOf[ChronoField])
      throw throw UnsupportedTemporalTypeException.field(field)
    else
      field.rangeRefinedBy(this)

  /** Gets the value of the specified field from this day-of-week as an {@code int}.
    *
    * This queries this day-of-week for the value for the specified field.
    * The returned value will always be within the valid range of values for the field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} then the
    * value of the day-of-week, from 1 to 7, will be returned.
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
    if (field eq DAY_OF_WEEK)
      getValue
    else
      range(field).checkValidIntValue(getLong(field), field)

  /** Gets the value of the specified field from this day-of-week as a {@code long}.
    *
    * This queries this day-of-week for the value for the specified field.
    * If it is not possible to return the value, because the field is not supported
    * or for some other reason, an exception is thrown.
    *
    * If the field is {@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} then the
    * value of the day-of-week, from 1 to 7, will be returned.
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
    if (field eq DAY_OF_WEEK)
      getValue.toLong
    else if (field.isInstanceOf[ChronoField])
      throw throw UnsupportedTemporalTypeException.field(field)
    else
      field.getFrom(this)

  /** Returns the day-of-week that is the specified number of days after this one.
    *
    * The calculation rolls around the end of the week from Sunday to Monday.
    * The specified period may be negative.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param days  the days to add, positive or negative
    * @return the resulting day-of-week, not null
    */
  def plus(days: Long): DayOfWeek = {
    val amount: Int = (days % 7).toInt
    DayOfWeek.ENUMS((ordinal + (amount + 7)) % 7)
  }

  /** Returns the day-of-week that is the specified number of days before this one.
    *
    * The calculation rolls around the start of the year from Monday to Sunday.
    * The specified period may be negative.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param days  the days to subtract, positive or negative
    * @return the resulting day-of-week, not null
    */
  def minus(days: Long): DayOfWeek = plus(-(days % 7))

  /** Queries this day-of-week using the specified query.
    *
    * This queries this day-of-week using the specified query strategy object.
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
      temporal.ChronoUnit.DAYS.asInstanceOf[R]
    else if ((query eq TemporalQueries.localDate) || (query eq TemporalQueries.localTime) || (query eq TemporalQueries.chronology) || (query eq TemporalQueries.zone) || (query eq TemporalQueries.zoneId) || (query eq TemporalQueries.offset))
      null.asInstanceOf[R]
    else
      query.queryFrom(this)

  /** Adjusts the specified temporal object to have this day-of-week.
    *
    * This returns a temporal object of the same observable type as the input
    * with the day-of-week changed to be the same as this.
    *
    * The adjustment is equivalent to using {@link Temporal#with(TemporalField, long)}
    * passing {@link ChronoField#DAY_OF_WEEK} as the field.
    * Note that this adjusts forwards or backwards within a Monday to Sunday week.
    * See {@link WeekFields#dayOfWeek} for localized week start days.
    * See {@link TemporalAdjusters} for other adjusters
    * with more control, such as {@code next(MONDAY)}.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#with(TemporalAdjuster)}:
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * temporal = thisDayOfWeek.adjustInto(temporal);
    * temporal = temporal.with(thisDayOfWeek);
    * </pre>
    *
    * For example, given a date that is a Wednesday, the following are output:
    * <pre>
    * dateOnWed.with(MONDAY);     // two days earlier
    * dateOnWed.with(TUESDAY);    // one day earlier
    * dateOnWed.with(WEDNESDAY);  // same date
    * dateOnWed.with(THURSDAY);   // one day later
    * dateOnWed.with(FRIDAY);     // two days later
    * dateOnWed.with(SATURDAY);   // three days later
    * dateOnWed.with(SUNDAY);     // four days later
    * </pre>
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param temporal  the target object to be adjusted, not null
    * @return the adjusted object, not null
    * @throws DateTimeException if unable to make the adjustment
    * @throws ArithmeticException if numeric overflow occurs
    */
  def adjustInto(temporal: Temporal): Temporal = temporal.`with`(DAY_OF_WEEK, getValue.toLong)
}
