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

import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import java.io.Serializable
import java.util.{ Arrays, Collections, Objects }
import java.util.regex.Matcher
import java.util.regex.Pattern
import org.threeten.bp.chrono.ChronoPeriod
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.DateTimeParseException
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAmount
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

@SerialVersionUID(-8290556941213247973L)
object Period {

  /** A constant for a period of zero. */
  lazy val ZERO: Period = new Period(0, 0, 0)

  /** The pattern for parsing. */
  private def PATTERN: Pattern = Pattern.compile(
    "([-+]?)P(?:([-+]?[0-9]+)Y)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)W)?(?:([-+]?[0-9]+)D)?",
    Pattern.CASE_INSENSITIVE
  )

  /** Obtains a {@code Period} representing a number of years.
    *
    * The resulting period will have the specified years.
    * The months and days units will be zero.
    *
    * @param years the number of years, positive or negative
    * @return the period of years, not null
    */
  def ofYears(years: Int): Period = create(years, 0, 0)

  /** Obtains a {@code Period} representing a number of months.
    *
    * The resulting period will have the specified months.
    * The years and days units will be zero.
    *
    * @param months the number of months, positive or negative
    * @return the period of months, not null
    */
  def ofMonths(months: Int): Period = create(0, months, 0)

  /** Obtains a {@code Period} representing a number of weeks.
    *
    * The resulting period will have days equal to the weeks multiplied by seven.
    * The years and months units will be zero.
    *
    * @param weeks the number of weeks, positive or negative
    * @return the period of days, not null
    */
  def ofWeeks(weeks: Int): Period = create(0, 0, Math.multiplyExact(weeks, 7))

  /** Obtains a {@code Period} representing a number of days.
    *
    * The resulting period will have the specified days.
    * The years and months units will be zero.
    *
    * @param days the number of days, positive or negative
    * @return the period of days, not null
    */
  def ofDays(days: Int): Period = create(0, 0, days)

  /** Obtains a {@code Period} representing a number of years, months and days.
    *
    * This creates an instance based on years, months and days.
    *
    * @param years  the amount of years, may be negative
    * @param months the amount of months, may be negative
    * @param days   the amount of days, may be negative
    * @return the period of years, months and days, not null
    */
  def of(years: Int, months: Int, days: Int): Period = create(years, months, days)

  /** Obtains an instance of {@code Period} from a temporal amount.
    *
    * This obtains a period based on the specified amount.
    * A {@code TemporalAmount} represents an  amount of time, which may be
    * date-based or time-based, which this factory extracts to a {@code Period}.
    *
    * The conversion loops around the set of units from the amount and uses
    * the {@link ChronoUnit#YEARS YEARS}, {@link ChronoUnit#MONTHS MONTHS}
    * and {@link ChronoUnit#DAYS DAYS} units to create a period.
    * If any other units are found then an exception is thrown.
    *
    * If the amount is a {@code ChronoPeriod} then it must use the ISO chronology.
    *
    * @param amount the temporal amount to convert, not null
    * @return the equivalent period, not null
    * @throws DateTimeException   if unable to convert to a { @code Period}
    * @throws ArithmeticException if the amount of years, months or days exceeds an int
    */
  def from(amount: TemporalAmount): Period = {

    def fromAmount: Period = {
      Objects.requireNonNull(amount, "amount")
      var years: Int  = 0
      var months: Int = 0
      var days: Int   = 0
      val units       = amount.getUnits.iterator
      while (units.hasNext) {
        val unit             = units.next()
        val unitAmount: Long = amount.get(unit)
        if (unit eq ChronoUnit.YEARS) years = Math.toIntExact(unitAmount)
        else if (unit eq ChronoUnit.MONTHS) months = Math.toIntExact(unitAmount)
        else if (unit eq ChronoUnit.DAYS) days = Math.toIntExact(unitAmount)
        else throw new DateTimeException(s"Unit must be Years, Months or Days, but was $unit")
      }
      create(years, months, days)
    }

    amount match {
      case period: Period => period
      case chrono: ChronoPeriod =>
        if (IsoChronology.INSTANCE != chrono.getChronology)
          throw new DateTimeException(s"Period requires ISO chronology: $amount")
        fromAmount
      case _ =>
        fromAmount
    }
  }

  /** Obtains a {@code Period} consisting of the number of years, months,
    * and days between two dates.
    *
    * The start date is included, but the end date is not.
    * The period is calculated by removing complete months, then calculating
    * the remaining number of days, adjusting to ensure that both have the same sign.
    * The number of months is then split into years and months based on a 12 month year.
    * A month is considered if the end day-of-month is greater than or equal to the start day-of-month.
    * For example, from {@code 2010-01-15} to {@code 2011-03-18} is one year, two months and three days.
    *
    * The result of this method can be a negative period if the end is before the start.
    * The negative sign will be the same in each of year, month and day.
    *
    * @param startDate the start date, inclusive, not null
    * @param endDate   the end date, exclusive, not null
    * @return the period between this date and the end date, not null
    * @see ChronoLocalDate#until(ChronoLocalDate)
    */
  def between(startDate: LocalDate, endDate: LocalDate): Period = startDate.until(endDate)

  /** Obtains a {@code Period} from a text string such as {@code PnYnMnD}.
    *
    * This will parse the string produced by {@code toString()} which is
    * based on the ISO-8601 period formats {@code PnYnMnD} and {@code PnW}.
    *
    * The string starts with an optional sign, denoted by the ASCII negative
    * or positive symbol. If negative, the whole period is negated.
    * The ASCII letter "P" is next in upper or lower case.
    * There are then four sections, each consisting of a number and a suffix.
    * At least one of the four sections must be present.
    * The sections have suffixes in ASCII of "Y", "M", "W" and "D" for
    * years, months, weeks and days, accepted in upper or lower case.
    * The suffixes must occur in order.
    * The number part of each section must consist of ASCII digits.
    * The number may be prefixed by the ASCII negative or positive symbol.
    * The number must parse to an {@code int}.
    *
    * The leading plus/minus sign, and negative values for other units are
    * not part of the ISO-8601 standard. In addition, ISO-8601 does not
    * permit mixing between the {@code PnYnMnD} and {@code PnW} formats.
    * Any week-based input is multiplied by 7 and treated as a number of days.
    *
    * For example, the following are valid inputs:
    * <pre>
    * "P2Y"             -- Period.ofYears(2)
    * "P3M"             -- Period.ofMonths(3)
    * "P4W"             -- Period.ofWeeks(4)
    * "P5D"             -- Period.ofDays(5)
    * "P1Y2M3D"         -- Period.of(1, 2, 3)
    * "P1Y2M3W4D"       -- Period.of(1, 2, 25)
    * "P-1Y2M"          -- Period.of(-1, 2, 0)
    * "-P1Y2M"          -- Period.of(-1, -2, 0)
    * </pre>
    *
    * @param text the text to parse, not null
    * @return the parsed period, not null
    * @throws DateTimeParseException if the text cannot be parsed to a period
    */
  def parse(text: CharSequence): Period = {
    Objects.requireNonNull(text, "text")
    val matcher: Matcher = PATTERN.matcher(text)
    if (matcher.matches) {
      val negate: Int        = if ("-" == matcher.group(1)) -1 else 1
      val yearMatch: String  = matcher.group(2)
      val monthMatch: String = matcher.group(3)
      val weekMatch: String  = matcher.group(4)
      val dayMatch: String   = matcher.group(5)
      if (yearMatch != null || monthMatch != null || weekMatch != null || dayMatch != null) {
        try {
          val years: Int  = parseNumber(text, yearMatch, negate)
          val months: Int = parseNumber(text, monthMatch, negate)
          val weeks: Int  = parseNumber(text, weekMatch, negate)
          var days: Int   = parseNumber(text, dayMatch, negate)
          days = Math.addExact(days, Math.multiplyExact(weeks, 7))
          return create(years, months, days)
        } catch {
          case ex: NumberFormatException =>
            throw new DateTimeParseException("Text cannot be parsed to a Period", text, 0, ex)
        }
      }
    }
    throw new DateTimeParseException("Text cannot be parsed to a Period", text, 0)
  }

  private def parseNumber(text: CharSequence, str: String, negate: Int): Int = {
    if (str == null) {
      return 0
    }
    val `val`: Int = str.toInt
    try {
      Math.multiplyExact(`val`, negate)
    } catch {
      case ex: ArithmeticException =>
        throw new DateTimeParseException("Text cannot be parsed to a Period", text, 0, ex)
    }
  }

  /** Creates an instance.
    *
    * @param years  the amount
    * @param months the amount
    * @param days   the amount
    */
  private def create(years: Int, months: Int, days: Int): Period =
    if ((years | months | days) == 0) ZERO
    else new Period(years, months, days)
}

/** A date-based amount of time, such as '2 years, 3 months and 4 days'.
  *
  * This class models a quantity or amount of time in terms of years, months and days.
  * See {@link Duration} for the time-based equivalent to this class.
  *
  * Durations and period differ in their treatment of daylight savings time
  * when added to {@link ZonedDateTime}. A {@code Duration} will add an exact
  * number of seconds, thus a duration of one day is always exactly 24 hours.
  * By contrast, a {@code Period} will add a conceptual day, trying to maintain
  * the local time.
  *
  * For example, consider adding a period of one day and a duration of one day to
  * 18:00 on the evening before a daylight savings gap. The {@code Period} will add
  * the conceptual day and result in a {@code ZonedDateTime} at 18:00 the following day.
  * By contrast, the {@code Duration} will add exactly 24 hours, resulting in a
  * {@code ZonedDateTime} at 19:00 the following day (assuming a one hour DST gap).
  *
  * The supported units of a period are {@link ChronoUnit#YEARS YEARS},
  * {@link ChronoUnit#MONTHS MONTHS} and {@link ChronoUnit#DAYS DAYS}.
  * All three fields are always present, but may be set to zero.
  *
  * The period may be used with any calendar system.
  * The meaning of a "year" or "month" is only applied when the object is added to a date.
  *
  * The period is modeled as a directed amount of time, meaning that individual parts of the
  * period may be negative.
  *
  * The months and years fields may be {@linkplain #normalized() normalized}.
  * The normalization assumes a 12 month year, so is not appropriate for all calendar systems.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor
  * @param years  the amount
  * @param months  the amount
  * @param days  the amount
  */
@SerialVersionUID(-8290556941213247973L)
final class Period private (private val years: Int, private val months: Int, private val days: Int)
    extends ChronoPeriod
    with Serializable {

  /** Resolves singletons.
    *
    * @return the resolved instance
    */
  private def readResolve: AnyRef =
    if ((years | months | days) == 0) Period.ZERO
    else this

  def getUnits: java.util.List[TemporalUnit] =
    Collections.unmodifiableList[TemporalUnit](Arrays.asList(YEARS, MONTHS, DAYS))

  def getChronology: Chronology = IsoChronology.INSTANCE

  def get(unit: TemporalUnit): Long =
    if (unit eq YEARS) years.toLong
    else if (unit eq MONTHS) months.toLong
    else if (unit eq DAYS) days.toLong
    else throw throw UnsupportedTemporalTypeException.unit(unit)

  /** Checks if all three units of this period are zero.
    *
    * A zero period has the value zero for the years, months and days units.
    *
    * @return true if this period is zero-length
    */
  override def isZero: Boolean = this eq Period.ZERO

  /** Checks if any of the three units of this period are negative.
    *
    * This checks whether the years, months or days units are less than zero.
    *
    * @return true if any unit of this period is negative
    */
  override def isNegative: Boolean = years < 0 || months < 0 || days < 0

  /** Gets the amount of years of this period.
    *
    * This returns the years unit.
    *
    * The months unit is not normalized with the years unit.
    * This means that a period of "15 months" is different to a period
    * of "1 year and 3 months".
    *
    * @return the amount of years of this period, may be negative
    */
  def getYears: Int = years

  /** Gets the amount of months of this period.
    *
    * This returns the months unit.
    *
    * The months unit is not normalized with the years unit.
    * This means that a period of "15 months" is different to a period
    * of "1 year and 3 months".
    *
    * @return the amount of months of this period, may be negative
    */
  def getMonths: Int = months

  /** Gets the amount of days of this period.
    *
    * This returns the days unit.
    *
    * @return the amount of days of this period, may be negative
    */
  def getDays: Int = days

  /** Returns a copy of this period with the specified amount of years.
    *
    * This sets the amount of the years unit in a copy of this period.
    * The months and days units are unaffected.
    *
    * The months unit is not normalized with the years unit.
    * This means that a period of "15 months" is different to a period
    * of "1 year and 3 months".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param years  the years to represent, may be negative
    * @return a { @code Period} based on this period with the requested years, not null
    */
  def withYears(years: Int): Period =
    if (years == this.years) this
    else Period.create(years, months, days)

  /** Returns a copy of this period with the specified amount of months.
    *
    * This sets the amount of the months unit in a copy of this period.
    * The years and days units are unaffected.
    *
    * The months unit is not normalized with the years unit.
    * This means that a period of "15 months" is different to a period
    * of "1 year and 3 months".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param months  the months to represent, may be negative
    * @return a { @code Period} based on this period with the requested months, not null
    */
  def withMonths(months: Int): Period =
    if (months == this.months) this
    else Period.create(years, months, days)

  /** Returns a copy of this period with the specified amount of days.
    *
    * This sets the amount of the days unit in a copy of this period.
    * The years and months units are unaffected.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param days  the days to represent, may be negative
    * @return a { @code Period} based on this period with the requested days, not null
    */
  def withDays(days: Int): Period =
    if (days == this.days) this
    else Period.create(years, months, days)

  /** Returns a copy of this period with the specified amount added.
    *
    * This input amount is converted to a {@code Period} using {@code from(TemporalAmount)}.
    * This operates separately on the years, months and days.
    *
    * For example, "1 year, 6 months and 3 days" plus "2 years, 2 months and 2 days"
    * returns "3 years, 8 months and 5 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToAdd  the period to add, not null
    * @return a { @code Period} based on this period with the requested period added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plus(amountToAdd: TemporalAmount): Period = {
    val amount: Period = Period.from(amountToAdd)
    Period.create(Math.addExact(years, amount.years),
                  Math.addExact(months, amount.months),
                  Math.addExact(days, amount.days))
  }

  /** Returns a copy of this period with the specified years added.
    *
    * This adds the amount to the years unit in a copy of this period.
    * The months and days units are unaffected.
    * For example, "1 year, 6 months and 3 days" plus 2 years returns "3 years, 6 months and 3 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToAdd  the years to add, positive or negative
    * @return a { @code Period} based on this period with the specified years added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusYears(yearsToAdd: Long): Period =
    if (yearsToAdd == 0) this
    else Period.create(Math.toIntExact(Math.addExact(years.toLong, yearsToAdd)), months, days)

  /** Returns a copy of this period with the specified months added.
    *
    * This adds the amount to the months unit in a copy of this period.
    * The years and days units are unaffected.
    * For example, "1 year, 6 months and 3 days" plus 2 months returns "1 year, 8 months and 3 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToAdd  the months to add, positive or negative
    * @return a { @code Period} based on this period with the specified months added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusMonths(monthsToAdd: Long): Period =
    if (monthsToAdd == 0) this
    else Period.create(years, Math.toIntExact(Math.addExact(months.toLong, monthsToAdd)), days)

  /** Returns a copy of this period with the specified days added.
    *
    * This adds the amount to the days unit in a copy of this period.
    * The years and months units are unaffected.
    * For example, "1 year, 6 months and 3 days" plus 2 days returns "1 year, 6 months and 5 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToAdd  the days to add, positive or negative
    * @return a { @code Period} based on this period with the specified days added, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def plusDays(daysToAdd: Long): Period =
    if (daysToAdd == 0) this
    else Period.create(years, months, Math.toIntExact(Math.addExact(days.toLong, daysToAdd)))

  /** Returns a copy of this period with the specified amount subtracted.
    *
    * This input amount is converted to a {@code Period} using {@code from(TemporalAmount)}.
    * This operates separately on the years, months and days.
    *
    * For example, "1 year, 6 months and 3 days" minus "2 years, 2 months and 2 days"
    * returns "-1 years, 4 months and 1 day".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param amountToSubtract  the period to subtract, not null
    * @return a { @code Period} based on this period with the requested period subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minus(amountToSubtract: TemporalAmount): Period = {
    val amount: Period = Period.from(amountToSubtract)
    Period.create(Math.subtractExact(years, amount.years),
                  Math.subtractExact(months, amount.months),
                  Math.subtractExact(days, amount.days))
  }

  /** Returns a copy of this period with the specified years subtracted.
    *
    * This subtracts the amount from the years unit in a copy of this period.
    * The months and days units are unaffected.
    * For example, "1 year, 6 months and 3 days" minus 2 years returns "-1 years, 6 months and 3 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param yearsToSubtract  the years to subtract, positive or negative
    * @return a { @code Period} based on this period with the specified years subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusYears(yearsToSubtract: Long): Period =
    if (yearsToSubtract == Long.MinValue) plusYears(Long.MaxValue).plusYears(1)
    else plusYears(-yearsToSubtract)

  /** Returns a copy of this period with the specified months subtracted.
    *
    * This subtracts the amount from the months unit in a copy of this period.
    * The years and days units are unaffected.
    * For example, "1 year, 6 months and 3 days" minus 2 months returns "1 year, 4 months and 3 days".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param monthsToSubtract  the years to subtract, positive or negative
    * @return a { @code Period} based on this period with the specified months subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusMonths(monthsToSubtract: Long): Period =
    if (monthsToSubtract == Long.MinValue) plusMonths(Long.MaxValue).plusMonths(1)
    else plusMonths(-monthsToSubtract)

  /** Returns a copy of this period with the specified days subtracted.
    *
    * This subtracts the amount from the days unit in a copy of this period.
    * The years and months units are unaffected.
    * For example, "1 year, 6 months and 3 days" minus 2 days returns "1 year, 6 months and 1 day".
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param daysToSubtract  the months to subtract, positive or negative
    * @return a { @code Period} based on this period with the specified days subtracted, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def minusDays(daysToSubtract: Long): Period =
    if (daysToSubtract == Long.MinValue) plusDays(Long.MaxValue).plusDays(1)
    else plusDays(-daysToSubtract)

  /** Returns a new instance with each element in this period multiplied
    * by the specified scalar.
    *
    * This simply multiplies each field, years, months, days and normalized time,
    * by the scalar. No normalization is performed.
    *
    * @param scalar  the scalar to multiply by, not null
    * @return a { @code Period} based on this period with the amounts multiplied by the scalar, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def multipliedBy(scalar: Int): Period =
    if ((this eq Period.ZERO) || scalar == 1) this
    else
      Period.create(Math.multiplyExact(years, scalar),
                    Math.multiplyExact(months, scalar),
                    Math.multiplyExact(days, scalar))

  /** Returns a new instance with each amount in this period negated.
    *
    * @return a { @code Period} based on this period with the amounts negated, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  override def negated: Period = multipliedBy(-1)

  /** Returns a copy of this period with the years and months normalized
    * using a 12 month year.
    *
    * This normalizes the years and months units, leaving the days unit unchanged.
    * The months unit is adjusted to have an absolute value less than 11,
    * with the years unit being adjusted to compensate. For example, a period of
    * "1 Year and 15 months" will be normalized to "2 years and 3 months".
    *
    * The sign of the years and months units will be the same after normalization.
    * For example, a period of "1 year and -25 months" will be normalized to
    * "-1 year and -1 month".
    *
    * This normalization uses a 12 month year which is not valid for all calendar systems.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return a { @code Period} based on this period with excess months normalized to years, not null
    * @throws ArithmeticException if numeric overflow occurs
    */
  def normalized: Period = {
    val totalMonths: Long = toTotalMonths
    val splitYears: Long  = totalMonths / 12
    val splitMonths: Int  = (totalMonths % 12).toInt
    if (splitYears == years && splitMonths == months) this
    else Period.create(Math.toIntExact(splitYears), splitMonths, days)
  }

  /** Gets the total number of months in this period using a 12 month year.
    *
    * This returns the total number of months in the period by multiplying the
    * number of years by 12 and adding the number of months.
    *
    * This uses a 12 month year which is not valid for all calendar systems.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @return the total number of months in the period, may be negative
    */
  def toTotalMonths: Long = years * 12L + months

  /** Adds this period to the specified temporal object.
    *
    * This returns a temporal object of the same observable type as the input
    * with this period added.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#plus(TemporalAmount)}.
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * dateTime = thisPeriod.addTo(dateTime);
    * dateTime = dateTime.plus(thisPeriod);
    * </pre>
    *
    * The calculation will add the years, then months, then days.
    * Only non-zero amounts will be added.
    * If the date-time has a calendar system with a fixed number of months in a
    * year, then the years and months will be combined before being added.
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
    Objects.requireNonNull(_temporal, "temporal")
    if (years != 0)
      if (months != 0) _temporal = _temporal.plus(toTotalMonths, MONTHS)
      else _temporal             = _temporal.plus(years.toLong, YEARS)
    else if (months != 0)
      _temporal = _temporal.plus(months.toLong, MONTHS)
    if (days != 0)
      _temporal = _temporal.plus(days.toLong, DAYS)
    _temporal
  }

  /** Subtracts this period from the specified temporal object.
    *
    * This returns a temporal object of the same observable type as the input
    * with this period subtracted.
    *
    * In most cases, it is clearer to reverse the calling pattern by using
    * {@link Temporal#minus(TemporalAmount)}.
    * <pre>
    * // these two lines are equivalent, but the second approach is recommended
    * dateTime = thisPeriod.subtractFrom(dateTime);
    * dateTime = dateTime.minus(thisPeriod);
    * </pre>
    *
    * The calculation operates as follows.
    * First, the chronology of the temporal is checked to ensure it is ISO chronology or null.
    * Second, if the months are zero, the years are added if non-zero, otherwise
    * the combination of years and months is added if non-zero.
    * Finally, any days are added.
    *
    * The calculation will subtract the years, then months, then days.
    * Only non-zero amounts will be subtracted.
    * If the date-time has a calendar system with a fixed number of months in a
    * year, then the years and months will be combined before being subtracted.
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
    Objects.requireNonNull(_temporal, "temporal")
    if (years != 0)
      if (months != 0) _temporal = _temporal.minus(toTotalMonths, MONTHS)
      else _temporal             = _temporal.minus(years.toLong, YEARS)
    else if (months != 0)
      _temporal = _temporal.minus(months.toLong, MONTHS)
    if (days != 0)
      _temporal = _temporal.minus(days.toLong, DAYS)
    _temporal
  }

  /** Checks if this period is equal to another period.
    *
    * The comparison is based on the amounts held in the period.
    * To be equal, the years, months and days units must be individually equal.
    * Note that this means that a period of "15 Months" is not equal to a period
    * of "1 Year and 3 Months".
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other period
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: Period =>
        (this eq other) || (years == other.years && months == other.months && days == other.days)
      case _ => false
    }

  /** A hash code for this period.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = years + Integer.rotateLeft(months, 8) + Integer.rotateLeft(days, 16)

  /** Outputs this period as a {@code String}, such as {@code P6Y3M1D}.
    *
    * The output will be in the ISO-8601 period format.
    * A zero period will be represented as zero days, 'P0D'.
    *
    * @return a string representation of this period, not null
    */
  override def toString: String =
    if (this eq Period.ZERO) "P0D"
    else {
      val buf: StringBuilder = new StringBuilder
      buf.append('P')
      if (years != 0)
        buf.append(years).append('Y')
      if (months != 0)
        buf.append(months).append('M')
      if (days != 0)
        buf.append(days).append('D')
      buf.toString
    }
}
