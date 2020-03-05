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
package org.threeten.bp.chrono

import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import java.util.{ Comparator, Objects }
import org.threeten.bp.DateTimeException
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
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

object ChronoLocalDate {

  /** Gets a comparator that compares {@code ChronoLocalDate} in
    * time-line order ignoring the chronology.
    *
    * This comparator differs from the comparison in {@link #compareTo} in that it
    * only compares the underlying date and not the chronology.
    * This allows dates in different calendar systems to be compared based
    * on the position of the date on the local time-line.
    * The underlying comparison is equivalent to comparing the epoch-day.
    *
    * @return a comparator that compares in time-line order ignoring the chronology
    * @see #isAfter
    * @see #isBefore
    * @see #isEqual
    */
  def timeLineOrder: Comparator[ChronoLocalDate] = DATE_COMPARATOR

  private def DATE_COMPARATOR: Comparator[ChronoLocalDate] =
    new Comparator[ChronoLocalDate] {
      override def compare(date1: ChronoLocalDate, date2: ChronoLocalDate): Int =
        java.lang.Long.compare(date1.toEpochDay, date2.toEpochDay)
    }

  /** Obtains an instance of {@code ChronoLocalDate} from a temporal object.
    *
    * This obtains a local date based on the specified temporal.
    * A {@code TemporalAccessor} represents an arbitrary set of date and time information,
    * which this factory converts to an instance of {@code ChronoLocalDate}.
    *
    * The conversion extracts and combines the chronology and the date
    * from the temporal object. The behavior is equivalent to using
    * {@link Chronology#date(TemporalAccessor)} with the extracted chronology.
    * Implementations are permitted to perform optimizations such as accessing
    * those fields that are equivalent to the relevant objects.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used as a query via method reference, {@code ChronoLocalDate::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the date, not null
    * @throws DateTimeException if unable to convert to a { @code ChronoLocalDate}
    * @see Chronology#date(TemporalAccessor)
    */
  def from(temporal: TemporalAccessor): ChronoLocalDate = {
    Objects.requireNonNull(temporal, "temporal")
    if (temporal.isInstanceOf[ChronoLocalDate])
      return temporal.asInstanceOf[ChronoLocalDate]
    val chrono: Chronology = temporal.query(TemporalQueries.chronology)
    if (chrono == null)
      throw new DateTimeException(
        s"No Chronology found to create ChronoLocalDate: ${temporal.getClass}"
      )
    chrono.date(temporal)
  }
}

/** A date without time-of-day or time-zone in an arbitrary chronology, intended
  * for advanced globalization use cases.
  *
  * <b>Most applications should declare method signatures, fields and variables
  * as {@link LocalDate}, not this interface.</b>
  *
  * A {@code ChronoLocalDate} is the abstract representation of a date where the
  * {@code Chronology chronology}, or calendar system, is pluggable.
  * The date is defined in terms of fields expressed by {@link TemporalField},
  * where most common implementations are defined in {@link ChronoField}.
  * The chronology defines how the calendar system operates and the meaning of
  * the standard fields.
  *
  * <h4>When to use this interface</h4>
  * The design of the API encourages the use of {@code LocalDate} rather than this
  * interface, even in the case where the application needs to deal with multiple
  * calendar systems. The rationale for this is explored in the following documentation.
  *
  * The primary use case where this interface should be used is where the generic
  * type parameter {@code <C>} is fully defined as a specific chronology.
  * In that case, the assumptions of that chronology are known at development
  * time and specified in the code.
  *
  * When the chronology is defined in the generic type parameter as ? or otherwise
  * unknown at development time, the rest of the discussion below applies.
  *
  * To emphasize the point, declaring a method signature, field or variable as this
  * interface type can initially seem like the sensible way to globalize an application,
  * however it is usually the wrong approach.
  * As such, it should be considered an application-wide architectural decision to choose
  * to use this interface as opposed to {@code LocalDate}.
  *
  * <h4>Architectural issues to consider</h4>
  * These are some of the points that must be considered before using this interface
  * throughout an application.
  *
  * 1) Applications using this interface, as opposed to using just {@code LocalDate},
  * face a significantly higher probability of bugs. This is because the calendar system
  * in use is not known at development time. A key cause of bugs is where the developer
  * applies assumptions from their day-to-day knowledge of the ISO calendar system
  * to code that is intended to deal with any arbitrary calendar system.
  * The section below outlines how those assumptions can cause problems
  * The primary mechanism for reducing this increased risk of bugs is a strong code review process.
  * This should also be considered a extra cost in maintenance for the lifetime of the code.
  *
  * 2) This interface does not enforce immutability of implementations.
  * While the implementation notes indicate that all implementations must be immutable
  * there is nothing in the code or type system to enforce this. Any method declared
  * to accept a {@code ChronoLocalDate} could therefore be passed a poorly or
  * maliciously written mutable implementation.
  *
  * 3) Applications using this interface  must consider the impact of eras.
  * {@code LocalDate} shields users from the concept of eras, by ensuring that {@code getYear()}
  * returns the proleptic year. That decision ensures that developers can think of
  * {@code LocalDate} instances as consisting of three fields - year, month-of-year and day-of-month.
  * By contrast, users of this interface must think of dates as consisting of four fields -
  * era, year-of-era, month-of-year and day-of-month. The extra era field is frequently
  * forgotten, yet it is of vital importance to dates in an arbitrary calendar system.
  * For example, in the Japanese calendar system, the era represents the reign of an Emperor.
  * Whenever one reign ends and another starts, the year-of-era is reset to one.
  *
  * 4) The only agreed international standard for passing a date between two systems
  * is the ISO-8601 standard which requires the ISO calendar system. Using this interface
  * throughout the application will inevitably lead to the requirement to pass the date
  * across a network or component boundary, requiring an application specific protocol or format.
  *
  * 5) Long term persistence, such as a database, will almost always only accept dates in the
  * ISO-8601 calendar system (or the related Julian-Gregorian). Passing around dates in other
  * calendar systems increases the complications of interacting with persistence.
  *
  * 6) Most of the time, passing a {@code ChronoLocalDate} throughout an application
  * is unnecessary, as discussed in the last section below.
  *
  * <h4>False assumptions causing bugs in multi-calendar system code</h4>
  * As indicated above, there are many issues to consider when try to use and manipulate a
  * date in an arbitrary calendar system. These are some of the key issues.
  *
  * Code that queries the day-of-month and assumes that the value will never be more than
  * 31 is invalid. Some calendar systems have more than 31 days in some months.
  *
  * Code that adds 12 months to a date and assumes that a year has been added is invalid.
  * Some calendar systems have a different number of months, such as 13 in the Coptic or Ethiopic.
  *
  * Code that adds one month to a date and assumes that the month-of-year value will increase
  * by one or wrap to the next year is invalid. Some calendar systems have a variable number
  * of months in a year, such as the Hebrew.
  *
  * Code that adds one month, then adds a second one month and assumes that the day-of-month
  * will remain close to its original value is invalid. Some calendar systems have a large difference
  * between the length of the longest month and the length of the shortest month.
  * For example, the Coptic or Ethiopic have 12 months of 30 days and 1 month of 5 days.
  *
  * Code that adds seven days and assumes that a week has been added is invalid.
  * Some calendar systems have weeks of other than seven days, such as the French Revolutionary.
  *
  * Code that assumes that because the year of {@code date1} is greater than the year of {@code date2}
  * then {@code date1} is after {@code date2} is invalid. This is invalid for all calendar systems
  * when referring to the year-of-era, and especially untrue of the Japanese calendar system
  * where the year-of-era restarts with the reign of every new Emperor.
  *
  * Code that treats month-of-year one and day-of-month one as the start of the year is invalid.
  * Not all calendar systems start the year when the month value is one.
  *
  * In general, manipulating a date, and even querying a date, is wide open to bugs when the
  * calendar system is unknown at development time. This is why it is essential that code using
  * this interface is subjected to additional code reviews. It is also why an architectural
  * decision to avoid this interface type is usually the correct one.
  *
  * <h4>Using LocalDate instead</h4>
  * The primary alternative to using this interface throughout your application is as follows.
  *<ul>
  * <li>Declare all method signatures referring to dates in terms of {@code LocalDate}.
  * <li>Either store the chronology (calendar system) in the user profile or lookup
  * the chronology from the user locale
  * <li>Convert the ISO {@code LocalDate} to and from the user's preferred calendar system during
  * printing and parsing
  * </ul><p>
  * This approach treats the problem of globalized calendar systems as a localization issue
  * and confines it to the UI layer. This approach is in keeping with other localization
  * issues in the java platform.
  *
  * As discussed above, performing calculations on a date where the rules of the calendar system
  * are pluggable requires skill and is not recommended.
  * Fortunately, the need to perform calculations on a date in an arbitrary calendar system
  * is extremely rare. For example, it is highly unlikely that the business rules of a library
  * book rental scheme will allow rentals to be for one month, where meaning of the month
  * is dependent on the user's preferred calendar system.
  *
  * A key use case for calculations on a date in an arbitrary calendar system is producing
  * a month-by-month calendar for display and user interaction. Again, this is a UI issue,
  * and use of this interface solely within a few methods of the UI layer may be justified.
  *
  * In any other part of the system, where a date must be manipulated in a calendar system
  * other than ISO, the use case will generally specify the calendar system to use.
  * For example, an application may need to calculate the next Islamic or Hebrew holiday
  * which may require manipulating the date.
  * This kind of use case can be handled as follows:
  *<ul>
  * <li>start from the ISO {@code LocalDate} being passed to the method
  * <li>convert the date to the alternate calendar system, which for this use case is known
  * rather than arbitrary
  * <li>perform the calculation
  * <li>convert back to {@code LocalDate}
  * </ul><p>
  * Developers writing low-level frameworks or libraries should also avoid this interface.
  * Instead, one of the two general purpose access interfaces should be used.
  * Use {@link TemporalAccessor} if read-only access is required, or use {@link Temporal}
  * if read-write access is required.
  *
  * <h3>Specification for implementors</h3>
  * This interface must be implemented with care to ensure other classes operate correctly.
  * All implementations that can be instantiated must be final, immutable and thread-safe.
  * Subclasses should be Serializable wherever possible.
  *
  * Additional calendar systems may be added to the system.
  * See {@link Chronology} for more details.
  */
trait ChronoLocalDate extends Temporal with TemporalAdjuster with Ordered[ChronoLocalDate] {

  /** Gets the chronology of this date.
    *
    * The {@code Chronology} represents the calendar system in use.
    * The era and other fields in {@link ChronoField} are defined by the chronology.
    *
    * @return the chronology, not null
    */
  def getChronology: Chronology

  /** Gets the era, as defined by the chronology.
    *
    * The era is, conceptually, the largest division of the time-line.
    * Most calendar systems have a single epoch dividing the time-line into two eras.
    * However, some have multiple eras, such as one for the reign of each leader.
    * The exact meaning is determined by the {@code Chronology}.
    *
    * All correctly implemented {@code Era} classes are singletons, thus it
    * is valid code to write {@code date.getEra() == SomeEra.NAME)}.
    *
    * @return the chronology specific era constant applicable at this date, not null
    */
  def getEra: Era = getChronology.eraOf(get(ERA))

  /** Checks if the year is a leap year, as defined by the calendar system.
    *
    * A leap-year is a year of a longer length than normal.
    * The exact meaning is determined by the chronology with the constraint that
    * a leap-year must imply a year-length longer than a non leap-year.
    *
    * The default implementation uses {@link Chronology#isLeapYear(long)}.
    *
    * @return true if this date is in a leap year, false otherwise
    */
  def isLeapYear: Boolean = getChronology.isLeapYear(getLong(YEAR))

  /** Returns the length of the month represented by this date, as defined by the calendar system.
    *
    * This returns the length of the month in days.
    *
    * @return the length of the month in days
    */
  def lengthOfMonth: Int

  /** Returns the length of the year represented by this date, as defined by the calendar system.
    *
    * This returns the length of the year in days.
    *
    * The default implementation uses {@link #isLeapYear()} and returns 365 or 366.
    *
    * @return the length of the year in days
    */
  def lengthOfYear: Int = if (isLeapYear) 366 else 365

  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField]) field.isDateBased
    else field != null && field.isSupportedBy(this)

  def isSupported(unit: TemporalUnit): Boolean =
    if (unit.isInstanceOf[ChronoUnit]) unit.isDateBased
    else unit != null && unit.isSupportedBy(this)

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDate =
    getChronology.ensureChronoLocalDate(super.`with`(adjuster))

  def `with`(field: TemporalField, newValue: Long): ChronoLocalDate

  override def plus(amount: TemporalAmount): ChronoLocalDate =
    getChronology.ensureChronoLocalDate(super.plus(amount))

  def plus(amountToAdd: Long, unit: TemporalUnit): ChronoLocalDate

  override def minus(amount: TemporalAmount): ChronoLocalDate =
    getChronology.ensureChronoLocalDate(super.minus(amount))

  override def minus(amountToSubtract: Long, unit: TemporalUnit): ChronoLocalDate =
    getChronology.ensureChronoLocalDate(super.minus(amountToSubtract, unit))

  override def query[R](query: TemporalQuery[R]): R =
    query match {
      case TemporalQueries.chronology => getChronology.asInstanceOf[R]
      case TemporalQueries.precision  => ChronoUnit.DAYS.asInstanceOf[R]
      case TemporalQueries.localDate  => LocalDate.ofEpochDay(toEpochDay).asInstanceOf[R]
      case TemporalQueries.localTime | TemporalQueries.zone | TemporalQueries.zoneId |
          TemporalQueries.offset =>
        null.asInstanceOf[R]
      case _ => super.query(query)
    }

  def adjustInto(temporal: Temporal): Temporal = temporal.`with`(EPOCH_DAY, toEpochDay)

  /** Calculates the period between this date and another date as a {@code ChronoPeriod}.
    *
    * This calculates the period between two dates. All supplied chronologies
    * calculate the period using years, months and days, however the
    * {@code ChronoPeriod} API allows the period to be represented using other units.
    *
    * The start and end points are {@code this} and the specified date.
    * The result will be negative if the end is before the start.
    * The negative sign will be the same in each of year, month and day.
    *
    * The calculation is performed using the chronology of this date.
    * If necessary, the input date will be converted to match.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param endDateExclusive  the end date, exclusive, which may be in any chronology, not null
    * @return the period between this date and the end date, not null
    * @throws DateTimeException if the period cannot be calculated
    * @throws ArithmeticException if numeric overflow occurs
    */
  def until(endDateExclusive: ChronoLocalDate): ChronoPeriod

  /** Formats this date using the specified formatter.
    *
    * This date will be passed to the formatter to produce a string.
    *
    * The default implementation must behave as follows:
    * <pre>
    * return formatter.format(this);
    * </pre>
    *
    * @param formatter  the formatter to use, not null
    * @return the formatted date string, not null
    * @throws DateTimeException if an error occurs during printing
    */
  def format(formatter: DateTimeFormatter): String = {
    Objects.requireNonNull(formatter, "formatter")
    formatter.format(this)
  }

  /** Combines this date with a time to create a {@code ChronoLocalDateTime}.
    *
    * This returns a {@code ChronoLocalDateTime} formed from this date at the specified time.
    * All possible combinations of date and time are valid.
    *
    * @param localTime  the local time to use, not null
    * @return the local date-time formed from this date and the specified time, not null
    */
  def atTime(localTime: LocalTime): ChronoLocalDateTime[_ <: ChronoLocalDate] =
    ChronoLocalDateTimeImpl.of(this, localTime)

  /** Converts this date to the Epoch Day.
    *
    * The {@link ChronoField#EPOCH_DAY Epoch Day count} is a simple
    * incrementing count of days where day 0 is 1970-01-01 (ISO).
    * This definition is the same for all chronologies, enabling conversion.
    *
    * @return the Epoch Day equivalent to this date
    */
  def toEpochDay: Long = getLong(EPOCH_DAY)

  /** Compares this date to another date, including the chronology.
    *
    * The comparison is based first on the underlying time-line date, then
    * on the chronology.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * For example, the following is the comparator order:
    * <ol>
    * <li>{@code 2012-12-03 (ISO)}</li>
    * <li>{@code 2012-12-04 (ISO)}</li>
    * <li>{@code 2555-12-04 (ThaiBuddhist)}</li>
    * <li>{@code 2012-12-05 (ISO)}</li>
    * </ol>
    * Values #2 and #3 represent the same date on the time-line.
    * When two values represent the same date, the chronology ID is compared to distinguish them.
    * This step is needed to make the ordering "consistent with equals".
    *
    * If all the date objects being compared are in the same chronology, then the
    * additional chronology stage is not required and only the local date is used.
    * To compare the dates of two {@code TemporalAccessor} instances, including dates
    * in two different chronologies, use {@link ChronoField#EPOCH_DAY} as a comparator.
    *
    * @param other  the other date to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(other: ChronoLocalDate): Int = {
    var cmp: Int = java.lang.Long.compare(toEpochDay, other.toEpochDay)
    if (cmp == 0)
      cmp = getChronology.compareTo(other.getChronology)
    cmp
  }

  /** Checks if this date is after the specified date ignoring the chronology.
    *
    * This method differs from the comparison in {@link #compareTo} in that it
    * only compares the underlying date and not the chronology.
    * This allows dates in different calendar systems to be compared based
    * on the time-line position.
    * This is equivalent to using {@code date1.toEpochDay() &gt; date2.toEpochDay()}.
    *
    * @param other  the other date to compare to, not null
    * @return true if this is after the specified date
    */
  def isAfter(other: ChronoLocalDate): Boolean = this.toEpochDay > other.toEpochDay

  /** Checks if this date is before the specified date ignoring the chronology.
    *
    * This method differs from the comparison in {@link #compareTo} in that it
    * only compares the underlying date and not the chronology.
    * This allows dates in different calendar systems to be compared based
    * on the time-line position.
    * This is equivalent to using {@code date1.toEpochDay() &lt; date2.toEpochDay()}.
    *
    * @param other  the other date to compare to, not null
    * @return true if this is before the specified date
    */
  def isBefore(other: ChronoLocalDate): Boolean = this.toEpochDay < other.toEpochDay

  /** Checks if this date is equal to the specified date ignoring the chronology.
    *
    * This method differs from the comparison in {@link #compareTo} in that it
    * only compares the underlying date and not the chronology.
    * This allows dates in different calendar systems to be compared based
    * on the time-line position.
    * This is equivalent to using {@code date1.toEpochDay() == date2.toEpochDay()}.
    *
    * @param other  the other date to compare to, not null
    * @return true if the underlying date is equal to the specified date
    */
  def isEqual(other: ChronoLocalDate): Boolean = this.toEpochDay == other.toEpochDay

  /** Checks if this date is equal to another date, including the chronology.
    *
    * Compares this date with another ensuring that the date and chronology are the same.
    *
    * To compare the dates of two {@code TemporalAccessor} instances, including dates
    * in two different chronologies, use {@link ChronoField#EPOCH_DAY} as a comparator.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other date
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ChronoLocalDate => (this eq other) || compareTo(other) == 0
      case _                      => false
    }

  /** A hash code for this date.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = {
    val epDay: Long = toEpochDay
    getChronology.hashCode ^ (epDay ^ (epDay >>> 32)).toInt
  }

  /** Outputs this date as a {@code String}.
    *
    * The output will include the full local date and the chronology ID.
    *
    * @return the formatted date, not null
    */
  override def toString: String = {
    val yoe: Long          = getLong(YEAR_OF_ERA)
    val moy: Long          = getLong(MONTH_OF_YEAR)
    val dom: Long          = getLong(DAY_OF_MONTH)
    val buf: StringBuilder = new StringBuilder(30)
    buf
      .append(getChronology.toString)
      .append(" ")
      .append(getEra)
      .append(" ")
      .append(yoe)
      .append(if (moy < 10) "-0" else "-")
      .append(moy)
      .append(if (dom < 10) "-0" else "-")
      .append(dom)
    buf.toString()
  }
}
