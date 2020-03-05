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

import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_MONTH
import org.threeten.bp.temporal.ChronoField.ALIGNED_WEEK_OF_YEAR
import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.PROLEPTIC_MONTH
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoField.YEAR_OF_ERA
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.TemporalAdjusters.nextOrSame
import java.io.Serializable
import java.util.{ Arrays, Objects }
import org.threeten.bp.Clock
import org.threeten.bp.DateTimeException
import org.threeten.bp.DayOfWeek
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.ZoneId
import org.threeten.bp.format.ResolverStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.ValueRange

@SerialVersionUID(1039765215346859963L)
object MinguoChronology {

  /** Singleton instance for the Minguo chronology. */
  lazy val INSTANCE: MinguoChronology = new MinguoChronology

  /** The difference in years between ISO and Minguo. */
  private[chrono] def YEARS_DIFFERENCE: Int = 1911
}

/** The Minguo calendar system.
  *
  * This chronology defines the rules of the Minguo calendar system.
  * This calendar system is primarily used in the Republic of China, often known as Taiwan.
  * Dates are aligned such that {@code 0001-01-01 (Minguo)} is {@code 1912-01-01 (ISO)}.
  *
  * The fields are defined as follows:
  *<ul>
  * <li>era - There are two eras, the current 'Republic' (ROC) and the previous era (BEFORE_ROC).
  * <li>year-of-era - The year-of-era for the current era increases uniformly from the epoch at year one.
  * For the previous era the year increases from one as time goes backwards.
  * The value for the current era is equal to the ISO proleptic-year minus 1911.
  * <li>proleptic-year - The proleptic year is the same as the year-of-era for the
  * current era. For the previous era, years have zero, then negative values.
  * The value is equal to the ISO proleptic-year minus 1911.
  * <li>month-of-year - The Minguo month-of-year exactly matches ISO.
  * <li>day-of-month - The Minguo day-of-month exactly matches ISO.
  * <li>day-of-year - The Minguo day-of-year exactly matches ISO.
  * <li>leap-year - The Minguo leap-year pattern exactly matches ISO, such that the two calendars
  * are never out of step.
  * </ul><p>
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  */
@SerialVersionUID(1039765215346859963L)
final class MinguoChronology private () extends Chronology with Serializable {

  /** Resolve singleton.
    *
    * @return the singleton instance, not null
    */
  private def readResolve: AnyRef = MinguoChronology.INSTANCE

  /** Gets the ID of the chronology - 'Minguo'.
    *
    * The ID uniquely identifies the {@code Chronology}.
    * It can be used to lookup the {@code Chronology} using {@link #of(String)}.
    *
    * @return the chronology ID - 'Minguo'
    * @see #getCalendarType()
    */
  def getId: String = "Minguo"

  /** Gets the calendar type of the underlying calendar system - 'roc'.
    *
    * The calendar type is an identifier defined by the
    * <em>Unicode Locale Data Markup Language (LDML)</em> specification.
    * It can be used to lookup the {@code Chronology} using {@link #of(String)}.
    * It can also be used as part of a locale, accessible via
    * {@link Locale#getUnicodeLocaleType(String)} with the key 'ca'.
    *
    * @return the calendar system type - 'roc'
    * @see #getId()
    */
  def getCalendarType: String =
    "roc"

  override def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): MinguoDate =
    super.date(era, yearOfEra, month, dayOfMonth).asInstanceOf[MinguoDate]

  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): MinguoDate =
    new MinguoDate(
      LocalDate.of(prolepticYear + MinguoChronology.YEARS_DIFFERENCE, month, dayOfMonth)
    )

  override def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): MinguoDate =
    super.dateYearDay(era, yearOfEra, dayOfYear).asInstanceOf[MinguoDate]

  def dateYearDay(prolepticYear: Int, dayOfYear: Int): MinguoDate =
    new MinguoDate(
      LocalDate.ofYearDay(prolepticYear + MinguoChronology.YEARS_DIFFERENCE, dayOfYear)
    )

  def dateEpochDay(epochDay: Long): MinguoDate = new MinguoDate(LocalDate.ofEpochDay(epochDay))

  def date(temporal: TemporalAccessor): MinguoDate =
    if (temporal.isInstanceOf[MinguoDate])
      temporal.asInstanceOf[MinguoDate]
    else
      new MinguoDate(LocalDate.from(temporal))

  override def localDateTime(temporal: TemporalAccessor): ChronoLocalDateTime[MinguoDate] =
    super.localDateTime(temporal).asInstanceOf[ChronoLocalDateTime[MinguoDate]]

  override def zonedDateTime(temporal: TemporalAccessor): ChronoZonedDateTime[MinguoDate] =
    super.zonedDateTime(temporal).asInstanceOf[ChronoZonedDateTime[MinguoDate]]

  override def zonedDateTime(instant: Instant, zone: ZoneId): ChronoZonedDateTime[MinguoDate] =
    super.zonedDateTime(instant, zone).asInstanceOf[ChronoZonedDateTime[MinguoDate]]

  override def dateNow: MinguoDate = super.dateNow.asInstanceOf[MinguoDate]

  override def dateNow(zone: ZoneId): MinguoDate = super.dateNow(zone).asInstanceOf[MinguoDate]

  override def dateNow(clock: Clock): MinguoDate = {
    Objects.requireNonNull(clock, "clock")
    super.dateNow(clock).asInstanceOf[MinguoDate]
  }

  /** Checks if the specified year is a leap year.
    *
    * Minguo leap years occur exactly in line with ISO leap years.
    * This method does not validate the year passed in, and only has a
    * well-defined result for years in the supported range.
    *
    * @param prolepticYear  the proleptic-year to check, not validated for range
    * @return true if the year is a leap year
    */
  def isLeapYear(prolepticYear: Long): Boolean =
    IsoChronology.INSTANCE.isLeapYear(prolepticYear + MinguoChronology.YEARS_DIFFERENCE)

  def prolepticYear(era: Era, yearOfEra: Int): Int =
    if (!era.isInstanceOf[MinguoEra])
      throw new ClassCastException("Era must be MinguoEra")
    else if (era eq MinguoEra.ROC)
      yearOfEra
    else 1 - yearOfEra

  def eraOf(eraValue: Int): MinguoEra = MinguoEra.of(eraValue)

  def eras: java.util.List[Era] = Arrays.asList[Era](MinguoEra.values: _*)

  def range(field: ChronoField): ValueRange =
    field match {
      case PROLEPTIC_MONTH =>
        val range: ValueRange = PROLEPTIC_MONTH.range
        ValueRange.of(range.getMinimum - MinguoChronology.YEARS_DIFFERENCE * 12L,
                      range.getMaximum - MinguoChronology.YEARS_DIFFERENCE * 12L)
      case YEAR_OF_ERA =>
        val range: ValueRange = YEAR.range
        ValueRange.of(1,
                      range.getMaximum - MinguoChronology.YEARS_DIFFERENCE,
                      -range.getMinimum + 1 + MinguoChronology.YEARS_DIFFERENCE)
      case YEAR =>
        val range: ValueRange = YEAR.range
        ValueRange.of(range.getMinimum - MinguoChronology.YEARS_DIFFERENCE,
                      range.getMaximum - MinguoChronology.YEARS_DIFFERENCE)
      case _ => field.range
    }

  override def resolveDate(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle
  ): MinguoDate = {
    if (fieldValues.containsKey(EPOCH_DAY))
      return dateEpochDay(fieldValues.remove(EPOCH_DAY))
    val prolepticMonth: java.lang.Long = fieldValues.remove(PROLEPTIC_MONTH)
    if (prolepticMonth != null) {
      if (resolverStyle ne ResolverStyle.LENIENT)
        PROLEPTIC_MONTH.checkValidValue(prolepticMonth)
      updateResolveMap(fieldValues, MONTH_OF_YEAR, Math.floorMod(prolepticMonth, 12) + 1L)
      updateResolveMap(fieldValues, YEAR, Math.floorDiv(prolepticMonth, 12))
    }
    val yoeLong: java.lang.Long = fieldValues.remove(YEAR_OF_ERA)
    if (yoeLong != null) {
      if (resolverStyle ne ResolverStyle.LENIENT)
        YEAR_OF_ERA.checkValidValue(yoeLong)
      val era: java.lang.Long = fieldValues.remove(ERA)
      if (era == null) {
        val year: java.lang.Long = fieldValues.get(YEAR)
        if (resolverStyle eq ResolverStyle.STRICT) {
          if (year != null)
            updateResolveMap(fieldValues,
                             YEAR,
                             if (year > 0) yoeLong else Math.subtractExact(1, yoeLong))
          else
            fieldValues.put(YEAR_OF_ERA, yoeLong)
        } else {
          updateResolveMap(
            fieldValues,
            YEAR,
            if (year == null || year > 0) yoeLong else Math.subtractExact(1, yoeLong)
          )
        }
      } else if (era.longValue == 1L) {
        updateResolveMap(fieldValues, YEAR, yoeLong)
      } else if (era.longValue == 0L) {
        updateResolveMap(fieldValues, YEAR, Math.subtractExact(1, yoeLong))
      } else {
        throw new DateTimeException(s"Invalid value for era: $era")
      }
    } else if (fieldValues.containsKey(ERA)) {
      ERA.checkValidValue(fieldValues.get(ERA))
    }
    if (fieldValues.containsKey(YEAR)) {
      if (fieldValues.containsKey(MONTH_OF_YEAR)) {
        if (fieldValues.containsKey(DAY_OF_MONTH)) {
          val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
            val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_MONTH), 1)
            return date(y, 1, 1).plusMonths(months).plusDays(days)
          } else {
            val moy: Int = range(MONTH_OF_YEAR)
              .checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR), MONTH_OF_YEAR)
            var dom: Int =
              range(DAY_OF_MONTH).checkValidIntValue(fieldValues.remove(DAY_OF_MONTH), DAY_OF_MONTH)
            if ((resolverStyle eq ResolverStyle.SMART) && dom > 28) {
              dom = Math.min(dom, date(y, moy, 1).lengthOfMonth)
            }
            return date(y, moy, dom)
          }
        }
        if (fieldValues.containsKey(ALIGNED_WEEK_OF_MONTH)) {
          if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_MONTH)) {
            val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long =
                Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val ad: Int = ALIGNED_DAY_OF_WEEK_IN_MONTH.checkValidIntValue(
              fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_MONTH)
            )
            val minguoDate: MinguoDate = date(y, moy, 1).plus((aw.toLong - 1) * 7 + (ad - 1), DAYS)
            if ((resolverStyle eq ResolverStyle.STRICT) && minguoDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return minguoDate
          }
          if (fieldValues.containsKey(DAY_OF_WEEK)) {
            val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
            if (resolverStyle eq ResolverStyle.LENIENT) {
              val months: Long = Math.subtractExact(fieldValues.remove(MONTH_OF_YEAR), 1)
              val weeks: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_MONTH), 1)
              val days: Long   = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
              return date(y, 1, 1).plus(months, MONTHS).plus(weeks, WEEKS).plus(days, DAYS)
            }
            val moy: Int = MONTH_OF_YEAR.checkValidIntValue(fieldValues.remove(MONTH_OF_YEAR))
            val aw: Int =
              ALIGNED_WEEK_OF_MONTH.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_MONTH))
            val dow: Int = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
            val minguoDate: MinguoDate =
              date(y, moy, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
            if ((resolverStyle eq ResolverStyle.STRICT) && minguoDate.get(MONTH_OF_YEAR) != moy)
              throw new DateTimeException("Strict mode rejected date parsed to a different month")
            return minguoDate
          }
        }
      }
      if (fieldValues.containsKey(DAY_OF_YEAR)) {
        val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val days: Long = Math.subtractExact(fieldValues.remove(DAY_OF_YEAR), 1)
          return dateYearDay(y, 1).plusDays(days)
        }
        val doy: Int = DAY_OF_YEAR.checkValidIntValue(fieldValues.remove(DAY_OF_YEAR))
        return dateYearDay(y, doy)
      }
      if (fieldValues.containsKey(ALIGNED_WEEK_OF_YEAR)) {
        if (fieldValues.containsKey(ALIGNED_DAY_OF_WEEK_IN_YEAR)) {
          val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val ad: Int = ALIGNED_DAY_OF_WEEK_IN_YEAR.checkValidIntValue(
            fieldValues.remove(ALIGNED_DAY_OF_WEEK_IN_YEAR)
          )
          val minguoDate: MinguoDate = date(y, 1, 1).plusDays((aw.toLong - 1) * 7 + (ad - 1))
          if ((resolverStyle eq ResolverStyle.STRICT) && minguoDate.get(YEAR) != y) {
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
          }
          return minguoDate
        }
        if (fieldValues.containsKey(DAY_OF_WEEK)) {
          val y: Int = YEAR.checkValidIntValue(fieldValues.remove(YEAR))
          if (resolverStyle eq ResolverStyle.LENIENT) {
            val weeks: Long = Math.subtractExact(fieldValues.remove(ALIGNED_WEEK_OF_YEAR), 1)
            val days: Long  = Math.subtractExact(fieldValues.remove(DAY_OF_WEEK), 1)
            return date(y, 1, 1).plus(weeks, WEEKS).plus(days, DAYS)
          }
          val aw: Int =
            ALIGNED_WEEK_OF_YEAR.checkValidIntValue(fieldValues.remove(ALIGNED_WEEK_OF_YEAR))
          val dow: Int = DAY_OF_WEEK.checkValidIntValue(fieldValues.remove(DAY_OF_WEEK))
          val minguoDate: MinguoDate =
            date(y, 1, 1).plus(aw.toLong - 1, WEEKS).`with`(nextOrSame(DayOfWeek.of(dow)))
          if ((resolverStyle eq ResolverStyle.STRICT) && minguoDate.get(YEAR) != y) {
            throw new DateTimeException("Strict mode rejected date parsed to a different month")
          }
          return minguoDate
        }
      }
    }
    null
  }
}
