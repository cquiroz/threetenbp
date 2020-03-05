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
package org.threeten.bp.temporal

import org.threeten.bp.DayOfWeek.THURSDAY
import org.threeten.bp.DayOfWeek.WEDNESDAY
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.EPOCH_DAY
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.FOREVER
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import java.util.{ Locale, Objects }
import org.threeten.bp.Duration
import org.threeten.bp.LocalDate
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.ResolverStyle

/** Fields and units specific to the ISO-8601 calendar system,
  * including quarter-of-year and week-based-year.
  *
  * This class defines fields and units that are specific to the ISO calendar system.
  *
  * <h3>Quarter of year</h3>
  * The ISO-8601 standard is based on the standard civic 12 month year.
  * This is commonly divided into four quarters, often abbreviated as Q1, Q2, Q3 and Q4.
  *
  * January, February and March are in Q1.
  * April, May and June are in Q2.
  * July, August and September are in Q3.
  * October, November and December are in Q4.
  *
  * The complete date is expressed using three fields:
  *<ul>
  * <li>{@link #DAY_OF_QUARTER DAY_OF_QUARTER} - the day within the quarter, from 1 to 90, 91 or 92
  * <li>{@link #QUARTER_OF_YEAR QUARTER_OF_YEAR} - the week within the week-based-year
  * <li>{@link ChronoField#YEAR YEAR} - the standard ISO year
  * </ul><p>
  *
  * <h3>Week based years</h3>
  * The ISO-8601 standard was originally intended as a data interchange format,
  * defining a string format for dates and times. However, it also defines an
  * alternate way of expressing the date, based on the concept of week-based-year.
  *
  * The date is expressed using three fields:
  *<ul>
  * <li>{@link ChronoField#DAY_OF_WEEK DAY_OF_WEEK} - the standard field defining the
  * day-of-week from Monday (1) to Sunday (7)
  * <li>{@link #WEEK_OF_WEEK_BASED_YEAR} - the week within the week-based-year
  * <li>{@link #WEEK_BASED_YEAR WEEK_BASED_YEAR} - the week-based-year
  * </ul><p>
  * The week-based-year itself is defined relative to the standard ISO proleptic year.
  * It differs from the standard year in that it always starts on a Monday.
  *
  * The first week of a week-based-year is the first Monday-based week of the standard
  * ISO year that has at least 4 days in the new year.
  *<ul>
  * <li>If January 1st is Monday then week 1 starts on January 1st
  * <li>If January 1st is Tuesday then week 1 starts on December 31st of the previous standard year
  * <li>If January 1st is Wednesday then week 1 starts on December 30th of the previous standard year
  * <li>If January 1st is Thursday then week 1 starts on December 29th of the previous standard year
  * <li>If January 1st is Friday then week 1 starts on January 4th
  * <li>If January 1st is Saturday then week 1 starts on January 3rd
  * <li>If January 1st is Sunday then week 1 starts on January 2nd
  * </ul><p>
  * There are 52 weeks in most week-based years, however on occasion there are 53 weeks.
  *
  * For example:
  *
  * <table cellpadding="0" cellspacing="3" border="0" style="text-align: left; width: 50%;">
  * <caption>Examples of Week based Years</caption>
  * <tr><th>Date</th><th>Day-of-week</th><th>Field values</th></tr>
  * <tr><th>2008-12-28</th><td>Sunday</td><td>Week 52 of week-based-year 2008</td></tr>
  * <tr><th>2008-12-29</th><td>Monday</td><td>Week 1 of week-based-year 2009</td></tr>
  * <tr><th>2008-12-31</th><td>Wednesday</td><td>Week 1 of week-based-year 2009</td></tr>
  * <tr><th>2009-01-01</th><td>Thursday</td><td>Week 1 of week-based-year 2009</td></tr>
  * <tr><th>2009-01-04</th><td>Sunday</td><td>Week 1 of week-based-year 2009</td></tr>
  * <tr><th>2009-01-05</th><td>Monday</td><td>Week 2 of week-based-year 2009</td></tr>
  * </table>
  *
  * <h3>Specification for implementors</h3>
  *
  * This class is immutable and thread-safe.
  */
object IsoFields {

  /** The field that represents the day-of-quarter.
    *
    * This field allows the day-of-quarter value to be queried and set.
    * The day-of-quarter has values from 1 to 90 in Q1 of a standard year, from 1 to 91
    * in Q1 of a leap year, from 1 to 91 in Q2 and from 1 to 92 in Q3 and Q4.
    *
    * The day-of-quarter can only be calculated if the day-of-year, month-of-year and year
    * are available.
    *
    * When setting this field, the value is allowed to be partially lenient, taking any
    * value from 1 to 92. If the quarter has less than 92 days, then day 92, and
    * potentially day 91, is in the following quarter.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val DAY_OF_QUARTER: TemporalField = Field.DAY_OF_QUARTER

  /** The field that represents the quarter-of-year.
    *
    * This field allows the quarter-of-year value to be queried and set.
    * The quarter-of-year has values from 1 to 4.
    *
    * The day-of-quarter can only be calculated if the month-of-year is available.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val QUARTER_OF_YEAR: TemporalField = Field.QUARTER_OF_YEAR

  /** The field that represents the week-of-week-based-year.
    *
    * This field allows the week of the week-based-year value to be queried and set.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val WEEK_OF_WEEK_BASED_YEAR: TemporalField = Field.WEEK_OF_WEEK_BASED_YEAR

  /** The field that represents the week-based-year.
    *
    * This field allows the week-based-year value to be queried and set.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val WEEK_BASED_YEAR: TemporalField = Field.WEEK_BASED_YEAR

  /** The unit that represents week-based-years for the purpose of addition and subtraction.
    *
    * This allows a number of week-based-years to be added to, or subtracted from, a date.
    * The unit is equal to either 52 or 53 weeks.
    * The estimated duration of a week-based-year is the same as that of a standard ISO
    * year at {@code 365.2425 Days}.
    *
    * The rules for addition add the number of week-based-years to the existing value
    * for the week-based-year field. If the resulting week-based-year only has 52 weeks,
    * then the date will be in week 1 of the following week-based-year.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val WEEK_BASED_YEARS: TemporalUnit = Unit.WEEK_BASED_YEARS

  /** Unit that represents the concept of a quarter-year.
    * For the ISO calendar system, it is equal to 3 months.
    * The estimated duration of a quarter-year is one quarter of {@code 365.2425 Days}.
    *
    * This unit is an immutable and thread-safe singleton.
    */
  lazy val QUARTER_YEARS: TemporalUnit = Unit.QUARTER_YEARS

  /** Implementation of the field. */
  private object Field {
    lazy val DAY_OF_QUARTER: Field = new Field("DAY_OF_QUARTER", 0) {
      override def toString: String  = "DayOfQuarter"
      def getBaseUnit: TemporalUnit  = DAYS
      def getRangeUnit: TemporalUnit = QUARTER_YEARS
      def range: ValueRange          = ValueRange.of(1, 90, 92)
      def isSupportedBy(temporal: TemporalAccessor): Boolean =
        temporal.isSupported(DAY_OF_YEAR) && temporal.isSupported(MONTH_OF_YEAR) && temporal
          .isSupported(YEAR) && isIso(temporal)
      def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = {
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        val qoy: Long = temporal.getLong(QUARTER_OF_YEAR)
        if (qoy == 1) {
          val year: Long = temporal.getLong(YEAR)
          return if (IsoChronology.INSTANCE.isLeapYear(year)) ValueRange.of(1, 91)
          else ValueRange.of(1, 90)
        } else if (qoy == 2) {
          return ValueRange.of(1, 91)
        } else if (qoy == 3 || qoy == 4) {
          return ValueRange.of(1, 92)
        }
        range
      }
      def getFrom(temporal: TemporalAccessor): Long = {
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        val doy: Int   = temporal.get(DAY_OF_YEAR)
        val moy: Int   = temporal.get(MONTH_OF_YEAR)
        val year: Long = temporal.getLong(YEAR)
        doy.toLong - QUARTER_DAYS(
          ((moy - 1) / 3) + (if (IsoChronology.INSTANCE.isLeapYear(year)) 4 else 0)
        ).toInt
      }
      def adjustInto[R <: Temporal](temporal: R, newValue: Long): R = {
        val curValue: Long = getFrom(temporal)
        range.checkValidValue(newValue, this)
        temporal
          .`with`(DAY_OF_YEAR, temporal.getLong(DAY_OF_YEAR) + (newValue - curValue))
          .asInstanceOf[R]
      }
      override def resolve(
        fieldValues:     java.util.Map[TemporalField, java.lang.Long],
        partialTemporal: TemporalAccessor,
        resolverStyle:   ResolverStyle
      ): TemporalAccessor = {
        val yearLong: java.lang.Long = fieldValues.get(YEAR)
        val qoyLong: java.lang.Long  = fieldValues.get(QUARTER_OF_YEAR)
        if (yearLong == null || qoyLong == null)
          return null
        val y: Int          = YEAR.checkValidIntValue(yearLong)
        val doq: Long       = fieldValues.get(DAY_OF_QUARTER)
        var date: LocalDate = null
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val qoy: Long = qoyLong
          date = LocalDate.of(y, 1, 1)
          date = date.plusMonths(Math.multiplyExact(Math.subtractExact(qoy, 1), 3))
          date = date.plusDays(Math.subtractExact(doq, 1))
        } else {
          val qoy: Int = QUARTER_OF_YEAR.range.checkValidIntValue(qoyLong, QUARTER_OF_YEAR)
          if (resolverStyle eq ResolverStyle.STRICT) {
            var max: Int = 92
            if (qoy == 1) {
              max = if (IsoChronology.INSTANCE.isLeapYear(y.toLong)) 91 else 90
            } else if (qoy == 2) {
              max = 91
            }
            ValueRange.of(1, max.toLong).checkValidValue(doq, this)
          } else {
            range.checkValidValue(doq, this)
          }
          date = LocalDate.of(y, ((qoy - 1) * 3) + 1, 1).plusDays(doq - 1)
        }
        fieldValues.remove(this)
        fieldValues.remove(YEAR)
        fieldValues.remove(QUARTER_OF_YEAR)
        date
      }
    }

    lazy val QUARTER_OF_YEAR: Field = new Field("QUARTER_OF_YEAR", 1) {
      override def toString: String  = "QuarterOfYear"
      def getBaseUnit: TemporalUnit  = QUARTER_YEARS
      def getRangeUnit: TemporalUnit = YEARS
      def range: ValueRange          = ValueRange.of(1, 4)
      def isSupportedBy(temporal: TemporalAccessor): Boolean =
        temporal.isSupported(MONTH_OF_YEAR) && isIso(temporal)
      def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = range
      def getFrom(temporal:        TemporalAccessor): Long = {
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        val moy: Long = temporal.getLong(MONTH_OF_YEAR)
        (moy + 2) / 3
      }
      def adjustInto[R <: Temporal](temporal: R, newValue: Long): R = {
        val curValue: Long = getFrom(temporal)
        range.checkValidValue(newValue, this)
        temporal
          .`with`(MONTH_OF_YEAR, temporal.getLong(MONTH_OF_YEAR) + (newValue - curValue) * 3)
          .asInstanceOf[R]
      }
    }

    lazy val WEEK_OF_WEEK_BASED_YEAR: Field = new Field("WEEK_OF_WEEK_BASED_YEAR", 2) {
      override def toString: String  = "WeekOfWeekBasedYear"
      def getBaseUnit: TemporalUnit  = WEEKS
      def getRangeUnit: TemporalUnit = WEEK_BASED_YEARS
      override def getDisplayName(locale: Locale): String = {
        Objects.requireNonNull(locale, "locale")
        "Week"
      }
      def range: ValueRange = ValueRange.of(1, 52, 53)
      def isSupportedBy(temporal: TemporalAccessor): Boolean =
        temporal.isSupported(EPOCH_DAY) && isIso(temporal)
      def rangeRefinedBy(temporal: TemporalAccessor): ValueRange =
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        else
          getWeekRange(LocalDate.from(temporal))
      def getFrom(temporal: TemporalAccessor): Long =
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        else
          getWeek(LocalDate.from(temporal)).toLong
      def adjustInto[R <: Temporal](temporal: R, newValue: Long): R = {
        range.checkValidValue(newValue, this)
        temporal.plus(Math.subtractExact(newValue, getFrom(temporal)), WEEKS).asInstanceOf[R]
      }
      override def resolve(
        fieldValues:     java.util.Map[TemporalField, java.lang.Long],
        partialTemporal: TemporalAccessor,
        resolverStyle:   ResolverStyle
      ): TemporalAccessor = {
        val wbyLong: java.lang.Long = fieldValues.get(WEEK_BASED_YEAR)
        val dowLong: java.lang.Long = fieldValues.get(DAY_OF_WEEK)
        if (wbyLong == null || dowLong == null)
          return null
        val wby: Int        = WEEK_BASED_YEAR.range.checkValidIntValue(wbyLong, WEEK_BASED_YEAR)
        val wowby: Long     = fieldValues.get(WEEK_OF_WEEK_BASED_YEAR)
        var date: LocalDate = null
        if (resolverStyle eq ResolverStyle.LENIENT) {
          var dow: Long   = dowLong
          var weeks: Long = 0
          if (dow > 7) {
            weeks = (dow - 1) / 7
            dow   = ((dow - 1) % 7) + 1
          } else if (dow < 1) {
            weeks = (dow / 7) - 1
            dow   = (dow % 7) + 7
          }
          date =
            LocalDate.of(wby, 1, 4).plusWeeks(wowby - 1).plusWeeks(weeks).`with`(DAY_OF_WEEK, dow)
        } else {
          val dow: Int = DAY_OF_WEEK.checkValidIntValue(dowLong)
          if (resolverStyle eq ResolverStyle.STRICT) {
            val temp: LocalDate   = LocalDate.of(wby, 1, 4)
            val range: ValueRange = getWeekRange(temp)
            range.checkValidValue(wowby, this)
          } else {
            range.checkValidValue(wowby, this)
          }
          date = LocalDate.of(wby, 1, 4).plusWeeks(wowby - 1).`with`(DAY_OF_WEEK, dow.toLong)
        }
        fieldValues.remove(this)
        fieldValues.remove(WEEK_BASED_YEAR)
        fieldValues.remove(DAY_OF_WEEK)
        date
      }
    }

    lazy val WEEK_BASED_YEAR: Field = new Field("WEEK_BASED_YEAR", 3) {
      override def toString: String  = "WeekBasedYear"
      def getBaseUnit: TemporalUnit  = WEEK_BASED_YEARS
      def getRangeUnit: TemporalUnit = FOREVER
      def range: ValueRange          = YEAR.range
      def isSupportedBy(temporal: TemporalAccessor): Boolean =
        temporal.isSupported(EPOCH_DAY) && isIso(temporal)
      def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = YEAR.range
      def getFrom(temporal:        TemporalAccessor): Long =
        if (!temporal.isSupported(this))
          throw UnsupportedTemporalTypeException.field(this)
        else getWeekBasedYear(LocalDate.from(temporal)).toLong
      def adjustInto[R <: Temporal](temporal: R, newValue: Long): R = {
        if (!isSupportedBy(temporal))
          throw UnsupportedTemporalTypeException.field(this)
        val newWby: Int     = range.checkValidIntValue(newValue, WEEK_BASED_YEAR)
        val date: LocalDate = LocalDate.from(temporal)
        val dow: Int        = date.get(DAY_OF_WEEK)
        var week: Int       = getWeek(date)
        if (week == 53 && getWeekRange(newWby) == 52) {
          week = 52
        }
        var resolved: LocalDate = LocalDate.of(newWby, 1, 4)
        val days: Int           = (dow - resolved.get(DAY_OF_WEEK)) + ((week - 1) * 7)
        resolved = resolved.plusDays(days.toLong)
        temporal.`with`(resolved).asInstanceOf[R]
      }
    }

    private lazy val QUARTER_DAYS: Array[Int] = Array(0, 90, 181, 273, 0, 91, 182, 274)

    private def isIso(temporal: TemporalAccessor): Boolean =
      Chronology.from(temporal) == IsoChronology.INSTANCE

    private def getWeekRange(date: LocalDate): ValueRange = {
      val wby: Int = getWeekBasedYear(date)
      ValueRange.of(1, getWeekRange(wby).toLong)
    }

    private def getWeekRange(wby: Int): Int = {
      val date: LocalDate = LocalDate.of(wby, 1, 1)
      if ((date.getDayOfWeek eq THURSDAY) || ((date.getDayOfWeek eq WEDNESDAY) && date.isLeapYear))
        53
      else 52
    }

    private def getWeek(date: LocalDate): Int = {
      val dow0: Int         = date.getDayOfWeek.ordinal
      val doy0: Int         = date.getDayOfYear - 1
      val doyThu0: Int      = doy0 + (3 - dow0)
      val alignedWeek: Int  = doyThu0 / 7
      val firstThuDoy0: Int = doyThu0 - (alignedWeek * 7)
      var firstMonDoy0: Int = firstThuDoy0 - 3
      if (firstMonDoy0 < -3) {
        firstMonDoy0 += 7
      }
      if (doy0 < firstMonDoy0)
        return getWeekRange(date.withDayOfYear(180).minusYears(1)).getMaximum.toInt
      var week: Int = ((doy0 - firstMonDoy0) / 7) + 1
      if (week == 53) {
        if (!(firstMonDoy0 == -3 || firstMonDoy0 == -2 && date.isLeapYear))
          week = 1
      }
      week
    }

    private def getWeekBasedYear(date: LocalDate): Int = {
      var year: Int = date.getYear
      var doy: Int  = date.getDayOfYear
      if (doy <= 3) {
        val dow: Int = date.getDayOfWeek.ordinal
        if (doy - dow < -2)
          year -= 1
      } else if (doy >= 363) {
        val dow: Int = date.getDayOfWeek.ordinal
        doy = doy - 363 - (if (date.isLeapYear) 1 else 0)
        if (doy - dow >= 0)
          year += 1
      }
      year
    }
  }

  private sealed abstract class Field(name: String, ordinal: Int)
      extends Enum[Field](name, ordinal)
      with TemporalField {
    def getDisplayName(locale: Locale): String = {
      Objects.requireNonNull(locale, "locale")
      toString
    }

    def resolve(
      fieldValues:     java.util.Map[TemporalField, java.lang.Long],
      partialTemporal: TemporalAccessor,
      resolverStyle:   ResolverStyle
    ): TemporalAccessor =
      null

    def isDateBased: Boolean = true

    def isTimeBased: Boolean = false
  }

  /** Implementation of the period unit. */
  private object Unit {
    lazy val WEEK_BASED_YEARS = new Unit("WeekBasedYears", 0, Duration.ofSeconds(31556952L))
    lazy val QUARTER_YEARS    = new Unit("QuarterYears", 1, Duration.ofSeconds(31556952L / 4))
  }

  /// !!! FIXME: Passing of name to the Enum constructor is not quite right.
  //             We should have a look at the compiled code to figure out what's happening exactly in the Java version.
  private final class Unit(name: String, ordinal: Int, private val duration: Duration)
      extends Enum[Unit](name, ordinal)
      with TemporalUnit {

    def getDuration: Duration = duration

    def isDurationEstimated: Boolean = true

    def isDateBased: Boolean = true

    def isTimeBased: Boolean = false

    def isSupportedBy(temporal: Temporal): Boolean = temporal.isSupported(EPOCH_DAY)

    def addTo[R <: Temporal](temporal: R, periodToAdd: Long): R =
      this match {
        case Unit.WEEK_BASED_YEARS =>
          val added: Long = Math.addExact(temporal.get(WEEK_BASED_YEAR).toLong, periodToAdd)
          temporal.`with`(WEEK_BASED_YEAR, added).asInstanceOf[R]
        case Unit.QUARTER_YEARS =>
          temporal
            .plus(periodToAdd / 256, YEARS)
            .plus((periodToAdd % 256) * 3, MONTHS)
            .asInstanceOf[R]
        case _ => throw new IllegalStateException("Unreachable")
      }

    def between(temporal1: Temporal, temporal2: Temporal): Long =
      this match {
        case Unit.WEEK_BASED_YEARS =>
          Math.subtractExact(temporal2.getLong(WEEK_BASED_YEAR), temporal1.getLong(WEEK_BASED_YEAR))
        case Unit.QUARTER_YEARS => temporal1.until(temporal2, MONTHS) / 3
        case _                  => throw new IllegalStateException("Unreachable")
      }

    override def toString: String = name
  }
}
