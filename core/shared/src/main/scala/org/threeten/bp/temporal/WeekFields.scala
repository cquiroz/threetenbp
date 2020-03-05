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

import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.YEAR
import org.threeten.bp.temporal.ChronoUnit.DAYS
import org.threeten.bp.temporal.ChronoUnit.MONTHS
import org.threeten.bp.temporal.ChronoUnit.WEEKS
import org.threeten.bp.temporal.ChronoUnit.YEARS
import java.io.InvalidObjectException
import java.io.Serializable
import java.util.{ GregorianCalendar, Locale, Objects }
import java.util.HashMap
import java.util.Map
import org.threeten.bp.DateTimeException
import org.threeten.bp.DayOfWeek
import org.threeten.bp.Year
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.format.ResolverStyle

@SerialVersionUID(-1177360819670808121L)
object WeekFields {

  /** The cache of rules by firstDayOfWeek plus minimalDays.
    * Initialized first to be available for definition of ISO, etc.
    */
  private lazy val CACHE: Map[String, WeekFields] =
    new HashMap[String, WeekFields]()

  /** The ISO-8601 definition, where a week starts on Monday and the first week
    * has a minimum of 4 days.
    *
    * The ISO-8601 standard defines a calendar system based on weeks.
    * It uses the week-based-year and week-of-week-based-year concepts to split
    * up the passage of days instead of the standard year/month/day.
    *
    * Note that the first week may start in the previous calendar year.
    * Note also that the first few days of a calendar year may be in the
    * week-based-year corresponding to the previous calendar year.
    */
  lazy val ISO: WeekFields = new WeekFields(DayOfWeek.MONDAY, 4)

  /** The common definition of a week that starts on Sunday.
    *
    * Defined as starting on Sunday and with a minimum of 1 day in the month.
    * This week definition is in use in the US and other European countries.
    *
    */
  lazy val SUNDAY_START: WeekFields = WeekFields.of(DayOfWeek.SUNDAY, 1)

  /** Obtains an instance of {@code WeekFields} appropriate for a locale.
    *
    * This will look up appropriate values from the provider of localization data.
    *
    * @param locale  the locale to use, not null
    * @return the week-definition, not null
    */
  def of(locale: Locale): WeekFields = {
    Objects.requireNonNull(locale, "locale")
    val newLocale               = new Locale(locale.getLanguage, locale.getCountry)
    val gcal: GregorianCalendar = new GregorianCalendar(newLocale)
    val calDow: Int             = gcal.getFirstDayOfWeek
    val dow: DayOfWeek          = DayOfWeek.SUNDAY.plus(calDow.toLong - 1)
    val minDays: Int            = gcal.getMinimalDaysInFirstWeek
    WeekFields.of(dow, minDays)
  }

  /** Obtains an instance of {@code WeekFields} from the first day-of-week and minimal days.
    *
    * The first day-of-week defines the ISO {@code DayOfWeek} that is day 1 of the week.
    * The minimal number of days in the first week defines how many days must be present
    * in a month or year, starting from the first day-of-week, before the week is counted
    * as the first week. A value of 1 will count the first day of the month or year as part
    * of the first week, whereas a value of 7 will require the whole seven days to be in
    * the new month or year.
    *
    * WeekFields instances are singletons; for each unique combination
    * of {@code firstDayOfWeek} and {@code minimalDaysInFirstWeek} the
    * the same instance will be returned.
    *
    * @param firstDayOfWeek  the first day of the week, not null
    * @param minimalDaysInFirstWeek  the minimal number of days in the first week, from 1 to 7
    * @return the week-definition, not null
    * @throws IllegalArgumentException if the minimal days value is less than one
    *                                  or greater than 7
    */
  def of(firstDayOfWeek: DayOfWeek, minimalDaysInFirstWeek: Int): WeekFields = {
    val key: String       = firstDayOfWeek.toString + minimalDaysInFirstWeek
    var rules: WeekFields = CACHE.get(key)
    if (rules == null) {
      rules = new WeekFields(firstDayOfWeek, minimalDaysInFirstWeek)
      CACHE.put(key, rules)
      rules = CACHE.get(key)
    }
    rules
  }

  /** Field type that computes DayOfWeek, WeekOfMonth, and WeekOfYear
    * based on a WeekFields.
    * A separate Field instance is required for each different WeekFields;
    * combination of start of week and minimum number of days.
    * Constructors are provided to create fields for DayOfWeek, WeekOfMonth,
    * and WeekOfYear.
    */
  private[temporal] object ComputedDayOfField {

    /** Returns a field to access the day of week,
      * computed based on a WeekFields.
      *
      * The WeekDefintion of the first day of the week is used with
      * the ISO DAY_OF_WEEK field to compute week boundaries.
      */
    private[temporal] def ofDayOfWeekField(weekDef: WeekFields): WeekFields.ComputedDayOfField =
      new WeekFields.ComputedDayOfField("DayOfWeek",
                                        weekDef,
                                        ChronoUnit.DAYS,
                                        ChronoUnit.WEEKS,
                                        DAY_OF_WEEK_RANGE)

    /** Returns a field to access the week of month,
      * computed based on a WeekFields.
      * @see WeekFields#weekOfMonth()
      */
    private[temporal] def ofWeekOfMonthField(weekDef: WeekFields): WeekFields.ComputedDayOfField =
      new WeekFields.ComputedDayOfField("WeekOfMonth",
                                        weekDef,
                                        ChronoUnit.WEEKS,
                                        ChronoUnit.MONTHS,
                                        WEEK_OF_MONTH_RANGE)

    /** Returns a field to access the week of year,
      * computed based on a WeekFields.
      * @see WeekFields#weekOfYear()
      */
    private[temporal] def ofWeekOfYearField(weekDef: WeekFields): WeekFields.ComputedDayOfField =
      new WeekFields.ComputedDayOfField("WeekOfYear",
                                        weekDef,
                                        ChronoUnit.WEEKS,
                                        ChronoUnit.YEARS,
                                        WEEK_OF_YEAR_RANGE)

    /** Returns a field to access the week of week-based-year,
      * computed based on a WeekFields.
      * @see WeekFields#weekOfWeekBasedYear()
      */
    private[temporal] def ofWeekOfWeekBasedYearField(
      weekDef: WeekFields
    ): WeekFields.ComputedDayOfField =
      new WeekFields.ComputedDayOfField("WeekOfWeekBasedYear",
                                        weekDef,
                                        ChronoUnit.WEEKS,
                                        IsoFields.WEEK_BASED_YEARS,
                                        WEEK_OF_WEEK_BASED_YEAR_RANGE)

    /** Returns a field to access the week-based-year,
      * computed based on a WeekFields.
      * @see WeekFields#weekBasedYear()
      */
    private[temporal] def ofWeekBasedYearField(weekDef: WeekFields): WeekFields.ComputedDayOfField =
      new WeekFields.ComputedDayOfField("WeekBasedYear",
                                        weekDef,
                                        IsoFields.WEEK_BASED_YEARS,
                                        ChronoUnit.FOREVER,
                                        WEEK_BASED_YEAR_RANGE)

    private def DAY_OF_WEEK_RANGE: ValueRange             = ValueRange.of(1, 7)
    private def WEEK_OF_MONTH_RANGE: ValueRange           = ValueRange.of(0, 1, 4, 6)
    private def WEEK_OF_YEAR_RANGE: ValueRange            = ValueRange.of(0, 1, 52, 54)
    private def WEEK_OF_WEEK_BASED_YEAR_RANGE: ValueRange = ValueRange.of(1, 52, 53)
    private def WEEK_BASED_YEAR_RANGE: ValueRange         = YEAR.range
  }

  private[temporal] class ComputedDayOfField(
    val name:      String,
    val weekDef:   WeekFields,
    val baseUnit:  TemporalUnit,
    val rangeUnit: TemporalUnit,
    val range:     ValueRange
  ) extends TemporalField {

    def getFrom(temporal: TemporalAccessor): Long = {
      val sow: Int    = weekDef.getFirstDayOfWeek.getValue
      val isoDow: Int = temporal.get(ChronoField.DAY_OF_WEEK)
      val dow: Int    = Math.floorMod(isoDow - sow, 7) + 1
      if (rangeUnit eq ChronoUnit.WEEKS)
        dow.toLong
      else if (rangeUnit eq ChronoUnit.MONTHS) {
        val dom: Int    = temporal.get(ChronoField.DAY_OF_MONTH)
        val offset: Int = startOfWeekOffset(dom, dow)
        computeWeek(offset, dom).toLong
      } else if (rangeUnit eq ChronoUnit.YEARS) {
        val doy: Int    = temporal.get(ChronoField.DAY_OF_YEAR)
        val offset: Int = startOfWeekOffset(doy, dow)
        computeWeek(offset, doy).toLong
      } else if (rangeUnit eq IsoFields.WEEK_BASED_YEARS)
        localizedWOWBY(temporal).toLong
      else if (rangeUnit eq ChronoUnit.FOREVER)
        localizedWBY(temporal).toLong
      else
        throw new IllegalStateException("unreachable")
    }

    private def localizedDayOfWeek(temporal: TemporalAccessor, sow: Int): Int = {
      val isoDow: Int = temporal.get(DAY_OF_WEEK)
      Math.floorMod(isoDow - sow, 7) + 1
    }

    private def localizedWeekOfMonth(temporal: TemporalAccessor, dow: Int): Long = {
      val dom: Int    = temporal.get(DAY_OF_MONTH)
      val offset: Int = startOfWeekOffset(dom, dow)
      computeWeek(offset, dom).toLong
    }

    private def localizedWeekOfYear(temporal: TemporalAccessor, dow: Int): Long = {
      val doy: Int    = temporal.get(DAY_OF_YEAR)
      val offset: Int = startOfWeekOffset(doy, dow)
      computeWeek(offset, doy).toLong
    }

    private def localizedWOWBY(temporal: TemporalAccessor): Int = {
      val sow: Int    = weekDef.getFirstDayOfWeek.getValue
      val isoDow: Int = temporal.get(DAY_OF_WEEK)
      val dow: Int    = Math.floorMod(isoDow - sow, 7) + 1
      val woy: Long   = localizedWeekOfYear(temporal, dow)
      if (woy == 0) {
        val previous: ChronoLocalDate =
          Chronology.from(temporal).date(temporal).minus(1, ChronoUnit.WEEKS)
        return localizedWeekOfYear(previous, dow).toInt + 1
      } else if (woy >= 53) {
        val offset: Int  = startOfWeekOffset(temporal.get(DAY_OF_YEAR), dow)
        val year: Int    = temporal.get(YEAR)
        val yearLen: Int = if (Year.isLeap(year.toLong)) 366 else 365
        val weekIndexOfFirstWeekNextYear: Int =
          computeWeek(offset, yearLen + weekDef.getMinimalDaysInFirstWeek)
        if (woy >= weekIndexOfFirstWeekNextYear) {
          return (woy - (weekIndexOfFirstWeekNextYear - 1)).toInt
        }
      }
      woy.toInt
    }

    private def localizedWBY(temporal: TemporalAccessor): Int = {
      val sow: Int    = weekDef.getFirstDayOfWeek.getValue
      val isoDow: Int = temporal.get(DAY_OF_WEEK)
      val dow: Int    = Math.floorMod(isoDow - sow, 7) + 1
      val year: Int   = temporal.get(YEAR)
      val woy: Long   = localizedWeekOfYear(temporal, dow)
      if (woy == 0)
        return year - 1
      else if (woy < 53)
        return year
      val offset: Int  = startOfWeekOffset(temporal.get(DAY_OF_YEAR), dow)
      val yearLen: Int = if (Year.isLeap(year.toLong)) 366 else 365
      val weekIndexOfFirstWeekNextYear: Int =
        computeWeek(offset, yearLen + weekDef.getMinimalDaysInFirstWeek)
      if (woy >= weekIndexOfFirstWeekNextYear)
        return year + 1
      year
    }

    /** Returns an offset to align week start with a day of month or day of year.
      *
      * @param day the day; 1 through infinity
      * @param dow the day of the week of that day; 1 through 7
      * @return an offset in days to align a day with the start of the first 'full' week
      */
    private def startOfWeekOffset(day: Int, dow: Int): Int = {
      val weekStart: Int = Math.floorMod(day - dow, 7)
      var offset: Int    = -weekStart
      if (weekStart + 1 > weekDef.getMinimalDaysInFirstWeek)
        offset = 7 - weekStart
      offset
    }

    /** Returns the week number computed from the reference day and reference dayOfWeek.
      *
      * @param offset the offset to align a date with the start of week
      *               from { @link #startOfWeekOffset}.
      * @param day  the day for which to compute the week number
      * @return the week number where zero is used for a partial week and 1 for the first full week
      */
    private def computeWeek(offset: Int, day: Int): Int = (7 + offset + (day - 1)) / 7

    def adjustInto[R <: Temporal](temporal: R, newValue: Long): R = {
      val newVal: Int     = range.checkValidIntValue(newValue, this)
      val currentVal: Int = temporal.get(this)
      if (newVal == currentVal)
        return temporal
      if (rangeUnit eq ChronoUnit.FOREVER) {
        val baseWowby: Int   = temporal.get(weekDef.weekOfWeekBasedYear)
        val diffWeeks: Long  = ((newValue - currentVal) * 52.1775).toLong
        var result: Temporal = temporal.plus(diffWeeks, ChronoUnit.WEEKS)
        if (result.get(this) > newVal) {
          val newWowby: Int = result.get(weekDef.weekOfWeekBasedYear)
          result = result.minus(newWowby.toLong, ChronoUnit.WEEKS)
        } else {
          if (result.get(this) < newVal)
            result = result.plus(2, ChronoUnit.WEEKS)
          val newWowby: Int = result.get(weekDef.weekOfWeekBasedYear)
          result = result.plus(baseWowby.toLong - newWowby.toLong, ChronoUnit.WEEKS)
          if (result.get(this) > newVal)
            result = result.minus(1, ChronoUnit.WEEKS)
        }
        return result.asInstanceOf[R]
      }
      val delta: Int = newVal - currentVal
      temporal.plus(delta.toLong, baseUnit).asInstanceOf[R]
    }

    def resolve(
      fieldValues:     java.util.Map[TemporalField, java.lang.Long],
      partialTemporal: TemporalAccessor,
      resolverStyle:   ResolverStyle
    ): TemporalAccessor = {
      val sow: Int = weekDef.getFirstDayOfWeek.getValue
      if (rangeUnit eq WEEKS) {
        val value: Long   = fieldValues.remove(this)
        val localDow: Int = range.checkValidIntValue(value, this)
        val isoDow: Int   = Math.floorMod((sow - 1) + (localDow - 1), 7) + 1
        fieldValues.put(DAY_OF_WEEK, isoDow.toLong)
        return null
      }
      if (!fieldValues.containsKey(DAY_OF_WEEK))
        return null
      if (rangeUnit eq ChronoUnit.FOREVER) {
        if (!fieldValues.containsKey(weekDef.weekOfWeekBasedYear))
          return null
        val chrono: Chronology    = Chronology.from(partialTemporal)
        val isoDow: Int           = DAY_OF_WEEK.checkValidIntValue(fieldValues.get(DAY_OF_WEEK))
        val dow: Int              = Math.floorMod(isoDow - sow, 7) + 1
        val wby: Int              = range.checkValidIntValue(fieldValues.get(this), this)
        var date: ChronoLocalDate = null
        var days: Long            = 0L
        if (resolverStyle eq ResolverStyle.LENIENT) {
          date = chrono.date(wby, 1, weekDef.getMinimalDaysInFirstWeek)
          val wowby: Long  = fieldValues.get(weekDef.weekOfWeekBasedYear)
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val weeks: Long  = wowby - localizedWeekOfYear(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        } else {
          date = chrono.date(wby, 1, weekDef.getMinimalDaysInFirstWeek)
          val wowby: Long = weekDef.weekOfWeekBasedYear.range
            .checkValidIntValue(
              fieldValues.get(weekDef.weekOfWeekBasedYear),
              weekDef.weekOfWeekBasedYear
            )
            .toLong
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val weeks: Long  = wowby - localizedWeekOfYear(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        }
        date = date.plus(days, DAYS)
        if (resolverStyle eq ResolverStyle.STRICT) {
          if (date.getLong(this) != fieldValues.get(this))
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
        }
        fieldValues.remove(this)
        fieldValues.remove(weekDef.weekOfWeekBasedYear)
        fieldValues.remove(DAY_OF_WEEK)
        return date
      }
      if (!fieldValues.containsKey(YEAR))
        return null
      val isoDow: Int        = DAY_OF_WEEK.checkValidIntValue(fieldValues.get(DAY_OF_WEEK))
      val dow: Int           = Math.floorMod(isoDow - sow, 7) + 1
      val year: Int          = YEAR.checkValidIntValue(fieldValues.get(YEAR))
      val chrono: Chronology = Chronology.from(partialTemporal)
      if (rangeUnit eq MONTHS) {
        if (!fieldValues.containsKey(MONTH_OF_YEAR))
          return null
        val value: Long           = fieldValues.remove(this)
        var date: ChronoLocalDate = null
        var days: Long            = 0L
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val month: Long = fieldValues.get(MONTH_OF_YEAR)
          date = chrono.date(year, 1, 1)
          date = date.plus(month - 1, MONTHS)
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val weeks: Long  = value - localizedWeekOfMonth(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        } else {
          val month: Int = MONTH_OF_YEAR.checkValidIntValue(fieldValues.get(MONTH_OF_YEAR))
          date = chrono.date(year, month, 8)
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val wom: Int     = range.checkValidIntValue(value, this)
          val weeks: Long  = wom - localizedWeekOfMonth(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        }
        date = date.plus(days, DAYS)
        if (resolverStyle eq ResolverStyle.STRICT) {
          if (date.getLong(MONTH_OF_YEAR) != fieldValues.get(MONTH_OF_YEAR))
            throw new DateTimeException("Strict mode rejected date parsed to a different month")
        }
        fieldValues.remove(this)
        fieldValues.remove(YEAR)
        fieldValues.remove(MONTH_OF_YEAR)
        fieldValues.remove(DAY_OF_WEEK)
        date
      } else if (rangeUnit eq YEARS) {
        val value: Long           = fieldValues.remove(this)
        var date: ChronoLocalDate = chrono.date(year, 1, 1)
        var days: Long            = 0L
        if (resolverStyle eq ResolverStyle.LENIENT) {
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val weeks: Long  = value - localizedWeekOfYear(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        } else {
          val dateDow: Int = localizedDayOfWeek(date, sow)
          val woy: Int     = range.checkValidIntValue(value, this)
          val weeks: Long  = woy - localizedWeekOfYear(date, dateDow)
          days = weeks * 7 + (dow - dateDow)
        }
        date = date.plus(days, DAYS)
        if (resolverStyle eq ResolverStyle.STRICT) {
          if (date.getLong(YEAR) != fieldValues.get(YEAR))
            throw new DateTimeException("Strict mode rejected date parsed to a different year")
        }
        fieldValues.remove(this)
        fieldValues.remove(YEAR)
        fieldValues.remove(DAY_OF_WEEK)
        date
      } else
        throw new IllegalStateException("unreachable")
    }

    def getBaseUnit: TemporalUnit = baseUnit

    def getRangeUnit: TemporalUnit = rangeUnit

    def isDateBased: Boolean = true

    def isTimeBased: Boolean = false

    def isSupportedBy(temporal: TemporalAccessor): Boolean =
      if (temporal.isSupported(ChronoField.DAY_OF_WEEK))
        if (rangeUnit eq ChronoUnit.WEEKS)
          true
        else if (rangeUnit eq ChronoUnit.MONTHS)
          temporal.isSupported(ChronoField.DAY_OF_MONTH)
        else if (rangeUnit eq ChronoUnit.YEARS)
          temporal.isSupported(ChronoField.DAY_OF_YEAR)
        else if (rangeUnit eq IsoFields.WEEK_BASED_YEARS)
          temporal.isSupported(ChronoField.EPOCH_DAY)
        else if (rangeUnit eq ChronoUnit.FOREVER)
          temporal.isSupported(ChronoField.EPOCH_DAY)
        else false
      else
        false

    def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = {
      if (rangeUnit eq ChronoUnit.WEEKS)
        return range
      var field: TemporalField = null
      if (rangeUnit eq ChronoUnit.MONTHS)
        field = ChronoField.DAY_OF_MONTH
      else if (rangeUnit eq ChronoUnit.YEARS)
        field = ChronoField.DAY_OF_YEAR
      else if (rangeUnit eq IsoFields.WEEK_BASED_YEARS)
        return rangeWOWBY(temporal)
      else if (rangeUnit eq ChronoUnit.FOREVER)
        return temporal.range(YEAR)
      else
        throw new IllegalStateException("unreachable")
      val sow: Int               = weekDef.getFirstDayOfWeek.getValue
      val isoDow: Int            = temporal.get(ChronoField.DAY_OF_WEEK)
      val dow: Int               = Math.floorMod(isoDow - sow, 7) + 1
      val offset: Int            = startOfWeekOffset(temporal.get(field), dow)
      val fieldRange: ValueRange = temporal.range(field)
      ValueRange.of(computeWeek(offset, fieldRange.getMinimum.toInt).toLong,
                    computeWeek(offset, fieldRange.getMaximum.toInt).toLong)
    }

    private def rangeWOWBY(temporal: TemporalAccessor): ValueRange = {
      val sow: Int    = weekDef.getFirstDayOfWeek.getValue
      val isoDow: Int = temporal.get(DAY_OF_WEEK)
      val dow: Int    = Math.floorMod(isoDow - sow, 7) + 1
      val woy: Long   = localizedWeekOfYear(temporal, dow)
      if (woy == 0)
        return rangeWOWBY(Chronology.from(temporal).date(temporal).minus(2, ChronoUnit.WEEKS))
      val offset: Int  = startOfWeekOffset(temporal.get(DAY_OF_YEAR), dow)
      val year: Int    = temporal.get(YEAR)
      val yearLen: Int = if (Year.isLeap(year.toLong)) 366 else 365
      val weekIndexOfFirstWeekNextYear: Int =
        computeWeek(offset, yearLen + weekDef.getMinimalDaysInFirstWeek)
      if (woy >= weekIndexOfFirstWeekNextYear)
        return rangeWOWBY(Chronology.from(temporal).date(temporal).plus(2, ChronoUnit.WEEKS))
      ValueRange.of(1, weekIndexOfFirstWeekNextYear.toLong - 1)
    }

    def getDisplayName(locale: Locale): String = {
      Objects.requireNonNull(locale, "locale")
      if (rangeUnit eq YEARS) "Week"
      else toString
    }

    override def toString: String = s"$name[${weekDef.toString}]"
  }

}

/** Localized definitions of the day-of-week, week-of-month and week-of-year fields.
  *
  * A standard week is seven days long, but cultures have different definitions for some
  * other aspects of a week. This class represents the definition of the week, for the
  * purpose of providing {@link TemporalField} instances.
  *
  * WeekFields provides three fields,
  * {@link #dayOfWeek()}, {@link #weekOfMonth()}, and {@link #weekOfYear()}
  * that provide access to the values from any {@link Temporal temporal object}.
  *
  * The computations for day-of-week, week-of-month, and week-of-year are based
  * on the  {@link ChronoField#YEAR proleptic-year},
  * {@link ChronoField#MONTH_OF_YEAR month-of-year},
  * {@link ChronoField#DAY_OF_MONTH day-of-month}, and
  * {@link ChronoField#DAY_OF_WEEK ISO day-of-week} which are based on the
  * {@link ChronoField#EPOCH_DAY epoch-day} and the chronology.
  * The values may not be aligned with the {@link ChronoField#YEAR_OF_ERA year-of-Era}
  * depending on the Chronology.
  *A week is defined by:
  * <ul>
  * <li>The first day-of-week.
  * For example, the ISO-8601 standard considers Monday to be the first day-of-week.
  * <li>The minimal number of days in the first week.
  * For example, the ISO-8601 standard counts the first week as needing at least 4 days.
  * </ul><p>
  * Together these two values allow a year or month to be divided into weeks.
  *
  * <h3>Week of Month</h3>
  * One field is used: week-of-month.
  * The calculation ensures that weeks never overlap a month boundary.
  * The month is divided into periods where each period starts on the defined first day-of-week.
  * The earliest period is referred to as week 0 if it has less than the minimal number of days
  * and week 1 if it has at least the minimal number of days.
  *
  * <table cellpadding="0" cellspacing="3" border="0" style="text-align: left; width: 50%;">
  * <caption>Examples of WeekFields</caption>
  * <tr><th>Date</th><td>Day-of-week</td>
  * <td>First day: Monday<br>Minimal days: 4</td><td>First day: Monday<br>Minimal days: 5</td></tr>
  * <tr><th>2008-12-31</th><td>Wednesday</td>
  * <td>Week 5 of December 2008</td><td>Week 5 of December 2008</td></tr>
  * <tr><th>2009-01-01</th><td>Thursday</td>
  * <td>Week 1 of January 2009</td><td>Week 0 of January 2009</td></tr>
  * <tr><th>2009-01-04</th><td>Sunday</td>
  * <td>Week 1 of January 2009</td><td>Week 0 of January 2009</td></tr>
  * <tr><th>2009-01-05</th><td>Monday</td>
  * <td>Week 2 of January 2009</td><td>Week 1 of January 2009</td></tr>
  * </table>
  *
  * <h3>Week of Year</h3>
  * One field is used: week-of-year.
  * The calculation ensures that weeks never overlap a year boundary.
  * The year is divided into periods where each period starts on the defined first day-of-week.
  * The earliest period is referred to as week 0 if it has less than the minimal number of days
  * and week 1 if it has at least the minimal number of days.
  *
  * This class is immutable and thread-safe.
  *
  * @constructor Creates an instance of the definition.
  *
  * @param firstDayOfWeek  the first day of the week, not null
  * @param minimalDays  the minimal number of days in the first week, from 1 to 7
  * @throws IllegalArgumentException if the minimal days value is invalid
  */
@SerialVersionUID(-1177360819670808121L)
final class WeekFields private (private val firstDayOfWeek: DayOfWeek, private val minimalDays: Int)
    extends Serializable {
  Objects.requireNonNull(firstDayOfWeek, "firstDayOfWeek")
  if (minimalDays < 1 || minimalDays > 7)
    throw new IllegalArgumentException("Minimal number of days is invalid")

  /** Returns a field to access the day of week based on this {@code WeekFields}.
    *
    * This is similar to {@link ChronoField#DAY_OF_WEEK} but uses values for
    * the day-of-week based on this {@code WeekFields}.
    * The days are numbered from 1 to 7 where the
    * {@link #getFirstDayOfWeek() first day-of-week} is assigned the value 1.
    *
    * For example, if the first day-of-week is Sunday, then that will have the
    * value 1, with other days ranging from Monday as 2 to Saturday as 7.
    *
    * In the resolving phase of parsing, a localized day-of-week will be converted
    * to a standardized {@code ChronoField} day-of-week.
    * The day-of-week must be in the valid range 1 to 7.
    * Other fields in this class build dates using the standardized day-of-week.
    *
    * @return a field providing access to the day-of-week with localized numbering, not null
    */
  @transient
  val dayOfWeek: TemporalField = WeekFields.ComputedDayOfField.ofDayOfWeekField(this)

  /** Returns a field to access the week of month based on this {@code WeekFields}.
    *
    * This represents the concept of the count of weeks within the month where weeks
    * start on a fixed day-of-week, such as Monday.
    * This field is typically used with {@link WeekFields#dayOfWeek()}.
    *
    * Week one (1) is the week starting on the {@link WeekFields#getFirstDayOfWeek}
    * where there are at least {@link WeekFields#getMinimalDaysInFirstWeek()} days in the month.
    * Thus, week one may start up to {@code minDays} days before the start of the month.
    * If the first week starts after the start of the month then the period before is week zero (0).
    *
    * For example:<br>
    * - if the 1st day of the month is a Monday, week one starts on the 1st and there is no week zero<br>
    * - if the 2nd day of the month is a Monday, week one starts on the 2nd and the 1st is in week zero<br>
    * - if the 4th day of the month is a Monday, week one starts on the 4th and the 1st to 3rd is in week zero<br>
    * - if the 5th day of the month is a Monday, week two starts on the 5th and the 1st to 4th is in week one<br>
    *
    * This field can be used with any calendar system.
    *
    * In the resolving phase of parsing, a date can be created from a year,
    * week-of-month, month-of-year and day-of-week.
    *
    * In {@linkplain ResolverStyle#STRICT strict mode}, all four fields are
    * validated against their range of valid values. The week-of-month field
    * is validated to ensure that the resulting month is the month requested.
    *
    * In {@linkplain ResolverStyle#SMART smart mode}, all four fields are
    * validated against their range of valid values. The week-of-month field
    * is validated from 0 to 6, meaning that the resulting date can be in a
    * different month to that specified.
    *
    * In {@linkplain ResolverStyle#LENIENT lenient mode}, the year and day-of-week
    * are validated against the range of valid values. The resulting date is calculated
    * equivalent to the following four stage approach.
    * First, create a date on the first day of the first week of January in the requested year.
    * Then take the month-of-year, subtract one, and add the amount in months to the date.
    * Then take the week-of-month, subtract one, and add the amount in weeks to the date.
    * Finally, adjust to the correct day-of-week within the localized week.
    *
    * @return a field providing access to the week-of-month, not null
    */
  @transient
  val weekOfMonth: TemporalField = WeekFields.ComputedDayOfField.ofWeekOfMonthField(this)

  /** Returns a field to access the week of year based on this {@code WeekFields}.
    *
    * This represents the concept of the count of weeks within the year where weeks
    * start on a fixed day-of-week, such as Monday.
    * This field is typically used with {@link WeekFields#dayOfWeek()}.
    *
    * Week one(1) is the week starting on the {@link WeekFields#getFirstDayOfWeek}
    * where there are at least {@link WeekFields#getMinimalDaysInFirstWeek()} days in the year.
    * Thus, week one may start up to {@code minDays} days before the start of the year.
    * If the first week starts after the start of the year then the period before is week zero (0).
    *
    * For example:<br>
    * - if the 1st day of the year is a Monday, week one starts on the 1st and there is no week zero<br>
    * - if the 2nd day of the year is a Monday, week one starts on the 2nd and the 1st is in week zero<br>
    * - if the 4th day of the year is a Monday, week one starts on the 4th and the 1st to 3rd is in week zero<br>
    * - if the 5th day of the year is a Monday, week two starts on the 5th and the 1st to 4th is in week one<br>
    *
    * This field can be used with any calendar system.
    *
    * In the resolving phase of parsing, a date can be created from a year,
    * week-of-year and day-of-week.
    *
    * In {@linkplain ResolverStyle#STRICT strict mode}, all three fields are
    * validated against their range of valid values. The week-of-year field
    * is validated to ensure that the resulting year is the year requested.
    *
    * In {@linkplain ResolverStyle#SMART smart mode}, all three fields are
    * validated against their range of valid values. The week-of-year field
    * is validated from 0 to 54, meaning that the resulting date can be in a
    * different year to that specified.
    *
    * In {@linkplain ResolverStyle#LENIENT lenient mode}, the year and day-of-week
    * are validated against the range of valid values. The resulting date is calculated
    * equivalent to the following three stage approach.
    * First, create a date on the first day of the first week in the requested year.
    * Then take the week-of-year, subtract one, and add the amount in weeks to the date.
    * Finally, adjust to the correct day-of-week within the localized week.
    *
    * @return a field providing access to the week-of-year, not null
    */
  @transient
  val weekOfYear: TemporalField = WeekFields.ComputedDayOfField.ofWeekOfYearField(this)

  /** Returns a field to access the week of a week-based-year based on this {@code WeekFields}.
    *
    * This represents the concept of the count of weeks within the year where weeks
    * start on a fixed day-of-week, such as Monday and each week belongs to exactly one year.
    * This field is typically used with {@link WeekFields#dayOfWeek()} and
    * {@link WeekFields#weekBasedYear()}.
    *
    * Week one(1) is the week starting on the {@link WeekFields#getFirstDayOfWeek}
    * where there are at least {@link WeekFields#getMinimalDaysInFirstWeek()} days in the year.
    * If the first week starts after the start of the year then the period before
    * is in the last week of the previous year.
    *
    * For example:<br>
    * - if the 1st day of the year is a Monday, week one starts on the 1st<br>
    * - if the 2nd day of the year is a Monday, week one starts on the 2nd and
    * the 1st is in the last week of the previous year<br>
    * - if the 4th day of the year is a Monday, week one starts on the 4th and
    * the 1st to 3rd is in the last week of the previous year<br>
    * - if the 5th day of the year is a Monday, week two starts on the 5th and
    * the 1st to 4th is in week one<br>
    *
    * This field can be used with any calendar system.
    *
    * In the resolving phase of parsing, a date can be created from a week-based-year,
    * week-of-year and day-of-week.
    *
    * In {@linkplain ResolverStyle#STRICT strict mode}, all three fields are
    * validated against their range of valid values. The week-of-year field
    * is validated to ensure that the resulting week-based-year is the
    * week-based-year requested.
    *
    * In {@linkplain ResolverStyle#SMART smart mode}, all three fields are
    * validated against their range of valid values. The week-of-week-based-year field
    * is validated from 1 to 53, meaning that the resulting date can be in the
    * following week-based-year to that specified.
    *
    * In {@linkplain ResolverStyle#LENIENT lenient mode}, the year and day-of-week
    * are validated against the range of valid values. The resulting date is calculated
    * equivalent to the following three stage approach.
    * First, create a date on the first day of the first week in the requested week-based-year.
    * Then take the week-of-week-based-year, subtract one, and add the amount in weeks to the date.
    * Finally, adjust to the correct day-of-week within the localized week.
    *
    * @return a field providing access to the week-of-week-based-year, not null
    */
  @transient
  val weekOfWeekBasedYear: TemporalField =
    WeekFields.ComputedDayOfField.ofWeekOfWeekBasedYearField(this)

  /** Returns a field to access the year of a week-based-year based on this {@code WeekFields}.
    *
    * This represents the concept of the year where weeks start on a fixed day-of-week,
    * such as Monday and each week belongs to exactly one year.
    * This field is typically used with {@link WeekFields#dayOfWeek()} and
    * {@link WeekFields#weekOfWeekBasedYear()}.
    *
    * Week one(1) is the week starting on the {@link WeekFields#getFirstDayOfWeek}
    * where there are at least {@link WeekFields#getMinimalDaysInFirstWeek()} days in the year.
    * Thus, week one may start before the start of the year.
    * If the first week starts after the start of the year then the period before
    * is in the last week of the previous year.
    *
    * This field can be used with any calendar system.
    *
    * In the resolving phase of parsing, a date can be created from a week-based-year,
    * week-of-year and day-of-week.
    *
    * In {@linkplain ResolverStyle#STRICT strict mode}, all three fields are
    * validated against their range of valid values. The week-of-year field
    * is validated to ensure that the resulting week-based-year is the
    * week-based-year requested.
    *
    * In {@linkplain ResolverStyle#SMART smart mode}, all three fields are
    * validated against their range of valid values. The week-of-week-based-year field
    * is validated from 1 to 53, meaning that the resulting date can be in the
    * following week-based-year to that specified.
    *
    * In {@linkplain ResolverStyle#LENIENT lenient mode}, the year and day-of-week
    * are validated against the range of valid values. The resulting date is calculated
    * equivalent to the following three stage approach.
    * First, create a date on the first day of the first week in the requested week-based-year.
    * Then take the week-of-week-based-year, subtract one, and add the amount in weeks to the date.
    * Finally, adjust to the correct day-of-week within the localized week.
    *
    * @return a field providing access to the week-based-year, not null
    */
  @transient
  val weekBasedYear: TemporalField = WeekFields.ComputedDayOfField.ofWeekBasedYearField(this)

  /** Ensure valid singleton.
    *
    * @return the valid week fields instance, not null
    * @throws InvalidObjectException if invalid
    */
  @throws[InvalidObjectException]
  private def readResolve: AnyRef =
    try WeekFields.of(firstDayOfWeek, minimalDays)
    catch {
      case ex: IllegalArgumentException =>
        throw new InvalidObjectException(s"Invalid WeekFields${ex.getMessage}")
    }

  /** Gets the first day-of-week.
    *
    * The first day-of-week varies by culture.
    * For example, the US uses Sunday, while France and the ISO-8601 standard use Monday.
    * This method returns the first day using the standard {@code DayOfWeek} enum.
    *
    * @return the first day-of-week, not null
    */
  def getFirstDayOfWeek: DayOfWeek = firstDayOfWeek

  /** Gets the minimal number of days in the first week.
    *
    * The number of days considered to define the first week of a month or year
    * varies by culture.
    * For example, the ISO-8601 requires 4 days (more than half a week) to
    * be present before counting the first week.
    *
    * @return the minimal number of days in the first week of a month or year, from 1 to 7
    */
  def getMinimalDaysInFirstWeek: Int = minimalDays

  /** Checks if this {@code WeekFields} is equal to the specified object.
    *
    * The comparison is based on the entire state of the rules, which is
    * the first day-of-week and minimal days.
    *
    * @param that  the other rules to compare to, null returns false
    * @return true if this is equal to the specified rules
    */
  override def equals(that: Any): Boolean = that match {
    case that: WeekFields => (this eq that) || (hashCode == that.hashCode)
    case _                => false
  }

  /** A hash code for this {@code WeekFields}.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = firstDayOfWeek.ordinal * 7 + minimalDays

  /** A string representation of this {@code WeekFields} instance.
    *
    * @return the string representation, not null
    */
  override def toString: String = s"WeekFields[$firstDayOfWeek,$minimalDays]"
}
