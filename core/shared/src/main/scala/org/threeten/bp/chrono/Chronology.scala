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

import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.InvalidObjectException
import java.io.ObjectStreamException
import java.util.{ Locale, Objects }
import java.util.HashMap
import java.util.Map

import org.threeten.bp.Clock
import org.threeten.bp.DateTimeException
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalTime
import org.threeten.bp.ZoneId
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.ResolverStyle
import org.threeten.bp.format.TextStyle
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

object Chronology {

  /** Map of available calendars by ID. */
  private lazy val CHRONOS_BY_ID: Map[String, Chronology] =
    new HashMap[String, Chronology]()

  /** Map of available calendars by calendar type. */
  private lazy val CHRONOS_BY_TYPE: Map[String, Chronology] =
    new HashMap[String, Chronology]()

  /** Obtains an instance of {@code Chronology} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code Chronology}.
    * If the specified temporal object does not have a chronology, {@link IsoChronology} is returned.
    *
    * The conversion will obtain the chronology using {@link TemporalQueries#chronology()}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code Chrono::from}.
    *
    * @param temporal  the temporal to convert, not null
    * @return the chronology, not null
    * @throws DateTimeException if unable to convert to an { @code Chronology}
    */
  def from(temporal: TemporalAccessor): Chronology = {
    Objects.requireNonNull(temporal, "temporal")
    val obj: Chronology = temporal.query(TemporalQueries.chronology)
    if (obj != null) obj
    else IsoChronology.INSTANCE
  }

  /** Obtains an instance of {@code Chronology} from a locale.
    *
    * This returns a {@code Chronology} based on the specified locale,
    * typically returning {@code IsoChronology}. Other calendar systems
    * are only returned if they are explicitly selected within the locale.
    *
    * The {@link Locale} class provide access to a range of information useful
    * for localizing an application. This includes the language and region,
    * such as "en-GB" for English as used in Great Britain.
    *
    * The {@code Locale} class also supports an extension mechanism that
    * can be used to identify a calendar system. The mechanism is a form
    * of key-value pairs, where the calendar system has the key "ca".
    * For example, the locale "en-JP-u-ca-japanese" represents the English
    * language as used in Japan with the Japanese calendar system.
    *
    * This method finds the desired calendar system by in a manner equivalent
    * to passing "ca" to {@link Locale#getUnicodeLocaleType(String)}.
    * If the "ca" key is not present, then {@code IsoChronology} is returned.
    *
    * Note that the behavior of this method differs from the older
    * {@link java.util.Calendar#getInstance(Locale)} method.
    * If that method receives a locale of "th_TH" it will return {@code BuddhistCalendar}.
    * By contrast, this method will return {@code IsoChronology}.
    * Passing the locale "th-TH-u-ca-buddhist" into either method will
    * result in the Thai Buddhist calendar system and is therefore the
    * recommended approach going forward for Thai calendar system localization.
    *
    * A similar, but simpler, situation occurs for the Japanese calendar system.
    * The locale "jp_JP_JP" has previously been used to access the calendar.
    * However, unlike the Thai locale, "ja_JP_JP" is automatically converted by
    * {@code Locale} to the modern and recommended form of "ja-JP-u-ca-japanese".
    * Thus, there is no difference in behavior between this method and
    * {@code Calendar#getInstance(Locale)}.
    *
    * @param locale  the locale to use to obtain the calendar system, not null
    * @return the calendar system associated with the locale, not null
    * @throws DateTimeException if the locale-specified calendar cannot be found
    */
  def ofLocale(locale: Locale): Chronology = {
    init()
    Objects.requireNonNull(locale, "locale")
    val localeType: String = locale.getUnicodeLocaleType("ca")
    if (localeType == null || ("iso" == localeType) || ("iso8601" == localeType))
      IsoChronology.INSTANCE
    else {
      val chrono: Chronology = CHRONOS_BY_TYPE.get(localeType)
      if (chrono == null)
        throw new DateTimeException(s"Unknown calendar system: $localeType")
      chrono
    }
  }

  /** Obtains an instance of {@code Chronology} from a chronology ID or
    * calendar system type.
    *
    * This returns a chronology based on either the ID or the type.
    * The {@link #getId() chronology ID} uniquely identifies the chronology.
    * The {@link #getCalendarType() calendar system type} is defined by the LDML specification.
    *
    * The chronology may be a system chronology or a chronology
    * provided by the application via ServiceLoader configuration.
    *
    * Since some calendars can be customized, the ID or type typically refers
    * to the default customization. For example, the Gregorian calendar can have multiple
    * cutover dates from the Julian, but the lookup only provides the default cutover date.
    *
    * @param id  the chronology ID or calendar system type, not null
    * @return the chronology with the identifier requested, not null
    * @throws DateTimeException if the chronology cannot be found
    */
  def of(id: String): Chronology = {
    init()
    var chrono: Chronology = CHRONOS_BY_ID.get(id)
    if (chrono != null)
      return chrono
    chrono = CHRONOS_BY_TYPE.get(id)
    if (chrono != null)
      return chrono
    throw new DateTimeException(s"Unknown chronology: $id")
  }

  /** Returns the available chronologies.
    *
    * Each returned {@code Chronology} is available for use in the system.
    *
    * @return the independent, modifiable set of the available chronology IDs, not null
    */
  def getAvailableChronologies: java.util.Set[Chronology] = {
    init()
    new java.util.HashSet[Chronology](CHRONOS_BY_ID.values)
  }

  private def init(): Unit =
    if (CHRONOS_BY_ID.isEmpty) {
      register(IsoChronology.INSTANCE)
      register(ThaiBuddhistChronology.INSTANCE)
      register(MinguoChronology.INSTANCE)
      register(JapaneseChronology.INSTANCE)
      register(HijrahChronology.INSTANCE)
      CHRONOS_BY_ID.put("Hijrah", HijrahChronology.INSTANCE)
      CHRONOS_BY_TYPE.put("islamic", HijrahChronology.INSTANCE)
      val chronologies: java.util.Iterator[Chronology] =
        ChronologyPlatformHelper.loadAdditionalChronologies
      while (chronologies.hasNext) {
        val chrono = chronologies.next()
        CHRONOS_BY_ID.put(chrono.getId, chrono)
        val `type`: String = chrono.getCalendarType
        if (`type` != null)
          CHRONOS_BY_TYPE.put(`type`, chrono)
      }
    }

  private def register(chrono: Chronology): Unit = {
    CHRONOS_BY_ID.put(chrono.getId, chrono)
    val `type`: String = chrono.getCalendarType
    if (`type` != null)
      CHRONOS_BY_TYPE.put(`type`, chrono)
    ()
  }

  @throws[IOException]
  private[chrono] def readExternal(in: DataInput): Chronology = {
    val id: String = in.readUTF
    Chronology.of(id)
  }
}

/** A calendar system, used to organize and identify dates.
  *
  * The main date and time API is built on the ISO calendar system.
  * This class operates behind the scenes to represent the general concept of a calendar system.
  * For example, the Japanese, Minguo, Thai Buddhist and others.
  *
  * Most other calendar systems also operate on the shared concepts of year, month and day,
  * linked to the cycles of the Earth around the Sun, and the Moon around the Earth.
  * These shared concepts are defined by {@link ChronoField} and are availalbe
  * for use by any {@code Chronology} implementation:
  * {{{
  * val isoDate: LocalDate = ...
  * val thaiDate: ChronoLocalDate[ThaiBuddhistChrono] = ...
  * val isoYear: Int = isoDate.get(ChronoField.YEAR);
  * val thaiYear: Int = thaiDate.get(ChronoField.YEAR);
  * }}}
  * As shown, although the date objects are in different calendar systems, represented by different
  * {@code Chronology} instances, both can be queried using the same constant on {@code ChronoField}.
  * For a full discussion of the implications of this, see {@link ChronoLocalDate}.
  * In general, the advice is to use the known ISO-based {@code LocalDate}, rather than
  * {@code ChronoLocalDate}.
  *
  * While a {@code Chronology} object typically uses {@code ChronoField} and is based on
  * an era, year-of-era, month-of-year, day-of-month model of a date, this is not required.
  * A {@code Chronology} instance may represent a totally different kind of calendar system,
  * such as the Mayan.
  *
  * In practical terms, the {@code Chronology} instance also acts as a factory.
  * The {@link #of(String)} method allows an instance to be looked up by identifier,
  * while the {@link #ofLocale(Locale)} method allows lookup by locale.
  *
  * The {@code Chronology} instance provides a set of methods to create {@code ChronoLocalDate} instances.
  * The date classes are used to manipulate specific dates.
  *
  *  - {@link #dateNow() dateNow()}
  *  - {@link #dateNow(Clock) dateNow(clock)}
  *  - {@link #dateNow(ZoneId) dateNow(zone)}
  *  - {@link #date(int, int, int) date(yearProleptic, month, day)}
  *  - {@link #date(Era, int, int, int) date(era, yearOfEra, month, day)}
  *  - {@link #dateYearDay(int, int) dateYearDay(yearProleptic, dayOfYear)}
  *  - {@link #dateYearDay(Era, int, int) dateYearDay(era, yearOfEra, dayOfYear)}
  *  - {@link #date(TemporalAccessor) date(TemporalAccessor)}
  *
  * <p id="addcalendars">Adding New Calendars</p>
  * The set of available chronologies can be extended by applications.
  * Adding a new calendar system requires the writing of an implementation of
  * {@code Chronology}, {@code ChronoLocalDate} and {@code Era}.
  * The majority of the logic specific to the calendar system will be in
  * {@code ChronoLocalDate}. The {@code Chronology} subclass acts as a factory.
  *
  * To permit the discovery of additional chronologies, the {@link java.util.ServiceLoader ServiceLoader}
  * is used. A file must be added to the {@code META-INF/services} directory with the
  * name 'org.threeten.bp.chrono.Chrono' listing the implementation classes.
  * See the ServiceLoader for more details on service loading.
  * For lookup by id or calendarType, the system provided calendars are found
  * first followed by application provided calendars.
  *
  * Each chronology must define a chronology ID that is unique within the system.
  * If the chronology represents a calendar system defined by the
  * <em>Unicode Locale Data Markup Language (LDML)</em> specification then that
  * calendar type should also be specified.
  *
  * <h3>Specification for implementors</h3>
  * This class must be implemented with care to ensure other classes operate correctly.
  * All implementations that can be instantiated must be final, immutable and thread-safe.
  * Subclasses should be Serializable wherever possible.
  */
trait Chronology extends Ordered[Chronology] {

  /** Casts the {@code Temporal} to {@code ChronoLocalDate} with the same chronology.
    *
    * @param temporal  a date-time to cast, not null
    * @return the date-time checked and cast to { @code ChronoLocalDate}, not null
    * @throws ClassCastException if the date-time cannot be cast to ChronoLocalDate
    *                            or the chronology is not equal this Chrono
    */
  private[chrono] def ensureChronoLocalDate[D <: ChronoLocalDate](temporal: Temporal): D = {
    val other: D = temporal.asInstanceOf[D]
    if (this != other.getChronology)
      throw new ClassCastException(
        s"Chrono mismatch, expected: $getId, actual: ${other.getChronology.getId}"
      )
    other
  }

  /** Casts the {@code Temporal} to {@code ChronoLocalDateTime} with the same chronology.
    *
    * @param temporal   a date-time to cast, not null
    * @return the date-time checked and cast to { @code ChronoLocalDateTime}, not null
    * @throws ClassCastException if the date-time cannot be cast to ChronoLocalDateTimeImpl
    *                            or the chronology is not equal this Chrono
    */
  private[chrono] def ensureChronoLocalDateTime[D <: ChronoLocalDate](
    temporal: Temporal
  ): ChronoLocalDateTimeImpl[D] = {
    val other: ChronoLocalDateTimeImpl[D] = temporal.asInstanceOf[ChronoLocalDateTimeImpl[D]]
    if (this != other.toLocalDate.getChronology)
      throw new ClassCastException(
        s"Chrono mismatch, required: $getId, supplied: ${other.toLocalDate.getChronology.getId}"
      )
    other
  }

  /** Casts the {@code Temporal} to {@code ChronoZonedDateTimeImpl} with the same chronology.
    *
    * @param temporal  a date-time to cast, not null
    * @return the date-time checked and cast to { @code ChronoZonedDateTimeImpl}, not null
    * @throws ClassCastException if the date-time cannot be cast to ChronoZonedDateTimeImpl
    *                            or the chronology is not equal this Chrono
    */
  private[chrono] def ensureChronoZonedDateTime[D <: ChronoLocalDate](
    temporal: Temporal
  ): ChronoZonedDateTimeImpl[D] = {
    val other: ChronoZonedDateTimeImpl[D] = temporal.asInstanceOf[ChronoZonedDateTimeImpl[D]]
    if (this != other.toLocalDate.getChronology)
      throw new ClassCastException(
        s"Chrono mismatch, required: $getId, supplied: ${other.toLocalDate.getChronology.getId}"
      )
    other
  }

  /** Gets the ID of the chronology.
    *
    * The ID uniquely identifies the {@code Chronology}.
    * It can be used to lookup the {@code Chronology} using {@link #of(String)}.
    *
    * @return the chronology ID, not null
    * @see #getCalendarType()
    */
  def getId: String

  /** Gets the calendar type of the underlying calendar system.
    *
    * The calendar type is an identifier defined by the
    * <em>Unicode Locale Data Markup Language (LDML)</em> specification.
    * It can be used to lookup the {@code Chronology} using {@link #of(String)}.
    * It can also be used as part of a locale, accessible via
    * {@link Locale#getUnicodeLocaleType(String)} with the key 'ca'.
    *
    * @return the calendar system type, null if the calendar is not defined by LDML
    * @see #getId()
    */
  def getCalendarType: String

  /** Obtains a local date in this chronology from the era, year-of-era,
    * month-of-year and day-of-month fields.
    *
    * @param era  the era of the correct type for the chronology, not null
    * @param yearOfEra  the chronology year-of-era
    * @param month  the chronology month-of-year
    * @param dayOfMonth  the chronology day-of-month
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    * @throws ClassCastException if the { @code era} is not of the correct type for the chronology
    */
  def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): ChronoLocalDate =
    date(prolepticYear(era, yearOfEra), month, dayOfMonth)

  /** Obtains a local date in this chronology from the proleptic-year,
    * month-of-year and day-of-month fields.
    *
    * @param prolepticYear  the chronology proleptic-year
    * @param month  the chronology month-of-year
    * @param dayOfMonth  the chronology day-of-month
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    */
  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): ChronoLocalDate

  /** Obtains a local date in this chronology from the era, year-of-era and
    * day-of-year fields.
    *
    * @param era  the era of the correct type for the chronology, not null
    * @param yearOfEra  the chronology year-of-era
    * @param dayOfYear  the chronology day-of-year
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    * @throws ClassCastException if the { @code era} is not of the correct type for the chronology
    */
  def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): ChronoLocalDate =
    dateYearDay(prolepticYear(era, yearOfEra), dayOfYear)

  /** Obtains a local date in this chronology from the proleptic-year and
    * day-of-year fields.
    *
    * @param prolepticYear  the chronology proleptic-year
    * @param dayOfYear  the chronology day-of-year
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    */
  def dateYearDay(prolepticYear: Int, dayOfYear: Int): ChronoLocalDate

  /** Obtains a local date in this chronology from the epoch-day.
    *
    * The definition of {@link ChronoField#EPOCH_DAY EPOCH_DAY} is the same
    * for all calendar systems, thus it can be used for conversion.
    *
    * @param epochDay  the epoch day
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    */
  def dateEpochDay(epochDay: Long): ChronoLocalDate

  /** Obtains a local date in this chronology from another temporal object.
    *
    * This creates a date in this chronology based on the specified {@code TemporalAccessor}.
    *
    * The standard mechanism for conversion between date types is the
    * {@link ChronoField#EPOCH_DAY local epoch-day} field.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the local date in this chronology, not null
    * @throws DateTimeException if unable to create the date
    */
  def date(temporal: TemporalAccessor): ChronoLocalDate

  /** Obtains the current local date in this chronology from the system clock in the default time-zone.
    *
    * This will query the {@link Clock#systemDefaultZone() system clock} in the default
    * time-zone to obtain the current date.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * This implementation uses {@link #dateNow(Clock)}.
    *
    * @return the current local date using the system clock and default time-zone, not null
    * @throws DateTimeException if unable to create the date
    */
  def dateNow: ChronoLocalDate = dateNow(Clock.systemDefaultZone)

  /** Obtains the current local date in this chronology from the system clock in the specified time-zone.
    *
    * This will query the {@link Clock#system(ZoneId) system clock} to obtain the current date.
    * Specifying the time-zone avoids dependence on the default time-zone.
    *
    * Using this method will prevent the ability to use an alternate clock for testing
    * because the clock is hard-coded.
    *
    * @param zone  the zone ID to use, not null
    * @return the current local date using the system clock, not null
    * @throws DateTimeException if unable to create the date
    */
  def dateNow(zone: ZoneId): ChronoLocalDate = dateNow(Clock.system(zone))

  /** Obtains the current local date in this chronology from the specified clock.
    *
    * This will query the specified clock to obtain the current date - today.
    * Using this method allows the use of an alternate clock for testing.
    * The alternate clock may be introduced using {@link Clock dependency injection}.
    *
    * @param clock  the clock to use, not null
    * @return the current local date, not null
    * @throws DateTimeException if unable to create the date
    */
  def dateNow(clock: Clock): ChronoLocalDate = {
    Objects.requireNonNull(clock, "clock")
    date(LocalDate.now(clock))
  }

  /** Obtains a local date-time in this chronology from another temporal object.
    *
    * This creates a date-time in this chronology based on the specified {@code TemporalAccessor}.
    *
    * The date of the date-time should be equivalent to that obtained by calling
    * {@link #date(TemporalAccessor)}.
    * The standard mechanism for conversion between time types is the
    * {@link ChronoField#NANO_OF_DAY nano-of-day} field.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the local date-time in this chronology, not null
    * @throws DateTimeException if unable to create the date-time
    */
  def localDateTime(temporal: TemporalAccessor): ChronoLocalDateTime[_] =
    try {
      val clDate: ChronoLocalDate = date(temporal)
      clDate.atTime(LocalTime.from(temporal))
    } catch {
      case ex: DateTimeException =>
        throw new DateTimeException(
          s"Unable to obtain ChronoLocalDateTime from TemporalAccessor: ${temporal.getClass}",
          ex
        )
    }

  /** Obtains a zoned date-time in this chronology from another temporal object.
    *
    * This creates a date-time in this chronology based on the specified {@code TemporalAccessor}.
    *
    * This should obtain a {@code ZoneId} using {@link ZoneId#from(TemporalAccessor)}.
    * The date-time should be obtained by obtaining an {@code Instant}.
    * If that fails, the local date-time should be used.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the zoned date-time in this chronology, not null
    * @throws DateTimeException if unable to create the date-time
    */
  def zonedDateTime(temporal: TemporalAccessor): ChronoZonedDateTime[_] =
    try {
      val zone: ZoneId = ZoneId.from(temporal)
      try {
        val instant: Instant = Instant.from(temporal)
        zonedDateTime(instant, zone)
      } catch {
        case _: DateTimeException =>
          val cldt
            : ChronoLocalDateTime[_] = localDateTime(temporal) /// !!! was _ <: ChronoLocalDate
          val cldtImpl: ChronoLocalDateTimeImpl[_ <: ChronoLocalDate] = ensureChronoLocalDateTime(
            cldt
          )
          ChronoZonedDateTimeImpl.ofBest(cldtImpl, zone, null)
      }
    } catch {
      case ex: DateTimeException =>
        throw new DateTimeException(
          s"Unable to obtain ChronoZonedDateTime from TemporalAccessor: ${temporal.getClass}",
          ex
        )
    }

  /** Obtains a zoned date-time in this chronology from an {@code Instant}.
    *
    * This creates a zoned date-time with the same instant as that specified.
    *
    * @param instant  the instant to create the date-time from, not null
    * @param zone  the time-zone, not null
    * @return the zoned date-time, not null
    * @throws DateTimeException if the result exceeds the supported range
    */
  def zonedDateTime(instant: Instant, zone: ZoneId): ChronoZonedDateTime[_ <: ChronoLocalDate] =
    ChronoZonedDateTimeImpl.ofInstant(this, instant, zone)

  /** Obtains a period for this chronology based on years, months and days.
    *
    * This returns a period tied to this chronology using the specified
    * years, months and days.  All supplied chronologies use periods
    * based on years, months and days, however the {@code ChronoPeriod} API
    * allows the period to be represented using other units.
    *
    * The default implementation returns an implementation class suitable
    * for most calendar systems. It is based solely on the three units.
    * Normalization, addition and subtraction derive the number of months
    * in a year from the {@link #range(ChronoField)}. If the number of
    * months within a year is fixed, then the calculation approach for
    * addition, subtraction and normalization is slightly different.
    *
    * If implementing an unusual calendar system that is not based on
    * years, months and days, or where you want direct control, then
    * the {@code ChronoPeriod} interface must be directly implemented.
    *
    * The returned period is immutable and thread-safe.
    *
    * @param years  the number of years, may be negative
    * @param months  the number of years, may be negative
    * @param days  the number of years, may be negative
    * @return the period in terms of this chronology, not null
    */
  def period(years: Int, months: Int, days: Int): ChronoPeriod =
    new ChronoPeriodImpl(this, years, months, days)

  /** Checks if the specified year is a leap year.
    *
    * A leap-year is a year of a longer length than normal.
    * The exact meaning is determined by the chronology according to the following constraints.
    *
    *  - a leap-year must imply a year-length longer than a non leap-year.
    *  - a chronology that does not support the concept of a year must return false.
    *
    *
    * @param prolepticYear  the proleptic-year to check, not validated for range
    * @return true if the year is a leap year
    */
  def isLeapYear(prolepticYear: Long): Boolean

  /** Calculates the proleptic-year given the era and year-of-era.
    *
    * This combines the era and year-of-era into the single proleptic-year field.
    *
    * @param era  the era of the correct type for the chronology, not null
    * @param yearOfEra  the chronology year-of-era
    * @return the proleptic-year
    * @throws DateTimeException if unable to convert
    * @throws ClassCastException if the { @code era} is not of the correct type for the chronology
    */
  def prolepticYear(era: Era, yearOfEra: Int): Int

  /** Creates the chronology era object from the numeric value.
    *
    * The era is, conceptually, the largest division of the time-line.
    * Most calendar systems have a single epoch dividing the time-line into two eras.
    * However, some have multiple eras, such as one for the reign of each leader.
    * The exact meaning is determined by the chronology according to the following constraints.
    *
    * The era in use at 1970-01-01 must have the value 1.
    * Later eras must have sequentially higher values.
    * Earlier eras must have sequentially lower values.
    * Each chronology must refer to an enum or similar singleton to provide the era values.
    *
    * This method returns the singleton era of the correct type for the specified era value.
    *
    * @param eraValue  the era value
    * @return the calendar system era, not null
    * @throws DateTimeException if unable to create the era
    */
  def eraOf(eraValue: Int): Era

  /** Gets the list of eras for the chronology.
    *
    * Most calendar systems have an era, within which the year has meaning.
    * If the calendar system does not support the concept of eras, an empty
    * list must be returned.
    *
    * @return the list of eras for the chronology, may be immutable, not null
    */
  def eras: java.util.List[Era]

  /** Gets the range of valid values for the specified field.
    *
    * All fields can be expressed as a {@code long} integer.
    * This method returns an object that describes the valid range for that value.
    *
    * Note that the result only describes the minimum and maximum valid values
    * and it is important not to read too much into them. For example, there
    * could be values within the range that are invalid for the field.
    *
    * This method will return a result whether or not the chronology supports the field.
    *
    * @param field  the field to get the range for, not null
    * @return the range of valid values for the field, not null
    * @throws DateTimeException if the range for the field cannot be obtained
    */
  def range(field: ChronoField): ValueRange

  /** Gets the textual representation of this chronology.
    *
    * This returns the textual name used to identify the chronology.
    * The parameters control the style of the returned text and the locale.
    *
    * @param style  the style of the text required, not null
    * @param locale  the locale to use, not null
    * @return the text value of the chronology, not null
    */
  def getDisplayName(style: TextStyle, locale: Locale): String =
    new DateTimeFormatterBuilder()
      .appendChronologyText(style)
      .toFormatter(locale)
      .format(new TemporalAccessor() {
        def isSupported(field: TemporalField): Boolean = false

        def getLong(field: TemporalField): Long =
          throw throw UnsupportedTemporalTypeException.field(field)

        override def query[R](query: TemporalQuery[R]): R =
          if (query eq TemporalQueries.chronology) this.asInstanceOf[R]
          else super.query(query)
      })

  /** Resolves parsed {@code ChronoField} values into a date during parsing.
    *
    * Most {@code TemporalField} implementations are resolved using the
    * resolve method on the field. By contrast, the {@code ChronoField} class
    * defines fields that only have meaning relative to the chronology.
    * As such, {@code ChronoField} date fields are resolved here in the
    * context of a specific chronology.
    *
    * The default implementation, which explains typical resolve behaviour,
    * is provided in {@link AbstractChronology}.
    *
    * @param fieldValues  the map of fields to values, which can be updated, not null
    * @param resolverStyle  the requested type of resolve, not null
    * @return the resolved date, null if insufficient information to create a date
    * @throws DateTimeException if the date cannot be resolved, typically
    *                           because of a conflict in the input data
    */
  def resolveDate(
    fieldValues:   java.util.Map[TemporalField, java.lang.Long],
    resolverStyle: ResolverStyle
  ): ChronoLocalDate

  /** Updates the map of field-values during resolution.
    *
    * @param field  the field to update, not null
    * @param value  the value to update, not null
    * @throws DateTimeException if a conflict occurs
    */
  private[chrono] def updateResolveMap(
    fieldValues: java.util.Map[TemporalField, java.lang.Long],
    field:       ChronoField,
    value:       Long
  ): Unit = {
    val current: java.lang.Long = fieldValues.get(field)
    if (current != null && current.longValue != value)
      throw new DateTimeException(
        s"Invalid state, field: $field $current conflicts with $field $value"
      )
    else {
      fieldValues.put(field, value)
      ()
    }
  }

  /** Compares this chronology to another chronology.
    *
    * The comparison order first by the chronology ID string, then by any
    * additional information specific to the subclass.
    * It is "consistent with equals", as defined by {@link Comparable}.
    *
    * The default implementation compares the chronology ID.
    * Subclasses must compare any additional state that they store.
    *
    * @param other  the other chronology to compare to, not null
    * @return the comparator value, negative if less, positive if greater
    */
  def compare(other: Chronology): Int = getId.compareTo(other.getId)

  /** Checks if this chronology is equal to another chronology.
    *
    * The comparison is based on the entire state of the object.
    *
    * The default implementation checks the type and calls {@link #compareTo(Chronology)}.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other chronology
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: Chronology => (this eq other) || compareTo(other) == 0
      case _                 => false
    }

  /** A hash code for this chronology.
    *
    * The default implementation is based on the ID and class.
    * Subclasses should add any additional state that they store.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = getClass.hashCode ^ getId.hashCode

  /** Outputs this chronology as a {@code String}, using the ID.
    *
    * @return a string representation of this chronology, not null
    */
  override def toString: String = getId

  private def writeReplace: AnyRef = new Ser(Ser.CHRONO_TYPE, this)

  /** Defend against malicious streams.
    * @return never
    * @throws InvalidObjectException always
    */
  @throws[ObjectStreamException]
  private def readResolve: AnyRef =
    throw new InvalidObjectException("Deserialization via serialization delegate")

  @throws[IOException]
  private[chrono] def writeExternal(out: DataOutput): Unit = out.writeUTF(getId)
}
