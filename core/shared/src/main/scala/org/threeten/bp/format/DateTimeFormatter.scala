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
package org.threeten.bp.format

import org.threeten.bp.temporal.ChronoField.DAY_OF_MONTH
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.DAY_OF_YEAR
import org.threeten.bp.temporal.ChronoField.HOUR_OF_DAY
import org.threeten.bp.temporal.ChronoField.MINUTE_OF_HOUR
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import org.threeten.bp.temporal.ChronoField.NANO_OF_SECOND
import org.threeten.bp.temporal.ChronoField.SECOND_OF_MINUTE
import org.threeten.bp.temporal.ChronoField.YEAR
import java.io.IOException
import java.text.FieldPosition
import java.text.Format
import java.text.ParseException
import java.text.ParsePosition
import java.lang.StringBuilder
import java.util.{ Arrays, Collections, Locale, Objects }

import org.threeten.bp.DateTimeException
import org.threeten.bp.Period
import org.threeten.bp.ZoneId
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder.CompositePrinterParser
import org.threeten.bp.temporal.IsoFields
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.format.internal.TTBPDateTimeParseContext
import org.threeten.bp.format.internal.TTBPDateTimePrintContext
import scala.collection.JavaConverters._

object DateTimeFormatter {

  /** Returns the ISO date formatter that prints/parses a date without an offset,
    * such as '2011-12-03'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended local date format.
    * The format consists of:
    *<ul>
    * <li>Four digits or more for the {@link ChronoField#YEAR year}.
    * Years in the range 0000 to 9999 will be pre-padded by zero to ensure four digits.
    * Years outside that range will have a prefixed positive or negative symbol.
    * <li>A dash
    * <li>Two digits for the {@link ChronoField#MONTH_OF_YEAR month-of-year}.
    * This is pre-padded by zero to ensure two digits.
    * <li>A dash
    * <li>Two digits for the {@link ChronoField#DAY_OF_MONTH day-of-month}.
    * This is pre-padded by zero to ensure two digits.
    * </ul><p>
    */
  def ISO_LOCAL_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(MONTH_OF_YEAR, 2)
      .appendLiteral('-')
      .appendValue(DAY_OF_MONTH, 2)
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses a date with an offset,
    * such as '2011-12-03+01:00'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset date format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_DATE}
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    */
  def ISO_OFFSET_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_DATE)
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses a date with the
    * offset if available, such as '2011-12-03' or '2011-12-03+01:00'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended date format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_DATE}
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def ISO_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_DATE)
      .optionalStart()
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO time formatter that prints/parses a time without an offset,
    * such as '10:15' or '10:15:30'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended local time format.
    * The format consists of:
    *<ul>
    * <li>Two digits for the {@link ChronoField#HOUR_OF_DAY hour-of-day}.
    * This is pre-padded by zero to ensure two digits.
    * <li>A colon
    * <li>Two digits for the {@link ChronoField#MINUTE_OF_HOUR minute-of-hour}.
    * This is pre-padded by zero to ensure two digits.
    * <li>If the second-of-minute is not available to print/parse then the format is complete.
    * <li>A colon
    * <li>Two digits for the {@link ChronoField#SECOND_OF_MINUTE second-of-minute}.
    * This is pre-padded by zero to ensure two digits.
    * <li>If the nano-of-second is zero or not available to print/parse then the format is complete.
    * <li>A decimal point
    * <li>One to nine digits for the {@link ChronoField#NANO_OF_SECOND nano-of-second}.
    * As many digits will be printed as required.
    * </ul><p>
    */
  def ISO_LOCAL_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)
      .optionalStart()
      .appendLiteral(':')
      .appendValue(SECOND_OF_MINUTE, 2)
      .optionalStart()
      .appendFraction(NANO_OF_SECOND, 0, 9, true)
      .toFormatter(ResolverStyle.STRICT)

  /** Returns the ISO time formatter that prints/parses a time with an offset,
    * such as '10:15+01:00' or '10:15:30+01:00'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset time format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_TIME}
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    */
  def ISO_OFFSET_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_TIME)
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)

  /** Returns the ISO time formatter that prints/parses a time, with the
    * offset if available, such as '10:15', '10:15:30' or '10:15:30+01:00'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset time format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_TIME}
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def ISO_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_TIME)
      .optionalStart()
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)

  /** Returns the ISO date formatter that prints/parses a date-time
    * without an offset, such as '2011-12-03T10:15:30'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset date-time format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_DATE}
    * <li>The letter 'T'. Parsing is case insensitive.
    * <li>The {@link #ISO_LOCAL_TIME}
    * </ul><p>
    */
  def ISO_LOCAL_DATE_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_DATE)
      .appendLiteral('T')
      .append(ISO_LOCAL_TIME)
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses a date-time
    * with an offset, such as '2011-12-03T10:15:30+01:00'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset date-time format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_DATE_TIME}
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    */
  def ISO_OFFSET_DATE_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(ISO_LOCAL_DATE_TIME)
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses a date-time with
    * offset and zone, such as '2011-12-03T10:15:30+01:00[Europe/Paris]'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * a format that extends the ISO-8601 extended offset date-time format
    * to add the time-zone.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_OFFSET_DATE_TIME}
    * <li>If the zone ID is not available or is a {@code ZoneOffset} then the format is complete.
    * <li>An open square bracket '['.
    * <li>The {@link ZoneId#getId() zone ID}. This is not part of the ISO-8601 standard.
    * Parsing is case sensitive.
    * <li>A close square bracket ']'.
    * </ul><p>
    */
  def ISO_ZONED_DATE_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .append(ISO_OFFSET_DATE_TIME)
      .optionalStart()
      .appendLiteral('[')
      .parseCaseSensitive
      .appendZoneRegionId
      .appendLiteral(']')
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses a date-time
    * with the offset and zone if available, such as '2011-12-03T10:15:30',
    * '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended offset date-time format.
    * The format consists of:
    *<ul>
    * <li>The {@link #ISO_LOCAL_DATE_TIME}
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * <li>If the zone ID is not available or is a {@code ZoneOffset} then the format is complete.
    * <li>An open square bracket '['.
    * <li>The {@link ZoneId#getId() zone ID}. This is not part of the ISO-8601 standard.
    * Parsing is case sensitive.
    * <li>A close square bracket ']'.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def ISO_DATE_TIME: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .append(ISO_LOCAL_DATE_TIME)
      .optionalStart()
      .appendOffsetId
      .optionalStart()
      .appendLiteral('[')
      .parseCaseSensitive
      .appendZoneRegionId
      .appendLiteral(']')
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses the ordinal date
    * without an offset, such as '2012-337'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended ordinal date format.
    * The format consists of:
    *<ul>
    * <li>Four digits or more for the {@link ChronoField#YEAR year}.
    * Years in the range 0000 to 9999 will be pre-padded by zero to ensure four digits.
    * Years outside that range will have a prefixed positive or negative symbol.
    * <li>A dash
    * <li>Three digits for the {@link ChronoField#DAY_OF_YEAR day-of-year}.
    * This is pre-padded by zero to ensure three digits.
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def ISO_ORDINAL_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(DAY_OF_YEAR, 3)
      .optionalStart()
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the ISO date formatter that prints/parses the week-based date
    * without an offset, such as '2012-W48-6'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 extended week-based date format.
    * The format consists of:
    *<ul>
    * <li>Four digits or more for the {@link IsoFields#WEEK_BASED_YEAR week-based-year}.
    * Years in the range 0000 to 9999 will be pre-padded by zero to ensure four digits.
    * Years outside that range will have a prefixed positive or negative symbol.
    * <li>A dash
    * <li>The letter 'W'. Parsing is case insensitive.
    * <li>Two digits for the {@link IsoFields#WEEK_OF_WEEK_BASED_YEAR week-of-week-based-year}.
    * This is pre-padded by zero to ensure three digits.
    * <li>A dash
    * <li>One digit for the {@link ChronoField#DAY_OF_WEEK day-of-week}.
    * The value run from Monday (1) to Sunday (7).
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID}. If the offset has seconds then
    * they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def ISO_WEEK_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .appendValue(IsoFields.WEEK_BASED_YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral("-W")
      .appendValue(IsoFields.WEEK_OF_WEEK_BASED_YEAR, 2)
      .appendLiteral('-')
      .appendValue(DAY_OF_WEEK, 1)
      .optionalStart()
      .appendOffsetId
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** The ISO instant formatter that formats or parses an instant in UTC,
    * such as '2011-12-03T10:15:30Z'.
    *
    * This returns an immutable formatter capable of formatting and parsing
    * the ISO-8601 instant format.
    * When formatting, the second-of-minute is always output.
    * The nano-of-second outputs zero, three, six or nine digits digits as necessary.
    * When parsing, time to at least the seconds field is required.
    * Fractional seconds from zero to nine are parsed.
    * The localized decimal style is not used.
    *
    * This is a special case formatter intended to allow a human readable form
    * of an {@link org.threeten.bp.Instant Instant}.
    * The {@code Instant} class is designed to
    * only represent a point in time and internally stores a value in nanoseconds
    * from a fixed epoch of 1970-01-01Z. As such, an {@code Instant} cannot be
    * formatted as a date or time without providing some form of time-zone.
    * This formatter allows the {@code Instant} to be formatted, by providing
    * a suitable conversion using {@code ZoneOffset.UTC}.
    *
    * The format consists of:
    * <ul>
    * <li>The {@link #ISO_OFFSET_DATE_TIME} where the instant is converted from
    * {@link ChronoField#INSTANT_SECONDS} and {@link ChronoField#NANO_OF_SECOND}
    * using the {@code UTC} offset. Parsing is case insensitive.
    * </ul>
    *
    * The returned formatter has no override chronology or zone.
    * It uses the {@link ResolverStyle#STRICT STRICT} resolver style.
    */
  def ISO_INSTANT: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive.appendInstant
      .toFormatter(ResolverStyle.STRICT)

  /** Returns the ISO date formatter that prints/parses a date without an offset,
    * such as '20111203'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * the ISO-8601 basic local date format.
    * The format consists of:
    *<ul>
    * <li>Four digits for the {@link ChronoField#YEAR year}.
    * Only years in the range 0000 to 9999 are supported.
    * <li>Two digits for the {@link ChronoField#MONTH_OF_YEAR month-of-year}.
    * This is pre-padded by zero to ensure two digits.
    * <li>Two digits for the {@link ChronoField#DAY_OF_MONTH day-of-month}.
    * This is pre-padded by zero to ensure two digits.
    * <li>If the offset is not available to print/parse then the format is complete.
    * <li>The {@link ZoneOffset#getId() offset ID} without colons. If the offset has
    * seconds then they will be handled even though this is not part of the ISO-8601 standard.
    * Parsing is case insensitive.
    * </ul><p>
    * As this formatter has an optional element, it may be necessary to parse using
    * {@link DateTimeFormatter#parseBest}.
    */
  def BASIC_ISO_DATE: DateTimeFormatter =
    new DateTimeFormatterBuilder().parseCaseInsensitive
      .appendValue(YEAR, 4)
      .appendValue(MONTH_OF_YEAR, 2)
      .appendValue(DAY_OF_MONTH, 2)
      .optionalStart()
      .appendOffset("+HHMMss", "Z")
      .toFormatter(ResolverStyle.STRICT)
      .withChronology(IsoChronology.INSTANCE)

  /** Returns the RFC-1123 date-time formatter, such as 'Tue, 3 Jun 2008 11:05:30 GMT'.
    *
    * This returns an immutable formatter capable of printing and parsing
    * most of the RFC-1123 format.
    * RFC-1123 updates RFC-822 changing the year from two digits to four.
    * This implementation requires a four digit year.
    * This implementation also does not handle North American or military zone
    * names, only 'GMT' and offset amounts.
    *
    * The format consists of:
    *<ul>
    * <li>If the day-of-week is not available to print/parse then jump to day-of-month.
    * <li>Three letter {@link ChronoField#DAY_OF_WEEK day-of-week} in English.
    * <li>A comma
    * <li>A space
    * <li>One or two digits for the {@link ChronoField#DAY_OF_MONTH day-of-month}.
    * <li>A space
    * <li>Three letter {@link ChronoField#MONTH_OF_YEAR month-of-year} in English.
    * <li>A space
    * <li>Four digits for the {@link ChronoField#YEAR year}.
    * Only years in the range 0000 to 9999 are supported.
    * <li>A space
    * <li>Two digits for the {@link ChronoField#HOUR_OF_DAY hour-of-day}.
    * This is pre-padded by zero to ensure two digits.
    * <li>A colon
    * <li>Two digits for the {@link ChronoField#MINUTE_OF_HOUR minute-of-hour}.
    * This is pre-padded by zero to ensure two digits.
    * <li>If the second-of-minute is not available to print/parse then jump to the next space.
    * <li>A colon
    * <li>Two digits for the {@link ChronoField#SECOND_OF_MINUTE second-of-minute}.
    * This is pre-padded by zero to ensure two digits.
    * <li>A space
    * <li>The {@link ZoneOffset#getId() offset ID} without colons or seconds.
    * An offset of zero uses "GMT". North American zone names and military zone names are not handled.
    * </ul><p>
    * Parsing is case insensitive.
    */
  val RFC_1123_DATE_TIME: DateTimeFormatter = {
    // Size checked
    val dow: Map[scala.Long, String] =
      Map(1L -> "Mon", 2L -> "Tue", 3L -> "Wed", 4L -> "Thu", 5L -> "Fri", 6L -> "Sat", 7L -> "Sun")
    val moy: Map[scala.Long, String] =
      Map(1L -> "Jan",
          2L -> "Feb",
          3L -> "Mar",
          4L -> "Apr",
          5L -> "May",
          6L -> "Jun",
          7L -> "Jul",
          8L -> "Aug",
          9L -> "Sep",
          10L -> "Oct",
          11L -> "Nov",
          12L -> "Dec")
    new DateTimeFormatterBuilder().parseCaseInsensitive.parseLenient
      .optionalStart()
      .appendText(DAY_OF_WEEK, dow.map(x => (Long.box(x._1), x._2)).asJava)
      .appendLiteral(", ")
      .optionalEnd()
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(' ')
      .appendText(MONTH_OF_YEAR, moy.map(x => (Long.box(x._1), x._2)).asJava)
      .appendLiteral(' ')
      .appendValue(YEAR, 4)
      .appendLiteral(' ')
      .appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)
      .optionalStart()
      .appendLiteral(':')
      .appendValue(SECOND_OF_MINUTE, 2)
      .optionalEnd()
      .appendLiteral(' ')
      .appendOffset("+HHMM", "GMT")
      .toFormatter(ResolverStyle.SMART)
      .withChronology(IsoChronology.INSTANCE)
  }

  /** Creates a formatter using the specified pattern.
    *
    * This method will create a formatter based on a simple pattern of letters and symbols.
    * For example, {@code d MMM yyyy} will format 2011-12-03 as '3 Dec 2011'.
    *
    * The returned formatter will use the default locale, but this can be changed
    * using {@link DateTimeFormatter#withLocale(Locale)}.
    *
    * All letters 'A' to 'Z' and 'a' to 'z' are reserved as pattern letters.
    * The following pattern letters are defined:
    * <pre>
    * Symbol  Meaning                     Presentation      Examples
    * ------  -------                     ------------      -------
    * G       era                         number/text       1; 01; AD; Anno Domini
    * y       year                        year              2004; 04
    * D       day-of-year                 number            189
    * M       month-of-year               number/text       7; 07; Jul; July; J
    * d       day-of-month                number            10
    *
    * Q       quarter-of-year             number/text       3; 03; Q3
    * Y       week-based-year             year              1996; 96
    * w       week-of-year                number            27
    * W       week-of-month               number            27
    * e       localized day-of-week       number            2; Tue; Tuesday; T
    * E       day-of-week                 number/text       2; Tue; Tuesday; T
    * F       week-of-month               number            3
    *
    * a       am-pm-of-day                text              PM
    * h       clock-hour-of-am-pm (1-12)  number            12
    * K       hour-of-am-pm (0-11)        number            0
    * k       clock-hour-of-am-pm (1-24)  number            0
    *
    * H       hour-of-day (0-23)          number            0
    * m       minute-of-hour              number            30
    * s       second-of-minute            number            55
    * S       fraction-of-second          fraction          978
    * A       milli-of-day                number            1234
    * n       nano-of-second              number            987654321
    * N       nano-of-day                 number            1234000000
    *
    * V       time-zone ID                zone-id           America/Los_Angeles; Z; -08:30
    * z       time-zone name              zone-name         Pacific Standard Time; PST
    * X       zone-offset 'Z' for zero    offset-X          Z; -08; -0830; -08:30; -083015; -08:30:15;
    * x       zone-offset                 offset-x          +0000; -08; -0830; -08:30; -083015; -08:30:15;
    * Z       zone-offset                 offset-Z          +0000; -0800; -08:00;
    *
    * p       pad next                    pad modifier      1
    *
    * '       escape for text             delimiter
    * ''      single quote                literal           '
    * [       optional section start
    * ]       optional section end
    * {}      reserved for future use
    * </pre>
    *
    * The count of pattern letters determine the format.
    *
    * <b>Text</b>: The text style is determined based on the number of pattern letters used.
    * Less than 4 pattern letters will use the {@link TextStyle#SHORT short form}.
    * Exactly 4 pattern letters will use the {@link TextStyle#FULL full form}.
    * Exactly 5 pattern letters will use the {@link TextStyle#NARROW narrow form}.
    *
    * <b>Number</b>: If the count of letters is one, then the value is printed using the minimum number
    * of digits and without padding as per {@link DateTimeFormatterBuilder#appendValue(TemporalField)}.
    * Otherwise, the count of digits is used as the width of the output field as per
    * {@link DateTimeFormatterBuilder#appendValue(TemporalField, int)}.
    *
    * <b>Number/Text</b>: If the count of pattern letters is 3 or greater, use the Text rules above.
    * Otherwise use the Number rules above.
    *
    * <b>Fraction</b>: Outputs the nano-of-second field as a fraction-of-second.
    * The nano-of-second value has nine digits, thus the count of pattern letters is from 1 to 9.
    * If it is less than 9, then the nano-of-second value is truncated, with only the most
    * significant digits being output.
    * When parsing in strict mode, the number of parsed digits must match the count of pattern letters.
    * When parsing in lenient mode, the number of parsed digits must be at least the count of pattern
    * letters, up to 9 digits.
    *
    * <b>Year</b>: The count of letters determines the minimum field width below which padding is used.
    * If the count of letters is two, then a {@link DateTimeFormatterBuilder#appendValueReduced reduced}
    * two digit form is used.
    * For printing, this outputs the rightmost two digits. For parsing, this will parse using the
    * base value of 2000, resulting in a year within the range 2000 to 2099 inclusive.
    * If the count of letters is less than four (but not two), then the sign is only output for negative
    * years as per {@link SignStyle#NORMAL}.
    * Otherwise, the sign is output if the pad width is exceeded, as per {@link SignStyle#EXCEEDS_PAD}
    *
    * <b>ZoneId</b>: This outputs the time-zone ID, such as 'Europe/Paris'.
    * If the count of letters is two, then the time-zone ID is output.
    * Any other count of letters throws {@code IllegalArgumentException}.
    *
    * <b>Zone names</b>: This outputs the display name of the time-zone ID.
    * If the count of letters is one, two or three, then the short name is output.
    * If the count of letters is four, then the full name is output.
    * Five or more letters throws {@code IllegalArgumentException}.
    *
    * <b>Offset X and x</b>: This formats the offset based on the number of pattern letters.
    * One letter outputs just the hour', such as '+01', unless the minute is non-zero
    * in which case the minute is also output, such as '+0130'.
    * Two letters outputs the hour and minute, without a colon, such as '+0130'.
    * Three letters outputs the hour and minute, with a colon, such as '+01:30'.
    * Four letters outputs the hour and minute and optional second, without a colon, such as '+013015'.
    * Five letters outputs the hour and minute and optional second, with a colon, such as '+01:30:15'.
    * Six or more letters throws {@code IllegalArgumentException}.
    * Pattern letter 'X' (upper case) will output 'Z' when the offset to be output would be zero,
    * whereas pattern letter 'x' (lower case) will output '+00', '+0000', or '+00:00'.
    *
    * <b>Offset Z</b>: This formats the offset based on the number of pattern letters.
    * One, two or three letters outputs the hour and minute, without a colon, such as '+0130'.
    * Four or more letters throws {@code IllegalArgumentException}.
    * The output will be '+0000' when the offset is zero.
    *
    * <b>Optional section</b>: The optional section markers work exactly like calling
    * {@link DateTimeFormatterBuilder#optionalStart()} and {@link DateTimeFormatterBuilder#optionalEnd()}.
    *
    * <b>Pad modifier</b>: Modifies the pattern that immediately follows to be padded with spaces.
    * The pad width is determined by the number of pattern letters.
    * This is the same as calling {@link DateTimeFormatterBuilder#padNext(int)}.
    *
    * For example, 'ppH' outputs the hour-of-day padded on the left with spaces to a width of 2.
    *
    * Any unrecognized letter is an error.
    * Any non-letter character, other than '[', ']', '{', '}' and the single quote will be output directly.
    * Despite this, it is recommended to use single quotes around all characters that you want to
    * output directly to ensure that future changes do not break your application.
    *
    * @param pattern  the pattern to use, not null
    * @return the formatter based on the pattern, not null
    * @throws IllegalArgumentException if the pattern is invalid
    * @see DateTimeFormatterBuilder#appendPattern(String)
    */
  def ofPattern(pattern: String): DateTimeFormatter =
    new DateTimeFormatterBuilder().appendPattern(pattern).toFormatter

  /** Creates a formatter using the specified pattern.
    *
    * This method will create a formatter based on a simple pattern of letters and symbols.
    * For example, {@code d MMM yyyy} will format 2011-12-03 as '3 Dec 2011'.
    *
    * See {@link #ofPattern(String)} for details of the pattern.
    *
    * The returned formatter will use the specified locale, but this can be changed
    * using {@link DateTimeFormatter#withLocale(Locale)}.
    *
    * @param pattern  the pattern to use, not null
    * @param locale  the locale to use, not null
    * @return the formatter based on the pattern, not null
    * @throws IllegalArgumentException if the pattern is invalid
    * @see DateTimeFormatterBuilder#appendPattern(String)
    */
  def ofPattern(pattern: String, locale: Locale): DateTimeFormatter =
    new DateTimeFormatterBuilder().appendPattern(pattern).toFormatter(locale)

  /** Returns a locale specific date format.
    *
    * This returns a formatter that will print/parse a date.
    * The exact format pattern used varies by locale.
    *
    * The locale is determined from the formatter. The formatter returned directly by
    * this method will use the {@link Locale#getDefault() default locale}.
    * The locale can be controlled using {@link DateTimeFormatter#withLocale(Locale) withLocale(Locale)}
    * on the result of this method.
    *
    * Note that the localized pattern is looked up lazily.
    * This {@code DateTimeFormatter} holds the style required and the locale,
    * looking up the pattern required on demand.
    *
    * @param dateStyle  the formatter style to obtain, not null
    * @return the date formatter, not null
    */
  def ofLocalizedDate(dateStyle: FormatStyle): DateTimeFormatter = {
    Objects.requireNonNull(dateStyle, "dateStyle")
    new DateTimeFormatterBuilder()
      .appendLocalized(dateStyle, null)
      .toFormatter
      .withChronology(IsoChronology.INSTANCE)
  }

  /** Returns a locale specific time format.
    *
    * This returns a formatter that will print/parse a time.
    * The exact format pattern used varies by locale.
    *
    * The locale is determined from the formatter. The formatter returned directly by
    * this method will use the {@link Locale#getDefault() default locale}.
    * The locale can be controlled using {@link DateTimeFormatter#withLocale(Locale) withLocale(Locale)}
    * on the result of this method.
    *
    * Note that the localized pattern is looked up lazily.
    * This {@code DateTimeFormatter} holds the style required and the locale,
    * looking up the pattern required on demand.
    *
    * @param timeStyle  the formatter style to obtain, not null
    * @return the time formatter, not null
    */
  def ofLocalizedTime(timeStyle: FormatStyle): DateTimeFormatter = {
    Objects.requireNonNull(timeStyle, "timeStyle")
    new DateTimeFormatterBuilder()
      .appendLocalized(null, timeStyle)
      .toFormatter
      .withChronology(IsoChronology.INSTANCE)
  }

  /** Returns a locale specific date-time format, which is typically of short length.
    *
    * This returns a formatter that will print/parse a date-time.
    * The exact format pattern used varies by locale.
    *
    * The locale is determined from the formatter. The formatter returned directly by
    * this method will use the {@link Locale#getDefault() default locale}.
    * The locale can be controlled using {@link DateTimeFormatter#withLocale(Locale) withLocale(Locale)}
    * on the result of this method.
    *
    * Note that the localized pattern is looked up lazily.
    * This {@code DateTimeFormatter} holds the style required and the locale,
    * looking up the pattern required on demand.
    *
    * @param dateTimeStyle  the formatter style to obtain, not null
    * @return the date-time formatter, not null
    */
  def ofLocalizedDateTime(dateTimeStyle: FormatStyle): DateTimeFormatter = {
    Objects.requireNonNull(dateTimeStyle, "dateTimeStyle")
    new DateTimeFormatterBuilder()
      .appendLocalized(dateTimeStyle, dateTimeStyle)
      .toFormatter
      .withChronology(IsoChronology.INSTANCE)
  }

  /** Returns a locale specific date and time format.
    *
    * This returns a formatter that will print/parse a date-time.
    * The exact format pattern used varies by locale.
    *
    * The locale is determined from the formatter. The formatter returned directly by
    * this method will use the {@link Locale#getDefault() default locale}.
    * The locale can be controlled using {@link DateTimeFormatter#withLocale(Locale) withLocale(Locale)}
    * on the result of this method.
    *
    * Note that the localized pattern is looked up lazily.
    * This {@code DateTimeFormatter} holds the style required and the locale,
    * looking up the pattern required on demand.
    *
    * @param dateStyle  the date formatter style to obtain, not null
    * @param timeStyle  the time formatter style to obtain, not null
    * @return the date, time or date-time formatter, not null
    */
  def ofLocalizedDateTime(dateStyle: FormatStyle, timeStyle: FormatStyle): DateTimeFormatter = {
    Objects.requireNonNull(dateStyle, "dateStyle")
    Objects.requireNonNull(timeStyle, "timeStyle")
    new DateTimeFormatterBuilder()
      .appendLocalized(dateStyle, timeStyle)
      .toFormatter
      .withChronology(IsoChronology.INSTANCE)
  }

  /** A query that provides access to the excess days that were parsed.
    *
    * This returns a singleton {@linkplain TemporalQuery query} that provides
    * access to additional information from the parse. The query always returns
    * a non-null period, with a zero period returned instead of null.
    *
    * There are two situations where this query may return a non-zero period.
    * <ul>
    * <li>If the {@code ResolverStyle} is {@code LENIENT} and a time is parsed
    * without a date, then the complete result of the parse consists of a
    * {@code LocalTime} and an excess {@code Period} in days.
    *
    * <li>If the {@code ResolverStyle} is {@code SMART} and a time is parsed
    * without a date where the time is 24:00:00, then the complete result of
    * the parse consists of a {@code LocalTime} of 00:00:00 and an excess
    * {@code Period} of one day.
    * </ul>
    *
    * In both cases, if a complete {@code ChronoLocalDateTime} or {@code Instant}
    * is parsed, then the excess days are added to the date part.
    * As a result, this query will return a zero period.
    *
    * The {@code SMART} behaviour handles the common "end of day" 24:00 value.
    * Processing in {@code LENIENT} mode also produces the same result:
    * <pre>
    * Text to parse        Parsed object                         Excess days
    * "2012-12-03T00:00"   LocalDateTime.of(2012, 12, 3, 0, 0)   ZERO
    * "2012-12-03T24:00"   LocalDateTime.of(2012, 12, 4, 0, 0)   ZERO
    * "00:00"              LocalTime.of(0, 0)                    ZERO
    * "24:00"              LocalTime.of(0, 0)                    Period.ofDays(1)
    * </pre>
    * The query can be used as follows:
    * <pre>
    * TemporalAccessor parsed = formatter.parse(str);
    * LocalTime time = parsed.query(LocalTime.FROM);
    * Period extraDays = parsed.query(DateTimeFormatter.parsedExcessDays());
    * </pre>
    * @return a query that provides access to the excess days that were parsed
    */
  def parsedExcessDays: TemporalQuery[Period] = PARSED_EXCESS_DAYS

  private lazy val PARSED_EXCESS_DAYS: TemporalQuery[Period] =
    new TemporalQuery[Period] {
      override def queryFrom(temporal: TemporalAccessor): Period =
        temporal match {
          case builder: DateTimeBuilder => builder.excessDays
          case _                        => Period.ZERO
        }
    }

  /** A query that provides access to whether a leap-second was parsed.
    *
    * This returns a singleton {@linkplain TemporalQuery query} that provides
    * access to additional information from the parse. The query always returns
    * a non-null boolean, true if parsing saw a leap-second, false if not.
    *
    * Instant parsing handles the special "leap second" time of '23:59:60'.
    * Leap seconds occur at '23:59:60' in the UTC time-zone, but at other
    * local times in different time-zones. To avoid this potential ambiguity,
    * the handling of leap-seconds is limited to
    * {@link DateTimeFormatterBuilder#appendInstant()}, as that method
    * always parses the instant with the UTC zone offset.
    *
    * If the time '23:59:60' is received, then a simple conversion is applied,
    * replacing the second-of-minute of 60 with 59. This query can be used
    * on the parse result to determine if the leap-second adjustment was made.
    * The query will return one second of excess if it did adjust to remove
    * the leap-second, and zero if not. Note that applying a leap-second
    * smoothing mechanism, such as UTC-SLS, is the responsibility of the
    * application, as follows:
    * <pre>
    * TemporalAccessor parsed = formatter.parse(str);
    * Instant instant = parsed.query(Instant::from);
    * if (parsed.query(DateTimeFormatter.parsedLeapSecond())) {
    * // validate leap-second is correct and apply correct smoothing
    * }
    * </pre>
    * @return a query that provides access to whether a leap-second was parsed
    */
  def parsedLeapSecond: TemporalQuery[Boolean] = PARSED_LEAP_SECOND

  private lazy val PARSED_LEAP_SECOND: TemporalQuery[Boolean] =
    new TemporalQuery[Boolean] {
      override def queryFrom(temporal: TemporalAccessor): Boolean =
        temporal match {
          case builder: DateTimeBuilder => builder.leapSecond
          case _                        => false
        }
    }

  /** Implements the classic Java Format API.
    *
    * @param formatter The formatter
    * @param query The query to be parsed
    */
  private[format] class ClassicFormat(
    private val formatter: DateTimeFormatter,
    private val query:     TemporalQuery[_ <: AnyRef]
  ) extends Format {

    def format(obj: AnyRef, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = {
      Objects.requireNonNull(obj, "obj")
      Objects.requireNonNull(toAppendTo, "toAppendTo")
      Objects.requireNonNull(pos, "pos")
      if (!obj.isInstanceOf[TemporalAccessor])
        throw new IllegalArgumentException("Format target must implement TemporalAccessor")
      pos.setBeginIndex(0)
      pos.setEndIndex(0)
      try formatter.formatTo(obj.asInstanceOf[TemporalAccessor], toAppendTo)
      catch {
        case ex: RuntimeException =>
          throw new IllegalArgumentException(ex.getMessage, ex)
      }
      toAppendTo
    }

    @throws[ParseException]
    override def parseObject(text: String): AnyRef = {
      Objects.requireNonNull(text, "text")
      try {
        if (query == null)
          return formatter
            .parseToBuilder(text, null)
            .resolve(formatter.getResolverStyle, formatter.getResolverFields)
        formatter.parse(text, query)
      } catch {
        case ex: DateTimeParseException =>
          throw new ParseException(ex.getMessage, ex.getErrorIndex)
        case ex: RuntimeException =>
          throw new ParseException(ex.getMessage, 0).initCause(ex).asInstanceOf[ParseException]
      }
    }

    def parseObject(text: String, pos: ParsePosition): AnyRef = {
      Objects.requireNonNull(text, "text")
      var unresolved: TTBPDateTimeParseContext#Parsed = null
      try unresolved = formatter.parseUnresolved0(text, pos)
      catch {
        case _: IndexOutOfBoundsException =>
          if (pos.getErrorIndex < 0)
            pos.setErrorIndex(0)
          return null
      }
      if (unresolved == null) {
        if (pos.getErrorIndex < 0)
          pos.setErrorIndex(0)
        return null
      }
      try {
        val builder: DateTimeBuilder =
          unresolved.toBuilder.resolve(formatter.getResolverStyle, formatter.getResolverFields)
        if (query == null)
          return builder
        builder.build(query)
      } catch {
        case _: RuntimeException =>
          pos.setErrorIndex(0)
          null
      }
    }
  }
}

/** Formatter for printing and parsing date-time objects.
  *
  * This class provides the main application entry point for printing and parsing.
  * Common instances of {@code DateTimeFormatter} are provided:
  *<ul>
  * <li>Using pattern letters, such as {@code yyyy-MMM-dd}
  * <li>Using localized styles, such as {@code long} or {@code medium}
  * <li>Using predefined constants, such as {@link #ISO_LOCAL_DATE}
  * </ul><p>
  *
  * For more complex formatters, a {@link DateTimeFormatterBuilder builder} is provided.
  *
  * In most cases, it is not necessary to use this class directly when formatting.
  * The main date-time classes provide two methods - one for formatting,
  * {@code format(DateTimeFormatter formatter)}, and one for parsing,
  * For example:
  * <pre>
  * String text = date.format(formatter);
  * LocalDate date = LocalDate.parse(text, formatter);
  * </pre>
  * Some aspects of printing and parsing are dependent on the locale.
  * The locale can be changed using the {@link #withLocale(Locale)} method
  * which returns a new formatter in the requested locale.
  *
  * Some applications may need to use the older {@link Format} class for formatting.
  * The {@link #toFormat()} method returns an implementation of the old API.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor
  *
  * @param printerParser  the printer/parser to use, not null
  * @param locale  the locale to use, not null
  * @param decimalStyle  the decimal style to use, not null
  * @param resolverStyle  the resolver style to use, not null
  * @param resolverFields  the fields to use during resolving, null for all fields
  * @param chrono  the chronology to use, null for no override
  * @param zone  the zone to use, null for no override
  */
final class DateTimeFormatter private[format] (
  private val printerParser:  CompositePrinterParser,
  private val locale:         Locale,
  private val decimalStyle:   DecimalStyle,
  private val resolverStyle:  ResolverStyle,
  private val resolverFields: java.util.Set[TemporalField],
  private val chrono:         Chronology,
  private val zone:           ZoneId
) {

  Objects.requireNonNull(printerParser, "printerParser")
  Objects.requireNonNull(locale, "locale")
  Objects.requireNonNull(decimalStyle, "decimalStyle")
  Objects.requireNonNull(resolverStyle, "resolverStyle")

  /** Gets the locale to be used during formatting.
    *
    * This is used to lookup any part of the formatter needing specific
    * localization, such as the text or localized pattern.
    *
    * @return the locale of this formatter, not null
    */
  def getLocale: Locale = locale

  /** Returns a copy of this formatter with a new locale.
    *
    * This is used to lookup any part of the formatter needing specific
    * localization, such as the text or localized pattern.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param locale  the new locale, not null
    * @return a formatter based on this formatter with the requested locale, not null
    */
  def withLocale(locale: Locale): DateTimeFormatter =
    if (this.locale == locale)
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)

  /** Gets the decimal style to be used during formatting.
    *
    * @return the decimal style of this formatter, not null
    */
  def getDecimalStyle: DecimalStyle = decimalStyle

  /** Returns a copy of this formatter with a new decimal style.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param decimalStyle  the new decimal style, not null
    * @return a formatter based on this formatter with the requested symbols, not null
    */
  def withDecimalStyle(decimalStyle: DecimalStyle): DateTimeFormatter =
    if (this.decimalStyle == decimalStyle)
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)

  /** Gets the overriding chronology to be used during formatting.
    *
    * This returns the override chronology, used to convert dates.
    * By default, a formatter has no override chronology, returning null.
    * See {@link #withChronology(Chronology)} for more details on overriding.
    *
    * @return the chronology of this formatter, null if no override
    */
  def getChronology: Chronology = chrono

  /** Returns a copy of this formatter with a new override chronology.
    *
    * This returns a formatter with similar state to this formatter but
    * with the override chronology set.
    * By default, a formatter has no override chronology, returning null.
    *
    * If an override is added, then any date that is printed or parsed will be affected.
    *
    * When printing, if the {@code Temporal} object contains a date then it will
    * be converted to a date in the override chronology.
    * Any time or zone will be retained unless overridden.
    * The converted result will behave in a manner equivalent to an implementation
    * of {@code ChronoLocalDate},{@code ChronoLocalDateTime} or {@code ChronoZonedDateTime}.
    *
    * When parsing, the override chronology will be used to interpret the
    * {@linkplain ChronoField fields} into a date unless the
    * formatter directly parses a valid chronology.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param chrono  the new chronology, not null
    * @return a formatter based on this formatter with the requested override chronology, not null
    */
  def withChronology(chrono: Chronology): DateTimeFormatter =
    if (Objects.equals(this.chrono, chrono))
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)

  /** Gets the overriding zone to be used during formatting.
    *
    * This returns the override zone, used to convert instants.
    * By default, a formatter has no override zone, returning null.
    * See {@link #withZone(ZoneId)} for more details on overriding.
    *
    * @return the chronology of this formatter, null if no override
    */
  def getZone: ZoneId = zone

  /** Returns a copy of this formatter with a new override zone.
    *
    * This returns a formatter with similar state to this formatter but
    * with the override zone set.
    * By default, a formatter has no override zone, returning null.
    *
    * If an override is added, then any instant that is printed or parsed will be affected.
    *
    * When printing, if the {@code Temporal} object contains an instant then it will
    * be converted to a zoned date-time using the override zone.
    * If the input has a chronology then it will be retained unless overridden.
    * If the input does not have a chronology, such as {@code Instant}, then
    * the ISO chronology will be used.
    * The converted result will behave in a manner equivalent to an implementation
    * of {@code ChronoZonedDateTime}.
    *
    * When parsing, the override zone will be used to interpret the
    * {@linkplain ChronoField fields} into an instant unless the
    * formatter directly parses a valid zone.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param zone  the new override zone, not null
    * @return a formatter based on this formatter with the requested override zone, not null
    */
  def withZone(zone: ZoneId): DateTimeFormatter =
    if (Objects.equals(this.zone, zone))
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)

  /** Gets the resolver style to use during parsing.
    *
    * This returns the resolver style, used during the second phase of parsing
    * when fields are resolved into dates and times.
    * By default, a formatter has the {@link ResolverStyle#SMART SMART} resolver style.
    * See {@link #withResolverStyle(ResolverStyle)} for more details.
    *
    * @return the resolver style of this formatter, not null
    */
  def getResolverStyle: ResolverStyle = resolverStyle

  /** Returns a copy of this formatter with a new resolver style.
    *
    * This returns a formatter with similar state to this formatter but
    * with the resolver style set. By default, a formatter has the
    * {@link ResolverStyle#SMART SMART} resolver style.
    *
    * Changing the resolver style only has an effect during parsing.
    * Parsing a text string occurs in two phases.
    * Phase 1 is a basic text parse according to the fields added to the builder.
    * Phase 2 resolves the parsed field-value pairs into date and/or time objects.
    * The resolver style is used to control how phase 2, resolving, happens.
    * See {@code ResolverStyle} for more information on the options available.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param resolverStyle  the new resolver style, not null
    * @return a formatter based on this formatter with the requested resolver style, not null
    */
  def withResolverStyle(resolverStyle: ResolverStyle): DateTimeFormatter = {
    Objects.requireNonNull(resolverStyle, "resolverStyle")
    if (Objects.equals(this.resolverStyle, resolverStyle))
      this
    else
      new DateTimeFormatter(printerParser,
                            locale,
                            decimalStyle,
                            resolverStyle,
                            resolverFields,
                            chrono,
                            zone)
  }

  /** Gets the resolver fields to use during parsing.
    *
    * This returns the resolver fields, used during the second phase of parsing
    * when fields are resolved into dates and times.
    * By default, a formatter has no resolver fields, and thus returns null.
    * See {@link #withResolverFields(Set)} for more details.
    *
    * @return the immutable set of resolver fields of this formatter, null if no fields
    */
  def getResolverFields: java.util.Set[TemporalField] = resolverFields

  /** Returns a copy of this formatter with a new set of resolver fields.
    *
    * This returns a formatter with similar state to this formatter but with
    * the resolver fields set. By default, a formatter has no resolver fields.
    *
    * Changing the resolver fields only has an effect during parsing.
    * Parsing a text string occurs in two phases.
    * Phase 1 is a basic text parse according to the fields added to the builder.
    * Phase 2 resolves the parsed field-value pairs into date and/or time objects.
    * The resolver fields are used to filter the field-value pairs between phase 1 and 2.
    *
    * This can be used to select between two or more ways that a date or time might
    * be resolved. For example, if the formatter consists of year, month, day-of-month
    * and day-of-year, then there are two ways to resolve a date.
    * Calling this method with the arguments {@link ChronoField#YEAR YEAR} and
    * {@link ChronoField#DAY_OF_YEAR DAY_OF_YEAR} will ensure that the date is
    * resolved using the year and day-of-year, effectively meaning that the month
    * and day-of-month are ignored during the resolving phase.
    *
    * In a similar manner, this method can be used to ignore secondary fields that
    * would otherwise be cross-checked. For example, if the formatter consists of year,
    * month, day-of-month and day-of-week, then there is only one way to resolve a
    * date, but the parsed value for day-of-week will be cross-checked against the
    * resolved date. Calling this method with the arguments {@link ChronoField#YEAR YEAR},
    * {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} and
    * {@link ChronoField#DAY_OF_MONTH DAY_OF_MONTH} will ensure that the date is
    * resolved correctly, but without any cross-check for the day-of-week.
    *
    * In implementation terms, this method behaves as follows. The result of the
    * parsing phase can be considered to be a map of field to value. The behavior
    * of this method is to cause that map to be filtered between phase 1 and 2,
    * removing all fields other than those specified as arguments to this method.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param resolverFields  the new set of resolver fields, null if no fields
    * @return a formatter based on this formatter with the requested resolver style, not null
    */
  def withResolverFields(resolverFields: TemporalField*): DateTimeFormatter =
    if (resolverFields == null) {
      new DateTimeFormatter(printerParser, locale, decimalStyle, resolverStyle, null, chrono, zone)
    } else {
      val fields: java.util.Set[TemporalField] =
        new java.util.HashSet[TemporalField](Arrays.asList(resolverFields: _*))
      if (Objects.equals(this.resolverFields, fields)) {
        this
      } else {
        new DateTimeFormatter(printerParser,
                              locale,
                              decimalStyle,
                              resolverStyle,
                              Collections.unmodifiableSet(fields),
                              chrono,
                              zone)
      }
    }

  /** Returns a copy of this formatter with a new set of resolver fields.
    *
    * This returns a formatter with similar state to this formatter but with
    * the resolver fields set. By default, a formatter has no resolver fields.
    *
    * Changing the resolver fields only has an effect during parsing.
    * Parsing a text string occurs in two phases.
    * Phase 1 is a basic text parse according to the fields added to the builder.
    * Phase 2 resolves the parsed field-value pairs into date and/or time objects.
    * The resolver fields are used to filter the field-value pairs between phase 1 and 2.
    *
    * This can be used to select between two or more ways that a date or time might
    * be resolved. For example, if the formatter consists of year, month, day-of-month
    * and day-of-year, then there are two ways to resolve a date.
    * Calling this method with the arguments {@link ChronoField#YEAR YEAR} and
    * {@link ChronoField#DAY_OF_YEAR DAY_OF_YEAR} will ensure that the date is
    * resolved using the year and day-of-year, effectively meaning that the month
    * and day-of-month are ignored during the resolving phase.
    *
    * In a similar manner, this method can be used to ignore secondary fields that
    * would otherwise be cross-checked. For example, if the formatter consists of year,
    * month, day-of-month and day-of-week, then there is only one way to resolve a
    * date, but the parsed value for day-of-week will be cross-checked against the
    * resolved date. Calling this method with the arguments {@link ChronoField#YEAR YEAR},
    * {@link ChronoField#MONTH_OF_YEAR MONTH_OF_YEAR} and
    * {@link ChronoField#DAY_OF_MONTH DAY_OF_MONTH} will ensure that the date is
    * resolved correctly, but without any cross-check for the day-of-week.
    *
    * In implementation terms, this method behaves as follows. The result of the
    * parsing phase can be considered to be a map of field to value. The behavior
    * of this method is to cause that map to be filtered between phase 1 and 2,
    * removing all fields other than those specified as arguments to this method.
    *
    * This instance is immutable and unaffected by this method call.
    *
    * @param resolverFields  the new set of resolver fields, null if no fields
    * @return a formatter based on this formatter with the requested resolver style, not null
    */
  def withResolverFields(resolverFields: java.util.Set[TemporalField]): DateTimeFormatter =
    if (resolverFields == null) {
      new DateTimeFormatter(printerParser, locale, decimalStyle, resolverStyle, null, chrono, zone)
    } else {
      if (Objects.equals(this.resolverFields, resolverFields)) {
        this
      } else {
        val _resolverFields =
          Collections.unmodifiableSet(new java.util.HashSet[TemporalField](resolverFields))
        new DateTimeFormatter(printerParser,
                              locale,
                              decimalStyle,
                              resolverStyle,
                              _resolverFields,
                              chrono,
                              zone)
      }
    }

  /** Formats a date-time object using this formatter.
    *
    * This formats the date-time to a String using the rules of the formatter.
    *
    * @param temporal  the temporal object to print, not null
    * @return the printed string, not null
    * @throws DateTimeException if an error occurs during formatting
    */
  def format(temporal: TemporalAccessor): String = {
    val buf: StringBuilder = new StringBuilder(32)
    formatTo(temporal, buf)
    buf.toString
  }

  /** Formats a date-time object to an {@code Appendable} using this formatter.
    *
    * This formats the date-time to the specified destination.
    * {@link Appendable} is a general purpose interface that is implemented by all
    * key character output classes including {@code StringBuffer}, {@code StringBuilder},
    * {@code PrintStream} and {@code Writer}.
    *
    * Although {@code Appendable} methods throw an {@code IOException}, this method does not.
    * Instead, any {@code IOException} is wrapped in a runtime exception.
    *
    * @param temporal  the temporal object to print, not null
    * @param appendable  the appendable to print to, not null
    * @throws DateTimeException if an error occurs during formatting
    */
  def formatTo(temporal: TemporalAccessor, appendable: Appendable): Unit = {
    Objects.requireNonNull(temporal, "temporal")
    Objects.requireNonNull(appendable, "appendable")
    try {
      val context: TTBPDateTimePrintContext = new TTBPDateTimePrintContext(temporal, this)
      if (appendable.isInstanceOf[StringBuilder]) {
        printerParser.print(context, appendable.asInstanceOf[StringBuilder])
        ()
      } else {
        val buf = new StringBuilder(32)
        printerParser.print(context, buf)
        appendable.append(buf)
        ()
      }
    } catch {
      case ex: IOException =>
        throw new DateTimeException(ex.getMessage, ex)
    }
  }

  /** Fully parses the text producing a temporal object.
    *
    * This parses the entire text producing a temporal object.
    * It is typically more useful to use {@link #parse(CharSequence, TemporalQuery)}.
    * The result of this method is {@code TemporalAccessor} which has been resolved,
    * applying basic validation checks to help ensure a valid date-time.
    *
    * If the parse completes without reading the entire length of the text,
    * or a problem occurs during parsing or merging, then an exception is thrown.
    *
    * @param text  the text to parse, not null
    * @return the parsed temporal object, not null
    * @throws DateTimeParseException if unable to parse the requested result
    */
  def parse(text: CharSequence): TemporalAccessor = {
    Objects.requireNonNull(text, "text")
    try parseToBuilder(text, null).resolve(resolverStyle, resolverFields)
    catch {
      case ex: DateTimeParseException => throw ex
      case ex: RuntimeException       => throw createError(text, ex)
    }
  }

  /** Parses the text using this formatter, providing control over the text position.
    *
    * This parses the text without requiring the parse to start from the beginning
    * of the string or finish at the end.
    * The result of this method is {@code TemporalAccessor} which has been resolved,
    * applying basic validation checks to help ensure a valid date-time.
    *
    * The text will be parsed from the specified start {@code ParsePosition}.
    * The entire length of the text does not have to be parsed, the {@code ParsePosition}
    * will be updated with the index at the end of parsing.
    *
    * The operation of this method is slightly different to similar methods using
    * {@code ParsePosition} on {@code java.text.Format}. That class will return
    * errors using the error index on the {@code ParsePosition}. By contrast, this
    * method will throw a {@link DateTimeParseException} if an error occurs, with
    * the exception containing the error index.
    * This change in behavior is necessary due to the increased complexity of
    * parsing and resolving dates/times in this API.
    *
    * If the formatter parses the same field more than once with different values,
    * the result will be an error.
    *
    * @param text  the text to parse, not null
    * @param position  the position to parse from, updated with length parsed
    *                  and the index of any error, not null
    * @return the parsed temporal object, not null
    * @throws DateTimeParseException if unable to parse the requested result
    * @throws IndexOutOfBoundsException if the position is invalid
    */
  def parse(text: CharSequence, position: ParsePosition): TemporalAccessor = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(position, "position")
    try parseToBuilder(text, position).resolve(resolverStyle, resolverFields)
    catch {
      case ex: DateTimeParseException    => throw ex
      case ex: IndexOutOfBoundsException => throw ex
      case ex: RuntimeException          => throw createError(text, ex)
    }
  }

  /** Fully parses the text producing an object of the specified type.
    *
    * Most applications should use this method for parsing.
    * It parses the entire text to produce the required date-time.
    * For example:
    * <pre>
    * LocalDateTime dt = parser.parse(str, LocalDateTime.FROM);
    * </pre>
    * If the parse completes without reading the entire length of the text,
    * or a problem occurs during parsing or merging, then an exception is thrown.
    *
    * @tparam T the type to extract
    * @param text  the text to parse, not null
    * @param type  the type to extract, not null
    * @return the parsed date-time, not null
    * @throws DateTimeParseException if unable to parse the requested result
    */
  def parse[T](text: CharSequence, `type`: TemporalQuery[T]): T = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(`type`, "type")
    try {
      val builder: DateTimeBuilder =
        parseToBuilder(text, null).resolve(resolverStyle, resolverFields)
      builder.build(`type`)
    } catch {
      case ex: DateTimeParseException => throw ex
      case ex: RuntimeException       => throw createError(text, ex)
    }
  }

  /** Fully parses the text producing an object of one of the specified types.
    *
    * This parse method is convenient for use when the parser can handle optional elements.
    * For example, a pattern of 'yyyy[-MM[-dd]]' can be fully parsed to a {@code LocalDate},
    * or partially parsed to a {@code YearMonth} or a {@code Year}.
    * The types must be specified in order, starting from the best matching full-parse option
    * and ending with the worst matching minimal parse option.
    *
    * The result is associated with the first type that successfully parses.
    * Normally, applications will use {@code instanceof} to check the result.
    * For example:
    * <pre>
    * TemporalAccessor dt = parser.parseBest(str, LocalDate.FROM, YearMonth.FROM);
    * if (dt instanceof LocalDate) {
    * ...
    * } else {
    * ...
    * }
    * </pre>
    * If the parse completes without reading the entire length of the text,
    * or a problem occurs during parsing or merging, then an exception is thrown.
    *
    * @param text  the text to parse, not null
    * @param types  the types to attempt to parse to, which must implement { @code TemporalAccessor}, not null
    * @return the parsed date-time, not null
    * @throws IllegalArgumentException if less than 2 types are specified
    * @throws DateTimeParseException if unable to parse the requested result
    */
  @scala.annotation.varargs
  def parseBest(text: CharSequence, types: TemporalQuery[_]*): TemporalAccessor = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(types, "types")
    if (types.length < 2) {
      throw new IllegalArgumentException("At least two types must be specified")
    }
    try {
      val builder: DateTimeBuilder =
        parseToBuilder(text, null).resolve(resolverStyle, resolverFields)
      for (tpe <- types) {
        try return builder.build(tpe).asInstanceOf[TemporalAccessor]
        catch {
          case _: RuntimeException =>
        }
      }
      throw new DateTimeException(
        s"Unable to convert parsed text to any specified type: ${types.mkString("[", ", ", "]")}"
      )
    } catch {
      case ex: DateTimeParseException => throw ex
      case ex: RuntimeException       => throw createError(text, ex)
    }
  }

  private def createError(text: CharSequence, ex: RuntimeException): DateTimeParseException = {
    val abbr =
      if (text.length > 64) {
        text.subSequence(0, 64).toString + "..."
      } else {
        text.toString
      }
    new DateTimeParseException(s"Text '$abbr' could not be parsed: ${ex.getMessage}", text, 0, ex)
  }

  /** Parses the text to a builder.
    *
    * This parses to a {@code DateTimeBuilder} ensuring that the text is fully parsed.
    * This method throws {@link DateTimeParseException} if unable to parse, or
    * some other {@code DateTimeException} if another date/time problem occurs.
    *
    * @param text  the text to parse, not null
    * @param position  the position to parse from, updated with length parsed
    *                  and the index of any error, null if parsing whole string
    * @return the engine representing the result of the parse, not null
    * @throws DateTimeParseException if the parse fails
    */
  private def parseToBuilder(text: CharSequence, position: ParsePosition): DateTimeBuilder = {
    val pos: ParsePosition                      = if (position != null) position else new ParsePosition(0)
    val result: TTBPDateTimeParseContext#Parsed = parseUnresolved0(text, pos)
    if (result == null || pos.getErrorIndex >= 0 || (position == null && pos.getIndex < text.length)) {
      val abbr =
        if (text.length > 64)
          text.subSequence(0, 64).toString + "..."
        else
          text.toString
      if (pos.getErrorIndex >= 0)
        throw new DateTimeParseException(
          s"Text '$abbr' could not be parsed at index ${pos.getErrorIndex}",
          text,
          pos.getErrorIndex
        )
      else
        throw new DateTimeParseException(
          s"Text '$abbr' could not be parsed, unparsed text found at index ${pos.getIndex}",
          text,
          pos.getIndex
        )
    }
    result.toBuilder
  }

  /** Parses the text using this formatter, without resolving the result, intended
    * for advanced use cases.
    *
    * Parsing is implemented as a two-phase operation.
    * First, the text is parsed using the layout defined by the formatter, producing
    * a {@code Map} of field to value, a {@code ZoneId} and a {@code Chronology}.
    * Second, the parsed data is <em>resolved</em>, by validating, combining and
    * simplifying the various fields into more useful ones.
    * This method performs the parsing stage but not the resolving stage.
    *
    * The result of this method is {@code TemporalAccessor} which represents the
    * data as seen in the input. Values are not validated, thus parsing a date string
    * of '2012-00-65' would result in a temporal with three fields - year of '2012',
    * month of '0' and day-of-month of '65'.
    *
    * The text will be parsed from the specified start {@code ParsePosition}.
    * The entire length of the text does not have to be parsed, the {@code ParsePosition}
    * will be updated with the index at the end of parsing.
    *
    * Errors are returned using the error index field of the {@code ParsePosition}
    * instead of {@code DateTimeParseException}.
    * The returned error index will be set to an index indicative of the error.
    * Callers must check for errors before using the context.
    *
    * If the formatter parses the same field more than once with different values,
    * the result will be an error.
    *
    * This method is intended for advanced use cases that need access to the
    * internal state during parsing. Typical application code should use
    * {@link #parse(CharSequence, TemporalQuery)} or the parse method on the target type.
    *
    * @param text  the text to parse, not null
    * @param position  the position to parse from, updated with length parsed
    *                  and the index of any error, not null
    * @return the parsed text, null if the parse results in an error
    * @throws DateTimeException if some problem occurs during parsing
    * @throws IndexOutOfBoundsException if the position is invalid
    */
  def parseUnresolved(text: CharSequence, position: ParsePosition): TemporalAccessor =
    parseUnresolved0(text, position)

  private def parseUnresolved0(
    text:     CharSequence,
    position: ParsePosition
  ): TTBPDateTimeParseContext#Parsed = {
    Objects.requireNonNull(text, "text")
    Objects.requireNonNull(position, "position")
    val context: TTBPDateTimeParseContext = new TTBPDateTimeParseContext(this)
    var pos: Int                          = position.getIndex
    pos = printerParser.parse(context, text, pos)
    if (pos < 0) {
      position.setErrorIndex(~pos)
      return null
    }
    position.setIndex(pos)
    context.toParsed
  }

  /** Returns the formatter as a composite printer parser.
    *
    * @param optional  whether the printer/parser should be optional
    * @return the printer/parser, not null
    */
  private[format] def toPrinterParser(optional: Boolean): CompositePrinterParser =
    printerParser.withOptional(optional)

  /** Returns this formatter as a {@code java.text.Format} instance.
    *
    * The returned {@link Format} instance will print any {@link TemporalAccessor}
    * and parses to a resolved {@link TemporalAccessor}.
    *
    * Exceptions will follow the definitions of {@code Format}, see those methods
    * for details about {@code IllegalArgumentException} during formatting and
    * {@code ParseException} or null during parsing.
    * The format does not support attributing of the returned format string.
    *
    * @return this formatter as a classic format instance, not null
    */
  def toFormat: Format = new DateTimeFormatter.ClassicFormat(this, null)

  /** Returns this formatter as a {@code java.text.Format} instance that will
    * parse to the specified type.
    *
    * The returned {@link Format} instance will print any {@link TemporalAccessor}
    * and parses to the type specified.
    * The type must be one that is supported by {@link #parse}.
    *
    * Exceptions will follow the definitions of {@code Format}, see those methods
    * for details about {@code IllegalArgumentException} during formatting and
    * {@code ParseException} or null during parsing.
    * The format does not support attributing of the returned format string.
    *
    * @param query  the query to parse to, not null
    * @return this formatter as a classic format instance, not null
    */
  def toFormat(query: TemporalQuery[_ <: AnyRef]): Format = {
    Objects.requireNonNull(query, "query")
    new DateTimeFormatter.ClassicFormat(this, query)
  }

  /** Returns a description of the underlying formatters.
    *
    * @return a description of this formatter, not null
    */
  override def toString: String = {
    val pattern: String = printerParser.toString
    if (pattern.startsWith("[")) pattern else pattern.substring(1, pattern.length - 1)
  }
}
