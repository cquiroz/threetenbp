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

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.{ Comparator, Locale, Objects }
import java.lang.Long

import org.threeten.bp.ZoneId
import org.threeten.bp.ZoneOffset
import org.threeten.bp.chrono.ChronoLocalDate
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.IsoFields
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.format.internal.TTBPDateTimeTextProvider
import org.threeten.bp.format.internal.TTBPSimpleDateTimeTextProvider
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder

object DateTimeFormatterBuilder {

  /** Query for a time-zone that is region-only. */
  private lazy val QUERY_REGION_ONLY: TemporalQuery[ZoneId] =
    new TemporalQuery[ZoneId] {
      override def queryFrom(temporal: TemporalAccessor): ZoneId = {
        val zone: ZoneId = temporal.query(TemporalQueries.zoneId)
        if (zone != null && !zone.isInstanceOf[ZoneOffset]) zone else null
      }
    }

  /** Gets the formatting pattern for date and time styles for a locale and chronology.
    * The locale and chronology are used to lookup the locale specific format
    * for the requested dateStyle and/or timeStyle.
    *
    * @param dateStyle  the FormatStyle for the date
    * @param timeStyle  the FormatStyle for the time
    * @param chrono  the Chronology, non-null
    * @param locale  the locale, non-null
    * @return the locale and Chronology specific formatting pattern
    * @throws IllegalArgumentException if both dateStyle and timeStyle are null
    */
  def getLocalizedDateTimePattern(
    dateStyle: FormatStyle,
    timeStyle: FormatStyle,
    chrono:    Chronology,
    locale:    Locale
  ): String = {
    Objects.requireNonNull(locale, "locale")
    Objects.requireNonNull(chrono, "chrono")
    if (dateStyle == null && timeStyle == null)
      throw new IllegalArgumentException("Either dateStyle or timeStyle must be non-null")
    val dateFormat: DateFormat =
      if (dateStyle != null) {
        if (timeStyle != null) {
          DateFormat.getDateTimeInstance(dateStyle.ordinal, timeStyle.ordinal, locale)
        } else {
          DateFormat.getDateInstance(dateStyle.ordinal, locale)
        }
      } else {
        DateFormat.getTimeInstance(timeStyle.ordinal, locale)
      }
    if (dateFormat.isInstanceOf[SimpleDateFormat]) {
      dateFormat.asInstanceOf[SimpleDateFormat].toPattern
    } else {
      throw new IllegalArgumentException("Unable to determine pattern")
    }
  }

  /** Map of letters to fields. */
  private lazy val FIELD_MAP: java.util.Map[Character, TemporalField] = {
    // Size checked
    val map = new java.util.HashMap[Character, TemporalField]
    map.put('G', ChronoField.ERA)
    map.put('y', ChronoField.YEAR_OF_ERA)
    map.put('u', ChronoField.YEAR)
    map.put('Q', IsoFields.QUARTER_OF_YEAR)
    map.put('q', IsoFields.QUARTER_OF_YEAR)
    map.put('M', ChronoField.MONTH_OF_YEAR)
    map.put('L', ChronoField.MONTH_OF_YEAR)
    map.put('D', ChronoField.DAY_OF_YEAR)
    map.put('d', ChronoField.DAY_OF_MONTH)
    map.put('F', ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH)
    map.put('E', ChronoField.DAY_OF_WEEK)
    map.put('c', ChronoField.DAY_OF_WEEK)
    map.put('e', ChronoField.DAY_OF_WEEK)
    map.put('a', ChronoField.AMPM_OF_DAY)
    map.put('H', ChronoField.HOUR_OF_DAY)
    map.put('k', ChronoField.CLOCK_HOUR_OF_DAY)
    map.put('K', ChronoField.HOUR_OF_AMPM)
    map.put('h', ChronoField.CLOCK_HOUR_OF_AMPM)
    map.put('m', ChronoField.MINUTE_OF_HOUR)
    map.put('s', ChronoField.SECOND_OF_MINUTE)
    map.put('S', ChronoField.NANO_OF_SECOND)
    map.put('A', ChronoField.MILLI_OF_DAY)
    map.put('n', ChronoField.NANO_OF_SECOND)
    map.put('N', ChronoField.NANO_OF_DAY)
    map
  }

  /** Length comparator. */
  private[format] lazy val LENGTH_SORT: Comparator[String] =
    new Comparator[String] {
      override def compare(str1: String, str2: String): Int =
        if (str1.length == str2.length) str1.compareTo(str2) else str1.length - str2.length
    }
}

/** Builder to create date-time formatters.
  *
  * This allows a {@code DateTimeFormatter} to be created.
  * All date-time formatters are created ultimately using this builder.
  *
  * The basic elements of date-time can all be added:
  *<ul>
  * <li>Value - a numeric value</li>
  * <li>Fraction - a fractional value including the decimal place. Always use this when
  * outputting fractions to ensure that the fraction is parsed correctly</li>
  * <li>Text - the textual equivalent for the value</li>
  * <li>OffsetId/Offset - the {@linkplain ZoneOffset zone offset}</li>
  * <li>ZoneId - the {@linkplain ZoneId time-zone} id</li>
  * <li>ZoneText - the name of the time-zone</li>
  * <li>Literal - a text literal</li>
  * <li>Nested and Optional - formats can be nested or made optional</li>
  * <li>Other - the printer and parser interfaces can be used to add user supplied formatting</li>
  * </ul><p>
  * In addition, any of the elements may be decorated by padding, either with spaces or any other character.
  *
  * Finally, a shorthand pattern, mostly compatible with {@code java.text.SimpleDateFormat SimpleDateFormat}
  * can be used, see {@link #appendPattern(String)}.
  * In practice, this simply parses the pattern and calls other methods on the builder.
  *
  * <h3>Specification for implementors</h3>
  * This class is a mutable builder intended for use from a single thread.
  *
  * @constructor Constructs a new instance of the builder.
  *
  * @param parent  the parent builder, not null
  * @param optional  whether the formatter is optional, not null
  */
final class DateTimeFormatterBuilder private (
  private val parent:   DateTimeFormatterBuilder,
  private val optional: Boolean
) {

  /** Constructs a new instance of the builder. */
  def this() {
    this(null, false)
  }

  /** The currently active builder, used by the outermost builder. */
  private var active: DateTimeFormatterBuilder = this

  /** The list of printers that will be used. */
  private val printerParsers: java.util.List[TTBPDateTimeFormatterBuilder.DateTimePrinterParser] =
    new java.util.ArrayList[TTBPDateTimeFormatterBuilder.DateTimePrinterParser]

  /** The width to pad the next field to. */
  private var padNextWidth: Int = 0

  /** The character to pad the next field with. */
  private var padNextChar: Char = 0

  /** The index of the last variable width value parser. */
  private var valueParserIndex: Int = -1

  /** Changes the parse style to be case sensitive for the remainder of the formatter.
    *
    * Parsing can be case sensitive or insensitive - by default it is case sensitive.
    * This method allows the case sensitivity setting of parsing to be changed.
    *
    * Calling this method changes the state of the builder such that all
    * subsequent builder method calls will parse text in case sensitive mode.
    * See {@link #parseCaseInsensitive} for the opposite setting.
    * The parse case sensitive/insensitive methods may be called at any point
    * in the builder, thus the parser can swap between case parsing modes
    * multiple times during the parse.
    *
    * Since the default is case sensitive, this method should only be used after
    * a previous call to {@code #parseCaseInsensitive}.
    *
    * @return this, for chaining, not null
    */
  def parseCaseSensitive: DateTimeFormatterBuilder = {
    appendInternal(TTBPDateTimeFormatterBuilder.SettingsParser.SENSITIVE)
    this
  }

  /** Changes the parse style to be case insensitive for the remainder of the formatter.
    *
    * Parsing can be case sensitive or insensitive - by default it is case sensitive.
    * This method allows the case sensitivity setting of parsing to be changed.
    *
    * Calling this method changes the state of the builder such that all
    * subsequent builder method calls will parse text in case sensitive mode.
    * See {@link #parseCaseSensitive()} for the opposite setting.
    * The parse case sensitive/insensitive methods may be called at any point
    * in the builder, thus the parser can swap between case parsing modes
    * multiple times during the parse.
    *
    * @return this, for chaining, not null
    */
  def parseCaseInsensitive: DateTimeFormatterBuilder = {
    appendInternal(TTBPDateTimeFormatterBuilder.SettingsParser.INSENSITIVE)
    this
  }

  /** Changes the parse style to be strict for the remainder of the formatter.
    *
    * Parsing can be strict or lenient - by default its strict.
    * This controls the degree of flexibility in matching the text and sign styles.
    *
    * When used, this method changes the parsing to be strict from this point onwards.
    * As strict is the default, this is normally only needed after calling {@link #parseLenient()}.
    * The change will remain in force until the end of the formatter that is eventually
    * constructed or until {@code parseLenient} is called.
    *
    * @return this, for chaining, not null
    */
  def parseStrict: DateTimeFormatterBuilder = {
    appendInternal(TTBPDateTimeFormatterBuilder.SettingsParser.STRICT)
    this
  }

  /** Changes the parse style to be lenient for the remainder of the formatter.
    * Note that case sensitivity is set separately to this method.
    *
    * Parsing can be strict or lenient - by default its strict.
    * This controls the degree of flexibility in matching the text and sign styles.
    * Applications calling this method should typically also call {@link #parseCaseInsensitive()}.
    *
    * When used, this method changes the parsing to be strict from this point onwards.
    * The change will remain in force until the end of the formatter that is eventually
    * constructed or until {@code parseStrict} is called.
    *
    * @return this, for chaining, not null
    */
  def parseLenient: DateTimeFormatterBuilder = {
    appendInternal(TTBPDateTimeFormatterBuilder.SettingsParser.LENIENT)
    this
  }

  /** Appends a default value for a field to the formatter for use in parsing.
    *
    * This appends an instruction to the builder to inject a default value
    * into the parsed result. This is especially useful in conjunction with
    * optional parts of the formatter.
    *
    * For example, consider a formatter that parses the year, followed by
    * an optional month, with a further optional day-of-month. Using such a
    * formatter would require the calling code to check whether a full date,
    * year-month or just a year had been parsed. This method can be used to
    * default the month and day-of-month to a sensible value, such as the
    * first of the month, allowing the calling code to always get a date.
    *
    * During formatting, this method has no effect.
    *
    * During parsing, the current state of the parse is inspected.
    * If the specified field has no associated value, because it has not been
    * parsed successfully at that point, then the specified value is injected
    * into the parse result. Injection is immediate, thus the field-value pair
    * will be visible to any subsequent elements in the formatter.
    * As such, this method is normally called at the end of the builder.
    *
    * @param field  the field to default the value of, not null
    * @param value  the value to default the field to
    * @return this, for chaining, not null
    */
  def parseDefaulting(field: TemporalField, value: Long): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    appendInternal(new TTBPDateTimeFormatterBuilder.DefaultingParser(field, value))
    this
  }

  /** Appends the value of a date-time field to the formatter using a normal
    * output style.
    *
    * The value of the field will be output during a print.
    * If the value cannot be obtained then an exception will be thrown.
    *
    * The value will be printed as per the normal print of an integer value.
    * Only negative numbers will be signed. No padding will be added.
    *
    * The parser for a variable width value such as this normally behaves greedily,
    * requiring one digit, but accepting as many digits as possible.
    * This behavior can be affected by 'adjacent value parsing'.
    * See {@link #appendValue(TemporalField, int)} for full details.
    *
    * @param field  the field to append, not null
    * @return this, for chaining, not null
    */
  def appendValue(field: TemporalField): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    appendValue(
      new TTBPDateTimeFormatterBuilder.NumberPrinterParser(field, 1, 19, SignStyle.NORMAL)
    )
    this
  }

  /** Appends the value of a date-time field to the formatter using a fixed
    * width, zero-padded approach.
    *
    * The value of the field will be output during a print.
    * If the value cannot be obtained then an exception will be thrown.
    *
    * The value will be zero-padded on the left. If the size of the value
    * means that it cannot be printed within the width then an exception is thrown.
    * If the value of the field is negative then an exception is thrown during printing.
    *
    * This method supports a special technique of parsing known as 'adjacent value parsing'.
    * This technique solves the problem where a variable length value is followed by one or more
    * fixed length values. The standard parser is greedy, and thus it would normally
    * steal the digits that are needed by the fixed width value parsers that follow the
    * variable width one.
    *
    * No action is required to initiate 'adjacent value parsing'.
    * When a call to {@code appendValue} with a variable width is made, the builder
    * enters adjacent value parsing setup mode. If the immediately subsequent method
    * call or calls on the same builder are to this method, then the parser will reserve
    * space so that the fixed width values can be parsed.
    *
    * For example, consider {@code builder.appendValue(YEAR).appendValue(MONTH_OF_YEAR, 2);}
    * The year is a variable width parse of between 1 and 19 digits.
    * The month is a fixed width parse of 2 digits.
    * Because these were appended to the same builder immediately after one another,
    * the year parser will reserve two digits for the month to parse.
    * Thus, the text '201106' will correctly parse to a year of 2011 and a month of 6.
    * Without adjacent value parsing, the year would greedily parse all six digits and leave
    * nothing for the month.
    *
    * Adjacent value parsing applies to each set of fixed width not-negative values in the parser
    * that immediately follow any kind of variable width value.
    * Calling any other append method will end the setup of adjacent value parsing.
    * Thus, in the unlikely event that you need to avoid adjacent value parsing behavior,
    * simply add the {@code appendValue} to another {@code DateTimeFormatterBuilder.
    * and add that to this builder.
    *
    * If adjacent parsing is active, then parsing must match exactly the specified
    * number of digits in both strict and lenient modes.
    * In addition, no positive or negative sign is permitted.
    *
    * @param field  the field to append, not null
    * @param width  the width of the printed field, from 1 to 19
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the width is invalid
    */
  def appendValue(field: TemporalField, width: Int): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    if (width < 1 || width > 19)
      throw new IllegalArgumentException(s"The width must be from 1 to 19 inclusive but was $width")
    appendValue(
      new TTBPDateTimeFormatterBuilder.NumberPrinterParser(field,
                                                           width,
                                                           width,
                                                           SignStyle.NOT_NEGATIVE)
    )
    this
  }

  /** Appends the value of a date-time field to the formatter providing full
    * control over printing.
    *
    * The value of the field will be output during a print.
    * If the value cannot be obtained then an exception will be thrown.
    *
    * This method provides full control of the numeric formatting, including
    * zero-padding and the positive/negative sign.
    *
    * The parser for a variable width value such as this normally behaves greedily,
    * accepting as many digits as possible.
    * This behavior can be affected by 'adjacent value parsing'.
    * See {@link #appendValue(TemporalField, int)} for full details.
    *
    * In strict parsing mode, the minimum number of parsed digits is {@code minWidth}.
    * In lenient parsing mode, the minimum number of parsed digits is one.
    *
    * If this method is invoked with equal minimum and maximum widths and a sign style of
    * {@code NOT_NEGATIVE} then it delegates to {@code appendValue(TemporalField,int)}.
    * In this scenario, the printing and parsing behavior described there occur.
    *
    * @param field  the field to append, not null
    * @param minWidth  the minimum field width of the printed field, from 1 to 19
    * @param maxWidth  the maximum field width of the printed field, from 1 to 19
    * @param signStyle  the positive/negative output style, not null
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the widths are invalid
    */
  def appendValue(
    field:     TemporalField,
    minWidth:  Int,
    maxWidth:  Int,
    signStyle: SignStyle
  ): DateTimeFormatterBuilder = {
    if (minWidth == maxWidth && (signStyle eq SignStyle.NOT_NEGATIVE))
      return appendValue(field, maxWidth)
    Objects.requireNonNull(field, "field")
    Objects.requireNonNull(signStyle, "signStyle")
    if (minWidth < 1 || minWidth > 19)
      throw new IllegalArgumentException(
        s"The minimum width must be from 1 to 19 inclusive but was $minWidth"
      )
    if (maxWidth < 1 || maxWidth > 19)
      throw new IllegalArgumentException(
        s"The maximum width must be from 1 to 19 inclusive but was $maxWidth"
      )
    if (maxWidth < minWidth)
      throw new IllegalArgumentException(
        s"The maximum width must exceed or equal the minimum width but $maxWidth < $minWidth"
      )
    appendValue(
      new TTBPDateTimeFormatterBuilder.NumberPrinterParser(field, minWidth, maxWidth, signStyle)
    )
    this
  }

  /** Appends the reduced value of a date-time field to the formatter.
    *
    * Since fields such as year vary by chronology, it is recommended to use the
    * {@link #appendValueReduced(TemporalField, int, int, ChronoLocalDate)} date}
    * variant of this method in most cases. This variant is suitable for
    * simple fields or working with only the ISO chronology.
    *
    * For formatting, the {@code width} and {@code maxWidth} are used to
    * determine the number of characters to format.
    * If they are equal then the format is fixed width.
    * If the value of the field is within the range of the {@code baseValue} using
    * {@code width} characters then the reduced value is formatted otherwise the value is
    * truncated to fit {@code maxWidth}.
    * The rightmost characters are output to match the width, left padding with zero.
    *
    * For strict parsing, the number of characters allowed by {@code width} to {@code maxWidth} are parsed.
    * For lenient parsing, the number of characters must be at least 1 and less than 10.
    * If the number of digits parsed is equal to {@code width} and the value is positive,
    * the value of the field is computed to be the first number greater than
    * or equal to the {@code baseValue} with the same least significant characters,
    * otherwise the value parsed is the field value.
    * This allows a reduced value to be entered for values in range of the baseValue
    * and width and absolute values can be entered for values outside the range.
    *
    * For example, a base value of {@code 1980} and a width of {@code 2} will have
    * valid values from {@code 1980} to {@code 2079}.
    * During parsing, the text {@code "12"} will result in the value {@code 2012} as that
    * is the value within the range where the last two characters are "12".
    * By contrast, parsing the text {@code "1915"} will result in the value {@code 1915}.
    *
    * @param field  the field to append, not null
    * @param width  the field width of the printed and parsed field, from 1 to 10
    * @param maxWidth  the maximum field width of the printed field, from 1 to 10
    * @param baseValue  the base value of the range of valid values
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the width or base value is invalid
    */
  def appendValueReduced(
    field:     TemporalField,
    width:     Int,
    maxWidth:  Int,
    baseValue: Int
  ): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    appendValue(
      new TTBPDateTimeFormatterBuilder.ReducedPrinterParser(field, width, maxWidth, baseValue, null)
    )
    this
  }

  /** Appends the reduced value of a date-time field to the formatter.
    *
    * This is typically used for formatting and parsing a two digit year.
    *
    * The base date is used to calculate the full value during parsing.
    * For example, if the base date is 1950-01-01 then parsed values for
    * a two digit year parse will be in the range 1950-01-01 to 2049-12-31.
    * Only the year would be extracted from the date, thus a base date of
    * 1950-08-25 would also parse to the range 1950-01-01 to 2049-12-31.
    * This behavior is necessary to support fields such as week-based-year
    * or other calendar systems where the parsed value does not align with
    * standard ISO years.
    *
    * The exact behavior is as follows. Parse the full set of fields and
    * determine the effective chronology using the last chronology if
    * it appears more than once. Then convert the base date to the
    * effective chronology. Then extract the specified field from the
    * chronology-specific base date and use it to determine the
    * {@code baseValue} used below.
    *
    * For formatting, the {@code width} and {@code maxWidth} are used to
    * determine the number of characters to format.
    * If they are equal then the format is fixed width.
    * If the value of the field is within the range of the {@code baseValue} using
    * {@code width} characters then the reduced value is formatted otherwise the value is
    * truncated to fit {@code maxWidth}.
    * The rightmost characters are output to match the width, left padding with zero.
    *
    * For strict parsing, the number of characters allowed by {@code width} to {@code maxWidth} are parsed.
    * For lenient parsing, the number of characters must be at least 1 and less than 10.
    * If the number of digits parsed is equal to {@code width} and the value is positive,
    * the value of the field is computed to be the first number greater than
    * or equal to the {@code baseValue} with the same least significant characters,
    * otherwise the value parsed is the field value.
    * This allows a reduced value to be entered for values in range of the baseValue
    * and width and absolute values can be entered for values outside the range.
    *
    * For example, a base value of {@code 1980} and a width of {@code 2} will have
    * valid values from {@code 1980} to {@code 2079}.
    * During parsing, the text {@code "12"} will result in the value {@code 2012} as that
    * is the value within the range where the last two characters are "12".
    * By contrast, parsing the text {@code "1915"} will result in the value {@code 1915}.
    *
    * @param field  the field to append, not null
    * @param width  the field width of the printed and parsed field, from 1 to 10
    * @param maxWidth  the maximum field width of the printed field, from 1 to 10
    * @param baseDate  the base date used to calculate the base value for the range
    *                  of valid values in the parsed chronology, not null
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the width or base value is invalid
    */
  def appendValueReduced(
    field:    TemporalField,
    width:    Int,
    maxWidth: Int,
    baseDate: ChronoLocalDate
  ): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    Objects.requireNonNull(baseDate, "baseDate")
    appendValue(
      new TTBPDateTimeFormatterBuilder.ReducedPrinterParser(field, width, maxWidth, 0, baseDate)
    )
    this
  }

  /** Appends a fixed width printer-parser.
    *
    * @param pp  the printer-parser, not null
    * @return this, for chaining, not null
    */
  private def appendValue(
    pp: TTBPDateTimeFormatterBuilder.NumberPrinterParser
  ): DateTimeFormatterBuilder = {
    // Size checked
    if (active.valueParserIndex >= 0 && active.printerParsers
          .get(active.valueParserIndex)
          .isInstanceOf[TTBPDateTimeFormatterBuilder.NumberPrinterParser]) {
      val activeValueParser: Int = active.valueParserIndex
      var basePP: TTBPDateTimeFormatterBuilder.NumberPrinterParser = active.printerParsers
        .get(activeValueParser)
        .asInstanceOf[TTBPDateTimeFormatterBuilder.NumberPrinterParser]
      if (pp.minWidth == pp.maxWidth && (pp.signStyle eq SignStyle.NOT_NEGATIVE)) {
        basePP = basePP.withSubsequentWidth(pp.maxWidth)
        appendInternal(pp.withFixedWidth)
        active.valueParserIndex = activeValueParser
      } else {
        basePP                  = basePP.withFixedWidth
        active.valueParserIndex = appendInternal(pp)
      }
      active.printerParsers.set(activeValueParser, basePP)
    } else {
      active.valueParserIndex = appendInternal(pp)
    }
    this
  }

  /** Appends the fractional value of a date-time field to the formatter.
    *
    * The fractional value of the field will be output including the
    * preceding decimal point. The preceding value is not output.
    * For example, the second-of-minute value of 15 would be output as {@code .25}.
    *
    * The width of the printed fraction can be controlled. Setting the
    * minimum width to zero will cause no output to be generated.
    * The printed fraction will have the minimum width necessary between
    * the minimum and maximum widths - trailing zeroes are omitted.
    * No rounding occurs due to the maximum width - digits are simply dropped.
    *
    * When parsing in strict mode, the number of parsed digits must be between
    * the minimum and maximum width. When parsing in lenient mode, the minimum
    * width is considered to be zero and the maximum is nine.
    *
    * If the value cannot be obtained then an exception will be thrown.
    * If the value is negative an exception will be thrown.
    * If the field does not have a fixed set of valid values then an
    * exception will be thrown.
    * If the field value in the date-time to be printed is invalid it
    * cannot be printed and an exception will be thrown.
    *
    * @param field  the field to append, not null
    * @param minWidth  the minimum width of the field excluding the decimal point, from 0 to 9
    * @param maxWidth  the maximum width of the field excluding the decimal point, from 1 to 9
    * @param decimalPoint  whether to output the localized decimal point symbol
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the field has a variable set of valid values or
    *                                  either width is invalid
    */
  def appendFraction(
    field:        TemporalField,
    minWidth:     Int,
    maxWidth:     Int,
    decimalPoint: Boolean
  ): DateTimeFormatterBuilder = {
    appendInternal(
      new TTBPDateTimeFormatterBuilder.FractionPrinterParser(field,
                                                             minWidth,
                                                             maxWidth,
                                                             decimalPoint)
    )
    this
  }

  /** Appends the text of a date-time field to the formatter using the full
    * text style.
    *
    * The text of the field will be output during a print.
    * The value must be within the valid range of the field.
    * If the value cannot be obtained then an exception will be thrown.
    * If the field has no textual representation, then the numeric value will be used.
    *
    * The value will be printed as per the normal print of an integer value.
    * Only negative numbers will be signed. No padding will be added.
    *
    * @param field  the field to append, not null
    * @return this, for chaining, not null
    */
  def appendText(field: TemporalField): DateTimeFormatterBuilder = appendText(field, TextStyle.FULL)

  /** Appends the text of a date-time field to the formatter.
    *
    * The text of the field will be output during a print.
    * The value must be within the valid range of the field.
    * If the value cannot be obtained then an exception will be thrown.
    * If the field has no textual representation, then the numeric value will be used.
    *
    * The value will be printed as per the normal print of an integer value.
    * Only negative numbers will be signed. No padding will be added.
    *
    * @param field  the field to append, not null
    * @param textStyle  the text style to use, not null
    * @return this, for chaining, not null
    */
  def appendText(field: TemporalField, textStyle: TextStyle): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    Objects.requireNonNull(textStyle, "textStyle")
    appendInternal(
      new TTBPDateTimeFormatterBuilder.TextPrinterParser(field,
                                                         textStyle,
                                                         TTBPDateTimeTextProvider.getInstance)
    )
    this
  }

  /** Appends the text of a date-time field to the formatter using the specified
    * map to supply the text.
    *
    * The standard text outputting methods use the localized text in the JDK.
    * This method allows that text to be specified directly.
    * The supplied map is not validated by the builder to ensure that printing or
    * parsing is possible, thus an invalid map may throw an error during later use.
    *
    * Supplying the map of text provides considerable flexibility in printing and parsing.
    * For example, a legacy application might require or supply the months of the
    * year as "JNY", "FBY", "MCH" etc. These do not match the standard set of text
    * for localized month names. Using this method, a map can be created which
    * defines the connection between each value and the text:
    * <pre>
    * Map&lt;Long, String&gt; map = new HashMap&lt;&gt;();
    * map.put(1, "JNY");
    * map.put(2, "FBY");
    * map.put(3, "MCH");
    * ...
    * builder.appendText(MONTH_OF_YEAR, map);
    * </pre>
    *
    * Other uses might be to output the value with a suffix, such as "1st", "2nd", "3rd",
    * or as Roman numerals "I", "II", "III", "IV".
    *
    * During printing, the value is obtained and checked that it is in the valid range.
    * If text is not available for the value then it is output as a number.
    * During parsing, the parser will match against the map of text and numeric values.
    *
    * @param field  the field to append, not null
    * @param textLookup  the map from the value to the text
    * @return this, for chaining, not null
    */
  def appendText(
    field:      TemporalField,
    textLookup: java.util.Map[Long, String]
  ): DateTimeFormatterBuilder = {
    Objects.requireNonNull(field, "field")
    Objects.requireNonNull(textLookup, "textLookup")
    import scala.collection.JavaConverters._
    val copy: Map[scala.Long, String] = textLookup.asScala.toMap.map {
      case (x, i) => (x.toLong, i)
    }
    val map: Map[TextStyle, Map[scala.Long, String]] = Map(TextStyle.FULL -> copy)
    val store: TTBPSimpleDateTimeTextProvider.LocaleStore =
      new TTBPSimpleDateTimeTextProvider.LocaleStore(map)
    val provider: TTBPDateTimeTextProvider = new TTBPDateTimeTextProvider() {
      override def getText(
        field:  TemporalField,
        value:  scala.Long,
        style:  TextStyle,
        locale: Locale
      ): String =
        store.getText(value, style)

      override def getTextIterator(
        field:  TemporalField,
        style:  TextStyle,
        locale: Locale
      ): Iterator[(String, scala.Long)] =
        store.getTextIterator(style)
    }
    appendInternal(
      new TTBPDateTimeFormatterBuilder.TextPrinterParser(field, TextStyle.FULL, provider)
    )
    this
  }

  /** Appends an instant using ISO-8601 to the formatter, formatting fractional
    * digits in groups of three.
    *
    * Instants have a fixed output format.
    * They are converted to a date-time with a zone-offset of UTC and formatted
    * using the standard ISO-8601 format.
    * With this method, formatting nano-of-second outputs zero, three, six
    * or nine digits digits as necessary.
    * The localized decimal style is not used.
    *
    * The instant is obtained using {@link ChronoField#INSTANT_SECONDS INSTANT_SECONDS}
    * and optionally (@code NANO_OF_SECOND). The value of {@code INSTANT_SECONDS}
    * may be outside the maximum range of {@code LocalDateTime}.
    *
    * The {@linkplain ResolverStyle resolver style} has no effect on instant parsing.
    * The end-of-day time of '24:00' is handled as midnight at the start of the following day.
    * The leap-second time of '23:59:59' is handled to some degree, see
    * {@link DateTimeFormatter#parsedLeapSecond()} for full details.
    *
    * An alternative to this method is to format/parse the instant as a single
    * epoch-seconds value. That is achieved using {@code appendValue(INSTANT_SECONDS)}.
    *
    * @return this, for chaining, not null
    */
  def appendInstant: DateTimeFormatterBuilder = {
    appendInternal(new TTBPDateTimeFormatterBuilder.InstantPrinterParser(-2))
    this
  }

  /** Appends an instant using ISO-8601 to the formatter with control over
    * the number of fractional digits.
    *
    * Instants have a fixed output format, although this method provides some
    * control over the fractional digits. They are converted to a date-time
    * with a zone-offset of UTC and printed using the standard ISO-8601 format.
    * The localized decimal style is not used.
    *
    * The {@code fractionalDigits} parameter allows the output of the fractional
    * second to be controlled. Specifying zero will cause no fractional digits
    * to be output. From 1 to 9 will output an increasing number of digits, using
    * zero right-padding if necessary. The special value -1 is used to output as
    * many digits as necessary to avoid any trailing zeroes.
    *
    * When parsing in strict mode, the number of parsed digits must match the
    * fractional digits. When parsing in lenient mode, any number of fractional
    * digits from zero to nine are accepted.
    *
    * The instant is obtained using {@link ChronoField#INSTANT_SECONDS INSTANT_SECONDS}
    * and optionally (@code NANO_OF_SECOND). The value of {@code INSTANT_SECONDS}
    * may be outside the maximum range of {@code LocalDateTime}.
    *
    * The {@linkplain ResolverStyle resolver style} has no effect on instant parsing.
    * The end-of-day time of '24:00' is handled as midnight at the start of the following day.
    * The leap-second time of '23:59:59' is handled to some degree, see
    * {@link DateTimeFormatter#parsedLeapSecond()} for full details.
    *
    * An alternative to this method is to format/parse the instant as a single
    * epoch-seconds value. That is achieved using {@code appendValue(INSTANT_SECONDS)}.
    *
    * @param fractionalDigits  the number of fractional second digits to format with,
    *                          from 0 to 9, or -1 to use as many digits as necessary
    * @return this, for chaining, not null
    */
  def appendInstant(fractionalDigits: Int): DateTimeFormatterBuilder =
    if (fractionalDigits < -1 || fractionalDigits > 9)
      throw new IllegalArgumentException(s"Invalid fractional digits: $fractionalDigits")
    else {
      appendInternal(new TTBPDateTimeFormatterBuilder.InstantPrinterParser(fractionalDigits))
      this
    }

  /** Appends the zone offset, such as '+01:00', to the formatter.
    *
    * This appends an instruction to print/parse the offset ID to the builder.
    * This is equivalent to calling {@code appendOffset("HH:MM:ss", "Z")}.
    *
    * @return this, for chaining, not null
    */
  def appendOffsetId: DateTimeFormatterBuilder = {
    appendInternal(TTBPDateTimeFormatterBuilder.OffsetIdPrinterParser.INSTANCE_ID)
    this
  }

  /** Appends the zone offset, such as '+01:00', to the formatter.
    *
    * This appends an instruction to print/parse the offset ID to the builder.
    *
    * During printing, the offset is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#offset()}.
    * It will be printed using the format defined below.
    * If the offset cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, the offset is parsed using the format defined below.
    * If the offset cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * The format of the offset is controlled by a pattern which must be one
    * of the following:
    *<ul>
    * <li>{@code +HH} - hour only, ignoring minute and second
    * <li>{@code +HHmm} - hour, with minute if non-zero, ignoring second, no colon
    * <li>{@code +HH:mm} - hour, with minute if non-zero, ignoring second, with colon
    * <li>{@code +HHMM} - hour and minute, ignoring second, no colon
    * <li>{@code +HH:MM} - hour and minute, ignoring second, with colon
    * <li>{@code +HHMMss} - hour and minute, with second if non-zero, no colon
    * <li>{@code +HH:MM:ss} - hour and minute, with second if non-zero, with colon
    * <li>{@code +HHMMSS} - hour, minute and second, no colon
    * <li>{@code +HH:MM:SS} - hour, minute and second, with colon
    * </ul><p>
    * The "no offset" text controls what text is printed when the total amount of
    * the offset fields to be output is zero.
    * Example values would be 'Z', '+00:00', 'UTC' or 'GMT'.
    * Three formats are accepted for parsing UTC - the "no offset" text, and the
    * plus and minus versions of zero defined by the pattern.
    *
    * @param pattern  the pattern to use, not null
    * @param noOffsetText  the text to use when the offset is zero, not null
    * @return this, for chaining, not null
    */
  def appendOffset(pattern: String, noOffsetText: String): DateTimeFormatterBuilder = {
    appendInternal(new TTBPDateTimeFormatterBuilder.OffsetIdPrinterParser(noOffsetText, pattern))
    this
  }

  /** Appends the localized zone offset, such as 'GMT+01:00', to the formatter.
    *
    * This appends a localized zone offset to the builder, the format of the
    * localized offset is controlled by the specified {@link FormatStyle style}
    * to this method:
    * <ul>
    * <li>{@link TextStyle#FULL full} - formats with localized offset text, such
    * as 'GMT, 2-digit hour and minute field, optional second field if non-zero,
    * and colon.
    * <li>{@link TextStyle#SHORT short} - formats with localized offset text,
    * such as 'GMT, hour without leading zero, optional 2-digit minute and
    * second if non-zero, and colon.
    * </ul>
    *
    * During formatting, the offset is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#offset()}.
    * If the offset cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, the offset is parsed using the format defined above.
    * If the offset cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * @param style  the format style to use, not null
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if style is neither { @link TextStyle#FULL
    * full} nor { @link TextStyle#SHORT short}
    */
  def appendLocalizedOffset(style: TextStyle): DateTimeFormatterBuilder = {
    Objects.requireNonNull(style, "style")
    if ((style ne TextStyle.FULL) && (style ne TextStyle.SHORT))
      throw new IllegalArgumentException("Style must be either full or short")
    appendInternal(new TTBPDateTimeFormatterBuilder.LocalizedOffsetPrinterParser(style))
    this
  }

  /** Appends the time-zone ID, such as 'Europe/Paris' or '+02:00', to the formatter.
    *
    * This appends an instruction to print/parse the zone ID to the builder.
    * The zone ID is obtained in a strict manner suitable for {@code ZonedDateTime}.
    * By contrast, {@code OffsetDateTime} does not have a zone ID suitable
    * for use with this method, see {@link #appendZoneOrOffsetId()}.
    *
    * During printing, the zone is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#zoneId()}.
    * It will be printed using the result of {@link ZoneId#getId()}.
    * If the zone cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, the zone is parsed and must match a known zone or offset.
    * If the zone cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * @return this, for chaining, not null
    * @see #appendZoneRegionId()
    */
  def appendZoneId: DateTimeFormatterBuilder = {
    appendInternal(
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zoneId, "ZoneId()")
    )
    this
  }

  /** Appends the time-zone region ID, such as 'Europe/Paris', to the formatter,
    * rejecting the zone ID if it is a {@code ZoneOffset}.
    *
    * This appends an instruction to print/parse the zone ID to the builder
    * only if it is a region-based ID.
    *
    * During printing, the zone is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#zoneId()}.
    * If the zone is a {@code ZoneOffset} or it cannot be obtained then
    * an exception is thrown unless the section of the formatter is optional.
    * If the zone is not an offset, then the zone will be printed using
    * the zone ID from {@link ZoneId#getId()}.
    *
    * During parsing, the zone is parsed and must match a known zone or offset.
    * If the zone cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    * Note that parsing accepts offsets, whereas printing will never produce
    * one, thus parsing is equivalent to {@code appendZoneId}.
    *
    * @return this, for chaining, not null
    * @see #appendZoneId()
    */
  def appendZoneRegionId: DateTimeFormatterBuilder = {
    appendInternal(
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(
        DateTimeFormatterBuilder.QUERY_REGION_ONLY,
        "ZoneRegionId()"
      )
    )
    this
  }

  /** Appends the time-zone ID, such as 'Europe/Paris' or '+02:00', to
    * the formatter, using the best available zone ID.
    *
    * This appends an instruction to print/parse the best available
    * zone or offset ID to the builder.
    * The zone ID is obtained in a lenient manner that first attempts to
    * find a true zone ID, such as that on {@code ZonedDateTime}, and
    * then attempts to find an offset, such as that on {@code OffsetDateTime}.
    *
    * During printing, the zone is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#zone()}.
    * It will be printed using the result of {@link ZoneId#getId()}.
    * If the zone cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, the zone is parsed and must match a known zone or offset.
    * If the zone cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * This method is is identical to {@code appendZoneId()} except in the
    * mechanism used to obtain the zone.
    *
    * @return this, for chaining, not null
    * @see #appendZoneId()
    */
  def appendZoneOrOffsetId: DateTimeFormatterBuilder = {
    appendInternal(
      new TTBPDateTimeFormatterBuilder.ZoneIdPrinterParser(TemporalQueries.zone, "ZoneOrOffsetId()")
    )
    this
  }

  /** Appends the time-zone name, such as 'British Summer Time', to the formatter.
    *
    * This appends an instruction to print the textual name of the zone to the builder.
    *
    * During printing, the zone is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#zoneId()}.
    * If the zone is a {@code ZoneOffset} it will be printed using the
    * result of {@link ZoneOffset#getId()}.
    * If the zone is not an offset, the textual name will be looked up
    * for the locale set in the {@link DateTimeFormatter}.
    * If the temporal object being printed represents an instant, then the text
    * will be the summer or winter time text as appropriate.
    * If the lookup for text does not find any suitable reuslt, then the
    * {@link ZoneId#getId() ID} will be printed instead.
    * If the zone cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * Parsing is not currently supported.
    *
    * @param textStyle  the text style to use, not null
    * @return this, for chaining, not null
    */
  def appendZoneText(textStyle: TextStyle): DateTimeFormatterBuilder = {
    appendInternal(new TTBPDateTimeFormatterBuilder.ZoneTextPrinterParser(textStyle))
    this
  }

  /** Appends the time-zone name, such as 'British Summer Time', to the formatter.
    *
    * This appends an instruction to format/parse the textual name of the zone to
    * the builder.
    *
    * During formatting, the zone is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#zoneId()}.
    * If the zone is a {@code ZoneOffset} it will be printed using the
    * result of {@link ZoneOffset#getId()}.
    * If the zone is not an offset, the textual name will be looked up
    * for the locale set in the {@link DateTimeFormatter}.
    * If the temporal object being printed represents an instant, then the text
    * will be the summer or winter time text as appropriate.
    * If the lookup for text does not find any suitable result, then the
    * {@link ZoneId#getId() ID} will be printed instead.
    * If the zone cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, either the textual zone name, the zone ID or the offset
    * is accepted. Many textual zone names are not unique, such as CST can be
    * for both "Central Standard Time" and "China Standard Time". In this
    * situation, the zone id will be determined by the region information from
    * formatter's  {@link DateTimeFormatter#getLocale() locale} and the standard
    * zone id for that area, for example, America/New_York for the America Eastern
    * zone. This method also allows a set of preferred {@link ZoneId} to be
    * specified for parsing. The matched preferred zone id will be used if the
    * textual zone name being parsed is not unique.
    *
    * If the zone cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * @param textStyle  the text style to use, not null
    * @param preferredZones  the set of preferred zone ids, not null
    * @return this, for chaining, not null
    */
  def appendZoneText(
    textStyle:      TextStyle,
    preferredZones: java.util.Set[ZoneId]
  ): DateTimeFormatterBuilder = {
    Objects.requireNonNull(preferredZones, "preferredZones")
    appendInternal(new TTBPDateTimeFormatterBuilder.ZoneTextPrinterParser(textStyle))
    this
  }

  /** Appends the chronology ID to the formatter.
    *
    * The chronology ID will be output during a print.
    * If the chronology cannot be obtained then an exception will be thrown.
    *
    * @return this, for chaining, not null
    */
  def appendChronologyId: DateTimeFormatterBuilder = {
    appendInternal(new TTBPDateTimeFormatterBuilder.ChronoPrinterParser(null))
    this
  }

  /** Appends the chronology ID, such as 'ISO' or 'ThaiBuddhist', to the formatter.
    *
    * This appends an instruction to format/parse the chronology ID to the builder.
    *
    * During printing, the chronology is obtained using a mechanism equivalent
    * to querying the temporal with {@link TemporalQueries#chronology()}.
    * It will be printed using the result of {@link Chronology#getId()}.
    * If the chronology cannot be obtained then an exception is thrown unless the
    * section of the formatter is optional.
    *
    * During parsing, the chronology is parsed and must match one of the chronologies
    * in {@link Chronology#getAvailableChronologies()}.
    * If the chronology cannot be parsed then an exception is thrown unless the
    * section of the formatter is optional.
    * The parser uses the {@linkplain #parseCaseInsensitive() case sensitive} setting.
    *
    * @return this, for chaining, not null
    */
  def appendChronologyText(textStyle: TextStyle): DateTimeFormatterBuilder = {
    Objects.requireNonNull(textStyle, "textStyle")
    appendInternal(new TTBPDateTimeFormatterBuilder.ChronoPrinterParser(textStyle))
    this
  }

  /** Appends a localized date-time pattern to the formatter.
    *
    * This appends a localized section to the builder, suitable for outputting
    * a date, time or date-time combination. The format of the localized
    * section is lazily looked up based on four items:
    *<ul>
    * <li>the {@code dateStyle} specified to this method
    * <li>the {@code timeStyle} specified to this method
    * <li>the {@code Locale} of the {@code DateTimeFormatter}
    * <li>the {@code Chronology}, selecting the best available
    * </ul><p>
    * During formatting, the chronology is obtained from the temporal object
    * being formatted, which may have been overridden by
    * {@link DateTimeFormatter#withChronology(Chronology)}.
    *
    * During parsing, if a chronology has already been parsed, then it is used.
    * Otherwise the default from {@code DateTimeFormatter.withChronology(Chronology)}
    * is used, with {@code IsoChronology} as the fallback.
    *
    * Note that this method provides similar functionality to methods on
    * {@code DateFormat} such as {@link DateFormat#getDateTimeInstance(int, int)}.
    *
    * @param dateStyle  the date style to use, null means no date required
    * @param timeStyle  the time style to use, null means no time required
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if both the date and time styles are null
    */
  def appendLocalized(dateStyle: FormatStyle, timeStyle: FormatStyle): DateTimeFormatterBuilder = {
    if (dateStyle == null && timeStyle == null)
      throw new IllegalArgumentException("Either the date or time style must be non-null")
    appendInternal(new TTBPDateTimeFormatterBuilder.LocalizedPrinterParser(dateStyle, timeStyle))
    this
  }

  /** Appends a character literal to the formatter.
    *
    * This character will be output during a print.
    *
    * @param literal  the literal to append, not null
    * @return this, for chaining, not null
    */
  def appendLiteral(literal: Char): DateTimeFormatterBuilder = {
    appendInternal(new TTBPDateTimeFormatterBuilder.CharLiteralPrinterParser(literal))
    this
  }

  /** Appends a string literal to the formatter.
    *
    * This string will be output during a print.
    *
    * If the literal is empty, nothing is added to the formatter.
    *
    * @param literal  the literal to append, not null
    * @return this, for chaining, not null
    */
  def appendLiteral(literal: String): DateTimeFormatterBuilder = {
    Objects.requireNonNull(literal, "literal")
    if (literal.length > 0) {
      if (literal.length == 1) {
        appendInternal(new TTBPDateTimeFormatterBuilder.CharLiteralPrinterParser(literal.charAt(0)))
      } else {
        appendInternal(new TTBPDateTimeFormatterBuilder.StringLiteralPrinterParser(literal))
      }
    }
    this
  }

  /** Appends all the elements of a formatter to the builder.
    *
    * This method has the same effect as appending each of the constituent
    * parts of the formatter directly to this builder.
    *
    * @param formatter  the formatter to add, not null
    * @return this, for chaining, not null
    */
  def append(formatter: DateTimeFormatter): DateTimeFormatterBuilder = {
    Objects.requireNonNull(formatter, "formatter")
    appendInternal(formatter.toPrinterParser(false))
    this
  }

  /** Appends a formatter to the builder which will optionally print/parse.
    *
    * This method has the same effect as appending each of the constituent
    * parts directly to this builder surrounded by an {@link #optionalStart()} and
    * {@link #optionalEnd()}.
    *
    * The formatter will print if data is available for all the fields contained within it.
    * The formatter will parse if the string matches, otherwise no error is returned.
    *
    * @param formatter  the formatter to add, not null
    * @return this, for chaining, not null
    */
  def appendOptional(formatter: DateTimeFormatter): DateTimeFormatterBuilder = {
    Objects.requireNonNull(formatter, "formatter")
    appendInternal(formatter.toPrinterParser(true))
    this
  }

  /** Appends the elements defined by the specified pattern to the builder.
    *
    * All letters 'A' to 'Z' and 'a' to 'z' are reserved as pattern letters.
    * The characters '{' and '}' are reserved for future use.
    * The characters '[' and ']' indicate optional patterns.
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
    * of digits and without padding as per {@link #appendValue(TemporalField)}. Otherwise, the
    * count of digits is used as the width of the output field as per {@link #appendValue(TemporalField, int)}.
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
    * If the count of letters is two, then a {@link #appendValueReduced reduced} two digit form is used.
    * For printing, this outputs the rightmost two digits. For parsing, this will parse using the
    * base value of 2000, resulting in a year within the range 2000 to 2099 inclusive.
    * If the count of letters is less than four (but not two), then the sign is only output for negative
    * years as per {@link SignStyle#NORMAL}.
    * Otherwise, the sign is output if the pad width is exceeded, as per {@link SignStyle#EXCEEDS_PAD}
    *
    * <b>ZoneId</b>: This outputs the time-zone ID, such as 'Europe/Paris'.
    * If the count of letters is two, then the time-zone ID is output.
    * Any other count of letters throws {@code IllegalArgumentException}.
    * <pre>
    * Pattern     Equivalent builder methods
    * VV          appendZoneId()
    * </pre>
    *
    * <b>Zone names</b>: This outputs the display name of the time-zone ID.
    * If the count of letters is one, two or three, then the short name is output.
    * If the count of letters is four, then the full name is output.
    * Five or more letters throws {@code IllegalArgumentException}.
    * <pre>
    * Pattern     Equivalent builder methods
    * z           appendZoneText(TextStyle.SHORT)
    * zz          appendZoneText(TextStyle.SHORT)
    * zzz         appendZoneText(TextStyle.SHORT)
    * zzzz        appendZoneText(TextStyle.FULL)
    * </pre>
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
    * <pre>
    * Pattern     Equivalent builder methods
    * X           appendOffset("+HHmm","Z")
    * XX          appendOffset("+HHMM","Z")
    * XXX         appendOffset("+HH:MM","Z")
    * XXXX        appendOffset("+HHMMss","Z")
    * XXXXX       appendOffset("+HH:MM:ss","Z")
    * x           appendOffset("+HHmm","+00")
    * xx          appendOffset("+HHMM","+0000")
    * xxx         appendOffset("+HH:MM","+00:00")
    * xxxx        appendOffset("+HHMMss","+0000")
    * xxxxx       appendOffset("+HH:MM:ss","+00:00")
    * </pre>
    *
    * <b>Offset Z</b>: This formats the offset based on the number of pattern letters.
    * One, two or three letters outputs the hour and minute, without a colon, such as '+0130'.
    * Four or more letters throws {@code IllegalArgumentException}.
    * The output will be '+0000' when the offset is zero.
    * <pre>
    * Pattern     Equivalent builder methods
    * Z           appendOffset("+HHMM","+0000")
    * ZZ          appendOffset("+HHMM","+0000")
    * ZZZ         appendOffset("+HHMM","+0000")
    * </pre>
    *
    * <b>Optional section</b>: The optional section markers work exactly like calling {@link #optionalStart()}
    * and {@link #optionalEnd()}.
    *
    * <b>Pad modifier</b>: Modifies the pattern that immediately follows to be padded with spaces.
    * The pad width is determined by the number of pattern letters.
    * This is the same as calling {@link #padNext(int)}.
    *
    * For example, 'ppH' outputs the hour-of-day padded on the left with spaces to a width of 2.
    *
    * Any unrecognized letter is an error.
    * Any non-letter character, other than '[', ']', '{', '}' and the single quote will be output directly.
    * Despite this, it is recommended to use single quotes around all characters that you want to
    * output directly to ensure that future changes do not break your application.
    *
    * Note that the pattern string is similar, but not identical, to
    * {@link java.text.SimpleDateFormat SimpleDateFormat}.
    * The pattern string is also similar, but not identical, to that defined by the
    * Unicode Common Locale Data Repository (CLDR/LDML).
    * Pattern letters 'E' and 'u' are merged, which changes the meaning of "E" and "EE" to be numeric.
    * Pattern letters 'X' is aligned with Unicode CLDR/LDML, which affects pattern 'X'.
    * Pattern letter 'y' and 'Y' parse years of two digits and more than 4 digits differently.
    * Pattern letters 'n', 'A', 'N', 'I' and 'p' are added.
    * Number types will reject large numbers.
    *
    * @param pattern  the pattern to add, not null
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if the pattern is invalid
    */
  def appendPattern(pattern: String): DateTimeFormatterBuilder = {
    Objects.requireNonNull(pattern, "pattern")
    parsePattern(pattern)
    this
  }

  def tooManyPattern(cur: Char): IllegalArgumentException =
    new IllegalArgumentException(s"Too many pattern letters: $cur")

  private def parsePattern(pattern: String): Unit = {
    var pos: Int = 0
    while (pos < pattern.length) {
      var cur: Char = pattern.charAt(pos)
      if ((cur >= 'A' && cur <= 'Z') || (cur >= 'a' && cur <= 'z')) {
        var start: Int = pos
        pos += 1
        while (pos < pattern.length && pattern.charAt(pos) == cur) {
          pos += 1
        }
        var count: Int = pos - start
        if (cur == 'p') {
          var pad: Int = 0
          if (pos < pattern.length) {
            cur = pattern.charAt(pos)
            if ((cur >= 'A' && cur <= 'Z') || (cur >= 'a' && cur <= 'z')) {
              pad   = count
              start = pos
              pos += 1
              while (pos < pattern.length && pattern.charAt(pos) == cur) {
                pos += 1
              }
              count = pos - start
            }
          }
          if (pad == 0)
            throw new IllegalArgumentException(
              s"Pad letter 'p' must be followed by valid pad pattern: $pattern"
            )
          padNext(pad)
        }
        val field: TemporalField = DateTimeFormatterBuilder.FIELD_MAP.get(cur)
        if (field != null)
          parseField(cur, count, field)
        else if (cur == 'z') {
          if (count > 4)
            throw tooManyPattern(cur)
          else if (count == 4)
            appendZoneText(TextStyle.FULL)
          else
            appendZoneText(TextStyle.SHORT)
        } else if (cur == 'V') {
          if (count != 2)
            throw new IllegalArgumentException(s"Pattern letter count must be 2: $cur")
          appendZoneId
        } else if (cur == 'Z') {
          if (count < 4)
            appendOffset("+HHMM", "+0000")
          else if (count == 4)
            appendLocalizedOffset(TextStyle.FULL)
          else if (count == 5)
            appendOffset("+HH:MM:ss", "Z")
          else
            throw tooManyPattern(cur)
        } else if (cur == 'O') {
          if (count == 1)
            appendLocalizedOffset(TextStyle.SHORT)
          else if (count == 4)
            appendLocalizedOffset(TextStyle.FULL)
          else
            throw new IllegalArgumentException(s"Pattern letter count must be 1 or 4: $cur")
        } else if (cur == 'X') {
          if (count > 5)
            throw tooManyPattern(cur)
          appendOffset(TTBPDateTimeFormatterBuilder.OffsetIdPrinterParser
                         .PATTERNS(count + (if (count == 1) 0 else 1)),
                       "Z")
        } else if (cur == 'x') {
          if (count > 5)
            throw tooManyPattern(cur)
          val zero: String = if (count == 1) "+00" else if (count % 2 == 0) "+0000" else "+00:00"
          appendOffset(TTBPDateTimeFormatterBuilder.OffsetIdPrinterParser
                         .PATTERNS(count + (if (count == 1) 0 else 1)),
                       zero)
        } else if (cur == 'W') {
          if (count > 1)
            throw tooManyPattern(cur)
          appendInternal(new TTBPDateTimeFormatterBuilder.WeekFieldsPrinterParser('W', count))
        } else if (cur == 'w') {
          if (count > 2)
            throw tooManyPattern(cur)
          appendInternal(new TTBPDateTimeFormatterBuilder.WeekFieldsPrinterParser('w', count))
        } else if (cur == 'Y') {
          appendInternal(new TTBPDateTimeFormatterBuilder.WeekFieldsPrinterParser('Y', count))
        } else {
          throw new IllegalArgumentException(s"Unknown pattern letter: $cur")
        }
        pos -= 1
      } else if (cur == '\'') {
        val start: Int = pos
        pos += 1
        // Size improved
        var break = false
        while (!break && pos < pattern.length) {
          if (pattern.charAt(pos) == '\'') {
            if (pos + 1 < pattern.length && pattern.charAt(pos + 1) == '\'')
              pos += 1
            else
              break = true
          }
          if (!break) {
            pos += 1
          }
        }

        if (pos >= pattern.length)
          throw new IllegalArgumentException(
            s"Pattern ends with an incomplete string literal: $pattern"
          )
        val str: String = pattern.substring(start + 1, pos)
        if (str.length == 0)
          appendLiteral('\'')
        else
          appendLiteral(str.replace("''", "'"))
      } else if (cur == '[')
        optionalStart()
      else if (cur == ']') {
        if (active.parent == null)
          throw new IllegalArgumentException("Pattern invalid as it contains ] without previous [")
        optionalEnd()
      } else if (cur == '{' || cur == '}' || cur == '#')
        throw new IllegalArgumentException(s"Pattern includes reserved character: '$cur'")
      else
        appendLiteral(cur)
      pos += 1
    }
  }

  private def parseField(cur: Char, count: Int, field: TemporalField): Unit = {
    val exc = tooManyPattern(cur)
    cur match {
      case 'u' | 'y' =>
        if (count == 2)
          appendValueReduced(field,
                             2,
                             2,
                             TTBPDateTimeFormatterBuilder.ReducedPrinterParser.BASE_DATE)
        else if (count < 4)
          appendValue(field, count, 19, SignStyle.NORMAL)
        else
          appendValue(field, count, 19, SignStyle.EXCEEDS_PAD)
      case 'M' | 'Q' =>
        count match {
          case 1 =>
            appendValue(field)
          case 2 =>
            appendValue(field, 2)
          case 3 =>
            appendText(field, TextStyle.SHORT)
          case 4 =>
            appendText(field, TextStyle.FULL)
          case 5 =>
            appendText(field, TextStyle.NARROW)
          case _ =>
            throw exc
        }
      case 'L' | 'q' =>
        count match {
          case 1 =>
            appendValue(field)
          case 2 =>
            appendValue(field, 2)
          case 3 =>
            appendText(field, TextStyle.SHORT_STANDALONE)
          case 4 =>
            appendText(field, TextStyle.FULL_STANDALONE)
          case 5 =>
            appendText(field, TextStyle.NARROW_STANDALONE)
          case _ =>
            throw exc
        }
      case 'e' =>
        count match {
          case 1 | 2 =>
            appendInternal(new TTBPDateTimeFormatterBuilder.WeekFieldsPrinterParser('e', count))
          case 3 =>
            appendText(field, TextStyle.SHORT)
          case 4 =>
            appendText(field, TextStyle.FULL)
          case 5 =>
            appendText(field, TextStyle.NARROW)
          case _ =>
            throw exc
        }
      case 'c' =>
        count match {
          case 1 =>
            appendInternal(new TTBPDateTimeFormatterBuilder.WeekFieldsPrinterParser('c', count))
          case 2 =>
            throw new IllegalArgumentException(s"Invalid number of pattern letters: $cur")
          case 3 =>
            appendText(field, TextStyle.SHORT_STANDALONE)
          case 4 =>
            appendText(field, TextStyle.FULL_STANDALONE)
          case 5 =>
            appendText(field, TextStyle.NARROW_STANDALONE)
          case _ =>
            throw exc
        }
      case 'a' =>
        if (count == 1)
          appendText(field, TextStyle.SHORT)
        else
          throw exc
      case 'E' | 'G' =>
        count match {
          case 1 | 2 | 3 =>
            appendText(field, TextStyle.SHORT)
          case 4 =>
            appendText(field, TextStyle.FULL)
          case 5 =>
            appendText(field, TextStyle.NARROW)
          case _ =>
            throw exc
        }
      case 'S' =>
        appendFraction(ChronoField.NANO_OF_SECOND, count, count, false)
      case 'F' =>
        if (count == 1)
          appendValue(field)
        else
          throw exc
      case 'd' | 'h' | 'H' | 'k' | 'K' | 'm' | 's' =>
        if (count == 1)
          appendValue(field)
        else if (count == 2)
          appendValue(field, count)
        else
          throw exc
      case 'D' =>
        if (count == 1)
          appendValue(field)
        else if (count <= 3)
          appendValue(field, count)
        else
          throw exc
      case _ =>
        if (count == 1)
          appendValue(field)
        else
          appendValue(field, count)
    }
    ()
  }

  /** Causes the next added printer/parser to pad to a fixed width using a space.
    *
    * This padding will pad to a fixed width using spaces.
    *
    * During formatting, the decorated element will be output and then padded
    * to the specified width. An exception will be thrown during printing if
    * the pad width is exceeded.
    *
    * During parsing, the padding and decorated element are parsed.
    * If parsing is lenient, then the pad width is treated as a maximum.
    * If parsing is case insensitive, then the pad character is matched ignoring case.
    * The padding is parsed greedily. Thus, if the decorated element starts with
    * the pad character, it will not be parsed.
    *
    * @param padWidth  the pad width, 1 or greater
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if pad width is too small
    */
  def padNext(padWidth: Int): DateTimeFormatterBuilder = padNext(padWidth, ' ')

  /** Causes the next added printer/parser to pad to a fixed width.
    *
    * This padding is intended for padding other than zero-padding.
    * Zero-padding should be achieved using the appendValue methods.
    *
    * During formatting, the decorated element will be output and then padded
    * to the specified width. An exception will be thrown during printing if
    * the pad width is exceeded.
    *
    * During parsing, the padding and decorated element are parsed.
    * If parsing is lenient, then the pad width is treated as a maximum.
    * If parsing is case insensitive, then the pad character is matched ignoring case.
    * The padding is parsed greedily. Thus, if the decorated element starts with
    * the pad character, it will not be parsed.
    *
    * @param padWidth  the pad width, 1 or greater
    * @param padChar  the pad character
    * @return this, for chaining, not null
    * @throws IllegalArgumentException if pad width is too small
    */
  def padNext(padWidth: Int, padChar: Char): DateTimeFormatterBuilder = {
    if (padWidth < 1)
      throw new IllegalArgumentException(s"The pad width must be at least one but was $padWidth")
    active.padNextWidth     = padWidth
    active.padNextChar      = padChar
    active.valueParserIndex = -1
    this
  }

  /** Mark the start of an optional section.
    *
    * The output of printing can include optional sections, which may be nested.
    * An optional section is started by calling this method and ended by calling
    * {@link #optionalEnd()} or by ending the build process.
    *
    * All elements in the optional section are treated as optional.
    * During printing, the section is only output if data is available in the
    * {@code TemporalAccessor} for all the elements in the section.
    * During parsing, the whole section may be missing from the parsed string.
    *
    * For example, consider a builder setup as
    * {@code builder.appendValue(HOUR_OF_DAY,2).optionalStart().appendValue(MINUTE_OF_HOUR,2)}.
    * The optional section ends automatically at the end of the builder.
    * During printing, the minute will only be output if its value can be obtained from the date-time.
    * During parsing, the input will be successfully parsed whether the minute is present or not.
    *
    * @return this, for chaining, not null
    */
  def optionalStart(): DateTimeFormatterBuilder = {
    active.valueParserIndex = -1
    active                  = new DateTimeFormatterBuilder(active, true)
    this
  }

  /** Ends an optional section.
    *
    * The output of printing can include optional sections, which may be nested.
    * An optional section is started by calling {@link #optionalStart()} and ended
    * using this method (or at the end of the builder).
    *
    * Calling this method without having previously called {@code optionalStart}
    * will throw an exception.
    * Calling this method immediately after calling {@code optionalStart} has no effect
    * on the formatter other than ending the (empty) optional section.
    *
    * All elements in the optional section are treated as optional.
    * During printing, the section is only output if data is available in the
    * {@code TemporalAccessor} for all the elements in the section.
    * During parsing, the whole section may be missing from the parsed string.
    *
    * For example, consider a builder setup as
    * {@code builder.appendValue(HOUR_OF_DAY,2).optionalStart().appendValue(MINUTE_OF_HOUR,2).optionalEnd()}.
    * During printing, the minute will only be output if its value can be obtained from the date-time.
    * During parsing, the input will be successfully parsed whether the minute is present or not.
    *
    * @return this, for chaining, not null
    * @throws IllegalStateException if there was no previous call to { @code optionalStart}
    */
  def optionalEnd(): DateTimeFormatterBuilder = {
    if (active.parent == null)
      throw new IllegalStateException(
        "Cannot call optionalEnd() as there was no previous call to optionalStart()"
      )
    if (active.printerParsers.size > 0) {
      val cpp: TTBPDateTimeFormatterBuilder.CompositePrinterParser =
        new TTBPDateTimeFormatterBuilder.CompositePrinterParser(active.printerParsers,
                                                                active.optional)
      active = active.parent
      appendInternal(cpp)
    } else {
      active = active.parent
    }
    this
  }

  /** Appends a printer and/or parser to the internal list handling padding.
    *
    * @param pp  the printer-parser to add, not null
    * @return the index into the active parsers list
    */
  private def appendInternal(pp: TTBPDateTimeFormatterBuilder.DateTimePrinterParser): Int = {
    var _pp = pp
    Objects.requireNonNull(_pp, "pp")
    if (active.padNextWidth > 0) {
      if (_pp != null)
        _pp = new TTBPDateTimeFormatterBuilder.PadPrinterParserDecorator(_pp,
                                                                         active.padNextWidth,
                                                                         active.padNextChar)
      active.padNextWidth = 0
      active.padNextChar  = 0
    }
    active.printerParsers.add(_pp)
    active.valueParserIndex = -1
    active.printerParsers.size - 1
  }

  /** Completes this builder by creating the DateTimeFormatter using the default locale.
    *
    * This will create a formatter with the default locale.
    * Numbers will be printed and parsed using the standard non-localized set of symbols.
    *
    * Calling this method will end any open optional sections by repeatedly
    * calling {@link #optionalEnd()} before creating the formatter.
    *
    * This builder can still be used after creating the formatter if desired,
    * although the state may have been changed by calls to {@code optionalEnd}.
    *
    * @return the created formatter, not null
    */
  def toFormatter: DateTimeFormatter = toFormatter(Locale.getDefault)

  /** Completes this builder by creating the DateTimeFormatter using the specified locale.
    *
    * This will create a formatter with the specified locale.
    * Numbers will be printed and parsed using the standard non-localized set of symbols.
    *
    * Calling this method will end any open optional sections by repeatedly
    * calling {@link #optionalEnd()} before creating the formatter.
    *
    * This builder can still be used after creating the formatter if desired,
    * although the state may have been changed by calls to {@code optionalEnd}.
    *
    * @param locale  the locale to use for formatting, not null
    * @return the created formatter, not null
    */
  def toFormatter(locale: Locale): DateTimeFormatter = {
    Objects.requireNonNull(locale, "locale")
    while (active.parent != null) optionalEnd()
    val pp: TTBPDateTimeFormatterBuilder.CompositePrinterParser =
      new TTBPDateTimeFormatterBuilder.CompositePrinterParser(printerParsers, false)
    new DateTimeFormatter(pp, locale, DecimalStyle.STANDARD, ResolverStyle.SMART, null, null, null)
  }

  private[format] def toFormatter(style: ResolverStyle): DateTimeFormatter =
    toFormatter.withResolverStyle(style)
}
