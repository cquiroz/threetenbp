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
package org.threeten.bp.format.internal

import java.util.{ Locale, Objects }

import org.threeten.bp.Period
import org.threeten.bp.ZoneId
import org.threeten.bp.chrono.Chronology
import org.threeten.bp.chrono.IsoChronology
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.format.DateTimeBuilder
import org.threeten.bp.format.DecimalStyle
import org.threeten.bp.format.DateTimeFormatter
import org.threeten.bp.format.internal.TTBPDateTimeFormatterBuilder.ReducedPrinterParser

object TTBPDateTimeParseContext {

  /** Compares two characters ignoring case.
    *
    * @param c1  the first
    * @param c2  the second
    * @return true if equal
    */
  def charEqualsIgnoreCase(c1: Char, c2: Char): Boolean =
    c1 == c2 || Character.toUpperCase(c1) == Character.toUpperCase(c2) || Character.toLowerCase(c1) == Character
      .toLowerCase(c2)
}

/** Context object used during date and time parsing.
  *
  * This class represents the current state of the parse.
  * It has the ability to store and retrieve the parsed values and manage optional segments.
  * It also provides key information to the parsing methods.
  *
  * Once parsing is complete, the {@link #toBuilder()} is typically used
  * to obtain a builder that can combine the separate parsed fields into meaningful values.
  *
  * <h3>Specification for implementors</h3>
  * This class is a mutable context intended for use from a single thread.
  * Usage of the class is thread-safe within standard parsing as a new instance of this class
  * is automatically created for each parse and parsing is single-threaded
  */
final class TTBPDateTimeParseContext(
  private var locale:             Locale,
  private var symbols:            DecimalStyle,
  private var overrideChronology: Chronology,
  private var overrideZone:       ZoneId,
  private var caseSensitive:      Boolean = true,
  private var strict:             Boolean = true
) {

  /** The list of parsed data.
    */
  private val parsed: java.util.ArrayList[TTBPDateTimeParseContext#Parsed] = {
    val list = new java.util.ArrayList[TTBPDateTimeParseContext#Parsed]
    list.add(new Parsed)
    list
  }

  /** Creates a new instance of the context.
    *
    * @param formatter  the formatter controlling the parse, not null
    */
  def this(formatter: DateTimeFormatter) {
    this(formatter.getLocale, formatter.getDecimalStyle, formatter.getChronology, formatter.getZone)
  }

  def this(locale: Locale, symbols: DecimalStyle, chronology: Chronology) {
    this(locale, symbols, chronology, null)
  }

  def this(other: TTBPDateTimeParseContext) {
    this(other.locale,
         other.symbols,
         other.overrideChronology,
         other.overrideZone,
         other.caseSensitive,
         other.strict)
  }

  /** Creates a copy of this context.
    */
  def copy: TTBPDateTimeParseContext = new TTBPDateTimeParseContext(this)

  /** Gets the locale.
    *
    * This locale is used to control localization in the parse except
    * where localization is controlled by the symbols.
    *
    * @return the locale, not null
    */
  def getLocale: Locale = locale

  /** Gets the formatting symbols.
    *
    * The symbols control the localization of numeric parsing.
    *
    * @return the formatting symbols, not null
    */
  def getSymbols: DecimalStyle = symbols

  /** Gets the effective chronology during parsing.
    *
    * @return the effective parsing chronology, not null
    */
  def getEffectiveChronology: Chronology = {
    var chrono: Chronology = currentParsed.chrono
    if (chrono == null) {
      chrono = overrideChronology
      if (chrono == null) {
        chrono = IsoChronology.INSTANCE
      }
    }
    chrono
  }

  /** Checks if parsing is case sensitive.
    *
    * @return true if parsing is case sensitive, false if case insensitive
    */
  def isCaseSensitive: Boolean = caseSensitive

  /** Sets whether the parsing is case sensitive or not.
    *
    * @param caseSensitive  changes the parsing to be case sensitive or not from now on
    */
  def setCaseSensitive(caseSensitive: Boolean): Unit = this.caseSensitive = caseSensitive

  /** Helper to compare two {@code CharSequence} instances.
    * This uses {@link #isCaseSensitive()}.
    *
    * @param cs1  the first character sequence, not null
    * @param offset1  the offset into the first sequence, valid
    * @param cs2  the second character sequence, not null
    * @param offset2  the offset into the second sequence, valid
    * @param length  the length to check, valid
    * @return true if equal
    */
  def subSequenceEquals(
    cs1:     CharSequence,
    offset1: Int,
    cs2:     CharSequence,
    offset2: Int,
    length:  Int
  ): Boolean = {
    if (offset1 + length > cs1.length || offset2 + length > cs2.length) {
      return false
    }
    if (isCaseSensitive) {
      {
        var i: Int = 0
        while (i < length) {
          val ch1: Char = cs1.charAt(offset1 + i)
          val ch2: Char = cs2.charAt(offset2 + i)
          if (ch1 != ch2) {
            return false
          }
          i += 1
        }
      }
    } else {
      {
        var i: Int = 0
        while (i < length) {
          val ch1: Char = cs1.charAt(offset1 + i)
          val ch2: Char = cs2.charAt(offset2 + i)
          if (ch1 != ch2 && Character.toUpperCase(ch1) != Character.toUpperCase(ch2) && Character
                .toLowerCase(ch1) != Character.toLowerCase(ch2)) {
            return false
          }
          i += 1
        }
      }
    }
    true
  }

  /** Helper to compare two {@code char}.
    * This uses {@link #isCaseSensitive()}.
    *
    * @param ch1  the first character
    * @param ch2  the second character
    * @return true if equal
    */
  def charEquals(ch1: Char, ch2: Char): Boolean =
    if (isCaseSensitive) {
      ch1 == ch2
    } else {
      TTBPDateTimeParseContext.charEqualsIgnoreCase(ch1, ch2)
    }

  /** Checks if parsing is strict.
    *
    * Strict parsing requires exact matching of the text and sign styles.
    *
    * @return true if parsing is strict, false if lenient
    */
  def isStrict: Boolean = strict

  /** Sets whether parsing is strict or lenient.
    *
    * @param strict  changes the parsing to be strict or lenient from now on
    */
  def setStrict(strict: Boolean): Unit = this.strict = strict

  /** Starts the parsing of an optional segment of the input.
    */
  def startOptional(): Unit = {
    parsed.add(currentParsed.copy)
    ()
  }

  /** Ends the parsing of an optional segment of the input.
    *
    * @param successful  whether the optional segment was successfully parsed
    */
  def endOptional(successful: Boolean): Unit =
    if (successful) {
      parsed.remove(parsed.size - 2)
      ()
    } else {
      parsed.remove(parsed.size - 1)
      ()
    }

  /** Gets the currently active temporal objects.
    *
    * @return the current temporal objects, not null
    */
  private def currentParsed: TTBPDateTimeParseContext#Parsed = parsed.get(parsed.size - 1)

  /** Gets the first value that was parsed for the specified field.
    *
    * This searches the results of the parse, returning the first value found
    * for the specified field. No attempt is made to derive a value.
    * The field may have an out of range value.
    * For example, the day-of-month might be set to 50, or the hour to 1000.
    *
    * @param field  the field to query from the map, null returns null
    * @return the value mapped to the specified field, null if field was not parsed
    */
  def getParsed(field: TemporalField): java.lang.Long = currentParsed.fieldValues.get(field)

  /** Stores the parsed field.
    *
    * This stores a field-value pair that has been parsed.
    * The value stored may be out of range for the field - no checks are performed.
    *
    * @param field  the field to set in the field-value map, not null
    * @param value  the value to set in the field-value map
    * @param errorPos  the position of the field being parsed
    * @param successPos  the position after the field being parsed
    * @return the new position
    */
  def setParsedField(field: TemporalField, value: Long, errorPos: Int, successPos: Int): Int = {
    Objects.requireNonNull(field, "field")
    val old: java.lang.Long = currentParsed.fieldValues.put(field, value)
    if (old != null && old.longValue != value) ~errorPos else successPos
  }

  /** Stores the parsed chronology.
    *
    * This stores the chronology that has been parsed.
    * No validation is performed other than ensuring it is not null.
    *
    * @param chrono  the parsed chronology, not null
    */
  def setParsed(chrono: Chronology): Unit = {
    Objects.requireNonNull(chrono, "chrono")
    val _currentParsed: TTBPDateTimeParseContext#Parsed = currentParsed
    _currentParsed.chrono = chrono
    if (_currentParsed.callbacks != null) {
      val callbacks: java.util.Iterator[Array[AnyRef]] =
        new java.util.ArrayList[Array[AnyRef]](_currentParsed.callbacks).iterator
      _currentParsed.callbacks.clear()
      while (callbacks.hasNext) {
        val objects                  = callbacks.next()
        val pp: ReducedPrinterParser = objects(0).asInstanceOf[ReducedPrinterParser]
        pp.setValue(this,
                    objects(1).asInstanceOf[java.lang.Long],
                    objects(2).asInstanceOf[Integer],
                    objects(3).asInstanceOf[Integer])
      }
    }
  }

  def addChronologyChangedParser(
    reducedPrinterParser: ReducedPrinterParser,
    value:                Long,
    errorPos:             Int,
    successPos:           Int
  ): Unit = {
    val _currentParsed: TTBPDateTimeParseContext#Parsed = currentParsed
    if (_currentParsed.callbacks == null)
      _currentParsed.callbacks = new java.util.ArrayList[Array[AnyRef]](2)
    _currentParsed.callbacks.add(
      Array[AnyRef](reducedPrinterParser,
                    value.asInstanceOf[AnyRef],
                    errorPos.asInstanceOf[AnyRef],
                    successPos.asInstanceOf[AnyRef])
    )
    ()
  }

  /** Stores the parsed zone.
    *
    * This stores the zone that has been parsed.
    * No validation is performed other than ensuring it is not null.
    *
    * @param zone  the parsed zone, not null
    */
  def setParsed(zone: ZoneId): Unit = {
    Objects.requireNonNull(zone, "zone")
    currentParsed.zone = zone
  }

  /** Stores the leap second.
    */
  def setParsedLeapSecond(): Unit = currentParsed.leapSecond = true

  /** Returns a {@code TemporalAccessor} that can be used to interpret
    * the results of the parse.
    *
    * @return an accessor with the results of the parse, not null
    */
  def toParsed: TTBPDateTimeParseContext#Parsed = currentParsed

  /** Returns a string version of the context for debugging.
    *
    * @return a string representation of the context data, not null
    */
  override def toString: String = currentParsed.toString

  /** Temporary store of parsed data.
    */
  final class Parsed() extends TemporalAccessor {
    var chrono: Chronology = null
    var zone: ZoneId       = null
    val fieldValues: java.util.Map[TemporalField, java.lang.Long] =
      new java.util.HashMap[TemporalField, java.lang.Long]
    var leapSecond: Boolean                      = false
    var excessDays: Period                       = Period.ZERO
    var callbacks: java.util.List[Array[AnyRef]] = null

    protected[format] def copy: TTBPDateTimeParseContext#Parsed = {
      val cloned: TTBPDateTimeParseContext#Parsed = new Parsed
      cloned.chrono = this.chrono
      cloned.zone   = this.zone
      cloned.fieldValues.putAll(this.fieldValues)
      cloned.leapSecond = this.leapSecond
      cloned
    }

    override def toString: String = s"${fieldValues.toString},$chrono,$zone"

    def isSupported(field: TemporalField): Boolean = fieldValues.containsKey(field)

    override def get(field: TemporalField): Int = {
      if (!fieldValues.containsKey(field))
        throw UnsupportedTemporalTypeException.field(field)
      val value: Long = fieldValues.get(field)
      Math.toIntExact(value)
    }

    def getLong(field: TemporalField): Long =
      if (!fieldValues.containsKey(field))
        throw UnsupportedTemporalTypeException.field(field)
      else
        fieldValues.get(field)

    override def query[R](query: TemporalQuery[R]): R =
      if (query eq TemporalQueries.chronology)
        chrono.asInstanceOf[R]
      else if ((query eq TemporalQueries.zoneId) || (query eq TemporalQueries.zone))
        zone.asInstanceOf[R]
      else
        super.query(query)

    /** Returns a {@code DateTimeBuilder} that can be used to interpret
      * the results of the parse.
      *
      * This method is typically used once parsing is complete to obtain the parsed data.
      * Parsing will typically result in separate fields, such as year, month and day.
      * The returned builder can be used to combine the parsed data into meaningful
      * objects such as {@code LocalDate}, potentially applying complex processing
      * to handle invalid parsed data.
      *
      * @return a new builder with the results of the parse, not null
      */
    def toBuilder: DateTimeBuilder = {
      val builder: DateTimeBuilder = new DateTimeBuilder
      builder.fieldValues.putAll(
        fieldValues.asInstanceOf[java.util.Map[_ <: TemporalField, _ <: java.lang.Long]]
      )
      builder.chrono = getEffectiveChronology
      if (zone != null)
        builder.zone = zone
      else
        builder.zone = overrideZone
      builder.leapSecond = leapSecond
      builder.excessDays = excessDays
      builder
    }
  }

  /** Sets the locale.
    *
    * This locale is used to control localization in the print output except
    * where localization is controlled by the symbols.
    *
    * @param locale  the locale, not null
    */
  def setLocale(locale: Locale): Unit = {
    Objects.requireNonNull(locale, "locale")
    this.locale = locale
  }
}
