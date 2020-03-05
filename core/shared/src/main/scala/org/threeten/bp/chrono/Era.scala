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

import java.util.Locale
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.TextStyle
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.UnsupportedTemporalTypeException

/** An era of the time-line.
  *
  * Most calendar systems have a single epoch dividing the time-line into two eras.
  * However, some calendar systems, have multiple eras, such as one for the reign
  * of each leader.
  * In all cases, the era is conceptually the largest division of the time-line.
  * Each chronology defines the Era's that are known Eras and a
  * {@link Chronology#eras Chrono.eras} to get the valid eras.
  *
  * For example, the Thai Buddhist calendar system divides time into two eras,
  * before and after a single date. By contrast, the Japanese calendar system
  * has one era for the reign of each Emperor.
  *
  * Instances of {@code Era} may be compared using the {@code ==} operator.
  *
  * <h3>Specification for implementors</h3>
  * This interface must be implemented with care to ensure other classes operate correctly.
  * All implementations must be singletons - final, immutable and thread-safe.
  * It is recommended to use an enum whenever possible.
  */
trait Era extends TemporalAccessor with TemporalAdjuster {

  /** Gets the numeric value associated with the era as defined by the chronology.
    * Each chronology defines the predefined Eras and methods to list the Eras
    * of the chronology.
    *
    * All fields, including eras, have an associated numeric value.
    * The meaning of the numeric value for era is determined by the chronology
    * according to these principles:
    *<ul>
    * <li>The era in use at the epoch 1970-01-01 (ISO) has the value 1.
    * <li>Later eras have sequentially higher values.
    * <li>Earlier eras have sequentially lower values, which may be negative.
    * </ul><p>
    *
    * @return the numeric era value
    */
  def getValue: Int

  /** Gets the textual representation of this era.
    *
    * This returns the textual name used to identify the era.
    * The parameters control the style of the returned text and the locale.
    *
    * If no textual mapping is found then the {@link #getValue() numeric value} is returned.
    *
    * @param style  the style of the text required, not null
    * @param locale  the locale to use, not null
    * @return the text value of the era, not null
    */
  def getDisplayName(style: TextStyle, locale: Locale): String =
    new DateTimeFormatterBuilder().appendText(ERA, style).toFormatter(locale).format(this)

  def isSupported(field: TemporalField): Boolean =
    if (field.isInstanceOf[ChronoField]) field eq ERA
    else field != null && field.isSupportedBy(this)

  override def get(field: TemporalField): Int =
    if (field eq ERA) getValue
    else range(field).checkValidIntValue(getLong(field), field)

  def getLong(field: TemporalField): Long =
    if (field eq ERA) getValue.toLong
    else if (field.isInstanceOf[ChronoField])
      throw UnsupportedTemporalTypeException.field(field)
    else field.getFrom(this)

  def adjustInto(temporal: Temporal): Temporal = temporal.`with`(ERA, getValue.toLong)

  override def query[R](query: TemporalQuery[R]): R =
    query match {
      case TemporalQueries.precision => ChronoUnit.ERAS.asInstanceOf[R]
      case TemporalQueries.chronology | TemporalQueries.zone | TemporalQueries.zoneId |
          TemporalQueries.offset | TemporalQueries.localDate | TemporalQueries.localTime =>
        null.asInstanceOf[R]
      case _ => query.queryFrom(this)
    }
}
