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

import java.util.Locale
import java.lang.Long

import org.scalatest.funsuite.AnyFunSuite
import org.threeten.bp.{ AssertionsHelper, LocalDateTime, Month }
import org.threeten.bp.temporal.ChronoField.{ DAY_OF_MONTH, DAY_OF_WEEK, MONTH_OF_YEAR }
import org.threeten.bp.temporal.TemporalField

/** Test text printing. */
class TestDateTimeTextPrinting extends AnyFunSuite with AssertionsHelper {

  def data_text: List[List[Any]] =
    List(
      List(DAY_OF_WEEK, TextStyle.FULL, 1, "Monday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 2, "Tuesday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 3, "Wednesday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 4, "Thursday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 5, "Friday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 6, "Saturday"),
      List(DAY_OF_WEEK, TextStyle.FULL, 7, "Sunday"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 1, "Mon"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 2, "Tue"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 3, "Wed"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 4, "Thu"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 5, "Fri"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 6, "Sat"),
      List(DAY_OF_WEEK, TextStyle.SHORT, 7, "Sun"),
      List(DAY_OF_MONTH, TextStyle.FULL, 1, "1"),
      List(DAY_OF_MONTH, TextStyle.FULL, 2, "2"),
      List(DAY_OF_MONTH, TextStyle.FULL, 3, "3"),
      List(DAY_OF_MONTH, TextStyle.FULL, 28, "28"),
      List(DAY_OF_MONTH, TextStyle.FULL, 29, "29"),
      List(DAY_OF_MONTH, TextStyle.FULL, 30, "30"),
      List(DAY_OF_MONTH, TextStyle.FULL, 31, "31"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 1, "1"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 2, "2"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 3, "3"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 28, "28"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 29, "29"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 30, "30"),
      List(DAY_OF_MONTH, TextStyle.SHORT, 31, "31"),
      List(MONTH_OF_YEAR, TextStyle.FULL, 1, "January"),
      List(MONTH_OF_YEAR, TextStyle.FULL, 12, "December"),
      List(MONTH_OF_YEAR, TextStyle.SHORT, 1, "Jan"),
      List(MONTH_OF_YEAR, TextStyle.SHORT, 12, "Dec")
    )

  test("appendText2arg_print") {
    data_text.foreach {
      case (field: TemporalField) :: (style: TextStyle) :: (value: Int) :: (expected: String) :: Nil =>
        val builder              = new DateTimeFormatterBuilder
        val f: DateTimeFormatter = builder.appendText(field, style).toFormatter(Locale.ENGLISH)
        var dt: LocalDateTime    = LocalDateTime.of(2010, 1, 1, 0, 0)
        dt = dt.`with`(field, value)
        val text: String = f.format(dt)
        assertEquals(text, expected)
      case _ =>
        fail()
    }
  }

  test("appendText1arg_print") {
    data_text.foreach {
      case (field: TemporalField) :: (style: TextStyle) :: (value: Int) :: (expected: String) :: Nil =>
        if (style eq TextStyle.FULL) {
          val builder              = new DateTimeFormatterBuilder
          val f: DateTimeFormatter = builder.appendText(field).toFormatter(Locale.ENGLISH)
          var dt: LocalDateTime    = LocalDateTime.of(2010, 1, 1, 0, 0)
          dt = dt.`with`(field, value)
          val text: String = f.format(dt)
          assertEquals(text, expected)
        }
      case _ =>
        fail()
    }
  }

  test("print_appendText2arg_french_long") {
    val builder = new DateTimeFormatterBuilder
    val f: DateTimeFormatter =
      builder.appendText(MONTH_OF_YEAR, TextStyle.FULL).toFormatter(Locale.FRENCH)
    val dt: LocalDateTime = LocalDateTime.of(2010, 1, 1, 0, 0)
    val text: String      = f.format(dt)
    assertEquals(text, "janvier")
  }

  test("print_appendText2arg_french_short") {
    val builder = new DateTimeFormatterBuilder
    val f: DateTimeFormatter =
      builder.appendText(MONTH_OF_YEAR, TextStyle.SHORT).toFormatter(Locale.FRENCH)
    val dt: LocalDateTime = LocalDateTime.of(2010, 1, 1, 0, 0)
    val text: String      = f.format(dt)
    assertEquals(text, "janv.")
  }

  test("appendTextMap") {
    val map: java.util.Map[Long, String] = new java.util.HashMap[Long, String]
    map.put(1L, "JNY")
    map.put(2L, "FBY")
    map.put(3L, "MCH")
    map.put(4L, "APL")
    map.put(5L, "MAY")
    map.put(6L, "JUN")
    map.put(7L, "JLY")
    map.put(8L, "AGT")
    map.put(9L, "SPT")
    map.put(10L, "OBR")
    map.put(11L, "NVR")
    map.put(12L, "DBR")
    val builder = new DateTimeFormatterBuilder
    builder.appendText(MONTH_OF_YEAR, map)
    val f: DateTimeFormatter = builder.toFormatter
    val dt: LocalDateTime    = LocalDateTime.of(2010, 1, 1, 0, 0)
    for (month <- Month.values) {
      assertEquals(f.format(dt.`with`(month)), map.get(month.getValue.toLong))
    }
  }

  test("appendTextMap_DOM") {
    val map: java.util.Map[Long, String] = new java.util.HashMap[Long, String]
    map.put(1L, "1st")
    map.put(2L, "2nd")
    map.put(3L, "3rd")
    val builder = new DateTimeFormatterBuilder
    builder.appendText(DAY_OF_MONTH, map)
    val f: DateTimeFormatter = builder.toFormatter
    val dt: LocalDateTime    = LocalDateTime.of(2010, 1, 1, 0, 0)
    assertEquals(f.format(dt.withDayOfMonth(1)), "1st")
    assertEquals(f.format(dt.withDayOfMonth(2)), "2nd")
    assertEquals(f.format(dt.withDayOfMonth(3)), "3rd")
  }

  test("appendTextMapIncomplete") {
    val map: java.util.Map[Long, String] = new java.util.HashMap[Long, String]
    map.put(1L, "JNY")
    val builder = new DateTimeFormatterBuilder
    builder.appendText(MONTH_OF_YEAR, map)
    val f: DateTimeFormatter = builder.toFormatter
    val dt: LocalDateTime    = LocalDateTime.of(2010, 2, 1, 0, 0)
    assertEquals(f.format(dt), "2")
  }

  test("formatFullDate") {
    Locale.setDefault(Locale.CANADA) // Passes for both US and CANADA
    val formatFullDate = DateTimeFormatter.ofPattern("MMMM dd, yyyy")
    val dt             = LocalDateTime.of(2018, 1, 30, 11, 2, 45, 750000)
    assertResult("January 30, 2018")(dt.format(formatFullDate))
  }

  test("formatDayOfWeek") {
    Locale.setDefault(Locale.CANADA) // Passes for US
    val formatDayOfWeek = DateTimeFormatter.ofPattern("cccc MMM d") // Thursday Jan. 25
    val dt              = LocalDateTime.of(2018, 1, 30, 11, 2, 45, 750000)
    assertResult("Tuesday Jan 30")(dt.format(formatDayOfWeek))
  }
  
  test("formatTime_AM_PM") {
    Locale.setDefault(Locale.CANADA) // Fails for both US and CANADA
    val formatTime_AM_PM = DateTimeFormatter.ofPattern("h:mm a") // 10:15 PM
    val dt               = LocalDateTime.of(2018, 1, 30, 11, 2, 45, 750000)
    assertResult("11:02 AM")(dt.format(formatTime_AM_PM))
  }

}
