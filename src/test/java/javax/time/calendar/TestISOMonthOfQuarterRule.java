/*
 * Copyright (c) 2010, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time.calendar;

import static org.testng.Assert.assertEquals;

import org.testng.annotations.Test;

/**
 * Test ISO MonthOfQuarter rule.
 *
 * @author Stephen Colebourne
 */
@Test
public class TestISOMonthOfQuarterRule extends AbstractTestDateTimeFieldRule {

    public TestISOMonthOfQuarterRule() {
        super(LocalDate.of(2009, 12, 26), 3, 3);
    }

    @Override
    protected DateTimeFieldRule<Integer> rule() {
        return ISOChronology.monthOfQuarterRule();
    }

    //-----------------------------------------------------------------------
    // Basics
    //-----------------------------------------------------------------------
    public void test_basics() throws Exception {
        DateTimeFieldRule<Integer> rule = ISOChronology.monthOfQuarterRule();
        assertEquals(rule.getReifiedType(), Integer.class);
        assertEquals(rule.getID(), "ISO.MonthOfQuarter");
        assertEquals(rule.getName(), "MonthOfQuarter");
        assertEquals(rule.getMinimumValue(), 1);
        assertEquals(rule.getMinimumValue(LocalDate.of(2007, 6, 20)), 1);
        assertEquals(rule.getLargestMinimumValue(), 1);
        assertEquals(rule.getMaximumValue(), 3);
        assertEquals(rule.getMaximumValue(LocalDate.of(2007, 6, 20)), 3);
        assertEquals(rule.getSmallestMaximumValue(), 3);
        assertEquals(rule.isFixedValueSet(), true);
        assertEquals(rule.getPeriodUnit(), ISOChronology.periodMonths());
        assertEquals(rule.getPeriodRange(), ISOChronology.periodQuarters());
    }

    //-----------------------------------------------------------------------
    // getValue(Calendrical)
    //-----------------------------------------------------------------------
    public void test_getValue_Calendrical_date() {
        assertEquals(rule().getValue(LocalDate.of(2007, 1, 20)), (Integer) 1);
        assertEquals(rule().getValue(LocalDate.of(2007, 2, 20)), (Integer) 2);
        assertEquals(rule().getValue(LocalDate.of(2007, 3, 20)), (Integer) 3);
        assertEquals(rule().getValue(LocalDate.of(2007, 4, 20)), (Integer) 1);
        assertEquals(rule().getValue(LocalDate.of(2007, 5, 20)), (Integer) 2);
        assertEquals(rule().getValue(LocalDate.of(2007, 6, 20)), (Integer) 3);
        assertEquals(rule().getValue(LocalDate.of(2007, 7, 20)), (Integer) 1);
        assertEquals(rule().getValue(LocalDate.of(2007, 8, 20)), (Integer) 2);
        assertEquals(rule().getValue(LocalDate.of(2007, 9, 20)), (Integer) 3);
        assertEquals(rule().getValue(LocalDate.of(2007, 10, 20)), (Integer) 1);
        assertEquals(rule().getValue(LocalDate.of(2007, 11, 20)), (Integer) 2);
        assertEquals(rule().getValue(LocalDate.of(2007, 12, 20)), (Integer) 3);
    }

    public void test_getValue_Calendrical_dateTime() {
        assertEquals(rule().getValue(LocalDateTime.of(2007, 1, 20, 12, 30)), (Integer) 1);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 2, 20, 12, 30)), (Integer) 2);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 3, 20, 12, 30)), (Integer) 3);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 4, 20, 12, 30)), (Integer) 1);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 5, 20, 12, 30)), (Integer) 2);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 6, 20, 12, 30)), (Integer) 3);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 7, 20, 12, 30)), (Integer) 1);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 8, 20, 12, 30)), (Integer) 2);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 9, 20, 12, 30)), (Integer) 3);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 10, 20, 12, 30)), (Integer) 1);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 11, 20, 12, 30)), (Integer) 2);
        assertEquals(rule().getValue(LocalDateTime.of(2007, 12, 20, 12, 30)), (Integer) 3);
    }

}
