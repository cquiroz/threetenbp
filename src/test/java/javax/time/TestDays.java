/*
 * Copyright (c) 2007, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.testng.annotations.Test;
import static org.testng.Assert.*;

/**
 * Test class.
 * 
 * @author Stephen Colebourne
 */
@Test
public class TestDays {

    //-----------------------------------------------------------------------
    public void test_factoryZeroSingleton() {
        assertSame(Days.ZERO, Days.days(0));
        assertSame(Days.ZERO, Days.days(0));
        assertEquals(0, Days.ZERO.getDays());
    }

    //-----------------------------------------------------------------------
    public void test_factoryGetDays() {
        assertEquals(1,  Days.days(1).getDays());
        assertEquals(2,  Days.days(2).getDays());
        assertEquals(Integer.MAX_VALUE,  Days.days(Integer.MAX_VALUE).getDays());
        assertEquals(-1,  Days.days(-1).getDays());
        assertEquals(-2,  Days.days(-2).getDays());
        assertEquals(Integer.MIN_VALUE,  Days.days(Integer.MIN_VALUE).getDays());
    }

    //-----------------------------------------------------------------------
    public void test_deserializationSingleton() throws Exception {
        Days orginal = Days.ZERO;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out = new ObjectOutputStream(baos);
        out.writeObject(orginal);
        out.close();
        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        ObjectInputStream in = new ObjectInputStream(bais);
        Days ser = (Days) in.readObject();
        assertSame(Days.ZERO, ser);
    }

    //-----------------------------------------------------------------------
    public void test_compareTo() {
        Days test5 = Days.days(5);
        Days test6 = Days.days(6);
        assertEquals(0, test5.compareTo(test5));
        assertEquals(-1, test5.compareTo(test6));
        assertEquals(1, test6.compareTo(test5));
    }

    @Test(expectedExceptions = {NullPointerException.class})
    public void test_compareTo_null() {
        Days test5 = Days.days(5);
        test5.compareTo(null);
    }

    //-----------------------------------------------------------------------
    public void test_isGreaterThan() {
        Days test5 = Days.days(5);
        Days test6 = Days.days(6);
        assertEquals(false, test5.isGreaterThan(test5));
        assertEquals(false, test5.isGreaterThan(test6));
        assertEquals(true, test6.isGreaterThan(test5));
    }

    @Test(expectedExceptions = {NullPointerException.class})
    public void test_isGreaterThan_null() {
        Days test5 = Days.days(5);
        test5.isGreaterThan(null);
    }

    //-----------------------------------------------------------------------
    public void test_isLessThan() {
        Days test5 = Days.days(5);
        Days test6 = Days.days(6);
        assertEquals(false, test5.isLessThan(test5));
        assertEquals(true, test5.isLessThan(test6));
        assertEquals(false, test6.isLessThan(test5));
    }

    @Test(expectedExceptions = {NullPointerException.class})
    public void test_isLessThan_null() {
        Days test5 = Days.days(5);
        test5.isLessThan(null);
    }

    //-----------------------------------------------------------------------
    public void test_equals() {
        Days test5 = Days.days(5);
        Days test6 = Days.days(6);
        assertEquals(true, test5.equals(test5));
        assertEquals(false, test5.equals(test6));
        assertEquals(false, test6.equals(test5));
    }

    public void test_equals_null() {
        Days test5 = Days.days(5);
        assertEquals(false, test5.equals(null));
    }

    public void test_equals_otherClass() {
        Days test5 = Days.days(5);
        assertEquals(false, test5.equals(""));
    }

    //-----------------------------------------------------------------------
    public void test_hashCode() {
        Days test5 = Days.days(5);
        Days test6 = Days.days(6);
        assertEquals(true, test5.hashCode() == test5.hashCode());
        assertEquals(false, test5.hashCode() == test6.hashCode());
    }

    //-----------------------------------------------------------------------
    public void test_plus() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(5), test5.plus(0));
        assertEquals(Days.days(7), test5.plus(2));
        assertEquals(Days.days(3), test5.plus(-2));
        assertEquals(Days.days(Integer.MAX_VALUE), Days.days(Integer.MAX_VALUE - 1).plus(1));
        assertEquals(Days.days(Integer.MIN_VALUE), Days.days(Integer.MIN_VALUE + 1).plus(-1));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_plus_overflowTooBig() {
        Days.days(Integer.MAX_VALUE - 1).plus(2);
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_plus_overflowTooSmall() {
        Days.days(Integer.MIN_VALUE + 1).plus(-2);
    }

    //-----------------------------------------------------------------------
    public void test_plus_Days() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(5), test5.plus(Days.days(0)));
        assertEquals(Days.days(7), test5.plus(Days.days(2)));
        assertEquals(Days.days(3), test5.plus(Days.days(-2)));
        assertEquals(Days.days(Integer.MAX_VALUE),
                Days.days(Integer.MAX_VALUE - 1).plus(Days.days(1)));
        assertEquals(Days.days(Integer.MIN_VALUE),
                Days.days(Integer.MIN_VALUE + 1).plus(Days.days(-1)));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_plus_Days_overflowTooBig() {
        Days.days(Integer.MAX_VALUE - 1).plus(Days.days(2));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_plus_Days_overflowTooSmall() {
        Days.days(Integer.MIN_VALUE + 1).plus(Days.days(-2));
    }

    @Test(expectedExceptions = {NullPointerException.class})
    public void test_plus_Days_null() {
        Days.days(Integer.MIN_VALUE + 1).plus(null);
    }

    //-----------------------------------------------------------------------
    public void test_minus() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(5), test5.minus(0));
        assertEquals(Days.days(3), test5.minus(2));
        assertEquals(Days.days(7), test5.minus(-2));
        assertEquals(Days.days(Integer.MAX_VALUE), Days.days(Integer.MAX_VALUE - 1).minus(-1));
        assertEquals(Days.days(Integer.MIN_VALUE), Days.days(Integer.MIN_VALUE + 1).minus(1));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_minus_overflowTooBig() {
        Days.days(Integer.MAX_VALUE - 1).minus(-2);
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_minus_overflowTooSmall() {
        Days.days(Integer.MIN_VALUE + 1).minus(2);
    }

    //-----------------------------------------------------------------------
    public void test_minus_Days() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(5), test5.minus(Days.days(0)));
        assertEquals(Days.days(3), test5.minus(Days.days(2)));
        assertEquals(Days.days(7), test5.minus(Days.days(-2)));
        assertEquals(Days.days(Integer.MAX_VALUE),
                Days.days(Integer.MAX_VALUE - 1).minus(Days.days(-1)));
        assertEquals(Days.days(Integer.MIN_VALUE),
                Days.days(Integer.MIN_VALUE + 1).minus(Days.days(1)));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_minus_Days_overflowTooBig() {
        Days.days(Integer.MAX_VALUE - 1).minus(Days.days(-2));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_minus_Days_overflowTooSmall() {
        Days.days(Integer.MIN_VALUE + 1).minus(Days.days(2));
    }

    @Test(expectedExceptions = {NullPointerException.class})
    public void test_minus_Days_null() {
        Days.days(Integer.MIN_VALUE + 1).minus(null);
    }

    //-----------------------------------------------------------------------
    public void test_multipliedBy() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(0), test5.multipliedBy(0));
        assertEquals(Days.days(5), test5.multipliedBy(1));
        assertEquals(Days.days(10), test5.multipliedBy(2));
        assertEquals(Days.days(15), test5.multipliedBy(3));
        assertEquals(Days.days(-15), test5.multipliedBy(-3));
    }

    public void test_multipliedBy_negate() {
        Days test5 = Days.days(5);
        assertEquals(Days.days(-15), test5.multipliedBy(-3));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_multipliedBy_overflowTooBig() {
        Days.days(Integer.MAX_VALUE / 2 + 1).multipliedBy(2);
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_multipliedBy_overflowTooSmall() {
        Days.days(Integer.MIN_VALUE / 2 - 1).multipliedBy(2);
    }

    //-----------------------------------------------------------------------
    public void test_dividedBy() {
        Days test12 = Days.days(12);
        assertEquals(Days.days(12), test12.dividedBy(1));
        assertEquals(Days.days(6), test12.dividedBy(2));
        assertEquals(Days.days(4), test12.dividedBy(3));
        assertEquals(Days.days(3), test12.dividedBy(4));
        assertEquals(Days.days(2), test12.dividedBy(5));
        assertEquals(Days.days(2), test12.dividedBy(6));
        assertEquals(Days.days(-4), test12.dividedBy(-3));
    }

    public void test_dividedBy_negate() {
        Days test12 = Days.days(12);
        assertEquals(Days.days(-4), test12.dividedBy(-3));
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_dividedBy_divideByZero() {
        Days.days(1).dividedBy(0);
    }

    //-----------------------------------------------------------------------
    public void test_negated() {
        assertEquals(Days.days(0), Days.days(0).negated());
        assertEquals(Days.days(-12), Days.days(12).negated());
        assertEquals(Days.days(12), Days.days(-12).negated());
        assertEquals(Days.days(-Integer.MAX_VALUE), Days.days(Integer.MAX_VALUE).negated());
    }

    @Test(expectedExceptions = {ArithmeticException.class})
    public void test_negated_overflow() {
        Days.days(Integer.MIN_VALUE).negated();
    }

    //-----------------------------------------------------------------------
    public void test_toString() {
        Days test5 = Days.days(5);
        assertEquals("P5D", test5.toString());
        Days testM1 = Days.days(-1);
        assertEquals("P-1D", testM1.toString());
    }

}
