/*
 * GNU General Public License v3.0
 *
 * Copyright (c) 2019, Miguel Gamboa (gamboa.pt)
 *
 *   All rights granted under this License are granted for the term of
 * copyright on the Program, and are irrevocable provided the stated
 * conditions are met.  This License explicitly affirms your unlimited
 * permission to run the unmodified Program.  The output from running a
 * covered work is covered by this License only if the output, given its
 * content, constitutes a covered work.  This License acknowledges your
 * rights of fair use or other equivalent, as provided by copyright law.
 *
 *   You may make, run and propagate covered works that you do not
 * convey, without conditions so long as your license otherwise remains
 * in force.  You may convey covered works to others for the sole purpose
 * of having them make modifications exclusively for you, or provide you
 * with facilities for running those works, provided that you comply with
 * the terms of this License in conveying all material for which you do
 * not control copyright.  Those thus making or running the covered works
 * for you must do so exclusively on your behalf, under your direction
 * and control, on terms that prohibit them from making any copies of
 * your copyrighted material outside their relationship with you.
 *
 *   Conveying under any other circumstances is permitted solely under
 * the conditions stated below.  Sublicensing is not allowed; section 10
 * makes it unnecessary.
 *
 */

package org.isel.jingle;

import junit.framework.AssertionFailedError;
import org.isel.jingle.util.queries.LazyQueries;
import org.junit.Test;

import java.util.List;
import java.util.Optional;
import java.util.Random;

import static java.lang.System.out;
import static java.util.Arrays.asList;
import static junit.framework.Assert.assertEquals;
import static org.isel.jingle.util.queries.LazyQueries.*;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;


public class LazyQueriesTest {
    @Test
    public void testMax(){
        Random r = new Random();
        Iterable<Integer> nrs = limit(generate(() -> r.nextInt(100)), 10);
        max(nrs).ifPresent(out::println);
    }

    @Test
    public void testSkip(){
        List<Integer> nrs = asList(1, 2, 3, 4, 5, 6, 7, 8);
        Object[] actual = toArray(skip(nrs, 3));
        Object[] expected = { 4, 5, 6, 7, 8 };
        assertArrayEquals(expected, actual);
    }
    @Test
    public void testLimit(){
        Iterable<Integer> nrs = LazyQueries.limit(iterate(1, n -> n + 1), 11);
        assertEquals(11,count(nrs));
    }

    @Test
    public void testGenerate(){
        Random r = new Random();
        Iterable<Integer> nrs = limit(generate(() -> r.nextInt(100)), 10);
        for(int n : nrs) out.println(n);
    }

    @Test
    public void testMap(){
        List<String> words = asList("super", "isel", "ola", "fcp");
        Object[] actual = toArray(map(words, w -> w.length()));
        Object[] expected = { 5, 4, 3, 3 };
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testFirstFilterMapNrs() throws Throwable{
        // An infinite Sequence CANNOT be converted to a Collection
        // Object[] nrs = toArray(iterate(1, n -> n + 1));

        Iterable<Integer> nrs = iterate(1, n -> n + 1);
        Optional<Integer> first = first(filter(map(nrs,
            n -> n * n),
            n -> n > 3));
        int val = first.orElseThrow(() ->{ throw new AssertionFailedError("Max returning NO value!"); });
        assertEquals(4, val);
    }

    @Test
    public void testFlatMap() {
        List<String> words = asList("super","", "isel");

        Iterable<Character> characters = flatMap(words, LazyQueries::toCharArray);
        Object[] actual = toArray(characters);
        Object[] expected = {'s','u','p','e','r','i','s','e','l'};
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testTakeWhile() {
        List<Integer> nrs = asList(1, 2, 3, 4, 5, 6, 7, 8);
        Iterable<Integer> i = takeWhile(nrs, n -> n <= 5);

        Object[] actual = toArray(i);
        Object[] expected = {1, 2, 3, 4, 5};
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testFrom() {
        String[] words ={"super", "isel"};
        Iterable<String> from = from(words);
        Object[] actual = toArray(from);
        assertArrayEquals(words, actual);
    }

    @Test
    public void testFirst() {
        List<Integer> nrs = asList(2, 3, 4, 5, 6, 7, 8);
        Optional<Integer> first = first(nrs);

        assertTrue(first.isPresent());
        assertEquals(2, 2);
    }

    @Test
    public void testCache(){
        Random r = new Random();
        Iterable<Integer> nrs = generate(() -> r.nextInt(100));
        nrs = cache(nrs);
        Object[] expected = toArray(limit(nrs, 10));
        Object[] actual = toArray(limit(nrs, 10));
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testFromArray(){
        Object[] expected = new Object[0];
        Iterable<Object> objects = from(expected);
        Object[] actual = toArray(objects);
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testFlatMap2() {
        List<String> words = asList("fgfsg", "", "kgh");
        Iterable<Character> characters = flatMap(words, LazyQueries::toCharArray);
    }

}