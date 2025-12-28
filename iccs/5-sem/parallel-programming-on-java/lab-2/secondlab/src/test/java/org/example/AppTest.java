package org.example;
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

public class AppTest {
    @Test
    public void testAdd() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        assertEquals(3, set.size());
        assertTrue(set.contains(5));
        assertTrue(set.contains(10));
        assertTrue(set.contains(1));
        assertFalse(set.contains(99));
    }
    @Test
    public void testRemove() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        assertTrue(set.remove(10));
        assertFalse(set.contains(10));
        assertEquals(2, set.size());
        assertFalse(set.remove(99)); // Удаление несуществующего элемента
    }
    @Test
    public void testAddDuplicate() {
        BestSSet<Integer> set = new BestSSet<>();
        assertTrue(set.add(5));    // первое добавление
        assertFalse(set.add(5));   // повторное добавление
        assertEquals(1, set.size());
    }
    @Test
    public void testRemoveNodeWithTwoChildren() {
        BestSSet<Integer> set = new BestSSet<>();
        //            8
        //          3   10
        //        1   6    14
        set.add(8);
        set.add(3);
        set.add(10);
        set.add(1);
        set.add(6);
        set.add(14);
        assertTrue(set.contains(3));
        assertTrue(set.remove(3));
        assertFalse(set.contains(3));
        assertEquals(5, set.size());
    }

    @Test
    public void testFirstAndLast() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        assertEquals(1, set.first());
        assertEquals(10, set.last());
    }
    @Test
    public void testEmptyFirstAndLast() {
        BestSSet<Integer> set = new BestSSet<>();
        assertThrows(NoSuchElementException.class, set::first);
        assertThrows(NoSuchElementException.class, set::last);
    }
    @Test
    public void testIsEmpty() {
        BestSSet<Integer> set = new BestSSet<>();
        assertTrue(set.isEmpty());
        set.add(1);
        assertFalse(set.isEmpty());
        set.remove(1);
        assertTrue(set.isEmpty());
    }
    @Test
    public void testSize() {
        BestSSet<Integer> set = new BestSSet<>();
        assertEquals(0, set.size());
        set.add(5);
        set.add(10);
        set.add(1);
        assertEquals(3, set.size());
        set.remove(10);
        assertEquals(2, set.size());
    }
    @Test
    public void testClear() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        set.clear();
        assertEquals(0, set.size());
        assertTrue(set.isEmpty());
        assertFalse(set.contains(5));
    }
    @Test
    public void testIterator() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        Integer[] expected = {1, 5, 10};
        int index = 0;
        for (Integer value : set) {
            assertEquals(expected[index++], value);
        }
    }
    @Test
    public void testIteratorOnEmpty() {
        BestSSet<Integer> set = new BestSSet<>();
        Iterator<Integer> it = set.iterator();
        assertFalse(it.hasNext());
        assertThrows(NoSuchElementException.class, it::next);
    }
    @Test
    public void testToArray() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        Object[] array = set.toArray();
        assertArrayEquals(new Object[]{1, 5, 10}, array);
    }
    @Test
    public void testToArrayWithType() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        Integer[] array = set.toArray(new Integer[0]);
        assertArrayEquals(new Integer[]{1, 5, 10}, array);
    }
    @Test
    public void testContainsAll() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(5);
        set.add(10);
        set.add(1);
        assertTrue(set.containsAll(Arrays.asList(5, 10)));
        assertFalse(set.containsAll(Arrays.asList(5, 99)));
    }
    @Test
    public void testAddAll() {
        BestSSet<Integer> set = new BestSSet<>();
        set.addAll(Arrays.asList(5, 10, 1));
        assertEquals(3, set.size());
        assertTrue(set.contains(5));
        assertTrue(set.contains(10));
        assertTrue(set.contains(1));
    }
    @Test
    public void testRemoveAll() {
        BestSSet<Integer> set = new BestSSet<>();
        set.addAll(Arrays.asList(5, 10, 1));
        set.removeAll(Arrays.asList(1, 10));
        assertEquals(1, set.size());
        assertTrue(set.contains(5));
        assertFalse(set.contains(1));
        assertFalse(set.contains(10));
    }
    @Test
    public void testComparator() {
        BestSSet<Integer> set = new BestSSet<>();
        assertNull(set.comparator());
    }
    @Test
    public void testSubSetAndOthers() {
        BestSSet<Integer> set = new BestSSet<>();
        set.add(1);
        set.add(2);

        assertThrows(UnsupportedOperationException.class, () -> set.subSet(1, 2));
        assertThrows(UnsupportedOperationException.class, () -> set.headSet(2));
        assertThrows(UnsupportedOperationException.class, () -> set.tailSet(1));
    }
    @Test
    public void testCompareWithTreeMap() {
        BestSSet<Integer> bestSet = new BestSSet<>();
        TreeMap<Integer, String> treeMap = new TreeMap<>();
        int[] values = {5, 10, 1, -3, 7};
        for (int v : values) {
            bestSet.add(v);
            treeMap.put(v, "dummy");
        }
        assertEquals(treeMap.size(), bestSet.size());
        for (int v : values) {
            assertTrue(bestSet.contains(v));
            assertTrue(treeMap.containsKey(v));
        }
        assertEquals(treeMap.firstKey(), bestSet.first());
        assertEquals(treeMap.lastKey(), bestSet.last());
        bestSet.remove(10);
        treeMap.remove(10);
        assertEquals(treeMap.size(), bestSet.size());
        assertFalse(bestSet.contains(10));
        assertFalse(treeMap.containsKey(10));
    }
}
