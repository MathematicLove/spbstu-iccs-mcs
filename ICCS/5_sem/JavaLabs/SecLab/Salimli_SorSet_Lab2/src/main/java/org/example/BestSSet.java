package org.example;
import java.util.*;

public class BestSSet<E extends Comparable<E>> implements SortedSet<E> {
    private Node<E> root;
    private int size;
    private static class Node<E> {
        E value;
        Node<E> left, right;

        Node(E value) {
            this.value = value;
        }
    }
    public BestSSet() {
        this.root = null;
        this.size = 0;
    }
    @Override
    public Comparator<? super E> comparator() {
        return null;
    }
    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        throw new UnsupportedOperationException("Метод subSet не реализован");
    }
    @Override
    public SortedSet<E> headSet(E toElement) {
        throw new UnsupportedOperationException("Метод headSet не реализован");
    }
    @Override
    public SortedSet<E> tailSet(E fromElement) {
        throw new UnsupportedOperationException("Метод tailSet не реализован");
    }
    @Override
    public E first() {
        if (root == null) throw new NoSuchElementException("Множество пустое");
        Node<E> current = root;
        while (current.left != null) {
            current = current.left;
        }
        return current.value;
    }
    @Override
    public E last() {
        if (root == null) throw new NoSuchElementException("Множество пустое");
        Node<E> current = root;
        while (current.right != null) {
            current = current.right;
        }
        return current.value;
    }
    @Override
    public int size() {
        return size;
    }
    @Override
    public boolean isEmpty() {
        return size == 0;
    }
    @Override
    public boolean contains(Object o) {
        return contains(root, (E) o);
    }
    private boolean contains(Node<E> node, E value) {
        if (node == null) return false;
        int cmp = value.compareTo(node.value);
        if (cmp < 0) return contains(node.left, value);
        else if (cmp > 0) return contains(node.right, value);
        else return true;
    }
    @Override
    public boolean add(E e) {
        if (contains(e)) return false;
        root = add(root, e);
        size++;
        return true;
    }
    private Node<E> add(Node<E> node, E value) {
        if (node == null) return new Node<>(value);
        int cmp = value.compareTo(node.value);
        if (cmp < 0) node.left = add(node.left, value);
        else if (cmp > 0) node.right = add(node.right, value);
        return node;
    }
    @Override
    public boolean remove(Object o) {
        if (!contains(o)) return false;
        root = remove(root, (E) o);
        size--;
        return true;
    }
    private Node<E> remove(Node<E> node, E value) {
        if (node == null) return null;
        int cmp = value.compareTo(node.value);
        if (cmp < 0) node.left = remove(node.left, value);
        else if (cmp > 0) node.right = remove(node.right, value);
        else {
            if (node.left == null) return node.right;
            if (node.right == null) return node.left;

            Node<E> min = getMin(node.right);
            node.value = min.value;
            node.right = remove(node.right, min.value);
        }
        return node;
    }
    private Node<E> getMin(Node<E> node) {
        while (node.left != null) node = node.left;
        return node;
    }
    @Override
    public void clear() {
        root = null;
        size = 0;
    }
    @Override
    public Iterator<E> iterator() {
        return new Iterator<E>() {
            private final Stack<Node<E>> stack = new Stack<>();
            {
                pushLeft(root);
            }
            private void pushLeft(Node<E> node) {
                while (node != null) {
                    stack.push(node);
                    node = node.left;
                }
            }
            @Override
            public boolean hasNext() {
                return !stack.isEmpty();
            }
            @Override
            public E next() {
                if (!hasNext()) throw new NoSuchElementException("Элементы отсутствуют");
                Node<E> node = stack.pop();
                pushLeft(node.right);
                return node.value;
            }
        };
    }
    @Override
    public Object[] toArray() {
        List<E> list = new ArrayList<>();
        for (E e : this) {
            list.add(e);
        }
        return list.toArray();
    }
    @Override
    public <T> T[] toArray(T[] a) {
        List<E> list = new ArrayList<>();
        for (E e : this) {
            list.add(e);
        }
        return list.toArray(a);
    }
    @Override
    public boolean containsAll(Collection<?> c) {
        for (Object o : c) {
            if (!contains(o)) return false;
        }
        return true;
    }
    @Override
    public boolean addAll(Collection<? extends E> c) {
        boolean modified = false;
        for (E e : c) {
            if (add(e)) modified = true;
        }
        return modified;
    }
    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException("Метод retainAll не реализован");
    }
    @Override
    public boolean removeAll(Collection<?> c) {
        boolean modified = false;
        for (Object o : c) {
            if (remove(o)) modified = true;
        }
        return modified;
    }
}
