package org.jsonparser.cat;

import java.util.LinkedList;
import java.util.List;

public class Cat {
    private List<AnimalPart> parts = new LinkedList<>();

    public Cat() {

    }

    public Cat(Tail tail, Paw... paws) {
        parts.add(tail);
        for (Paw paw : paws) {
            parts.add(paw);
        }
    }

    public List<AnimalPart> getParts() {
        return parts;
    }
}
