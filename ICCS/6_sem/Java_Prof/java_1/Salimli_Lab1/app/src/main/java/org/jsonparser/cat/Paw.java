package org.jsonparser.cat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Paw extends AnimalPart {
    private boolean isFront = true;
    private List<Claw> claws = new ArrayList<>();

    public Paw(boolean isFront, Claw... claws) {
        this.name = "paw";
        this.isFront = isFront;
        this.claws.addAll(Arrays.asList(claws));
    }

    @Override
    public String getName() {
        return isFront ? "Front paw" : "Back paw";
    }

    public List<Claw> getClaws() {
        return claws;
    }
}
