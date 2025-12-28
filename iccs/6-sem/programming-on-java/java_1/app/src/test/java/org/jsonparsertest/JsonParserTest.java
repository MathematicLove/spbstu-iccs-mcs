package org.jsonparsertest;

import org.jsonparser.cat.AnimalPart;
import org.jsonparser.cat.Cat;
import org.jsonparser.cat.Claw;
import org.jsonparser.cat.Paw;
import org.jsonparser.cat.Tail;
import org.jsonparser.parser.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class JsonParserTest {

    @Test
    void testParseSimpleJSONIntoMap() {
        String json = "{\"mark\":\"Volvo\",\"model\":\"X90\",\"v\":12,\"crashed_Painted\":false}";
        Map<String, Object> resultMap = JsonParser.parseJsonToMap(json);
        assertNotNull(resultMap, "Map не должен быть null");
        assertEquals("Volvo", resultMap.get("mark"));
        assertEquals("X90", resultMap.get("model"));
        assertEquals(12, resultMap.get("v"));
        assertEquals(false, resultMap.get("crashed_Painted"));
    }

    @Test
    void testDeserializeFutureCar() throws Exception {
        String json = "{\"mark\":\"Volvo\",\"model\":\"X90\",\"v\":12,\"crashed_Painted\":false}";
        class FutureCar {
            public String mark;
            public String model;
            public int v;
            public boolean crashed_Painted;

            public FutureCar() {
            }

            public FutureCar(String mark, String model, int v, boolean crashed) {
                this.mark = mark;
                this.model = model;
                this.v = v;
                this.crashed_Painted = crashed;
            }
        }

        FutureCar futureCar = JsonParser.parseJsonToObject(json, FutureCar.class);
        assertNotNull(futureCar);
        assertEquals("Volvo", futureCar.mark);
        assertEquals("X90", futureCar.model);
        assertEquals(12, futureCar.v);
        assertFalse(futureCar.crashed_Painted);
    }

    @Test
    void testSerializeFutureCar() throws Exception {
        class FutureCar {
            public String mark;
            public String model;
            public int v;
            public boolean crashed_Painted;

            public FutureCar(String mark, String model, int v, boolean crashed) {
                this.mark = mark;
                this.model = model;
                this.v = v;
                this.crashed_Painted = crashed;
            }
        }

        FutureCar futureCar = new FutureCar("Volvo", "X90", 12, false);
        String json = JsonParser.toJson(futureCar);

        assertNotNull(json);
        assertTrue(json.contains("\"mark\":\"Volvo\""));
        assertTrue(json.contains("\"model\":\"X90\""));
        assertTrue(json.contains("\"v\":12"));
        assertTrue(json.contains("\"crashed_Painted\":false"));
    }

    @Test
    void testCatSerializationAndDeserialization() throws Exception {
        Cat catOriginal = new Cat(
                new Tail(),
                new Paw(true,  new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(true,  new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(false, new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(false, new Claw(), new Claw(), new Claw(), new Claw())
        );

        String catJson = JsonParser.toJson(catOriginal);
        System.out.println("catJson = " + catJson);

        assertTrue(catJson.contains("\"length\":10.2"),
                "Должно быть поле '\"length\":10.2' в JSON ");
        assertTrue(catJson.contains("\"Pretty fluffy tail\""),
                "Должен быть хвост с именем 'Pretty fluffy tail'");
        assertTrue(catJson.contains("\"paw\""),
                "Должна встречаться строка 'paw' (лапа)");
        assertTrue(catJson.contains("\"claw\""),
                "Должна встречаться строка 'claw' (когти)");
        assertTrue(catJson.contains("\"parts\""),
                "Должно быть поле 'parts'");

        Cat catParsed = JsonParser.parseJsonToObject(catJson, Cat.class);
        assertNotNull(catParsed);
        assertNotNull(catParsed.getParts());
        assertEquals(5, catParsed.getParts().size(),
                "Список parts должен содержать 5 элементов");

        AnimalPart part0 = catParsed.getParts().get(0);
        assertEquals("Pretty fluffy tail", part0.getName(),
                "Первый элемент должен иметь name='Pretty fluffy tail'");

        for (int i = 1; i < 5; i++) {
            AnimalPart pawPart = catParsed.getParts().get(i);
            assertEquals("paw", pawPart.name,
                    "Элемент №" + i + " должен иметь name='paw'");
        }
    }
}
