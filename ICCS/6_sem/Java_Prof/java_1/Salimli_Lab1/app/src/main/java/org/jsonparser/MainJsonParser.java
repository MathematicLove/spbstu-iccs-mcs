package org.jsonparser;

import org.jsonparser.parser.JsonParser;
import org.jsonparser.ui.MinUI;
import org.jsonparser.cat.*;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class MainJsonParser {

    public static class FutureCar {
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

    public static void main(String[] args) throws Exception {
        MinUI ui = new MinUI();

        String json = "{\"mark\":\"Volvo\",\"model\":\"X90\",\"v\":12, \"crashed_Painted\": false}";
        FutureCar futureCar = JsonParser.parseJsonToObject(json, FutureCar.class);
        System.out.println("futureCar.mark = " + futureCar.mark);
        System.out.println("futureCar.model = " + futureCar.model);
        System.out.println("futureCar.v = " + futureCar.v);
        System.out.println("futureCar.crashed_Painted = " + futureCar.crashed_Painted);
        String generatedJson = JsonParser.toJson(futureCar);
        System.out.println("Serialized back to JSON: " + generatedJson);

        Map<String, Object> map = JsonParser.parseJsonToMap(json);

        ui.start("Парсинг в Map");
        System.out.println("Результат: " + map);
        ui.end("Парсинг в Map");

        Cat cat = new Cat(
                new Tail(),
                new Paw(true,  new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(true,  new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(false, new Claw(), new Claw(), new Claw(), new Claw()),
                new Paw(false, new Claw(), new Claw(), new Claw(), new Claw())
        );

        ui.start("Сериализация кошки");
        String catJson = JsonParser.toJson(cat);
        System.out.println("Cat JSON: " + catJson);
        ui.end("Сериализация кошки");

        ui.start("Десериализация кошки");
        Cat catParsed = JsonParser.parseJsonToObject(catJson, Cat.class);
        System.out.println("Cat parts:");
        for (AnimalPart part : catParsed.getParts()) {
            System.out.println("    " + part.getName());
        }
        ui.end("Десериализация кошки");

        try (FileWriter jsonW = new FileWriter("kotiki.json", false)) {
            jsonW.write(catJson);
            System.out.println("Котик записан в kotiki.json");
        } catch (IOException e) {
            System.out.println("Упс! Запись не удалась");
        }
    }
}
