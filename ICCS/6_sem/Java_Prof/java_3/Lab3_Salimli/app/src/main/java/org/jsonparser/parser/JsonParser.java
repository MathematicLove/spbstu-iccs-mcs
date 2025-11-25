package org.jsonparser.parser;

import sun.misc.Unsafe;
import java.lang.reflect.*;
import java.util.*;

public class JsonParser {

    private static final Unsafe UNSAFE;
    static {
        try {
            Field f = Unsafe.class.getDeclaredField("theUnsafe");
            f.setAccessible(true);
            UNSAFE = (Unsafe) f.get(null);
        } catch (Exception e) {
            throw new RuntimeException("Unable to initialize Unsafe", e);
        }
    }

    private static List<Field> getAllFields(Class<?> cls) {
        List<Field> fields = new ArrayList<>();
        while (cls != null) {
            fields.addAll(Arrays.asList(cls.getDeclaredFields()));
            cls = cls.getSuperclass();
        }
        return fields;
    }

    public static Map<String, Object> parseJsonToMap(String json) {
        json = json.trim();
        if (!json.startsWith("{") || !json.endsWith("}")) {
            throw new IllegalArgumentException("JSON must start with '{' and end with '}'");
        }
        json = json.substring(1, json.length() - 1).trim();
        Map<String, Object> result = new HashMap<>();

        while (!json.isEmpty()) {
            int colonIndex = json.indexOf(":");
            if (colonIndex == -1) break;

            String key = json.substring(0, colonIndex).trim().replaceAll("\"", "");
            json = json.substring(colonIndex + 1).trim();

            Object value;
            if (json.startsWith("\"")) {
                int endIndex = json.indexOf("\"", 1);
                value = json.substring(1, endIndex);
                json = json.substring(endIndex + 1).trim();
            } else if (json.startsWith("{")) {
                int endIndex = findMatchingCurlyBraceIndex(json);
                value = parseJsonToMap(json.substring(0, endIndex + 1));
                json = json.substring(endIndex + 1).trim();
            } else if (json.startsWith("[")) {
                int endIndex = findMatchingSquareBracketIndex(json);
                value = parseJsonArray(json.substring(0, endIndex + 1));
                json = json.substring(endIndex + 1).trim();
            } else {
                int endIndex = json.indexOf(",");
                if (endIndex == -1) endIndex = json.length();
                String valStr = json.substring(0, endIndex).trim();
                value = parsePrimitive(valStr);
                json = json.substring(endIndex).trim();
            }

            result.put(key, value);
            if (json.startsWith(",")) {
                json = json.substring(1).trim();
            }
        }
        return result;
    }

    private static int findMatchingCurlyBraceIndex(String json) {
        int count = 0;
        boolean inString = false;
        for (int i = 0; i < json.length(); i++) {
            char c = json.charAt(i);
            if (c == '"' && (i == 0 || json.charAt(i - 1) != '\\')) {
                inString = !inString;
            }
            if (!inString) {
                if (c == '{') count++;
                if (c == '}') count--;
                if (count == 0) {
                    return i;
                }
            }
        }
        throw new IllegalArgumentException("Unmatched braces { } in JSON");
    }

    private static int findMatchingSquareBracketIndex(String json) {
        int count = 0;
        boolean inString = false;
        for (int i = 0; i < json.length(); i++) {
            char c = json.charAt(i);
            if (c == '"' && (i == 0 || json.charAt(i - 1) != '\\')) {
                inString = !inString;
            }
            if (!inString) {
                if (c == '[') count++;
                if (c == ']') count--;
                if (count == 0) {
                    return i;
                }
            }
        }
        throw new IllegalArgumentException("Unmatched brackets [ ] in JSON");
    }

    public static List<Object> parseJsonArray(String json) {
        if (!json.startsWith("[") || !json.endsWith("]")) {
            throw new IllegalArgumentException("Invalid JSON array format");
        }
        String inner = json.substring(1, json.length() - 1).trim();
        List<Object> list = new ArrayList<>();
        if (inner.isEmpty()) return list;

        int start = 0;
        int braceCount = 0;
        int bracketCount = 0;
        boolean inString = false;

        for (int i = 0; i < inner.length(); i++) {
            char c = inner.charAt(i);
            if (c == '"' && (i == 0 || inner.charAt(i - 1) != '\\')) {
                inString = !inString;
            } else if (!inString) {
                if (c == '{') braceCount++;
                else if (c == '}') braceCount--;
                else if (c == '[') bracketCount++;
                else if (c == ']') bracketCount--;
                else if (c == ',' && braceCount == 0 && bracketCount == 0) {
                    String elementStr = inner.substring(start, i).trim();
                    if (!elementStr.isEmpty()) {
                        list.add(parseJsonValue(elementStr));
                    }
                    start = i + 1;
                }
            }
        }
        String elementStr = inner.substring(start).trim();
        if (!elementStr.isEmpty()) {
            list.add(parseJsonValue(elementStr));
        }
        return list;
    }

    private static Object parseJsonValue(String json) {
        json = json.trim();
        if (json.isEmpty()) return null;

        if (json.startsWith("\"")) {
            // Строка с экранированием
            StringBuilder sb = new StringBuilder();
            boolean escape = false;
            for (int i = 1; i < json.length(); i++) {
                char c = json.charAt(i);
                if (escape) {
                    sb.append(c);
                    escape = false;
                } else if (c == '\\') {
                    escape = true;
                } else if (c == '"') {
                    return sb.toString();
                } else {
                    sb.append(c);
                }
            }
            return sb.toString();
        } else if (json.startsWith("{")) {
            int endIndex = findMatchingCurlyBraceIndex(json);
            return parseJsonToMap(json.substring(0, endIndex + 1));
        } else if (json.startsWith("[")) {
            int endIndex = findMatchingSquareBracketIndex(json);
            return parseJsonArray(json.substring(0, endIndex + 1));
        } else {
            return parsePrimitive(json);
        }
    }

    private static Object parsePrimitive(String value) {
        value = value.trim();
        if (value.equals("null")) return null;
        if (value.equals("true")) return true;
        if (value.equals("false")) return false;
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e1) {
            try {
                return Double.parseDouble(value);
            } catch (NumberFormatException e2) {
                return value;
            }
        }
    }

    public static String toJson(Object obj) throws IllegalAccessException {
        if (obj == null) return "null";
        StringBuilder sb = new StringBuilder("{");
        List<Field> fields = getAllFields(obj.getClass());
        for (Field field : fields) {
            field.setAccessible(true);
            sb.append("\"").append(field.getName()).append("\":");
            Object value = field.get(obj);
            sb.append(toJsonValue(value)).append(",");
        }
        if (sb.charAt(sb.length() - 1) == ',') {
            sb.deleteCharAt(sb.length() - 1);
        }
        sb.append("}");
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static String mapToJson(Map<String, Object> map) throws IllegalAccessException {
        StringBuilder sb = new StringBuilder("{");
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            sb.append("\"").append(entry.getKey()).append("\":");
            Object val = entry.getValue();
            if (val instanceof Map) {
                sb.append(mapToJson((Map<String, Object>) val));
            } else if (val instanceof List) {
                sb.append(listToJson((List<Object>) val));
            } else if (val instanceof String) {
                sb.append("\"").append(val).append("\"");
            } else {
                sb.append(val);
            }
            sb.append(",");
        }
        if (sb.charAt(sb.length() - 1) == ',') {
            sb.deleteCharAt(sb.length() - 1);
        }
        sb.append("}");
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static String listToJson(List<Object> list) throws IllegalAccessException {
        StringBuilder sb = new StringBuilder("[");
        for (Object item : list) {
            if (item instanceof Map) {
                sb.append(mapToJson((Map<String, Object>) item));
            } else if (item instanceof List) {
                sb.append(listToJson((List<Object>) item));
            } else if (item instanceof String) {
                sb.append("\"").append(item).append("\"");
            } else {
                sb.append(item);
            }
            sb.append(",");
        }
        if (sb.charAt(sb.length() - 1) == ',') {
            sb.deleteCharAt(sb.length() - 1);
        }
        sb.append("]");
        return sb.toString();
    }

    private static String toJsonValue(Object value) throws IllegalAccessException {
        if (value == null) return "null";
        Class<?> cls = value.getClass();

        if (cls.isArray()) {
            int length = Array.getLength(value);
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < length; i++) {
                Object elem = Array.get(value, i);
                sb.append(toJsonValue(elem)).append(",");
            }
            if (sb.charAt(sb.length() - 1) == ',') {
                sb.deleteCharAt(sb.length() - 1);
            }
            sb.append("]");
            return sb.toString();
        }

        if (value instanceof Collection) {
            Collection<?> coll = (Collection<?>) value;
            StringBuilder sb = new StringBuilder("[");
            for (Object elem : coll) {
                sb.append(toJsonValue(elem)).append(",");
            }
            if (sb.charAt(sb.length() - 1) == ',') {
                sb.deleteCharAt(sb.length() - 1);
            }
            sb.append("]");
            return sb.toString();
        }
        if (value instanceof String) {
            return "\"" + value + "\"";
        }
        if (value instanceof Number || value instanceof Boolean) {
            return value.toString();
        }
        return toJson(value);
    }

    @SuppressWarnings("unchecked")
    public static <T> T parseJsonToObject(String json, Class<T> clazz) throws Exception {
        Map<String, Object> map = parseJsonToMap(json);
        Object instance;
        try {
            instance = clazz.getDeclaredConstructor().newInstance();
        } catch (NoSuchMethodException e) {
            instance = UNSAFE.allocateInstance(clazz);
        }

        List<Field> fields = getAllFields(clazz);
        for (Field field : fields) {
            field.setAccessible(true);
            if (map.containsKey(field.getName())) {
                Object value = map.get(field.getName());
                Class<?> fieldType = field.getType();

                if (value instanceof Map && !fieldType.isAssignableFrom(Map.class)) {
                    String nestedJson = mapToJson((Map<String, Object>) value);
                    value = parseJsonToObject(nestedJson, fieldType);
                }
                else if (value instanceof List && Collection.class.isAssignableFrom(fieldType)) {
                    value = convertListField((List<?>) value, field);
                }
                field.set(instance, value);
            }
        }
        return clazz.cast(instance);
    }

    @SuppressWarnings("unchecked")
    private static Object convertListField(List<?> origList, Field field) throws Exception {
        List<Object> convertedList = new ArrayList<>();
        Type genericType = field.getGenericType();
        Class<?> elementType = Object.class;

        if (genericType instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) genericType;
            Type[] typeArgs = pt.getActualTypeArguments();
            if (typeArgs != null && typeArgs.length > 0 && typeArgs[0] instanceof Class) {
                elementType = (Class<?>) typeArgs[0];
            }
        }
        for (Object elem : origList) {
            if (elem instanceof Map && !elementType.isAssignableFrom(Map.class)) {
                elem = parseJsonToObject(mapToJson((Map<String, Object>) elem), elementType);
            }
            convertedList.add(elem);
        }
        return convertedList;
    }
}
