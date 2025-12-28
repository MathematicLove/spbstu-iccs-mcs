package ru.spbstu.telematics.java;
import java.security.SecureRandom;
import java.util.Scanner;
public class App 
{
	private static final String alphavit = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm0123456789";
	private static final SecureRandom rand = new SecureRandom();

	public static String generatorStrok(int length){
	StringBuilder oS = new StringBuilder(length);
	for(int i = 0; i < length; i++){
	oS.append(alphavit.charAt(rand.nextInt(alphavit.length())));
}
	return oS.toString();
}
    public static void main( String[] args )
    {
	System.out.println("--Супер строка--");
	System.out.print("Введите длину строки: ");
	Scanner scanner = new Scanner(System.in);
	try {
		int length = Integer.parseInt(scanner.nextLine());
			if (length <= 0) {
			System.out.println("Длина должна быть > 0!");
			return;
		}
		System.out.println("Результат: " + generatorStrok(length));
		} catch (NumberFormatException e) {
		System.out.println("Ошибка! Введите целое число!");
		} finally {
			scanner.close();
}
}
}