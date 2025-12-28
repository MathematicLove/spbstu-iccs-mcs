package ru.spbstu.telematics.java;
import org.junit.Test;
import static org.junit.Assert.*;

public class JunitApp{
@Test
public void testGeneratorStrokL(){
	int length = 10;
	String rand = App.generatorStrok(length);
	assertEquals("Error! need int and > 0 length!", length, rand.length());
}

@Test
public void testGeneratorStrokA(){
	int length = 20;
	String rand = App.generatorStrok(length);
	String alphavit = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm0123456789";
	for(char c : rand.toCharArray()){
	assertTrue("Need only alphabet chars", alphavit.indexOf(c) >= 0);
}
}
}

