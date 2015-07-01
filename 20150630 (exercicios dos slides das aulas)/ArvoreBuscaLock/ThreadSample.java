import java.util.Random;

public class ThreadSample extends Thread {
	public void run() {
		Random r = new Random();
		int c = 0, x;

		while (c < 2000) {
			x = r.nextInt(100000);
			// if you want this running, change this to a number <1159
			// multiply 2 or more random numbers smaller than 1159 DO NOT work
			Main.t.insert(x);
			c++;
		}
	}
}