import java.util.Random;

public class ThreadSample extends Thread {
	public void run() {
		Random r = new Random();
		int x = r.nextInt(200);
		Main.q.insert(x);
	}
}