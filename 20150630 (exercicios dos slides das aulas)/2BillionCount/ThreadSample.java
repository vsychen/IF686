public class ThreadSample extends Thread {
	public void run() {
		try {
			while (Main.count <= 2000000000) {
				System.out.println(Main.count++);
				sleep(1);
			}
		} catch (InterruptedException e) {

		}
	}
}
